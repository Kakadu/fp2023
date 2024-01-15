(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Types
open Constraints
open Ast

open Common
module As = Assumptions

open GenMonad.Gen
open GenMonad.Let_syntax
open GenMonad.Let

let rec gen : expression -> (As.t * Ty.t * Eff.t) GenMonad.t = function
  | Exp_ident id ->
      let* var = fresh_var in
      let* eff = fresh_eff in
      return (As.singleton id var, !var, eff)
  | Exp_constant const ->
      let* eff = fresh_eff in
      return (As.empty, type_of_constant const, eff)
  | Exp_fun (args, e) ->
      let* as_pat, bounds_pat, tys_pat = Pattern.gen_many args in
      let* as_e, ty_e, eff_e =
        extend_varset (Pattern.BoundVars.vars bounds_pat) (gen e)
      in

      let constrs =
        Pattern.BoundVars.fold bounds_pat ~init:[]
          ~f:(fun ~key:id ~data:var_pat acc ->
            let cs =
              As.lookup as_e id
              |> List.map ~f:(fun var_expr -> !var_expr == !var_pat)
            in
            cs :: acc )
      in
      let* () = add_constrs (List.concat_no_order constrs) in

      let ty_arg_last, tys_arg_rest =
        (List.last_exn tys_pat, List.drop_last_exn tys_pat)
      in
      let* ty_res =
        GenMonad.List.fold_right tys_arg_rest
          ~init:(Ty_arr (ty_arg_last, eff_e, ty_e))
          ~f:(fun arg acc ->
            let* eff = fresh_eff in
            return @@ Ty.Ty_arr (arg, eff, acc) )
      in

      let* eff = fresh_eff in
      return
        (as_pat ++ (as_e -- Pattern.BoundVars.idents bounds_pat), ty_res, eff)
  | Exp_apply (e_fun, e_arg) ->
      let* as_fun, ty_fun, eff_fun = gen e_fun in
      let* as_arg, ty_arg, eff_arg = gen e_arg in
      let* ty_res = fresh_var >>| ( ! ) in

      let* () =
        add_constrs
          [ty_fun == Ty_arr (ty_arg, eff_arg, ty_res); eff_fun === eff_arg]
      in
      return (as_fun ++ as_arg, ty_res, eff_arg)
  | Exp_let (Nonrecursive, bindings, e2) ->
      let* pat, e1 =
        match bindings with
        | [{pat; expr}] ->
            return (pat, expr)
        | _ ->
            fail @@ NotImplemented "several value bindings (and)"
      in

      let* as_pat, bounds_pat, ty_pat = Pattern.gen pat in
      let* as1, ty1, eff1 = gen e1 in
      let* as2, ty2, eff2 = gen e2 in

      let* () =
        add_constrs
          [ ty_pat == ty1
          ; (* unify eff1 & eff2 only after ImplInstConstrs with eff1 are solved *)
            EffEqConstr (eff1, eff2, EffEq_Late) ]
      in
      let* mset = varset in
      let constrs =
        Pattern.BoundVars.fold bounds_pat ~init:[]
          ~f:(fun ~key:id ~data:var_pat acc ->
            let cs =
              As.lookup as2 id
              |> List.map ~f:(fun var_expr ->
                     Constr.ImplInstConstr (!var_expr, mset, !var_pat, eff1) )
            in
            cs :: acc )
      in
      let* () = add_constrs (List.concat_no_order constrs) in

      return
        ( as_pat ++ as1 ++ (as2 -- Pattern.BoundVars.idents bounds_pat)
        , ty2
        , eff2 )
  | Exp_let (Recursive, bindings, e2) ->
      let* id, e1 =
        match bindings with
        | [{pat; expr}] -> (
          match pat with
          | Pat_var id ->
              return (id, expr)
          | _ ->
              fail NotVarLHSRec )
        | _ ->
            fail @@ NotImplemented "mutually recursive bindings"
      in
      let* () = check_rec_rhs id e1 in

      let* as1, ty1, eff1 = gen e1 in
      let* as2, ty2, eff2 = gen e2 in

      let* () =
        add_constrs
          ( As.lookup as1 id
          |> List.map ~f:(fun var_expr ->
                 (* must not unify effects in `!var_expr` & `ty1`
                    to ensure proper type of `id` *)
                 Constr.TyEqConstr (!var_expr, ty1, Dont_unify_eff) ) )
      in

      let* () =
        add_constrs
          [ (* unify eff1 & eff2 only after ImplInstConstrs with eff1 are solved *)
            EffEqConstr (eff1, eff2, EffEq_Late) ]
      in
      let* mset = varset in
      let* () =
        add_constrs
          ( As.lookup as2 id
          |> List.map ~f:(fun var_expr ->
                 Constr.ImplInstConstr (!var_expr, mset, ty1, eff1) ) )
      in

      return (as1 ++ as2 -- [id], ty2, eff2)
  | Exp_ifthenelse (e_cond, e_th, e_el) ->
      let* as_cond, ty_cond, eff_cond = gen e_cond in
      let* as_th, ty_th, eff_th = gen e_th in
      let* as_el, ty_el, eff_el =
        match e_el with
        | None ->
            let* eff = fresh_eff in
            return (As.empty, Ty.unit, eff)
        | Some e ->
            gen e
      in

      let* () =
        add_constrs
          [ ty_cond == Ty.bool
          ; ty_th == ty_el
          ; eff_cond === eff_th
          ; eff_th === eff_el ]
      in
      return (as_cond ++ as_th ++ as_el, ty_th, eff_th)
  | Exp_tuple exprs ->
      let* asm, tys, eff = gen_many exprs in
      return (asm, Ty.Ty_tuple tys, eff)
  | Exp_construct (con_id, con_arg) ->
      let* var_con = fresh_var in
      let as_con = As.singleton con_id var_con in
      let ty_con = !var_con in

      let* ty_res = fresh_var >>| ( ! ) in
      let* as_arg, eff_res =
        match con_arg with
        | None ->
            let* () = add_con_assumpt con_id NoArgs in

            let* () = add_constrs [ty_con == ty_res] in
            let* eff = fresh_eff in
            return (As.empty, eff)
        | Some con_arg ->
            let* () = add_con_assumpt con_id SomeArgs in

            let* as_arg, ty_arg, eff_arg = gen con_arg in
            let* () =
              add_constrs [ty_con == Ty_arr (ty_arg, eff_arg, ty_res)]
            in
            return (as_arg, eff_arg)
      in

      return (as_con ++ as_arg, ty_res, eff_res)
  | Exp_sequence (e1, e2) ->
      let* as1, _, eff1 = gen e1 in
      let* as2, ty2, eff2 = gen e2 in
      let* () = add_constrs [eff1 === eff2] in
      return (as1 ++ as2, ty2, eff1)
  | Exp_match (e, cases) ->
      let* as_e, ty_e, eff_e = gen e in

      let gen_case {left= pat; right= e_rhs} =
        let* as_pat, bounds_pat, ty_pat = Pattern.gen pat in
        let* as_rhs, ty_rhs, eff_rhs = gen e_rhs in

        let* () = add_constrs [ty_pat == ty_e] in
        let* mset = varset in
        let constrs =
          Pattern.BoundVars.fold bounds_pat ~init:[]
            ~f:(fun ~key:id ~data:var_pat acc ->
              let cs =
                (* generalize in patterns *)
                As.lookup as_rhs id
                |> List.map ~f:(fun var_expr ->
                       Constr.ImplInstConstr (!var_expr, mset, !var_pat, eff_e) )
              in
              cs :: acc )
        in
        let* () = add_constrs (List.concat_no_order constrs) in

        return
          ( as_pat ++ (as_rhs -- Pattern.BoundVars.idents bounds_pat)
          , ty_rhs
          , eff_rhs )
      in

      let* ty_res = fresh_var >>| ( ! ) in
      let* as_cases =
        GenMonad.List.fold cases ~init:As.empty ~f:(fun acc case ->
            let* as_case, ty_case, eff_case = gen_case case in
            let* () =
              add_constrs
                [ ty_case == ty_res
                ; (* unify eff_case & eff_e only after ImplInstConstrs with eff_e are solved *)
                  EffEqConstr (eff_case, eff_e, EffEq_Late) ]
            in
            return (acc ++ as_case) )
      in

      return (as_e ++ as_cases, ty_res, eff_e)
  | Exp_try (e, cases) ->
      let* as_e, ty_e, eff_e = gen e in
      let* eff_res = fresh_eff in

      let* as_cases, eff =
        GenMonad.List.fold_right cases ~init:(As.empty, eff_res)
          ~f:(fun {left= pat; right= e_rhs} (as_acc, eff_acc) ->
            let* as_pat, _, ty_pat = Pattern.gen pat in
            let* as_rhs, ty_rhs, eff_rhs = gen e_rhs in

            let* exn_type = fresh_var >>| ( ! ) in
            let* () =
              add_constrs
                [ty_rhs == ty_e; ty_pat == Ty.exn exn_type; eff_rhs === eff_res]
            in

            return
              ( as_acc ++ as_pat ++ as_rhs
              , Eff.Eff_row (Eff.Label.exn exn_type, eff_acc) ) )
      in
      let* () = add_constrs [eff_e === eff] in

      return (as_e ++ as_cases, ty_e, eff_res)
  | Exp_function cases ->
      let* ty_arg = fresh_var >>| ( ! ) in

      let gen_case {left= pat; right= e_rhs} =
        let* as_pat, bounds_pat, ty_pat = Pattern.gen pat in
        let* as_rhs, ty_rhs, eff_rhs =
          extend_varset (Pattern.BoundVars.vars bounds_pat) (gen e_rhs)
        in

        let* () = add_constrs [ty_pat == ty_arg] in
        let constrs =
          Pattern.BoundVars.fold bounds_pat ~init:[]
            ~f:(fun ~key:id ~data:var_pat acc ->
              let cs =
                As.lookup as_rhs id
                |> List.map ~f:(fun var_expr -> !var_expr == !var_pat)
              in
              cs :: acc )
        in
        let* () = add_constrs (List.concat_no_order constrs) in

        return
          ( as_pat ++ (as_rhs -- Pattern.BoundVars.idents bounds_pat)
          , ty_rhs
          , eff_rhs )
      in

      let* ty_res = fresh_var >>| ( ! ) in
      let* eff_res = fresh_eff in
      let* as_cases =
        GenMonad.List.fold cases ~init:As.empty ~f:(fun acc case ->
            let* as_case, ty_case, eff_case = gen_case case in
            let* () = add_constrs [ty_case == ty_res; eff_case === eff_res] in
            return (acc ++ as_case) )
      in

      let* eff = fresh_eff in
      return (as_cases, Ty.Ty_arr (ty_arg, eff_res, ty_res), eff)

and gen_many exprs =
  let* eff_res = fresh_eff in
  let* as_res, tys_res =
    GenMonad.List.fold_right exprs ~init:(As.empty, []) ~f:(fun expr acc ->
        let* asm, ty, eff = gen expr in
        let* () = add_constrs [eff === eff_res] in
        return (As.merge asm (fst acc), ty :: snd acc) )
  in
  return (as_res, tys_res, eff_res)
