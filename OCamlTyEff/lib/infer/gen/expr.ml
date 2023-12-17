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

let rec gen = function
  | Exp_ident id ->
      let* var = fresh_var in
      return (As.singleton id var, !var)
  | Exp_constant const ->
      return (As.empty, type_of_constant const)
  | Exp_fun (args, e) ->
      let* as_pat, bounds_pat, tys_pat = Pattern.gen_many args in
      let* as_e, ty_e =
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

      let ty_res = List.fold_right tys_pat ~init:ty_e ~f:( @> ) in
      return (as_pat ++ (as_e -- Pattern.BoundVars.idents bounds_pat), ty_res)
  | Exp_apply (e_fun, e_arg) ->
      let* as_fun, ty_fun = gen e_fun in
      let* as_arg, ty_arg = gen e_arg in
      let* ty_res = fresh_var >>| ( ! ) in

      let* () = add_constrs [ty_fun == ty_arg @> ty_res] in
      return (as_fun ++ as_arg, ty_res)
  | Exp_let (Nonrecursive, bindings, e2) ->
      let* pat, e1 =
        match bindings with
        | [{pat; expr}] ->
            return (pat, expr)
        | _ ->
            fail @@ NotImplemented "several value bindings (and)"
      in

      let* as_pat, bounds_pat, ty_pat = Pattern.gen pat in
      let* as1, ty1 = gen e1 in
      let* as2, ty2 = gen e2 in

      let* () = add_constrs [ty_pat == ty1] in
      let* mset = varset in
      let constrs =
        Pattern.BoundVars.fold bounds_pat ~init:[]
          ~f:(fun ~key:id ~data:var_pat acc ->
            let cs =
              As.lookup as2 id
              |> List.map ~f:(fun var_expr ->
                     Constr.ImplInstConstr (!var_expr, mset, !var_pat) )
            in
            cs :: acc )
      in
      let* () = add_constrs (List.concat_no_order constrs) in

      return (as_pat ++ as1 ++ (as2 -- Pattern.BoundVars.idents bounds_pat), ty2)
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

      (* XXX: check rhs of let rec.
         e.g. `let rec x = x + 1 in ..` must be rejected *)
      let* as1, ty1 = gen e1 in
      let* as2, ty2 = gen e2 in

      let* () =
        add_constrs
          (As.lookup as1 id |> List.map ~f:(fun var_expr -> !var_expr == ty1))
      in

      let* mset = varset in
      let* () =
        add_constrs
          ( As.lookup as2 id
          |> List.map ~f:(fun var_expr ->
                 Constr.ImplInstConstr (!var_expr, mset, ty1) ) )
      in

      return (as1 ++ as2 -- [id], ty2)
  | Exp_ifthenelse (e_cond, e_th, e_el) ->
      let* as_cond, ty_cond = gen e_cond in
      let* as_th, ty_th = gen e_th in
      let* as_el, ty_el =
        match e_el with None -> return (As.empty, Ty.unit) | Some e -> gen e
      in

      let* () = add_constrs [ty_cond == Ty.bool; ty_th == ty_el] in
      return (as_cond ++ as_th ++ as_el, ty_th)
  | Exp_tuple exprs ->
      let* asm, tys = gen_many exprs in
      return (asm, Ty.Ty_tuple tys)
  | Exp_construct (con_id, con_arg) ->
      let* var_con = fresh_var in
      let as_con = As.singleton con_id var_con in
      let ty_con = !var_con in

      let* ty_res = fresh_var >>| ( ! ) in
      let* as_arg, constr =
        match con_arg with
        | None ->
            let* () = add_con_assumpt con_id NoArgs in
            return (As.empty, ty_con == ty_res)
        | Some con_arg ->
            let* () = add_con_assumpt con_id SomeArgs in
            let* as_arg, ty_arg = gen con_arg in
            return (as_arg, ty_con == ty_arg @> ty_res)
      in
      let* () = add_constrs [constr] in

      return (as_con ++ as_arg, ty_res)
  | Exp_sequence (e1, e2) ->
      let* as1, _ = gen e1 in
      let* as2, ty2 = gen e2 in
      return (as1 ++ as2, ty2)
  | Exp_match (e, cases) ->
      let* as_e, ty_e = gen e in

      let gen_case {left= pat; right= e_rhs} =
        let* as_pat, bounds_pat, ty_pat = Pattern.gen pat in
        let* as_rhs, ty_rhs = gen e_rhs in

        let* () = add_constrs [ty_pat == ty_e] in
        let* mset = varset in
        let constrs =
          Pattern.BoundVars.fold bounds_pat ~init:[]
            ~f:(fun ~key:id ~data:var_pat acc ->
              let cs =
                (* generalize in patterns *)
                As.lookup as_rhs id
                |> List.map ~f:(fun var_expr ->
                       Constr.ImplInstConstr (!var_expr, mset, !var_pat) )
              in
              cs :: acc )
        in
        let* () = add_constrs (List.concat_no_order constrs) in

        return
          (as_pat ++ (as_rhs -- Pattern.BoundVars.idents bounds_pat), ty_rhs)
      in

      let* ty_res = fresh_var >>| ( ! ) in
      let* as_cases =
        GenMonad.List.fold cases ~init:As.empty ~f:(fun acc case ->
            let* as_case, ty_case = gen_case case in
            let* () = add_constrs [ty_case == ty_res] in
            return (acc ++ as_case) )
      in

      return (as_e ++ as_cases, ty_res)
  | Exp_function cases ->
      let* ty_arg = fresh_var >>| ( ! ) in

      let gen_case {left= pat; right= e_rhs} =
        let* as_pat, bounds_pat, ty_pat = Pattern.gen pat in
        let* as_rhs, ty_rhs =
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
          (as_pat ++ (as_rhs -- Pattern.BoundVars.idents bounds_pat), ty_rhs)
      in

      let* ty_res = fresh_var >>| ( ! ) in
      let* as_cases =
        GenMonad.List.fold cases ~init:As.empty ~f:(fun acc case ->
            let* as_case, ty_case = gen_case case in
            let* () = add_constrs [ty_case == ty_res] in
            return (acc ++ as_case) )
      in

      return (as_cases, ty_arg @> ty_res)

and gen_many exprs =
  GenMonad.List.fold_right exprs ~init:(As.empty, []) ~f:(fun expr acc ->
      let* asm, ty = gen expr in
      return (As.merge asm (fst acc), ty :: snd acc) )
