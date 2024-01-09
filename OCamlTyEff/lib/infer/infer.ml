(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

module TyError = TyError

open! Base
open Monads.Std
open Types

open Constraints
open Gen
open Solve

(**
  Close effects in a type, i.e. remove effect variables that appear only once
  E.g. 1) [int -'e-> int] ---> [int -> int]
       2) [(int -'e1-> int) -'e2-> int -'e1-> int]
            ---> [(int -'e1-> int) -> int -'e1-> int]
*)
let close_effs ty =
  let rec eff_vars = function
    | Ty.Ty_arr (l, eff, r) ->
        List.concat [eff_vars l; VarSet.to_list (Eff.vars eff); eff_vars r]
    | Ty_tuple tys ->
        List.map ~f:eff_vars tys |> List.concat
    | Ty_con (_, tys) ->
        List.map ~f:eff_vars tys |> List.concat
    | Ty_var _ ->
        []
  in
  let ty_eff_vars = eff_vars ty in
  let subst =
    List.fold ty_eff_vars ~init:Sub.empty ~f:(fun sub elt ->
        match elt with
        | Var_eff var when List.count ty_eff_vars ~f:(VarSet.Elt.equal elt) = 1
          ->
            Sub.compose (Sub.singleton_eff var Eff_total) sub
        | _ ->
            sub )
  in
  Sub.apply_to_ty subst ty

(** Close effects, rename (to a, b ..) and quantify all variables in a type *)
let close_over ty =
  let ty = close_effs ty in

  let open Monad.State in
  let fresh_ty_var =
    let* cnt_ty, cnt_eff = get () in
    let* () = put (cnt_ty + 1, cnt_eff) in

    (* try to use english letter. if out of bounds then "t{cnt}" *)
    let var =
      let default () = Var.Var ("t" ^ Int.to_string cnt_ty) in
      (* 'a' = 97 *)
      match Char.of_int (cnt_ty + 96) with
      | None ->
          default ()
      | Some ch when Char.( > ) ch 'z' ->
          default ()
      | Some ch ->
          Var.Var (Char.to_string ch)
    in
    return var
  in
  let fresh_eff_var =
    let* cnt_ty, cnt_eff = get () in
    let* () = put (cnt_ty, cnt_eff + 1) in
    let name = if cnt_eff = 0 then "e" else "e" ^ Int.to_string cnt_eff in
    return @@ Var.Var name
  in

  (* construct set of new variables
     and substitution that maps vars to new ones *)
  let m =
    VarSet.fold_right (Ty.vars ty)
      ~init:(return (VarSet.empty, Sub.empty))
      ~f:(fun elt acc ->
        let* fresh, single =
          match elt with
          | Var_ty var ->
              let* fresh = fresh_ty_var in
              return
                (VarSet.Elt.Var_ty fresh, Sub.singleton_ty var (Ty_var fresh))
          | Var_eff var ->
              let* fresh = fresh_eff_var in
              return
                (VarSet.Elt.Var_eff fresh, Sub.singleton_eff var (Eff_var fresh))
        in

        let* set, sub = acc in
        return (VarSet.add set fresh, Sub.compose single sub) )
  in
  let new_vars, subst = fst @@ Monad.State.run m (1, 0) in
  (* quantify all type variables *)
  Scheme.Forall (new_vars, Sub.apply_to_ty subst ty)

(**
  Open effects in a type, i.e. add polymorphic tail to all effects
  E.g. 1) [int -> int] ---> ['e. int -'e-> int]
       2) [string -[console]-> unit] ---> ['e. string -[console | 'e]-> unit]
*)
let open_effs (Scheme.Forall (quantified, ty)) =
  let open Monad.State in
  let fresh_var =
    let* cnt = get () in
    let* () = put (cnt + 1) in
    return @@ Var.Var ("open" ^ Int.to_string cnt)
  in

  let rec open_eff = function
    | Eff.Eff_total ->
        let* fresh = fresh_var in
        return (VarSet.singleton_eff fresh, Eff.Eff_var fresh)
    | Eff_var _ as eff ->
        return (VarSet.empty, eff)
    | Eff_row (lbl, eff_rest) ->
        let* new_vars, eff_rest_opened = open_eff eff_rest in
        return (new_vars, Eff.Eff_row (lbl, eff_rest_opened))
  in

  let rec helper = function
    | Ty.Ty_arr (l, eff, r) ->
        let* new_vars_l, l_opened = helper l in
        let* new_vars_eff, eff_opened = open_eff eff in
        let* new_vars_r, r_opened = helper r in
        return
          ( VarSet.union_list [new_vars_l; new_vars_eff; new_vars_r]
          , Ty.Ty_arr (l_opened, eff_opened, r_opened) )
    | Ty_tuple tys ->
        let* new_vars, tys_opened = open_many tys in
        return (new_vars, Ty.Ty_tuple tys_opened)
    | Ty_con (id, tys) ->
        let* new_vars, tys_opened = open_many tys in
        return (new_vars, Ty.Ty_con (id, tys_opened))
    | Ty_var _ as ty ->
        return (VarSet.empty, ty)
  and open_many =
    List.fold_right ~init:(VarSet.empty, []) ~f:(fun ty (vars_acc, tys_acc) ->
        let* new_vars, ty_opened = helper ty in
        return (VarSet.union vars_acc new_vars, ty_opened :: tys_acc) )
  in

  let new_vars, ty_opened = fst @@ Monad.State.run (helper ty) 1 in
  Scheme.Forall (VarSet.union quantified new_vars, ty_opened)

let infer_structure_item env str_item =
  let ( let* ) x f = Result.bind x ~f in
  let fail = Result.fail in
  let return = Result.return in

  let* asm, bound_vars, ty_res, _, gen_cs, con_assumpt = gen str_item in

  (* create new constrainsts based on type environment *)
  let* env_cs =
    Assumptions.fold asm ~init:(return ConstrSet.empty)
      ~f:(fun ~key:id ~data:vars acc ->
        let* acc = acc in

        (* try to find ident in type environment *)
        let* sc =
          Env.find env id
          |> Option.value_map
               ~default:(fail @@ TyError.UnboundVariable id)
               ~f:return
        in

        (* check that constructors are applied *)
        let* () =
          let mismatch = fail @@ TyError.ConstructorArityMismatch id in
          let assert_eq ar1 ar2 =
            if ConArityAssumpt.equal_arity ar1 ar2 then return () else mismatch
          in

          match ConArityAssumpt.find con_assumpt id with
          | None ->
              (* not a constructor *)
              return ()
          | Some arity -> (
            match sc with
            | Forall (_, Ty_con (_, _)) ->
                assert_eq arity NoArgs
            | Forall (_, Ty_arr (_, _, Ty_con (_, _))) ->
                assert_eq arity SomeArgs
            | _ ->
                mismatch )
        in

        (* add new constraints based on opened scheme from Env *)
        let sc_opened = open_effs sc in
        let new_cs =
          ConstrSet.of_list
          @@ List.map vars ~f:(fun var ->
                 Constr.ExplInstConstr (Ty_var var, sc_opened) )
        in
        return @@ ConstrSet.union acc new_cs )
  in

  (* solve constrainsts *)
  let* sub = solve @@ ConstrSet.union gen_cs env_cs in
  let ty_res = Sub.apply_to_ty sub ty_res in

  (* add new bounds to type environment *)
  let new_env =
    BoundVars.fold bound_vars ~init:env ~f:(fun ~key:id ~data:tv acc ->
        let ty = Sub.apply_to_ty sub (Ty_var tv) in
        Env.set acc ~key:id ~data:(close_over ty) )
  in

  return (new_env, BoundVars.idents bound_vars, close_over ty_res)
