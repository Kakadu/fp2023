(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

module TyError = TyError

open! Base
open Monads.Std
open Types

open Constraints
open Gen
open Solve

(** Rename (to a, b ..) and quantify all type variables in a type *)
let close_over ty =
  let open Monad.State in
  let fresh =
    let* cnt = get () in
    let* () = put (cnt + 1) in

    (* try to use english letter. if out of bounds then "t{cnt}" *)
    let var =
      let default () = Var.Var ("t" ^ Int.to_string cnt) in
      (* 'a' = 97 *)
      match Char.of_int (cnt + 96) with
      | None ->
          default ()
      | Some ch when Char.( > ) ch 'z' ->
          default ()
      | Some ch ->
          Var.Var (Char.to_string ch)
    in
    return var
  in

  (* construct set of new variables
     and substitution that maps vars to new ones *)
  let m =
    VarSet.fold_right (Ty.vars ty)
      ~init:(return (VarSet.empty, Sub.empty))
      ~f:(fun var acc ->
        let* fresh_var = fresh in
        let single = Sub.singleton var (Ty_var fresh_var) in
        let* set, sub = acc in
        return (VarSet.add set fresh_var, Sub.compose single sub) )
  in
  let new_vars, subst = fst @@ Monad.State.run m 1 in

  (* quantify all type variables *)
  Scheme.Forall (new_vars, Sub.apply subst ty)

let infer_structure_item env str_item =
  let ( let* ) x f = Result.bind x ~f in
  let fail = Result.fail in
  let return = Result.return in

  let* asm, bound_vars, ty_res, gen_cs, con_assumpt = gen str_item in

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
            | Forall (_, Ty_arr (_, Ty_con (_, _))) ->
                assert_eq arity SomeArgs
            | _ ->
                mismatch )
        in

        (* add new constraints based on scheme from Env *)
        let new_cs =
          ConstrSet.of_list
          @@ List.map vars ~f:(fun var ->
                 Constr.ExplInstConstr (Ty_var var, sc) )
        in
        return @@ ConstrSet.union acc new_cs )
  in

  (* solve constrainsts *)
  let* sub = solve @@ ConstrSet.union gen_cs env_cs in
  let ty_res = Sub.apply sub ty_res in

  (* add new bounds to type environment *)
  let new_env =
    BoundVars.fold bound_vars ~init:env ~f:(fun ~key:id ~data:tv acc ->
        let ty = Sub.apply sub (Ty_var tv) in
        Env.set acc ~key:id ~data:(close_over ty) )
  in

  return (new_env, BoundVars.idents bound_vars, close_over ty_res)
