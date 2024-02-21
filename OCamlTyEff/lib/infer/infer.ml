(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

module TyError = TyError

open! Base
open Types

open Constraints
open Gen
open Solve
open CloseOver

let infer_structure_item env str_item =
  let ( let* ) x f = Result.bind x ~f in
  let fail = Result.fail in
  let return = Result.return in

  let types_arity = Env.get_types_arity env in
  let* asm, bound_vars, ty_res, eff, gen_cs, con_assumpt, defined_types =
    gen types_arity str_item
  in

  (* set arity in environment for newly defined types *)
  let env =
    Env.set_types_arity env
      (List.fold defined_types ~init:types_arity ~f:(fun acc {id; arity} ->
           Map.set acc ~key:id ~data:arity ) )
  in

  (* create new constraints based on type environment *)
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

  (* solve constraints *)
  let* sub = solve @@ ConstrSet.union gen_cs env_cs in
  let env = Sub.apply_to_env sub env in
  let is_ref = Eff.contains (Sub.apply_to_eff sub eff) (Eff.Label.ref ()) in

  (* add new bounds to type environment *)
  let env =
    BoundVars.fold bound_vars ~init:env ~f:(fun ~key:id ~data:tv acc ->
        let ty = Sub.apply_to_ty sub (Ty_var tv) in
        let ty, new_counter =
          close_over
            (if is_ref then weaken ty else ty)
            ~weak_counter:(Env.get_weak_counter acc)
        in
        let acc = Env.set_weak_counter acc new_counter in
        Env.set acc ~key:id ~data:ty )
  in

  let ty_res, env =
    match ty_res with
    | None ->
        (None, env)
    | Some ty ->
        let ty = Sub.apply_to_ty sub ty in
        let ty, counter =
          close_over
            (if is_ref then weaken ty else ty)
            ~weak_counter:(Env.get_weak_counter env)
        in
        (Some ty, Env.set_weak_counter env counter)
  in
  return (env, BoundVars.idents bound_vars, ty_res)
