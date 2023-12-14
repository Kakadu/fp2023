module TyError = TyError

open! Base
open Types

open Constraints
open Gen
open Solve

let ( let* ) x f = Result.bind x ~f
let fail = Result.fail
let return = Result.return

let infer_structure_item env str_item =
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
  let sc_res = Scheme.Forall (Ty.vars ty_res, ty_res) in

  (* add new bounds to type environment *)
  let new_env =
    BoundVars.fold bound_vars ~init:env ~f:(fun ~key:id ~data:tv acc ->
        let ty = Sub.apply sub (Ty_var tv) in
        let sc =
          (* quantify all type variables *)
          Scheme.Forall (Ty.vars ty, ty)
        in
        Env.set acc ~key:id ~data:sc )
  in

  return (new_env, BoundVars.idents bound_vars, sc_res)
