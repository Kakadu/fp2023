module TyError = TyError

open! Base
open Types
open Ast

open Constraints
open Gen
open Solve

let ( let* ) x f = Result.bind x ~f

let fail = Result.fail

open Result.Let_syntax

let infer_structure_item env str_item =
  let* asm, ty, gen_cs, con_assumpt = gen str_item in

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
          let fail = fail @@ TyError.ConstructorArityMismatch id in
          let assert_eq ar1 ar2 =
            if ConArityAssumpt.equal_arity ar1 ar2 then return () else fail
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
                fail )
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
  let ty = Sub.apply sub ty in

  (* quantify all type variables *)
  let sc = Scheme.Forall (Ty.vars ty, ty) in
  match str_item with
  | Str_value (_, [{pat= Pat_var name; expr= _}]) ->
      return (sc, Env.set env ~key:(Ident name) ~data:sc)
  | _ ->
      return (sc, env)
