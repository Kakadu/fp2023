open! Base
open GenStr
open Solve
open Types
open Constraints
open Ast
module Sub = Substitution
open SolveMonad.Solve
open SolveMonad.Let_syntax
open SolveMonad.Let

(* reexport module so it can be accessed outside *)
module TyError = TyError

let infer_structure_item env str_item =
  let (asm, ty), gen_cs = GenMonad.run (gen_str str_item) in
  let m =
    let* env_cs =
      Assumptions.fold asm ~init:(return ConstrSet.empty)
        ~f:(fun ~key:id ~data:vars acc ->
          let* acc = acc in
          (* try to find ident in type environment *)
          let* sc =
            let to_name (Ident.Ident name) = name in
            Env.find env id
            |> Option.value_map
                 ~default:(fail @@ UnboundVariable (to_name id))
                 ~f:return
          in
          (* add new constraints based on scheme from Env *)
          let new_cs =
            ConstrSet.of_list
            @@ List.map vars ~f:(fun var ->
                   Constr.ExplInstConstr (Ty_var var, sc) )
          in
          return @@ ConstrSet.union acc new_cs )
    in
    let* sub = solve @@ ConstrSet.union gen_cs env_cs in
    return (Sub.apply sub ty)
  in
  SolveMonad.run m
  |> Result.map ~f:(fun ty ->
         (* quantify all type variables *)
         let sc = Scheme.Forall (Ty.vars ty, ty) in
         match str_item with
         | Str_value (_, [{pat= Pat_var name; expr= _}]) ->
             (sc, Env.set env ~key:(Ident name) ~data:sc)
         | _ ->
             (sc, env) )
