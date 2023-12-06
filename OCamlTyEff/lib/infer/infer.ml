open! Base
open GenExpr
open Solve
open Types
open Constraints
open Ast
module Sub = Substitution
open SolveMonad.Solve
open SolveMonad.Let_syntax
open SolveMonad.Let

let infer_expr env expr =
  let (asm, ty), gen_cs = GenMonad.run (gen_expr expr) in
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
