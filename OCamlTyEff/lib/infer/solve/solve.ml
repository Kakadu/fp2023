module Sub = Sub

open! Base
open Types
open Constraints
open Unify

open SolveMonad.Solve
open SolveMonad.Let_syntax
open SolveMonad.Let

let generalize free ty = Scheme.Forall (VarSet.diff (Ty.vars ty) free, ty)

let instantiate (Scheme.Forall (quantified, ty)) =
  (* construct substitution that maps quantified vars to fresh vars *)
  let* subst =
    VarSet.fold quantified ~init:(return Sub.empty) ~f:(fun acc var ->
        let* fresh_var = fresh_var >>| fun tv -> Ty.Ty_var tv in
        let single = Sub.singleton var fresh_var in
        let* acc = acc in
        return @@ Sub.compose single acc )
  in
  return @@ Sub.apply subst ty

let activevars =
  let activevars_single = function
    | Constr.EqConstr (ty1, ty2) ->
        VarSet.union (Ty.vars ty1) (Ty.vars ty2)
    | ImplInstConstr (ty1, mset, ty2) ->
        VarSet.union (Ty.vars ty1) (VarSet.inter mset (Ty.vars ty2))
    | ExplInstConstr (ty, sc) ->
        VarSet.union (Ty.vars ty) (Scheme.free_vars sc)
  in
  ConstrSet.fold ~init:VarSet.empty ~f:(fun acc constr ->
      VarSet.union acc (activevars_single constr) )

let solve cs =
  let rec solve' cs =
    let next_solvable =
      ConstrSet.find_map cs ~f:(fun constr ->
          let rest = ConstrSet.remove cs constr in
          match constr with
          | EqConstr (_, _) | ExplInstConstr (_, _) ->
              Some (constr, rest)
          | ImplInstConstr (_, mset, t2)
            when VarSet.is_empty
                 @@ VarSet.inter (activevars rest)
                      (VarSet.diff (Ty.vars t2) mset) ->
              Some (constr, rest)
          | ImplInstConstr (_, _, _) ->
              None )
    in
    match next_solvable with
    | None ->
        (* no constraints left to solve *)
        return Sub.empty
    | Some constr -> (
      match constr with
      | EqConstr (t1, t2), cs ->
          let* s1 = unify t1 t2 in
          let* s2 = solve' (Sub.apply_to_constrs s1 cs) in
          return @@ Sub.compose s2 s1
      | ImplInstConstr (t1, mset, t2), cs ->
          solve' @@ ConstrSet.add cs (ExplInstConstr (t1, generalize mset t2))
      | ExplInstConstr (ty, sc), cs ->
          let* ty' = instantiate sc in
          solve' @@ ConstrSet.add cs (EqConstr (ty, ty')) )
  in
  SolveMonad.run @@ solve' cs
