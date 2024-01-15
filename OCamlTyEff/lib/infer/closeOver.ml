(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Monads.Std

open Types
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
    | Ty_tuple tys | Ty_con (_, tys) ->
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

type rename_state = {cnt_ty: int; cnt_eff: int; cnt_weak: int; sub: Sub.t}

(** Rename (to a, b .., e, e1, .., _weak1, _weak2) all variables in a type.
    Accepts weak counter to rename weaks and returns it's possibly modified version *)
let rename_vars ty ~weak_counter =
  let open Monad.State in
  let next_weak_var cnt_weak =
    Var.Var_weak ("_weak" ^ Int.to_string cnt_weak)
  in

  (* Rename type variable to a, b, c, .. or _weak1, _weak2, .. for weaks *)
  let rename_ty_var ((Var.Var name | Var_weak name) as var) =
    let next_var cnt_ty =
      (* try to use english letter. if out of bounds then "t{cnt}" *)
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
    let* ({cnt_ty; cnt_weak; sub; _} as st) = get () in
    if
      Sub.mem sub var
      || (* do not rename weak var if already named properly.
            XXX: better way to do this? *)
      String.is_prefix name ~prefix:"_weak"
    then return ()
    else
      let fresh, cnt_ty, cnt_weak =
        match var with
        | Var _ ->
            (next_var cnt_ty, cnt_ty + 1, cnt_weak)
        | Var_weak _ ->
            (next_weak_var cnt_weak, cnt_ty, cnt_weak + 1)
      in
      put
        { st with
          cnt_ty
        ; cnt_weak
        ; sub= Sub.compose sub (Sub.singleton_ty var (Ty_var fresh)) }
  in

  (* Rename effect variable to e, e1, e2, .. or _weak1, _weak2, .. for weaks *)
  let rename_eff_var ((Var.Var name | Var_weak name) as var) =
    let next_var cnt_eff =
      let name = if cnt_eff = 0 then "e" else "e" ^ Int.to_string cnt_eff in
      Var.Var name
    in

    let* ({cnt_eff; cnt_weak; sub; _} as st) = get () in
    if Sub.mem sub var || String.is_prefix name ~prefix:"_weak" then return ()
    else
      let fresh, cnt_eff, cnt_weak =
        match var with
        | Var _ ->
            (next_var cnt_eff, cnt_eff + 1, cnt_weak)
        | Var_weak _ ->
            (next_weak_var cnt_weak, cnt_eff, cnt_weak + 1)
      in
      put
        { st with
          cnt_weak
        ; cnt_eff
        ; sub= Sub.compose sub (Sub.singleton_eff var (Eff_var fresh)) }
  in

  let rec rename_eff = function
    | Eff.Eff_var var ->
        rename_eff_var var
    | Eff_total ->
        return ()
    | Eff_row (_, eff_rest) ->
        rename_eff eff_rest
  in
  let rec rename_type = function
    | Ty.Ty_var var ->
        rename_ty_var var
    | Ty_arr (l, eff, r) ->
        let* () = rename_type l in
        let* () = rename_eff eff in
        rename_type r
    | Ty_tuple tys | Ty_con (_, tys) ->
        List.iter tys ~f:rename_type
  in

  let {cnt_weak= new_weak_counter; sub; _} =
    snd
    @@ run (rename_type ty)
         {cnt_ty= 1; cnt_eff= 0; cnt_weak= weak_counter; sub= Sub.empty}
  in
  (Sub.apply_to_ty sub ty, new_weak_counter)

let close_over ty ~weak_counter =
  let ty_closed = close_effs ty in
  let ty_renamed, new_counter = rename_vars ty_closed ~weak_counter in
  (generalize VarSet.empty ty_renamed, new_counter)

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

  let rec open_ty = function
    | Ty.Ty_arr (l, eff, r) ->
        let* new_vars_l, l_opened = open_ty l in
        let* new_vars_eff, eff_opened = open_eff eff in
        let* new_vars_r, r_opened = open_ty r in
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
        let* new_vars, ty_opened = open_ty ty in
        return (VarSet.union vars_acc new_vars, ty_opened :: tys_acc) )
  in

  let new_vars, ty_opened = fst @@ run (open_ty ty) 1 in
  Scheme.Forall (VarSet.union quantified new_vars, ty_opened)
