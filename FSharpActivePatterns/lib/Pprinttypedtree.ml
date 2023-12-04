(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree
open Base

type 'a result =
  | Add of 'a
  | Same of 'a

(** Convert number to letter *)

let rec transform_binder binder =
  if 0 <= binder && binder < 26
  then (
    let letter = char_of_int (97 + binder) in
    Format.sprintf "'%c" letter)
  else (
    let div = binder / 26 in
    let remainder = binder mod 26 in
    let letter = transform_binder remainder in
    Format.sprintf "%s%d" letter div)
;;

let add map k v =
  match Map.add map ~key:k ~data:v with
  | `Ok a -> a
  | `Duplicate -> map
;;

let add_if_not_in map k v =
  match Map.find map k with
  | None -> Add (add map k v)
  | Some _ -> Same map
;;

let empty = Base.Map.empty (module Base.Int)

let unify2 map1 map2 =
  Map.fold map1 ~init:map2 ~f:(fun ~key ~data acc ->
    match add_if_not_in acc key data with
    | Add map | Same map -> map)
;;

let pp_list helper l sep =
  let open Format in
  pp_print_list ~pp_sep:(fun ppf _ -> fprintf ppf sep) (fun ppf ty -> helper ppf ty) l
;;

let pp_pv helper ppf l =
  let open Format in
  let c, l = l in
  match l with
  | [] -> fprintf ppf "%s" c
  | _ -> fprintf ppf "%s of %a" c (fun ppf -> pp_list helper ppf " * ") l
;;

(** Conver binder to letters and print type using letters *)
let pp_typ_letter ppf ty =
  let rec get_subs subs index =
    let list_helper isubs iindex l =
      List.fold l ~init:(isubs, iindex) ~f:(fun (s1, i1) ty ->
        let s2, i2 = get_subs s1 i1 ty in
        let subs = unify2 s1 s2 in
        subs, i2)
    in
    function
    | Ty_var n ->
      (match add_if_not_in subs n index with
       | Add subs -> subs, index + 1
       | Same subs -> subs, index)
    | Prim _ -> subs, index
    | Arrow (l, r) ->
      let s1, i1 = get_subs subs index l in
      let subs = unify2 subs s1 in
      let s2, i2 = get_subs subs i1 r in
      let subs = unify2 subs s2 in
      subs, i2
    | List t -> get_subs subs index t
    | Tuple ts -> list_helper subs index ts
  in
  let subs, _ = get_subs empty 0 ty in
  let open Format in
  let rec helper ppf = function
    | Ty_var n ->
      (match Map.find subs n with
       | Some v ->
         let letter = transform_binder v in
         fprintf ppf "%s" letter
       | None -> fprintf ppf "'_%d" n)
    | Prim s -> pp_print_string ppf s
    | Arrow (l, r) -> fprintf ppf "(%a -> %a)" helper l helper r
    | List t -> fprintf ppf "%a list" helper t
    | Tuple ts -> fprintf ppf "(%a)" (fun ppf -> pp_list helper ppf " * ") ts
  in
  helper ppf ty
;;

(** Print type using binders not letters *)

let rec pp_typ_binder ppf =
  let open Format in
  function
  | Ty_var n -> fprintf ppf "'_%d" n
  | Prim s -> pp_print_string ppf s
  | Arrow (l, r) -> fprintf ppf "(%a -> %a)" pp_typ_binder l pp_typ_binder r
  | List t -> fprintf ppf "%a list" pp_typ_binder t
  | Tuple ts -> fprintf ppf "(%a)" (fun ppf -> pp_list pp_typ_binder ppf " * ") ts
;;
