(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree
open Base

let pp_list helper l sep =
  let open Format in
  pp_print_list ~pp_sep:(fun ppf _ -> fprintf ppf sep) (fun ppf ty -> helper ppf ty) l
;;

let pp_acase helper ppf l =
  let open Format in
  let c, l = l in
  match l with
  | [] -> fprintf ppf "%s" c
  | _ -> fprintf ppf "%s of %a" c (fun ppf -> pp_list helper ppf " * ") l
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
  | ActiveCases (_, ts) ->
    fprintf ppf "[ %a ]" (fun ppf -> pp_list (pp_acase pp_typ_binder) ppf " | ") ts
;;
