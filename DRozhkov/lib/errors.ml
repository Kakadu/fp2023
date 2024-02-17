(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Typedtree

type error =
  | Occurs_check of int * typ
  | Unbound_value of string
  | Ivalid_format_concat of typ * typ

let pp_error fmt = function
  | Occurs_check (t, typ) ->
    fprintf fmt "The type variable '%d occurs inside %a" t pp_typ typ
  | Unbound_value x -> fprintf fmt "Unbound value %s" x
  | Ivalid_format_concat (l, r) ->
    fprintf fmt "Failed to unify types %a and %a" pp_typ l pp_typ r
;;
