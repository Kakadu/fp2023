(** Copyright 2021-2023, Kakadu, RozhkovAleksandr*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format
open Typedtree

type error =
  | Occurs_check
  | No_variable of string
  | Unification_failed of typ * typ

let pp_error ppf : error -> _ = function
  | Occurs_check -> fprintf ppf "Occurs check failed"
  | No_variable s -> Format.fprintf ppf "Undefined variable: '%s'" s
  | Unification_failed (l, r) ->
    Format.fprintf ppf "Unification fail on %a and %a" pp_typ l pp_typ r
;;
