(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Pretty printer goes here *)

open Ast
open Utils

let pp =
  let mangle t fmt x =
    if is_free_in x t then Format.fprintf fmt "%s" x else Format.fprintf fmt "_"
  in
  let rec pp fmt = function
    | Var s -> Format.fprintf fmt "%s" s
    | App (l, r) -> Format.fprintf fmt "(%a %a)" pp l pp r
    | Abs (x, Abs (y, Var z)) when x = z && y <> z -> Format.fprintf fmt "⊤"
    | Abs (x, Abs (y, Var z)) when y = z && x <> z -> Format.fprintf fmt "⊥"
    | Abs (f, Abs (x, Var z)) when x = z && x <> f -> Format.fprintf fmt "0"
    | Abs (f, Abs (x, App (Var g, Var z))) when x = z && x <> f && g = f ->
      Format.fprintf fmt "1"
    | Abs (f, Abs (x, App (Var g, App (Var h, Var z))))
      when x = z && x <> f && g = f && h = g -> Format.fprintf fmt "2"
    | Abs (v1, Abs (v2, Abs (v3, Abs (v4, t)))) ->
      Format.fprintf
        fmt
        "(λ %a %a %a %a -> %a)"
        (mangle t)
        v1
        (mangle t)
        v2
        (mangle t)
        v3
        (mangle t)
        v4
        pp
        t
    | Abs (v1, Abs (v2, Abs (v3, t))) ->
      Format.fprintf
        fmt
        "(λ %a %a %a -> %a)"
        (mangle t)
        v1
        (mangle t)
        v2
        (mangle t)
        v3
        pp
        t
    | Abs (v1, Abs (v2, t)) ->
      Format.fprintf fmt "(λ %a %a -> %a)" (mangle t) v1 (mangle t) v2 pp t
    | Abs (x, t) -> Format.fprintf fmt "(λ %a -> %a)" (mangle t) x pp t
  in
  pp
;;
