(** Copyright 2021-2023, ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* increment the counter when creating a new variable *)
type counter = int [@@deriving show { with_path = false }]

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type counter_set = VarSet.t [@@deriving show { with_path = false }]

type ty =
  | TVar of int (* 'a, 'b type *)
  | TPrim of string (* primitive types *)
  | TArr of ty * ty (* 'a -> 'a *)
  | TUnit
  | TTuple of ty list (* (int, string) *)
  | TList of ty (* int list *)
[@@deriving show { with_path = false }]

type scheme = Scheme of VarSet.t * ty [@@deriving show { with_path = false }]

(* let tvar x = TVar x
   let tint = TPrim "int"
   let tbool = TPrim "bool"
   let tchar = TPrim "char"
   let tstring = TPrim "string"
   let tunit = TUnit *)
