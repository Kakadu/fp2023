(** Copyright 2021-2023, ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type tyvar = int [@@deriving show { with_path = false }]

type primitives =
  | Int
  | String
  | Char
  | Bool
  | Unit
[@@deriving show { with_path = false }]

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end
[@@deriving show { with_path = false }]

type typ =
  | TVar of tyvar (* 'a *)
  | TPrim of primitives (* int, bool, etc. *)
  | TArr of typ * typ (* 'a -> 'a *)
  | TTuple of typ list (* (int, string) *)
  | TList of typ (* int list *)
[@@deriving show { with_path = false }]

type scheme = Scheme of VarSet.t * typ [@@deriving show { with_path = false }]

type error =
  [ `OccursCheck
  | `NoVariable of string
  | `NoConstructor of string
  | `UnificationFailed of typ * typ
  ]
[@@deriving show { with_path = false }]

let tvar x = TVar x
let tint = TPrim Int
let tbool = TPrim Bool
let tchar = TPrim Char
let tunit = TPrim Unit
let tstring = TPrim String
let tarrow l r = TArr (l, r)
let ttuple x = TTuple x
let tlist x = TList x
let ( @-> ) = tarrow
