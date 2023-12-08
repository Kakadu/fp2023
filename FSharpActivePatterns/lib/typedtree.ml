(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binder = int [@@deriving show]
type id = string [@@deriving show { with_path = false }]

module VarSetInit = struct
  include Caml.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type ac = id * typ list [@@deriving show { with_path = false }]

and typ =
  | Prim of string (** Available ground types *)
  | Ty_var of binder (** 'a, 'b types *)
  | Arrow of typ * typ (** Function type: 'a -> 'a *)
  | List of typ (** List type: int list *)
  | Tuple of typ list (** Typle type: [int, string] means (int, string) *)
  | ActiveCases of binder * ac list
[@@deriving show { with_path = false }]

(** Type constructors *)

let arrow l r = Arrow (l, r)
let list_typ x = List x
let tuple_typ x = Tuple x
let cases_typ x v = ActiveCases (x, v)
let ( @-> ) = arrow
