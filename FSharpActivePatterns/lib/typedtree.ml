(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binder = int [@@deriving show { with_path = false }]

module VarSetInit = struct
  include Caml.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end


(** Represent the type of expression *)
type typ =
  | Prim of string (** Represents available ground types *)
  | Ty_var of binder (** Represents 'a, 'b types *)
  | Arrow of typ * typ (** Represents function type: 'a -> 'a *)
  | List of typ (** Represents list type: int list *)
  | Tuple of typ list (** Represents typle type: [int, string] means (int, string) *)
[@@deriving show { with_path = false }]

(** Type constructors *)

let arrow l r = Arrow (l, r)
let list_typ x = List x 
let tuple_typ x = Tuple x
let ( @-> ) = arrow
