(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module VarSetInit : sig
  type elt = int
  type t

  val empty : t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val pp : Format.formatter -> t -> unit
end

type typ =
  | Prim of string (** Available ground types *)
  | Ty_var of int (** a, b types *)
  | Arrow of typ * typ (** Function type: 'a -> 'a *)
  | List_typ of typ (** List type: int list *)
  | Tuple_typ of typ list (** Typle type: [int, string] means (int, string) *)

val pp_typ_binder : Format.formatter -> typ -> unit
