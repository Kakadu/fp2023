(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type type_var = int

type primitive_type =
  | Int
  | Bool
  | Unit
  | Char
  | String

val equal_primitive_type : primitive_type -> primitive_type -> bool
val pp_primitive_type : Format.formatter -> primitive_type -> unit
val show_primitive_type : primitive_type -> string

type typ =
  | TPrim of primitive_type (* int, bool, unit, char, string *)
  | TVar of type_var (* 'a, 'b, etc. *)
  | TArr of typ * typ (* 'a -> 'b *)
  | TTuple of typ list (* 'a * int * char *)
  | TList of typ (* 'a list *)
  | TEffect of typ (* 'a effect *)
  | TContinuePoint (* continue k 0 - here k type is continue point *)
  | TContinuation of
      typ (* expression 'continue k x' type is continuation ; typ = x type *)

module TVarSet : sig
  include module type of Set.Make (Int)
end

module VarSet : sig
  include module type of Set.Make (String)
end

type scheme = Scheme of TVarSet.t * typ

val tint : typ
val tbool : typ
val tunit : typ
val tchar : typ
val tstring : typ
val tvar : type_var -> typ
val tarrow : typ -> typ -> typ
val ttuple : typ list -> typ
val tlist : typ -> typ
val teffect : typ -> typ
val tcontinue_point : typ
val tcontinuation : typ -> typ
val ( @-> ) : typ -> typ -> typ
