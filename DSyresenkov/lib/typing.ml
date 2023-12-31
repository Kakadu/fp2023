(** Copyright 2021-2023, Ilya Syresenkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Set of variables *)
open Format

type id = string [@@deriving show { with_path = false }]

module VarSet = struct
  include Stdlib.Set.Make (Int)
end

(* Base types *)
type base_type =
  | BInt (** Basic integer type *)
  | BBool (** Basic bool type *)
  | BUnit (** Unit type *)
[@@deriving show { with_path = false }]

(* Types *)
type ty =
  | TBase of base_type (** Type of integer *)
  | TVar of int
  | TArrow of ty * ty (** Type of function ty1 -> ty2 *)
  | TTuple of ty * ty * ty list (** Type of tuple *)
  | TList of ty (** Type of list *)
[@@deriving show { with_path = false }]

type error =
  | OccursCheckFailed of int * ty (** OCaml's Occurs check *)
  | UndeclaredVariable of id (** Attempt to use non-initialized variable *)
  | UnificationFailed of ty * ty (** Failed to unify left and right types *)
  | NotImplemented (** Still not implemented features *)
[@@deriving show { with_path = false }]

let rec pp_ty fmt = function
  | TBase ty ->
    (match ty with
     | BInt -> fprintf fmt "int"
     | BBool -> fprintf fmt "bool"
     | BUnit -> fprintf fmt "unit")
  | TVar x -> fprintf fmt "'%d" x
  | TArrow (l, r) ->
    (match l, r with
     | TArrow (_, _), _ -> fprintf fmt "(%a) -> %a" pp_ty l pp_ty r
     | _, _ -> fprintf fmt "%a -> %a" pp_ty l pp_ty r)
  | TTuple (ty1, ty2, tys) ->
    fprintf
      fmt
      "%a"
      (pp_print_list
         ~pp_sep:(fun fmt _ -> fprintf fmt " * ")
         (fun fmt ty -> pp_ty fmt ty))
      (ty1 :: ty2 :: tys)
  | TList t -> fprintf fmt "%a list" pp_ty t
;;

type scheme = S of VarSet.t * ty
