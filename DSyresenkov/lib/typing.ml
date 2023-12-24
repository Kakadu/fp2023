(** Copyright 2021-2023, Ilya Syresenkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Set of variables *)
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
  | NoVariable of id (** Attempt to use non-initialized variable *)
  | UnificationFailed of ty * ty (** Failed to unify left and right types *)
  | PatternRebound (** Attempt to use in pattern single bound two times (h :: h :: tl) *)
  | NotImplemented (** Still not implemented features *)
[@@deriving show { with_path = false }]

let rec pp_type fmt typ =
  let open Format in
  let fmt_arrpw = function
    | TArrow _ -> format_of_string "(%a)"
    | _ -> format_of_string "%a"
  in
  match typ with
  | TBase x ->
    (match x with
     | BInt -> fprintf fmt "int"
     | BBool -> fprintf fmt "bool"
     | BUnit -> fprintf fmt "unit")
  | TTuple (ty1, ty2, tys) ->
    fprintf
      fmt
      "%a"
      (pp_print_list
         ~pp_sep:(fun _ _ -> fprintf fmt " * ")
         (fun fmt typ -> pp_type fmt typ))
      (ty1 :: ty2 :: tys)
  | TList typ -> fprintf fmt (fmt_arrpw typ ^^ " list") pp_type typ
  | TArrow (l, r) -> fprintf fmt (fmt_arrpw l ^^ " -> %a") pp_type l pp_type r
  | TVar var -> fprintf fmt "%s" @@ "'" ^ Char.escaped (Char.chr (var + 97))
;;

let print_ty typ =
  let s = Format.asprintf "%a" pp_type typ in
  Format.printf "%s\n" s
;;

type scheme = S of VarSet.t * ty
