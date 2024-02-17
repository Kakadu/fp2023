(** Copyright 2023-2024, Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binder = int [@@deriving show { with_path = false }]

module VarSet = struct
  include Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

type ty =
  | TPrim of string (** int, string *)
  | TVar of binder (** Type variable *)
  | TArrow of ty * ty (** Function type *)
  | TTuple of ty list (** Tuple type *)
  | TList of ty (** List type *)

type scheme = S of VarSet.t * ty (** \forall a1 a2 ... an . ty *)

let arrow l r = TArrow (l, r)
let int_typ = TPrim "int"
let bool_typ = TPrim "bool"
let string_typ = TPrim "string"
let unit_typ = TPrim "unit"
let tuple_typ t = TTuple t
let list_typ t = TList t

(* TODO: fix func printing*)
let rec pp_typ ppf =
  let open Format in
  function
  | TVar n -> fprintf ppf "'_%d" n
  | TPrim s -> fprintf ppf "%s" s
  | TArrow (l, r) -> fprintf ppf "(%a -> %a)" pp_typ l pp_typ r
  | TTuple tl ->
    fprintf
      ppf
      "%a"
      (pp_print_list ~pp_sep:(fun _ _ -> fprintf ppf " * ") (fun ppf ty -> pp_typ ppf ty))
      tl
  | TList t -> fprintf ppf "%a list" pp_typ t
;;
