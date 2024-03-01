(** Copyright 2023-2024, Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binder = int

type ty =
  | TPrim of string (** int, string *)
  | TVar of binder (** Type variable *)
  | TArrow of ty * ty (** Function type *)
  | TTuple of ty list (** Tuple type *)
  | TList of ty (** List type *)

let arrow l r = TArrow (l, r)
let int_typ = TPrim "int"
let bool_typ = TPrim "bool"
let string_typ = TPrim "string"
let unit_typ = TPrim "unit"
let tuple_typ t = TTuple t
let list_typ t = TList t

let rec pp_typ ppf =
  let open Format in
  function
  | TVar n -> fprintf ppf "'%d" n
  | TPrim s -> fprintf ppf "%s" s
  | TArrow (l, r) ->
    (match l with
     | TArrow _ -> fprintf ppf "(%a) -> %a" pp_typ l pp_typ r
     | _ -> fprintf ppf "%a -> %a" pp_typ l pp_typ r)
  | TTuple tl ->
    fprintf
      ppf
      "(%a)"
      (pp_print_list
         ~pp_sep:(fun _ _ -> fprintf ppf " * ")
         (fun ppf ty ->
           match ty with
           | TArrow _ -> fprintf ppf "(%a)" pp_typ ty
           | _ -> fprintf ppf "%a" pp_typ ty))
      tl
  | TList t -> fprintf ppf "%a list" pp_typ t
;;
