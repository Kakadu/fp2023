(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving eq, show { with_path = false }]

module VarSetInit = struct
  include Caml.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type typ =
  | Prim of string (** Available ground types *)
  | Ty_var of int (** a, b types *)
  | Arrow of typ * typ (** Function type: 'a -> 'a *)
  | List_typ of typ (** List type: int list *)
  | Tuple_typ of typ list (** Typle type: [int, string] means (int, string) *)
[@@deriving show { with_path = false }]

let pp_list helper l sep =
  let open Format in
  pp_print_list ~pp_sep:(fun ppf _ -> fprintf ppf sep) (fun ppf ty -> helper ppf ty) l
;;

let rec pp_typ_binder ppf =
  let open Format in
  function
  | Ty_var n -> fprintf ppf "%d" n
  | Prim s -> pp_print_string ppf s
  | Arrow (l, r) -> fprintf ppf "(%a -> %a)" pp_typ_binder l pp_typ_binder r
  | List_typ t -> fprintf ppf "%a list" pp_typ_binder t
  | Tuple_typ ts -> fprintf ppf "(%a)" (fun ppf -> pp_list pp_typ_binder ppf " * ") ts
;;
