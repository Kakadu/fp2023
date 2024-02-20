(** Copyright 2023-2024, David Akhmedov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type type_variable_number = int

type ground =
  | GInt (** Int *)
  | GBool (** Bool *)
  | GUnit (** Unit — () *)
[@@deriving eq, show { with_path = false }]

type typ =
  | TVar of type_variable_number
  | TArr of typ * typ
  | TTuple of typ list
  | TList of typ
  | TGround of ground

let tint = TGround GInt
let tbool = TGround GBool
let tunit = TGround GUnit
let tarrow left_type right_type = TArr (left_type, right_type)
let ttuple type_list = TTuple type_list
let tlist typ = TList typ
let tvar n = TVar n

let rec pp_type fmt typ =
  let open Format in
  let arrow = function
    | TArr _ -> format_of_string "(%a)"
    | _ -> format_of_string "%a"
  in
  match typ with
  | TGround x ->
    (match x with
     | GInt -> fprintf fmt "int"
     | GBool -> fprintf fmt "bool"
     | GUnit -> fprintf fmt "Unit")
  | TVar var ->
    let ascii_code_of_a = 97 in
    fprintf fmt "%s" ("'" ^ Char.escaped (Char.chr (var + ascii_code_of_a)))
  | TTuple value_list ->
    let pp_tuple value_list =
      let pp_el fmt typ =
        let s =
          match typ with
          | TTuple _ -> format_of_string "(%a)"
          | _ -> format_of_string "%a"
        in
        fprintf fmt s pp_type typ
      in
      let pp_list fmt delimiter =
        pp_print_list
          ~pp_sep:(fun fmt _ -> fprintf fmt delimiter)
          (fun fmt value -> pp_el fmt value)
          fmt
      in
      pp_list fmt " * " value_list
    in
    pp_tuple value_list
  | TList typ ->
    fprintf
      fmt
      ((match typ with
        | TGround _ | TVar _ | TList _ -> "%a"
        | _ -> "(%a)")
       ^^ " list")
      pp_type
      typ
  | TArr (typ_left, typ_right) ->
    fprintf fmt (arrow typ_left ^^ " -> %a") pp_type typ_left pp_type typ_right
;;

let edit_numbers_in_typ typ =
  let empty = Base.Map.empty (module Base.Int) in
  let add map old_n new_n = Base.Map.update map old_n ~f:(fun _ -> new_n) in
  let lookup map key = Base.Map.find map key in
  let rec helper typ total map =
    match typ with
    | TVar x ->
      let n, total, map =
        match lookup map x with
        | None -> total, total + 1, add map x total
        | Some n -> n, total, map
      in
      TVar n, total, map
    | TGround _ -> typ, total, map
    | TTuple xs ->
      let res, total, map =
        List.fold_left
          (fun (acc, total, map) typ ->
            let typ, total, map = helper typ total map in
            typ :: acc, total, map)
          ([], total, map)
          xs
      in
      TTuple (List.rev res), total, map
    | TList ltyp ->
      let res, total, map = helper ltyp total map in
      TList res, total, map
    | TArr (l, r) ->
      let res_l, total, map = helper l total map in
      let res_r, total, map = helper r total map in
      TArr (res_l, res_r), total, map
  in
  let typ, _, _ = helper typ 0 empty in
  typ
;;

let print_typ fmt ?(carriage = false) typ =
  Format.fprintf
    fmt
    ("%s" ^^ if carriage then "\n" else "")
    (Format.asprintf "%a" pp_type (edit_numbers_in_typ typ))
;;

type error =
  | OccursCheck
  | UnboundValue of ident
  | MismatchValues of typ * typ (** For pattern matching errors *)
  | UnificationFailed of typ * typ
  | ParserAvoidedError
  (** Use the parser to get the AST: the parser does some transformations of expressions *)
  | WildcardNotExpected

let pp_error fmt err =
  let open Format in
  match err with
  | OccursCheck -> fprintf fmt "Occurs check failed"
  | MismatchValues (t1, t2) ->
    fprintf
      fmt
      "This pattern matches values of type %a but a pattern was expected which matches \
       values of type %a"
      pp_type
      t1
      pp_type
      t2
  | UnboundValue identifier -> fprintf fmt "Unbound value %s" identifier
  | UnificationFailed (t1, t2) ->
    fprintf
      fmt
      "This expression has type %a but an expression was expected of %a"
      pp_type
      t1
      pp_type
      t2
  | ParserAvoidedError ->
    fprintf
      fmt
      "Use the parser to get the AST: the parser does some transformations of expressions"
  | WildcardNotExpected -> fprintf fmt {| wildcard " _ " not expected |}
;;

let print_type_error error =
  let s = Format.asprintf "%a" pp_error error in
  Format.printf "Typecheck error: %s\n" s
;;
