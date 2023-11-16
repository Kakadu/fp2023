(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Angstrom
open Ast
open Common

(** ['a] *)
let parse_var =
  char '\'' *> (parse_lowercase_ident <|> parse_capitalized_ident)
  >>| fun name -> Typ_var name

(**
  typeconstr
  | ( typexpr { , typexpr } ) typeconstr
*)
let parse_constr ptyp =
  both
    (option [] (char '(' *> sep_by1 (ws *> char ',') ptyp <* ws <* char ')'))
    (ws *> parse_lowercase_ident)
  >>| fun (args, name) -> Typ_constr (Ident name, args)

(**
  typexpr
  | typexpr typeconstr
*)
let parse_app ptyp =
  let parse_single =
    ws
    *> choice [parse_var; parse_constr ptyp; char '(' *> ptyp <* ws <* char ')']
  in
  let* arg = parse_single in
  option None (ws *> parse_lowercase_ident >>| Option.some)
  >>| Option.value_map ~default:arg ~f:(fun name ->
          Typ_constr (Ident name, [arg]) )

(* ======= Operators parsing ======= *)

type typ_infix_op = OpArrow | OpTuple

let peek_infix_op =
  let peek_tuple_op =
    peek_char_fail
    >>= fun c ->
    if Char.equal c '*' then return {op= OpTuple; op_length= 1}
    else fail "not a type tuple operator"
  in
  let peek_arrow_op =
    peek_string 2
    >>= fun s ->
    if String.equal s "->" then return {op= OpArrow; op_length= 2}
    else fail "not a type arrow operator"
  in
  peek_tuple_op <|> peek_arrow_op

let get_infix_binding_power = function OpArrow -> (2, 1) | OpTuple -> (4, 3)

let parse_typ =
  let fold_infix acc (op, rhs) =
    match op with
    | OpArrow ->
        Typ_arrow (acc, rhs)
    | OpTuple -> (
      match rhs with
      | Typ_tuple tl ->
          Typ_tuple (acc :: tl)
      | _ ->
          Typ_tuple [acc; rhs] )
  in
  fix (fun ptyp ->
      parse_operators
        ~infix:
          { peek= peek_infix_op
          ; get_binding_power= get_infix_binding_power
          ; fold= fold_infix }
        (parse_app ptyp) )
