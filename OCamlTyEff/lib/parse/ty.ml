(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Angstrom
open Types
open Common

(** ['a] *)
let parse_var =
  char '\'' *> (parse_lowercase_ident <|> parse_capitalized_ident)
  >>| fun name -> Ty.Ty_var (Var name)

(**
  typeconstr
  | ( typexpr { , typexpr } ) typeconstr
*)
let parse_constr pty =
  both
    (option [] (char '(' *> sep_by1 (ws *> char ',') pty <* ws <* char ')'))
    (ws *> parse_lowercase_ident)
  >>| fun (args, name) -> Ty.Ty_con (Ident name, args)

(**
  typexpr
  | typexpr typeconstr
*)
let parse_app pty =
  let parse_single =
    ws *> choice [parse_var; parse_constr pty; char '(' *> pty <* ws <* char ')']
  in
  let* arg = parse_single in
  option None (ws *> parse_lowercase_ident >>| Option.some)
  >>| Option.value_map ~default:arg ~f:(fun name -> Ty_con (Ident name, [arg]))

(* ======= Operators parsing ======= *)

type ty_infix_op = OpArrow | OpTuple

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

let parse_ty =
  let fold_infix acc (op, rhs) =
    match op with
    | OpArrow ->
        Ty.Ty_arr (acc, Eff.Eff_total, rhs)
    | OpTuple -> (
      match rhs with
      | Ty_tuple tl ->
          Ty_tuple (acc :: tl)
      | _ ->
          Ty_tuple [acc; rhs] )
  in
  fix (fun pty ->
      parse_operators
        ~infix:
          { peek= peek_infix_op
          ; get_binding_power= get_infix_binding_power
          ; fold= fold_infix }
        (parse_app pty) )
