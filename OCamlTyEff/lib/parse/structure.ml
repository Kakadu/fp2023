(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Angstrom
open Ast
open Common
open Expr
open Pattern

(**
  [let P1 = E1 and P2 = E2 and ...]
  [let rec ValName1 PArg1 = E1 and P1 = E2 and ...]
*)
let parse_str_let =
  parse_let_binding parse_expression parse_pattern
  >>| fun (rec_flag, bindings) -> Str_value (rec_flag, bindings)

(** [A of string] *)
let parse_con_decl =
  lift2
    (fun name arg -> {id= Ident name; arg})
    parse_constr_name
    ( option None (ws *> string "of" >>| Option.some)
    >>= function None -> return None | Some _ -> Ty.parse_ty >>| Option.some )

(** [('a, 'b, 'c)] *)
let parse_type_params =
  let comma = ws *> char ',' *> ws in
  let parse_multiple =
    char '(' *> ws *> sep_by1 comma Ty.parse_var <* ws <* char ')'
  in
  let parse_single = Ty.parse_var >>| fun var -> [var] in
  parse_single <|> parse_multiple

(** [type ('a, 'b) ab = A | B of T1 ...] *)
let parse_str_type =
  let pipe = ws *> char '|' *> ws in
  string "type"
  *> lift3
       (fun params name variants -> Str_type {id= Ident name; params; variants})
       (ws *> option [] parse_type_params)
       (ws *> parse_lowercase_ident)
       (ws *> string "=" *> ws *> sep_by1 pipe parse_con_decl)

(** [exception Some_exc of string] *)
let parse_str_exception =
  string "exception" *> ws *> parse_con_decl >>| fun decl -> Str_exception decl

let parse_structure : structure t =
  let parse_structure_item =
    (*
       XXX: parsing of let structure items seems to require 2 passes.
       first it tries to parse let expression, fails when doesn't find `in`,
       then starts all over again trying to parse let structure item.

       we probably should use lookahead to check for `in`
    *)
    ws
    *> choice
         [ (parse_expression >>| fun e -> Str_eval e)
         ; parse_str_let
         ; parse_str_type
         ; parse_str_exception ]
  in
  let semicolons = ws *> option () (string ";;" *> return ()) in
  sep_by semicolons parse_structure_item <* semicolons <* ws
