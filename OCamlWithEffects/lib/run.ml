(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Auxiliary
open Parser
open Parser_errors
open Parser_pprint
open Inferencer
open Inf_pprint
open Interpreter
open Int_pprint

let parse_with_print code =
  (* The 'parse' function converts the result to a string.
     Used in tests. *)
  match parse code with
  | Ok ast -> print_parser_result ast
  | Error _ -> print_parser_error syntax_error
;;

let inference_program ast =
  (* Here AST is a list of SDeclarations. *)
  let typ =
    match run_program_inferencer ast with
    | Ok (env, names_list) -> print_program_type env names_list
    | Error e -> print_inferencer_error e
  in
  typ
;;

let inference_expr ast =
  (* Here AST is an arbitrary expression. *)
  let typ =
    match run_expr_inferencer ast with
    | Ok typ -> print_expr_type typ
    | Error e -> print_inferencer_error e
  in
  typ
;;

let inference program =
  (* Main function for inference. *)
  let ast = parse program in
  let res =
    match ast with
    | Ok ast ->
      (match determine_ast_type ast with
       | FreeExpression ->
         (match ast with
          | [ SExpression e ] -> inference_expr e
          | _ -> print_parser_error syntax_error)
       | DeclarationList -> inference_program ast
       | MixedList -> print_parser_error syntax_error)
    | Error _ -> print_parser_error syntax_error
  in
  res
;;

let interpret_program ast =
  (* Here AST is a list of SDeclarations. *)
  let res =
    match run_program_inferencer ast with
    | Ok (typ_env, names_list) ->
      (match run_program_interpreter ast with
       | Ok val_env -> print_program_value val_env typ_env names_list
       | Error e -> print_interpreter_error e)
    | Error e -> print_inferencer_error e
  in
  res
;;

let interpret_expr ast =
  (* Here AST is an arbitrary expression. *)
  let res =
    match run_expr_inferencer ast with
    | Ok typ ->
      (match run_expr_interpreter ast with
       | Ok value -> print_expr_value value typ
       | Error e -> print_interpreter_error e)
    | Error e -> print_inferencer_error e
  in
  res
;;

let interpret program =
  (* Main function for inference + interpret. *)
  let ast = parse program in
  let res =
    match ast with
    | Ok ast ->
      (match determine_ast_type ast with
       | FreeExpression ->
         (match ast with
          | [ SExpression x ] -> interpret_expr x
          | _ -> print_parser_error syntax_error)
       | DeclarationList -> interpret_program ast
       | MixedList -> print_parser_error syntax_error)
    | Error _ -> print_parser_error syntax_error
  in
  res
;;
