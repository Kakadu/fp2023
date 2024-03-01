(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Angstrom
open CommonParser
open Types
open Values

type func_annotation =
  { self : Ast.variable
  ; tp : Ast.tp
  ; parameters : Ast.variable list
  }
[@@deriving show { with_path = false }]

let parse_function_annotation =
  lift3
    (fun ret name args ->
      let arg_types =
        List.map
          (function
            | tp, _ -> tp)
          args
      in
      let arg_vars =
        List.map
          (function
            | _, var -> var)
          args
      in
      { self = name; parameters = arg_vars; tp = Ast.TFunc (ret, arg_types) })
    (whitespaces *> parse_additional_type)
    parse_global_variable
    (whitespaces
     *> char '('
     *> sep_by
          comma
          (whitespaces
           *> lift2 (fun tp name -> tp, name) parse_main_type parse_local_variable)
     <* whitespaces
     <* char ')')
;;

let%expect_test _ =
  test_parse parse_function_annotation show_func_annotation "i32 @fac(i32 %0, i34 %1)";
  [%expect
    {|
    { self = (GlobalVar "fac");
      tp = (TFunc ((TInteger 32), [(TInteger 32); (TInteger 34)]));
      parameters = [(LocalVar "0"); (LocalVar "1")] } |}]
;;

let parse_basic_block_variable =
  whitespaces *> parse_name <* whitespaces <* char ':' >>| fun name -> Ast.LocalVar name
;;

let parse_basic_block_body =
  let rec help lst =
    Instructions.parse_instruction
    >>= fun instr ->
    match instr with
    | Ast.Terminator _ -> return (instr :: lst)
    | _ -> help (instr :: lst)
  in
  help [] >>| List.rev
;;

let parse_start_basic_block =
  lift2
    (fun self instructions -> self, Ast.CLabel instructions)
    (parse_basic_block_variable <|> return (Ast.LocalVar "<start>"))
    parse_basic_block_body
;;

let parse_basic_block =
  lift2
    (fun self instructions -> self, Ast.CLabel instructions)
    parse_basic_block_variable
    parse_basic_block_body
;;

let parse_function_body =
  whitespaces
  *> char '{'
  *> whitespaces
  *> lift2 (fun h tl -> h :: tl) parse_start_basic_block (many parse_basic_block)
  <* whitespaces
  <* char '}'
;;

let parse_function =
  whitespaces *> word "define" *> parse_function_annotation
  >>= fun annot ->
  parse_function_body
  >>= fun bbs ->
  return
    ( annot.tp
    , annot.self
    , Ast.CFunc
        ({ ftp = annot.tp; parameters = annot.parameters; basic_blocks = bbs } : Ast.func)
    , 1 )
;;

let parse_glob_var =
  let* variable = whitespaces *> parse_global_variable <* whitespaces <* char '=' in
  let* tp = whitespaces *> word "global" *> whitespaces *> parse_main_type in
  let* const = whitespaces *> parse_const tp in
  let* alignment = Instructions.parse_align in
  return (tp, variable, const, alignment)
;;

let start_parse : Ast.glob_list t =
  many (choice [ parse_function; parse_glob_var ]) <* whitespaces
;;

let parse_program prog = Angstrom.parse_string ~consume:Consume.All start_parse prog
