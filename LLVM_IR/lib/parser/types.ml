(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Angstrom
open CommonParser

let parse_primitive_type =
  whitespaces
  *> (char 'i' *> parse_integer
      >>= (fun size -> return (Ast.TInteger size))
      <|> (parse_word
           >>= function
           | "float" -> return Ast.TFloat
           | "ptr" -> return Ast.TPointer
           | _ -> fail "Parsing error: unknown primitive type"))
;;

let%expect_test _ =
  test_parse parse_primitive_type Ast.show_tp "i64";
  [%expect {| (TInteger 64) |}]
;;

let%expect_test _ =
  test_parse parse_primitive_type Ast.show_tp "    i2   ";
  [%expect {| (TInteger 2) |}]
;;

let%expect_test _ =
  test_parse parse_primitive_type Ast.show_tp "    c2   ";
  [%expect {| Error |}]
;;

let%expect_test _ =
  test_parse parse_primitive_type Ast.show_tp "float";
  [%expect {| TFloat |}]
;;

let parse_main_type =
  fix (fun parse_type ->
    let parse_array_type =
      whitespaces
      *> char '['
      *> whitespaces
      *> lift2
           (fun size t -> Ast.TArr (size, t))
           (parse_integer <* whitespaces <* char 'x')
           (parse_type <* whitespaces <* char ']')
    and parse_vector_type =
      whitespaces
      *> char '<'
      *> whitespaces
      *> lift2
           (fun size t -> Ast.TVector (size, t))
           (parse_integer <* whitespaces <* char 'x')
           (parse_primitive_type <* whitespaces <* char '>')
    and parse_structure_type =
      lift3
        (fun h tl _ -> Ast.TStruct (List.cons h tl))
        (whitespaces *> char '{' *> whitespaces *> parse_type)
        (many (comma *> parse_type))
        (whitespaces *> char '}')
    in
    choice
      [ parse_primitive_type; parse_vector_type; parse_array_type; parse_structure_type ])
;;

let%expect_test _ =
  test_parse parse_main_type Ast.show_tp "<4xi32>";
  [%expect {| (TVector (4, (TInteger 32))) |}]
;;

let%expect_test _ =
  test_parse parse_main_type Ast.show_tp "[4x[5x<4  x ptr>]]";
  [%expect {| (TArr (4, (TArr (5, (TVector (4, TPointer)))))) |}]
;;

let%expect_test _ =
  test_parse parse_main_type Ast.show_tp "{i32, i42, float}";
  [%expect {| (TStruct [(TInteger 32); (TInteger 42); TFloat]) |}]
;;

let%expect_test _ =
  test_parse parse_main_type Ast.show_tp "{ [4x{i34}], float}";
  [%expect {| (TStruct [(TArr (4, (TStruct [(TInteger 34)]))); TFloat]) |}]
;;

let%expect_test _ =
  test_parse parse_main_type Ast.show_tp "{ , float}";
  [%expect {| Error |}]
;;

let%expect_test _ =
  test_parse parse_main_type Ast.show_tp "{i32, i42,\n  ;some comment \n float}";
  [%expect {| (TStruct [(TInteger 32); (TInteger 42); TFloat]) |}]
;;

let parse_additional_type =
  whitespaces
  *> choice
       [ parse_main_type
       ; (parse_word
          >>= function
          | "void" -> return Ast.TVoid
          | "label" -> return Ast.TLabel
          | _ -> fail "Parsing error: unknown type")
       ]
;;

let additional_type tp =
  parse_additional_type
  >>= fun parsed_tp ->
  if tp = parsed_tp then return tp else fail "Parser error: get unexpected type"
;;
