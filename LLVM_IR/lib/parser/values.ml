(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Angstrom
open CommonParser
open Types

let rec parse_const tp =
  let rec makeList i arg = if i = 0 then [] else arg :: makeList (i - 1) arg in
  match tp with
  | Ast.TVoid -> return Ast.CVoid
  | Ast.TInteger size -> parse_const_integer size
  | Ast.TFloat -> parse_const_float
  | Ast.TPointer -> parse_const_pointer
  | Ast.TVector (size, vtp) ->
    parse_const_aggregate_type '<' '>' (makeList size vtp)
    >>= fun lst -> return (Ast.CVector lst)
  | Ast.TArr (size, atp) ->
    parse_const_aggregate_type '[' ']' (makeList size atp)
    >>= fun lst -> return (Ast.CArr lst)
  | Ast.TStruct slst ->
    parse_const_aggregate_type '{' '}' slst >>= fun lst -> return (Ast.CStruct lst)
  | _ -> fail "Parser error: Get unknown type"

and parse_const_aggregate_type ob cb tp_list =
  char ob
  *> whitespaces
  *> sep_by
       comma
       (whitespaces *> parse_main_type
        >>= fun tp -> whitespaces *> parse_const tp >>| fun c -> tp, c)
  <* whitespaces
  <* char cb
  >>= fun lst ->
  let readed_types =
    List.map
      (function
        | tp, _ -> tp)
      lst
  in
  let consts =
    List.map
      (function
        | _, c -> c)
      lst
  in
  if List.equal Ast.equal_tp readed_types tp_list
  then return consts
  else fail "Parser error: constant vector/array length mismatch"

and parse_const_array size vector_tp =
  char '<'
  *> whitespaces
  *> sep_by
       comma
       (whitespaces *> parse_main_type
        >>= fun tp ->
        whitespaces
        *>
        if vector_tp = tp
        then parse_const tp
        else fail "Parser error: constant vector type mismatch")
  <* whitespaces
  <* char '>'
  >>= fun lst ->
  if List.length lst = size
  then return (Ast.CVector lst)
  else fail "Parser error: constant vector length mismatch"

and parse_const_pointer =
  parse_global_variable
  >>| (fun var -> Ast.CPointer (Ast.PointerGlob var))
  <|> word "null" *> return (Ast.CPointer (Ast.PointerInt 0))

and parse_const_float =
  parse_word
  >>= fun str ->
  match Float.of_string_opt str with
  | Some res -> return (Ast.CFloat res)
  | None -> fail "Can't parse float"

and parse_const_integer size =
  choice
    [ (let* sign = choice [ char '-' *> return (-1L); return 1L ] in
       parse_integer64 >>| fun value -> Common.IrInts.create (Int64.mul sign value) size)
    ; (if size = 1
       then
         choice
           [ word "true" *> return (Common.IrInts.create 1L size)
           ; word "false" *> return (Common.IrInts.create 0L size)
           ]
       else fail "Parser error: can't parse i1 const")
    ]
;;

let parse_value tp =
  whitespaces
  *> choice
       [ (parse_local_variable >>| fun var -> Ast.FromVariable (var, tp))
       ; (parse_const tp >>| fun const -> Ast.Const const)
       ]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "void ";
  [%expect {| (Const CVoid) |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "float 2.3 ";
  [%expect {| (Const (CFloat 2.3)) |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "i1 432 ";
  [%expect {| (Const (CInteger (1, 0L))) |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "ptr @G ";
  [%expect {| (Const (CPointer (PointerGlob (GlobalVar "G")))) |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "{i32, float} { i32 4, float 17.0} ";
  [%expect {| (Const (CStruct [(CInteger (32, 4L)); (CFloat 17.)])) |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "<4 x i32> < i32 42, i32 11, i32 74, i32 100 > ";
  [%expect
    {|
    (Const
       (CVector
          [(CInteger (32, 42L)); (CInteger (32, 11L)); (CInteger (32, 74L));
            (CInteger (32, 100L))])) |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "[2 x [2 x i32]] [[2 x i32] [i32 1, i32 3], [2 x i32] [i32 2, i32 4]] ";
  [%expect
    {|
    (Const
       (CArr
          [(CArr [(CInteger (32, 1L)); (CInteger (32, 3L))]);
            (CArr [(CInteger (32, 2L)); (CInteger (32, 4L))])])) |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "{i32, int43} { i32 4, float 17.0} ";
  [%expect {| Error |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "<4 x i32> < i32 42, i32 11, i32 100 > ";
  [%expect {| Error |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "[2 x [2 x i32]] [[2 x i32] [i32 1, i31 3], [2 x i32] [i32 2, i32 4]] ";
  [%expect {| Error |}]
;;

let%expect_test _ =
  test_parse
    (parse_additional_type <* whitespaces >>= fun f -> parse_value f)
    Ast.show_value
    "i32 %12 ";
  [%expect {| (FromVariable ((LocalVar "12"), (TInteger 32))) |}]
;;

let parse_type_with_value =
  parse_additional_type >>= fun tp -> parse_value tp >>| fun value -> tp, value
;;

let parse_type_with_value2 =
  parse_additional_type
  >>= fun tp ->
  parse_value tp >>= fun v1 -> comma *> parse_value tp >>| fun v2 -> tp, v1, v2
;;

let type_with_value tp =
  parse_type_with_value
  >>= function
  | parsed_type, value when parsed_type = tp -> return value
  | _ -> fail "Parser error: get unexpected type"
;;
