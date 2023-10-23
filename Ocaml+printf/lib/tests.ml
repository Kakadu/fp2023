(** Copyright 2021-2023, aartdem *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Parser
open Ast

(* simple unit tests *)
let%test _ = parse_string ~consume:All expr_valname "_" = Ok (Expr_val (LCIdent "_"))

let%test _ =
  parse_string ~consume:All expr_valname "_Aasd0320" = Ok (Expr_val (LCIdent "_Aasd0320"))
;;

let%test _ = parse_string ~consume:All expr_valname "a" = Ok (Expr_val (LCIdent "a"))
let%test _ = parse_string ~consume:All (parenthesis (char 'a')) "  (  a  )" = Ok 'a'
let%test _ = parse_string ~consume:All const_integer "123" = Ok (Int 123)

(* helper function for inline testing *)
let test_parser p input expected =
  let result = parse_string p ~consume:All input in
  match result with
  | Ok actual ->
    let res1 = equal_program expected actual in
    if res1 = false
    then Format.printf "Expected: %a\nActual: %a\n" pp_program expected pp_program actual;
    res1
  | Error msg ->
    Format.printf "Parse error %s\n" msg;
    false
;;

(* tests for interpretator *)

let%test _ =
  let expexted =
    [ Let_decl (false, LCIdent "_a", Expr_const (Int 1))
    ; Let_decl (false, LCIdent "_b", Bin_op (Add, Expr_const (Int 1), Expr_const (Int 2)))
    ]
  in
  test_parser program_parser "  ;; ;;;;   ;; ;;let _a=1;; ;;;; let   _b =1+2  " expexted
;;

let%test _ =
  let expexted =
    [ Let_decl
        ( false
        , LCIdent "abcde"
        , Fun
            ( LCIdent "a"
            , Fun
                ( LCIdent "b"
                , Fun
                    (LCIdent "c", Fun (LCIdent "d", Fun (LCIdent "e", Expr_const (Int 1))))
                ) ) )
    ]
  in
  test_parser program_parser "let abcde a  b  c  d  e   =1     " expexted
;;

let%test _ =
  let expexted =
    [ Let_decl
        ( false
        , LCIdent "a"
        , Bin_op
            ( Sub
            , Bin_op
                ( Mul
                , Bin_op
                    ( Div
                    , Expr_const (Int 10)
                    , Bin_op (Add, Expr_const (Int 2), Expr_const (Int 5)) )
                , Bin_op (Sub, Expr_const (Int 7), Expr_const (Int 1)) )
            , Bin_op
                ( Add
                , Expr_const (Int 1)
                , Bin_op (Mul, Expr_const (Int 2), Expr_const (Int 3)) ) ) )
    ]
  in
  test_parser program_parser "let a =  10/( 2+5 )*( 7  - 1) - ( 1 + 2*  3 ) " expexted
;;

let%test _ =
  let expexted =
    [ Let_decl
        ( false
        , LCIdent "x"
        , ITE
            ( Expr_const (Int 1)
            , ITE
                ( Expr_const (Int 2)
                , ITE (Expr_const (Int 3), Expr_const (Int 4), Expr_const (Int 5))
                , Expr_const (Int 6) )
            , Expr_const (Int 7) ) )
    ]
  in
  test_parser
    program_parser
    "let x = if 1 then if 2 then if 3 then 4 else 5 else 6 else 7"
    expexted
;;

(* factorail *)

let%test _ =
  let expected =
    [ Let_decl
        ( true
        , LCIdent "fac"
        , Fun
            ( LCIdent "n"
            , ITE
                ( Bin_op (Leq, Expr_val (LCIdent "n"), Expr_const (Int 1))
                , Expr_const (Int 1)
                , Bin_op
                    ( Mul
                    , Expr_val (LCIdent "n")
                    , App
                        ( Expr_val (LCIdent "fac")
                        , Bin_op (Sub, Expr_val (LCIdent "n"), Expr_const (Int 1)) ) ) )
            ) )
    ]
  in
  test_parser
    program_parser
    "let rec fac n = if n <= 1 then 1 else n * fac (n - 1)"
    expected
;;
