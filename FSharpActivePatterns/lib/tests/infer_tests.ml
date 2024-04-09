(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns_lib
open Inferencer

let%expect_test _ =
  let open Ast in
  let _ =
    let e = [ ConstExpr (CInt 4) ] in
    check_types e |> run_infer
  in
  [%expect {| int |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = [ ConstExpr (CBool true) ] in
    check_types e |> run_infer
  in
  [%expect {| bool |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = [ ConstExpr (CString "1") ] in
    check_types e |> run_infer
  in
  [%expect {| string |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      [ TupleExpr
          [ ConstExpr (CInt 1)
          ; TupleExpr [ ConstExpr (CInt 2); ConstExpr (CInt 3); ConstExpr (CInt 4) ]
          ; ConstExpr (CInt 5)
          ]
      ]
    in
    check_types e |> run_infer
  in
  [%expect {| (int * (int * int * int) * int) |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = [ IfExpr (ConstExpr (CBool true), ConstExpr (CInt 4), ConstExpr (CInt 5)) ] in
    check_types e |> run_infer
  in
  [%expect {| int |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      [ TupleExpr
          [ ConstExpr (CInt 1)
          ; ListExpr
              ( ConstExpr (CInt 2)
              , ListExpr
                  (ConstExpr (CInt 3), ListExpr (ConstExpr (CInt 4), ConstExpr CNil)) )
          ; ConstExpr (CInt 5)
          ]
      ]
    in
    check_types e |> run_infer
  in
  [%expect {| string list |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      [ BinExpr
          ( Add
          , BinExpr (Add, ConstExpr (CInt 1), ConstExpr (CInt 2))
          , BinExpr
              ( Div
              , BinExpr (Mul, ConstExpr (CInt 1), ConstExpr (CInt 3))
              , ConstExpr (CInt 3) ) )
      ]
    in
    check_types e |> run_infer
  in
  [%expect {| int |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = [ LetExpr (false, "x", ConstExpr (CInt 5)) ] in
    check_types e |> run_infer
  in
  [%expect {| int |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      [ AppExpr
          (FunExpr (Var "x", BinExpr (Mul, VarExpr "x", VarExpr "x")), ConstExpr (CInt 5))
      ]
    in
    check_types e |> run_infer
  in
  [%expect {| int |}]
;;

let%expect_test _ =
  let _ =
    let e = [] in
    check_types e |> run_infer
  in
  [%expect {| Error: empty program |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = [ BinExpr (Less, ConstExpr (CInt 1), ConstExpr (CInt 3)) ] in
    check_types e |> run_infer
  in
  [%expect {| bool |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e = [ BinExpr (Or, ConstExpr (CInt 1), ConstExpr (CInt 3)) ] in
    check_types e |> run_infer
  in
  [%expect {| bool |}]
;;

let%expect_test _ =
  let open Ast in
  let _ =
    let e =
      [ LetExpr
          ( false
          , "check"
          , FunExpr
              ( Var "input"
              , MatchExpr
                  ( VarExpr "input"
                  , [ Const (CInt 2), ConstExpr (CInt 5)
                    ; Const (CInt 5), ConstExpr (CInt 10)
                    ] ) ) )
      ]
    in
    check_types e |> run_infer
  in
  [%expect {| (int -> int) |}]
;;
