(** Copyright 2021-2024, ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Base
open Parser

(* parse *)

let parse p s show_program =
  match parse_string ~consume:All p s with
  | Ok ast -> print_endline (show_program ast)
  | Error msg -> failwith msg
;;

let%expect_test _ =
  parse pexpr "let rec fac x = if x = 1 then x else x * fac (x - 1)" Ast.show_expr;
  [%expect
    {|
    (ELet (
       (Rec, "fac",
        (EFun ((PVar "x"),
           (EIfThenElse ((EBinOp (Eq, (EVar "x"), (EConst (Int 1)))), (EVar "x"),
              (EBinOp (Mult, (EVar "x"),
                 (EApp ((EVar "fac"),
                    (EBinOp (Minus, (EVar "x"), (EConst (Int 1))))))
                 ))
              ))
           ))),
       EUnit)) |}]
;;

(* infer *)

open Inferencer
open Typing

let pp_infer e =
  match run_inference e with
  | Ok ty -> Stdlib.Format.printf "%a" pp_type ty
  | Error err -> Stdlib.Format.printf "%a" pp_error err
;;

(* let parse_program = parse_string ~consume:Consume.All (many1 (plet pexpr) <* (skip_while Char.is_whitespace)) *)

let pp_parse_expr_and_infer input =
  match Parser.parse_expr input with
  | Ok e -> pp_infer e
  | Error _ -> Stdlib.print_endline "Failed to parse"
;;

let%expect_test _ =
  pp_parse_expr_and_infer "fun x -> x * 2";
  [%expect {| int -> int |}]
;;

let%expect_test _ =
  pp_parse_expr_and_infer "fun x -> true * 2";
  [%expect {| Unification failed on bool and int |}]
;;

let%expect_test _ =
  pp_parse_expr_and_infer "let rec fac x = if x = 1 then x else x * fac (x - 1)";
  [%expect {| int -> int |}]
;;

let%expect_test _ =
  pp_parse_expr_and_infer "fun x y -> if (x<y) then x else y";
  [%expect {| c -> c -> c |}]
;;

(** interpret *)

open Interpreter
open Interpreter.PP

let pp_interpret_expr ast =
  match run_inference ast with
  | Ok _ ->
    (match run_expr_interpreter ast with
     | Ok value -> print_value value
     | Error e -> print_error e)
  | Error e -> print_type_error e
;;

let pp_run code =
  match Parser.parse_expr code with
  | Ok e -> pp_interpret_expr e
  | Error msg -> failwith msg
;;

let%expect_test _ =
  pp_run "2";
  [%expect {| 2 |}]
;;

let%expect_test _ =
  pp_run "2 * 2";
  [%expect {| 4 |}]
;;

let%expect_test _ =
  pp_run "fun x -> x * 2";
  [%expect {| <fun> |}]
;;

let%expect_test _ =
  pp_run "2 / 2";
  [%expect {| 1 |}]
;;

let%expect_test _ =
  pp_run "2 + 3";
  [%expect {| 5 |}]
;;

let%expect_test _ =
  pp_run "x = 3";
  [%expect {| Undefined variable: x |}]
;;

let%expect_test _ =
  pp_run "let x = 3";
  [%expect {| 3 |}]
;;
