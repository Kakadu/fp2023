(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns_lib
open Ast
open Parser

(* TESTS  PARSER*)

let start_test parser show input =
  let res = start_parsing parser input in
  match res with
  | Ok res -> Format.printf "%s" (show res)
  | Error err -> Format.printf "%s" err
;;

(* Test const parser *)

let%expect_test _ =
  let test = "true" in
  start_test parse_const show_const test;
  [%expect {| (CBool true) |}]
;;

let%expect_test _ =
  let test = "\"itsastring\"" in
  start_test parse_const show_const test;
  [%expect {| (CString "itsastring") |}]
;;

let%expect_test _ =
  let test = "951753" in
  start_test parse_const show_const test;
  [%expect {| (CInt 951753) |}]
;;

let%expect_test _ =
  let test = "-951753" in
  start_test parse_const show_const test;
  [%expect {| (CInt -951753) |}]
;;

(* Test pattern parser *)

let%expect_test _ =
  let test = "varname1" in
  start_test parse_var show_pattern test;
  [%expect {| (Var "varname1") |}]
;;

let%expect_test _ =
  let test = "1varname" in
  start_test parse_var show_pattern test;
  [%expect {| : Identifier first sumbol is letter, not digit |}]
;;

let%expect_test _ =
  let test = "a,b,c" in
  start_test pat show_pattern test;
  [%expect {| (Tuple [(Var "a"); (Var "b"); (Var "c")]) |}]
;;

let%expect_test _ =
  let test = "[((1));2;3]" in
  start_test pat show_pattern test;
  [%expect {| (PCon ((Const (CInt 1)), (PCon ((Const (CInt 2)), (Const (CInt 3)))))) |}]
;;

let%expect_test _ =
  let test = "hd::tl" in
  start_test pat show_pattern test;
  [%expect {| (PCon ((Var "hd"), (Var "tl"))) |}]
;;

(* Test expr *)

let%expect_test _ =
  let test = "[1;2; 3]" in
  start_test parse show_expr test;
  [%expect
    {|
      (ListExpr ((ConstExpr (CInt 1)),
         (ListExpr ((ConstExpr (CInt 2)),
            (ListExpr ((ConstExpr (CInt 3)), (ConstExpr CNil)))))
         )) |}]
;;

let%expect_test _ =
  let test = "fun x -> x * x " in
  start_test parse show_expr test;
  [%expect {| (FunExpr ((Var "x"), (BinExpr (Mul, (VarExpr "x"), (VarExpr "x"))))) |}]
;;

let%expect_test _ =
  let test = "(1, [2;((3));4], 5)" in
  start_test parse show_expr test;
  [%expect
    {|
      (TupleExpr
         [(ConstExpr (CInt 1));
           (ListExpr ((ConstExpr (CInt 2)),
              (ListExpr ((ConstExpr (CInt 3)),
                 (ListExpr ((ConstExpr (CInt 4)), (ConstExpr CNil)))))
              ));
           (ConstExpr (CInt 5))]) |}]
;;

let%expect_test _ =
  let test = " (1  +  2)+ (1 *  3 /  3) <= v  " in
  start_test parse show_expr test;
  [%expect
    {|
    (BinExpr (LEq,
       (BinExpr (Add,
          (BinExpr (Add, (ConstExpr (CInt 1)), (ConstExpr (CInt 2)))),
          (BinExpr (Div,
             (BinExpr (Mul, (ConstExpr (CInt 1)), (ConstExpr (CInt 3)))),
             (ConstExpr (CInt 3))))
          )),
       (VarExpr "v"))) |}]
;;

let%expect_test _ =
  let test = "let f x = x+x" in
  start_test parse show_expr test;
  [%expect
    {|
    (LetExpr (false, "f",
       (FunExpr ((Var "x"), (BinExpr (Add, (VarExpr "x"), (VarExpr "x"))))))) |}]
;;

let%expect_test _ =
  let test = "[]" in
  start_test parse show_expr test;
  [%expect {| (ConstExpr CNil) |}]
;;

let%expect_test _ =
  let test = "true" in
  start_test parse show_expr test;
  [%expect {| (ConstExpr (CBool true)) |}]
;;

let%expect_test _ =
  let test = "7 + 10 + 4 * 50 + 19 / 3 + (10 - 5) " in
  start_test parse show_expr test;
  [%expect
    {|
      (BinExpr (Add,
         (BinExpr (Add,
            (BinExpr (Add,
               (BinExpr (Add, (ConstExpr (CInt 7)), (ConstExpr (CInt 10)))),
               (BinExpr (Mul, (ConstExpr (CInt 4)), (ConstExpr (CInt 50)))))),
            (BinExpr (Div, (ConstExpr (CInt 19)), (ConstExpr (CInt 3)))))),
         (BinExpr (Sub, (ConstExpr (CInt 10)), (ConstExpr (CInt 5)))))) |}]
;;

let%expect_test _ =
  let test = "let x = 5" in
  start_test parse show_expr test;
  [%expect {| (LetExpr (false, "x", (ConstExpr (CInt 5)))) |}]
;;

let%expect_test _ =
  let test = "let rec fact n = if n = 1 then 1 else n * (fact (n - 1))" in
  start_test parse show_expr test;
  [%expect
    {|
    (LetExpr (true, "fact",
       (FunExpr ((Var "n"),
          (IfExpr ((BinExpr (Eq, (VarExpr "n"), (ConstExpr (CInt 1)))),
             (ConstExpr (CInt 1)),
             (BinExpr (Mul, (VarExpr "n"),
                (AppExpr ((VarExpr "fact"),
                   (BinExpr (Sub, (VarExpr "n"), (ConstExpr (CInt 1))))))
                ))
             ))
          ))
       )) |}]
;;

let%expect_test _ =
  let test = "let check input = match input with | 2 -> 5 | 5 -> 10" in
  start_test parse show_expr test;
  [%expect
    {|
      (LetExpr (false, "check",
         (FunExpr ((Var "input"),
            (MatchExpr ((VarExpr "input"),
               [((Const (CInt 2)), (ConstExpr (CInt 5)));
                 ((Const (CInt 5)), (ConstExpr (CInt 10)))]
               ))
            ))
         )) |}]
;;

let%expect_test _ =
  let test = "match x with Some x -> x | None -> 0" in
  start_test parse show_expr test;
  [%expect
    {|
    (MatchExpr ((VarExpr "x"),
       [((Case ("Some", [(Var "x")])), (VarExpr "x"));
         ((Case ("None", [])), (ConstExpr (CInt 0)))]
       )) |}]
;;

let%expect_test _ =
  let test = "let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd" in
  start_test parse show_expr test;
  [%expect
    {|
    (LetActExpr (["Even"; "Odd"],
       (FunExpr ((Var "input"),
          (IfExpr (
             (BinExpr (Eq,
                (BinExpr (Mod, (VarExpr "input"), (ConstExpr (CInt 2)))),
                (ConstExpr (CInt 0)))),
             (CaseExpr "Even"), (CaseExpr "Odd")))
          ))
       )) |}]
;;

let%expect_test _ =
  let test = "let (|Even|_|) v = if v % 2 = 0 then Even else None " in
  start_test parse show_expr test;
  [%expect
    {|
    (LetActExpr (["Even"; "_"],
       (FunExpr ((Var "v"),
          (IfExpr (
             (BinExpr (Eq, (BinExpr (Mod, (VarExpr "v"), (ConstExpr (CInt 2)))),
                (ConstExpr (CInt 0)))),
             (CaseExpr "Even"), (CaseExpr "None")))
          ))
       )) |}]
;;

let%expect_test _ =
  let test = "let (|Even|) v = v" in
  start_test parse show_expr test;
  [%expect {| (LetActExpr (["Even"], (FunExpr ((Var "v"), (VarExpr "v"))))) |}]
;;

let%expect_test _ =
  let test = "let greet (Default value) = value" in
  start_test parse show_expr test;
  [%expect
    {|
    (LetExpr (false, "greet",
       (FunExpr ((Case ("Default", [(Var "value")])), (VarExpr "value"))))) |}]
;;

let%expect_test _ =
  let test =
    "   let check value =\n   match value with\n   | Even -> 25\n   | Odd -> 53"
  in
  start_test parse show_expr test;
  [%expect
    {|
    (LetExpr (false, "check",
       (FunExpr ((Var "value"),
          (MatchExpr ((VarExpr "value"),
             [((Case ("Even", [])), (ConstExpr (CInt 25)));
               ((Case ("Odd", [])), (ConstExpr (CInt 53)))]
             ))
          ))
       )) |}]
;;

let%expect_test _ =
  let test =
    "let rec fact x k = if x = 1 then k x else fact (x - 1) (fun n -> n * k x)"
  in
  start_test parse show_expr test;
  [%expect
    {|
      (LetExpr (true, "fact",
         (FunExpr ((Var "x"),
            (FunExpr ((Var "k"),
               (IfExpr ((BinExpr (Eq, (VarExpr "x"), (ConstExpr (CInt 1)))),
                  (AppExpr ((VarExpr "k"), (VarExpr "x"))),
                  (AppExpr (
                     (AppExpr ((VarExpr "fact"),
                        (BinExpr (Sub, (VarExpr "x"), (ConstExpr (CInt 1)))))),
                     (FunExpr ((Var "n"),
                        (BinExpr (Mul, (VarExpr "n"),
                           (AppExpr ((VarExpr "k"), (VarExpr "x")))))
                        ))
                     ))
                  ))
               ))
            ))
         )) |}]
;;

let%expect_test _ =
  let test = "let plusfive x = let five a = a + 5 in five x" in
  start_test parse show_expr test;
  [%expect
    {|
    (LetExpr (false, "plusfive",
       (FunExpr ((Var "x"),
          (LetInExpr (false, "five",
             (FunExpr ((Var "a"),
                (BinExpr (Add, (VarExpr "a"), (ConstExpr (CInt 5)))))),
             (AppExpr ((VarExpr "five"), (VarExpr "x")))))
          ))
       )) |}]
;;

let%expect_test _ =
  let test =
    " let list_rev list = \n\
    \ let rec helper acc l = \n\
    \   match l with \n\
    \    | [] -> acc \n\
    \    | hd :: tl -> (helper (hd :: acc) tl) in \n\
    \ helper [] list \n"
  in
  start_test parse show_expr test;
  [%expect
    {|
    (LetExpr (false, "list_rev",
       (FunExpr ((Var "list"),
          (LetInExpr (true, "helper",
             (FunExpr ((Var "acc"),
                (FunExpr ((Var "l"),
                   (MatchExpr ((VarExpr "l"),
                      [((Const CNil), (VarExpr "acc"));
                        ((PCon ((Var "hd"), (Var "tl"))),
                         (AppExpr (
                            (AppExpr ((VarExpr "helper"),
                               (BinExpr (Con, (VarExpr "hd"), (VarExpr "acc"))))),
                            (VarExpr "tl"))))
                        ]
                      ))
                   ))
                )),
             (AppExpr ((AppExpr ((VarExpr "helper"), (ConstExpr CNil))),
                (VarExpr "list")))
             ))
          ))
       )) |}]
;;

let%expect_test _ =
  let test = "let result = list_fold [1; 2; 3; 4; 5] 0 (fun acc el -> acc + el)" in
  start_test parse show_expr test;
  [%expect
    {|
    (LetExpr (false, "result",
       (AppExpr (
          (AppExpr (
             (AppExpr ((VarExpr "list_fold"),
                (ListExpr ((ConstExpr (CInt 1)),
                   (ListExpr ((ConstExpr (CInt 2)),
                      (ListExpr ((ConstExpr (CInt 3)),
                         (ListExpr ((ConstExpr (CInt 4)),
                            (ListExpr ((ConstExpr (CInt 5)), (ConstExpr CNil)))))
                         ))
                      ))
                   ))
                )),
             (ConstExpr (CInt 0)))),
          (FunExpr ((Var "acc"),
             (FunExpr ((Var "el"),
                (BinExpr (Add, (VarExpr "acc"), (VarExpr "el")))))
             ))
          ))
       )) |}]
;;

let%expect_test _ =
  let test =
    "  \n\
    \   let rec listmap f list =\n\
    \       match list with\n\
    \         | [] -> []\n\
    \         | hd :: tl -> ((f hd) :: (listmap f tl))"
  in
  start_test parse show_expr test;
  [%expect
    {|
    (LetExpr (true, "listmap",
       (FunExpr ((Var "f"),
          (FunExpr ((Var "list"),
             (MatchExpr ((VarExpr "list"),
                [((Const CNil), (ConstExpr CNil));
                  ((PCon ((Var "hd"), (Var "tl"))),
                   (BinExpr (Con, (AppExpr ((VarExpr "f"), (VarExpr "hd"))),
                      (AppExpr ((AppExpr ((VarExpr "listmap"), (VarExpr "f"))),
                         (VarExpr "tl")))
                      )))
                  ]
                ))
             ))
          ))
       )) |}]
;;
