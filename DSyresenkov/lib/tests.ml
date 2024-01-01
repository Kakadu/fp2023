(** Copyright 2021-2023, Ilya Syresenkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Miniml_lib
open Ast

module ParserTests = struct
  open Parser

  let pp printer parser input =
    match parser input with
    | Result.Ok res -> Stdlib.Format.printf "%a" printer res
    | _ -> Stdlib.print_endline "Failed to parse"
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "let f x = x";
    [%expect {| (ELet (NonRec, "f", (EFun ("x", (EVar "x"))), None)) |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "if f x then x else 0";
    [%expect
      {| (EBranch ((EApp ((EVar "f"), (EVar "x"))), (EVar "x"), (EConst (CInt 0)))) |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "[1 + 2; 1 * 2; 1 - 2; 1 / 2]";
    [%expect
      {| 
    (EList
       [(EBinop (Add, (EConst (CInt 1)), (EConst (CInt 2))));
         (EBinop (Mul, (EConst (CInt 1)), (EConst (CInt 2))));
         (EBinop (Sub, (EConst (CInt 1)), (EConst (CInt 2))));
         (EBinop (Div, (EConst (CInt 1)), (EConst (CInt 2))))]) 
    |}]
  ;;

  let%expect_test _ =
    pp
      pp_expr
      parse_expr
      {|match x with | h1 :: h2 :: tl -> if h1 >= h2 then h1 else h2 | h1 :: [] -> h1 | _ -> 0|};
    [%expect
      {|
        (EMatch ((EVar "x"),
           [((PCons ((PVar "h1"), (PVar "h2"), [(PVar "tl")])),
             (EBranch ((EBinop (Geq, (EVar "h1"), (EVar "h2"))), (EVar "h1"),
                (EVar "h2"))));
             ((PCons ((PVar "h1"), PEmpty, [])), (EVar "h1"));
             (PWild, (EConst (CInt 0)))]
           )) 
    |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "[1; 2], [3; 4]";
    [%expect
      {|
    (ETuple ((EList [(EConst (CInt 1)); (EConst (CInt 2))]),
       (EList [(EConst (CInt 3)); (EConst (CInt 4))]), [])) 
    |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "[1, 2, 3; 4, 5, 6]";
    [%expect
      {|
    (EList
       [(ETuple ((EConst (CInt 1)), (EConst (CInt 2)), [(EConst (CInt 3))]));
         (ETuple ((EConst (CInt 4)), (EConst (CInt 5)), [(EConst (CInt 6))]))]) 
    |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "let () = () in ()";
    [%expect {| (ELet (NonRec, "()", (EConst CUnit), (Some (EConst CUnit)))) |}]
  ;;
end

module InferTests = struct
  open Infer
  open Typing

  let pp_infer e =
    match run_infer e with
    | Ok ty -> Stdlib.Format.printf "%a" pp_ty ty
    | Error err -> Stdlib.Format.printf "%a" pp_error err
  ;;

  let pp_parse_and_infer input =
    match Parser.parse_expr input with
    | Result.Ok e -> pp_infer e
    | _ -> Stdlib.print_endline "Failed to parse"
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let x = (42, false, fun x -> x)";
    [%expect {| int * bool * ('0 -> '0) |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let f x y = [x; y; x = y]";
    [%expect {| bool -> bool -> bool list |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let f x y = [x; y] in f 42";
    [%expect {| int -> int list |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer
      "let rec fact x useless_var = if x = 1 then x else x * fact (x - 1) useless_var";
    [%expect {| int -> '2 -> int |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let rec fact x = if x = 1 then x else x * fact (x - 1) in fact 42";
    [%expect {| int |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let f x = match x with | (h :: tl1) :: tl2 -> true | _ -> false";
    [%expect {| '2 list list -> bool |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer
      {|
    let rec fold_left op acc xs = match xs with
    | []   -> acc
    | h :: t -> fold_left op (op acc h) t
    |};
    [%expect {| ('12 -> '5 -> '12) -> '12 -> '5 list -> '12 |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer
      {|
    let split xs = match xs with 
      | a :: b :: tl -> a, b, tl
    |};
    [%expect {| '3 list -> '3 * '3 * '3 list |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer
      {|
    let rec even_length xs = match xs with 
    | h :: h :: tl -> even_length tl
    | h :: [] -> false
    | _ -> true
    |};
    [%expect {| '3 list -> bool |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "[(fun x y -> x = y); (fun x y -> x <> y)]";
    [%expect {| ('3 -> '3 -> bool) list |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "fun x -> x, fun x y -> x, y";
    [%expect {| '0 -> '0 * ('1 -> '2 -> '1 * '2) |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "(fun x -> x), (fun x y -> x, y)";
    [%expect {| ('0 -> '0) * ('1 -> '2 -> '1 * '2) |}]
  ;;

  (* Errors *)

  let%expect_test _ =
    pp_parse_and_infer "let f x = x + y";
    [%expect {| Unbound value y |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let rec f x = f";
    [%expect {| The type variable '0 occurs inside '1 -> '0 |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "[1; 1, 2]";
    [%expect {| Failed to unify types int and int * int |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let f x = [fun x -> x + x; fun x -> x >= x]";
    [%expect {| Failed to unify types int and bool |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let () = if true then 1";
    [%expect {| Failed to unify types int and unit |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer
      "let f x = match x with | a :: b -> a | ((a :: true) :: c) :: tl -> c ";
    [%expect {| Failed to unify types '4 list and bool |}]
  ;;
end
