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

  let%expect_test _ =
    pp
      pp_expr
      parse_expr
      {|
      let rec equal_lengths l1 l2 =
        match l1, l2 with
        | _ :: _, [] | [], _ :: _ -> false
        | [], [] -> true
        | _ :: tl1, _ :: tl2 -> equal_lengths tl1 tl2
      |};
    [%expect
      {|
        (ELet (Rec, "equal_lengths",
           (EFun ("l1",
              (EFun ("l2",
                 (EMatch ((ETuple ((EVar "l1"), (EVar "l2"), [])),
                    [((POr ((PTuple ((PCons (PWild, PWild, [])), PEmpty, [])),
                         (PTuple (PEmpty, (PCons (PWild, PWild, [])), [])), [])),
                      (EConst (CBool false)));
                      ((PTuple (PEmpty, PEmpty, [])), (EConst (CBool true)));
                      ((PTuple ((PCons (PWild, (PVar "tl1"), [])),
                          (PCons (PWild, (PVar "tl2"), [])), [])),
                       (EApp ((EApp ((EVar "equal_lengths"), (EVar "tl1"))),
                          (EVar "tl2"))))
                      ]
                    ))
                 ))
              )),
           None)) |}]
  ;;

  let%expect_test _ =
    pp
      pp_expr
      parse_expr
      {|
      let rec zip_sum_all lst init =
        match lst with
        | (l, r) :: tl -> zip_sum_all tl l + r + init
        | [] -> init
      |};
    [%expect
      {|
            (ELet (Rec, "zip_sum_all",
               (EFun ("lst",
                  (EFun ("init",
                     (EMatch ((EVar "lst"),
                        [((PCons ((PTuple ((PVar "l"), (PVar "r"), [])), (PVar "tl"),
                             [])),
                          (EBinop (Add,
                             (EBinop (Add,
                                (EApp ((EApp ((EVar "zip_sum_all"), (EVar "tl"))),
                                   (EVar "l"))),
                                (EVar "r"))),
                             (EVar "init"))));
                          (PEmpty, (EVar "init"))]
                        ))
                     ))
                  )),
               None)) |}]
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
    [%expect {| ('12 -> '6 -> '12) -> '12 -> '6 list -> '12 |}]
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
    [%expect {| '7 list -> bool |}]
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

  let%expect_test _ =
    pp_parse_and_infer
      {| let permutation_of_123 l =
        match l with
        | 1 :: 2 :: 3 :: []
        | 1 :: 3 :: 2 :: []
        | 2 :: 1 :: 3 :: []
        | 2 :: 3 :: 1 :: []
        | 3 :: 1 :: 2 :: []
        | 3 :: 2 :: 1 :: [] -> true
        | _ -> false |};
    [%expect {| int list -> bool |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer
      {|
    let rec equal_lengths l1 l2 =
      match l1, l2 with
      | _ :: _, [] | [], _ :: _ -> false
      | [], [] -> true
      | _ :: tl1, _ :: tl2 -> equal_lengths tl1 tl2
    |};
    [%expect {| '12 list -> '14 list -> bool |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer
      {| 
    let rec zip_sum_all lst init =
      match lst with
      | (l, r) :: tl -> zip_sum_all tl l + r + init
      | [] -> init 
    |};
    [%expect {| (int * int) list -> int -> int |}]
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

  let%expect_test _ =
    pp_parse_and_infer
      "let f x = match x with | a :: b :: y :: tl | a :: b :: z :: tl -> tl";
    [%expect {| Variable z doesn't occure in some 'or' patterns |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let f x = match x with | a :: 1 :: _ | a :: false :: _ -> a";
    [%expect
      {| Variable a has different types in 'or' patterns: int and bool are not equal |}]
  ;;
end
