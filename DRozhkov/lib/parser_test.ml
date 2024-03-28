(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser
open Base

let test_parse input =
  let open Stdlib.Format in
  match parse input with
  | Ok ast -> printf "%a\n" pp_exprs ast
  | Error s -> printf "Parsing error: %s\n" s
;;

let%expect_test _ =
  test_parse {|
        let x = 5 + 6
        let y = 7 + 8
        let z = x + y
      |};
  [%expect
    {|
      [(ELet (NoRec, "x", (EBinop ((EConst (Int 5)), Plus, (EConst (Int 6)))), None
          ));
        (ELet (NoRec, "y", (EBinop ((EConst (Int 7)), Plus, (EConst (Int 8)))),
           None));
        (ELet (NoRec, "z", (EBinop ((Var "x"), Plus, (Var "y"))), None))]
    |}]
;;

let%expect_test _ =
  test_parse
    {|
        let x = 5 in
        let rec fact = if x < 1 then 1 else x * fact (x - 1)
      |};
  [%expect
    {|
        [(ELet (NoRec, "x", (EConst (Int 5)),
            (Some (ELet (Rec, "fact",
                     (EIfThenElse ((EBinop ((Var "x"), Less, (EConst (Int 1)))),
                        (EConst (Int 1)),
                        (EBinop ((Var "x"), Mult,
                           (EApp ((Var "fact"),
                              (EBinop ((Var "x"), Minus, (EConst (Int 1))))))
                           ))
                        )),
                     None)))
            ))
          ]
    |}]
;;

let%expect_test _ =
  test_parse {|
        let x = [1; 2; 3]
      |};
  [%expect
    {|
        [(ELet (NoRec, "x",
            (EList [(EConst (Int 1)); (EConst (Int 2)); (EConst (Int 3))]), None))
          ]
    |}]
;;

let%expect_test _ =
  test_parse {|
        let x = true
      |};
  [%expect {|
        [(ELet (NoRec, "x", (EConst (Bool true)), None))]
    |}]
;;

let%expect_test _ =
  test_parse
    {|
      let mymatch x =
        match x with
          | 1 -> true
          | _ -> false
 
      let result = mymatch 2
       |};
  [%expect
    {| 
        [(ELet (NoRec, "mymatch",
            (EFun ("x",
               (EMatch ((Var "x"),
                  [((PConst (Int 1)), (EConst (Bool true)));
                    (PDash, (EConst (Bool false)))]
                  ))
               )),
            None));
          (ELet (NoRec, "result", (EApp ((Var "mymatch"), (EConst (Int 2)))), None))]
      |}]
;;

let%expect_test _ =
  test_parse
    {|
      let x = true
    
      let mymatch x =
        match x with
          | z -> 100
          | _ -> 99
       |};
  [%expect
    {| 
        [(ELet (NoRec, "x", (EConst (Bool true)), None));
          (ELet (NoRec, "mymatch",
             (EFun ("x",
                (EMatch ((Var "x"),
                   [((PVar "z"), (EConst (Int 100))); (PDash, (EConst (Int 99)))]))
                )),
             None))
          ]
      |}]
;;

let%expect_test _ =
  test_parse
    {|
      let mymatch x =
        match x with
          | hd :: tl -> hd
          | _ -> 0
       |};
  [%expect
    {| 
         [(ELet (NoRec, "mymatch",
             (EFun ("x",
                (EMatch ((Var "x"),
                   [((PList ((PVar "hd"), (PVar "tl"))), (Var "hd"));
                     (PDash, (EConst (Int 0)))]
                   ))
                )),
             None))
           ]
      |}]
;;

let%expect_test _ =
  test_parse
    {|
      let mymatch x =
        match x with
          | hd :: tl -> tl
          | _ -> 0
       |};
  [%expect
    {| 
         [(ELet (NoRec, "mymatch",
             (EFun ("x",
                (EMatch ((Var "x"),
                   [((PList ((PVar "hd"), (PVar "tl"))), (Var "tl"));
                     (PDash, (EConst (Int 0)))]
                   ))
                )),
             None))
           ]
      |}]
;;

let%expect_test _ =
  test_parse
    {|
      let mymatch x =
        match x with
          | hd :: tl -> hd
          | _ -> 0
       |};
  [%expect
    {| 
         [(ELet (NoRec, "mymatch",
             (EFun ("x",
                (EMatch ((Var "x"),
                   [((PList ((PVar "hd"), (PVar "tl"))), (Var "hd"));
                     (PDash, (EConst (Int 0)))]
                   ))
                )),
             None))
           ]
      |}]
;;
