(** Copyright 2023-2024, Lesh79 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser

let test_parse str expected =
  match parse str with
  | Ok actual ->
    let is_eq = List.equal equal_decl expected actual in
    if is_eq then () else Format.printf "Actual %a\n" pp_program actual;
    is_eq
  | Error err ->
    Format.printf "%s\n" err;
    false
;;

let%test _ = test_parse "let f = 5" [ DeclLet (false, "f", EInt 5) ]

let%test _ =
  test_parse
    "let rec fac = fun n -> if n < 2 then 1 else n * fac (n-1)"
    [ DeclLet
        ( true
        , "fac"
        , EFun
            ( PVar "n"
            , EIfThenElse
                ( EBinop (Lt, EVar "n", EInt 2)
                , EInt 1
                , EBinop
                    (Multi, EVar "n", EApp (EVar "fac", EBinop (Minus, EVar "n", EInt 1)))
                ) ) )
    ]
;;

let%test _ =
  test_parse
    "let rec fib = fun n -> if n < 2 then n else fib (n - 1) + fib (n - 2) let test = \
     fib 0"
    [ DeclLet
        ( true
        , "fib"
        , EFun
            ( PVar "n"
            , EIfThenElse
                ( EBinop (Lt, EVar "n", EInt 2)
                , EVar "n"
                , EBinop
                    ( Plus
                    , EApp (EVar "fib", EBinop (Minus, EVar "n", EInt 1))
                    , EApp (EVar "fib", EBinop (Minus, EVar "n", EInt 2)) ) ) ) )
    ; DeclLet (false, "test", EApp (EVar "fib", EInt 0))
    ]
;;

let%test _ =
  test_parse
    "let fff = fun n -> if n then 10 else (1, 2, 3)"
    [ DeclLet
        ( false
        , "fff"
        , EFun
            (PVar "n", EIfThenElse (EVar "n", EInt 10, ETuple [ EInt 1; EInt 2; EInt 3 ]))
        )
    ]
;;

let%test _ =
  test_parse
    "let x = (false || true) && true"
    [ DeclLet (false, "x", EBinop (And, EBinop (Or, EBool false, EBool true), EBool true))
    ]
;;

let%test _ =
  test_parse
    "let x = if 1+1=2 then 1 else 0"
    [ DeclLet
        ( false
        , "x"
        , EIfThenElse
            (EBinop (Equal, EBinop (Plus, EInt 1, EInt 1), EInt 2), EInt 1, EInt 0) )
    ]
;;

let%test _ =
  test_parse
    "let a =fun x -> x\n      let b = a 5"
    [ DeclLet (false, "a", EFun (PVar "x", EVar "x"))
    ; DeclLet (false, "b", EApp (EVar "a", EInt 5))
    ]
;;

let%test _ =
  test_parse
    "let input = fun x -> match x with | 1 -> \"one\" | 2 -> \"two\" | _ -> \"other\""
    [ DeclLet
        ( false
        , "input"
        , EFun
            ( PVar "x"
            , EMatch
                ( EVar "x"
                , [ PInt 1, EString "one"
                  ; PInt 2, EString "two"
                  ; PVar "_", EString "other"
                  ] ) ) )
    ]
;;

let%test _ =
  test_parse
    "let summ = fun x -> x + 8\n  let res = summ 8"
    [ DeclLet (false, "summ", EFun (PVar "x", EBinop (Plus, EVar "x", EInt 8)))
    ; DeclLet (false, "res", EApp (EVar "summ", EInt 8))
    ]
;;

let%test _ =
  test_parse "let x = (1, 2)" [ DeclLet (false, "x", ETuple [ EInt 1; EInt 2 ]) ]
;;

let%test _ =
  test_parse
    "let rec fix = (fun f -> (fun x -> f (fix f) x))\n\
    \  let fac_cps = (fix (fun self -> (fun (n,k) -> if n <= 1 then k 1 else self (n - \
     1) (fun x -> k (n * x)))))\n\
    \  let a = fac_cps (6, (fun x -> x))"
    [ DeclLet
        ( true
        , "fix"
        , EFun
            ( PVar "f"
            , EFun
                (PVar "x", EApp (EApp (EVar "f", EApp (EVar "fix", EVar "f")), EVar "x"))
            ) )
    ; DeclLet
        ( false
        , "fac_cps"
        , EApp
            ( EVar "fix"
            , EFun
                ( PVar "self"
                , EFun
                    ( PTuple [ PVar "n"; PVar "k" ]
                    , EIfThenElse
                        ( EBinop (Ltq, EVar "n", EInt 1)
                        , EApp (EVar "k", EInt 1)
                        , EApp
                            ( EApp (EVar "self", EBinop (Minus, EVar "n", EInt 1))
                            , EFun
                                ( PVar "x"
                                , EApp (EVar "k", EBinop (Multi, EVar "n", EVar "x")) ) )
                        ) ) ) ) )
    ; DeclLet
        (false, "a", EApp (EVar "fac_cps", ETuple [ EInt 6; EFun (PVar "x", EVar "x") ]))
    ]
;;
