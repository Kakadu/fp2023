(** Copyright 2021-2023, LeonidElkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format
open Parser

let test_parse str expected =
  match parse str with
  | Ok actual ->
    let is_eq = List.equal equal_decl expected actual in
    if is_eq then () else printf "Actual %a\n" pp_program actual;
    is_eq
  | Error err ->
    printf "%s\n" err;
    false
;;

let%test _ =
  test_parse
    " let rec fact ( n : int ) : int = if n < 1 then 1 else n * fact (n - 1);; "
    [ LetDecl
        ( true
        , "fact"
        , Fun
            ( PVar "n"
            , IfThenElse
                ( BinOp (Less, Var "n", Const (Int 1))
                , Const (Int 1)
                , BinOp
                    ( Asterisk
                    , Var "n"
                    , Apply (Var "fact", BinOp (Dash, Var "n", Const (Int 1))) ) ) ) )
    ]
;;

let%test _ =
  test_parse
    " let foo = fun n -> n + 1 "
    [ LetDecl (false, "foo", Fun (PVar "n", BinOp (Plus, Var "n", Const (Int 1)))) ]
;;

let%test _ =
  test_parse
    " let foo ( n: int * string list * bool * char ) = 1 "
    [ LetDecl (false, "foo", Fun (PVar "n", Const (Int 1))) ]
;;

let%test _ =
  test_parse
    " let foo ( n: int option * string option list option * bool option * char option ) \
     = 1 "
    [ LetDecl (false, "foo", Fun (PVar "n", Const (Int 1))) ]
;;

let%test _ =
  test_parse
    " let foo ((((n : (((((int))))))))) = 1 "
    [ LetDecl (false, "foo", Fun (PVar "n", Const (Int 1))) ]
;;

let%test _ =
  test_parse
    " let foo ( n : (int -> int -> int) option) = 1 "
    [ LetDecl (false, "foo", Fun (PVar "n", Const (Int 1))) ]
;;

let%test _ =
  test_parse
    " let foo ?(n = 1) = 1 "
    [ LetDecl
        (false, "foo", Fun (PArg ("n", Optional (Some (Const (Int 1)))), Const (Int 1)))
    ]
;;

let%test _ =
  test_parse
    " let foo ?(n = (1 : int)) = 1 "
    [ LetDecl
        (false, "foo", Fun (PArg ("n", Optional (Some (Const (Int 1)))), Const (Int 1)))
    ]
;;

let%test _ =
  test_parse
    " let foo ?n = 1 "
    [ LetDecl (false, "foo", Fun (PArg ("n", Optional None), Const (Int 1))) ]
;;

let%test _ =
  test_parse
    " let foo ?( n : int) = 1 "
    [ LetDecl (false, "foo", Fun (PArg ("n", Optional None), Const (Int 1))) ]
;;

let%test _ =
  test_parse
    " let foo ~x = 1 "
    [ LetDecl (false, "foo", Fun (PArg ("x", Label None), Const (Int 1))) ]
;;

let%test _ =
  test_parse
    " let foo ~x:x1 = 1 "
    [ LetDecl (false, "foo", Fun (PArg ("x", Label (Some "x1")), Const (Int 1))) ]
;;

let%test _ =
  test_parse
    " let foo ~(x:int) = 1 "
    [ LetDecl (false, "foo", Fun (PArg ("x", Label None), Const (Int 1))) ]
;;

let%test _ =
  test_parse
    " let foo ~(x:(x1:int)) = 1 "
    [ LetDecl (false, "foo", Fun (PArg ("x", Label (Some "x1")), Const (Int 1))) ]
;;

let%test _ =
  test_parse
    " let foo ( n : ( int * string list * bool * char ) list ) = 1 "
    [ LetDecl (false, "foo", Fun (PVar "n", Const (Int 1))) ]
;;

let%test _ =
  test_parse
    " let foo ( n : (( int * (string * bool * char * int) list * bool * char ) list * \
     string * bool ) list ) = 1 "
    [ LetDecl (false, "foo", Fun (PVar "n", Const (Int 1))) ]
;;

let%test _ =
  test_parse
    " let foo n = let helper n = n * 3 in helper n "
    [ LetDecl
        ( false
        , "foo"
        , Fun
            ( PVar "n"
            , EDecl
                ( LetDecl
                    ( false
                    , "helper"
                    , Fun (PVar "n", BinOp (Asterisk, Var "n", Const (Int 3))) )
                , Apply (Var "helper", Var "n") ) ) )
    ]
;;

let%test _ =
  test_parse
    " let foo = fun n -> n + 1 "
    [ LetDecl (false, "foo", Fun (PVar "n", BinOp (Plus, Var "n", Const (Int 1)))) ]
;;

let%test _ =
  test_parse
    " let foo = fun n -> ( let helper n = n * 3 in helper n ) "
    (*Doesn't work without brackets*)
    [ LetDecl
        ( false
        , "foo"
        , Fun
            ( PVar "n"
            , EDecl
                ( LetDecl
                    ( false
                    , "helper"
                    , Fun (PVar "n", BinOp (Asterisk, Var "n", Const (Int 3))) )
                , Apply (Var "helper", Var "n") ) ) )
    ]
;;

let%test _ =
  test_parse
    " let foo1 = true let foo2 = \"lets'go\""
    [ LetDecl (false, "foo1", Const (Bool true))
    ; LetDecl (false, "foo2", Const (String "lets'go"))
    ]
;;

let%test _ =
  test_parse
    " let foo x = if x = 2 then x - 1 else x / 2"
    [ LetDecl
        ( false
        , "foo"
        , Fun
            ( PVar "x"
            , IfThenElse
                ( BinOp (Eq, Var "x", Const (Int 2))
                , BinOp (Dash, Var "x", Const (Int 1))
                , BinOp (Slash, Var "x", Const (Int 2)) ) ) )
    ]
;;

let%test _ =
  test_parse
    " let foo x = if x <> 2 && x >= 3 || x <= 8 then x - 1 else x * 2"
    [ LetDecl
        ( false
        , "foo"
        , Fun
            ( PVar "x"
            , IfThenElse
                ( BinOp
                    ( Or
                    , BinOp
                        ( And
                        , BinOp (Neq, Var "x", Const (Int 2))
                        , BinOp (Greaterq, Var "x", Const (Int 3)) )
                    , BinOp (Lessq, Var "x", Const (Int 8)) )
                , BinOp (Dash, Var "x", Const (Int 1))
                , BinOp (Asterisk, Var "x", Const (Int 2)) ) ) )
    ]
;;

let%test _ =
  test_parse
    " let foo l = match l with | [] -> 1 | hd :: tl -> 1 | _ -> helper x"
    [ LetDecl
        ( false
        , "foo"
        , Fun
            ( PVar "l"
            , Match
                ( Var "l"
                , [ PNill, Const (Int 1)
                  ; PCons (PVar "hd", PVar "tl"), Const (Int 1)
                  ; PEmpty, Apply (Var "helper", Var "x")
                  ] ) ) )
    ]
;;

let%test _ =
  test_parse
    " let is_empty l = match l with | [] -> true | hd :: tl -> false"
    [ LetDecl
        ( false
        , "is_empty"
        , Fun
            ( PVar "l"
            , Match
                ( Var "l"
                , [ PNill, Const (Bool true)
                  ; PCons (PVar "hd", PVar "tl"), Const (Bool false)
                  ] ) ) )
    ]
;;

let%test _ =
  test_parse
    " let foo l = match l with [] -> false | hd :: tl -> true"
    [ LetDecl
        ( false
        , "foo"
        , Fun
            ( PVar "l"
            , Match
                ( Var "l"
                , [ PNill, Const (Bool false)
                  ; PCons (PVar "hd", PVar "tl"), Const (Bool true)
                  ] ) ) )
    ]
;;

let%test _ =
  test_parse
    " let foo l = match l with [] -> false | hd :: tl -> true;;\n\
    \    let foo1 l = match l with [] -> false | hd :: tl -> true;; "
    [ LetDecl
        ( false
        , "foo"
        , Fun
            ( PVar "l"
            , Match
                ( Var "l"
                , [ PNill, Const (Bool false)
                  ; PCons (PVar "hd", PVar "tl"), Const (Bool true)
                  ] ) ) )
    ; LetDecl
        ( false
        , "foo1"
        , Fun
            ( PVar "l"
            , Match
                ( Var "l"
                , [ PNill, Const (Bool false)
                  ; PCons (PVar "hd", PVar "tl"), Const (Bool true)
                  ] ) ) )
    ]
;;

let%test _ =
  test_parse
    " let foo1 ~x = x + 1;; let foo2 = foo1 ~x:1"
    [ LetDecl
        (false, "foo1", Fun (PArg ("x", Label None), BinOp (Plus, Var "x", Const (Int 1))))
    ; LetDecl (false, "foo2", Apply (Var "foo1", LabeledVar ("x", Const (Int 1))))
    ]
;;

let%test _ = test_parse "" []
let%test _ = test_parse "\n" []
let%test _ = test_parse "let n = 5" [ LetDecl (false, "n", Const (Int 5)) ]

let%test _ =
  test_parse
    "let x : bool = (false || true) && true"
    [ LetDecl
        ( false
        , "x"
        , BinOp (And, BinOp (Or, Const (Bool false), Const (Bool true)), Const (Bool true))
        )
    ]
;;

let%test _ =
  test_parse
    "let n = fun x -> x+ 5\n    let a = n 4"
    [ LetDecl (false, "n", Fun (PVar "x", BinOp (Plus, Var "x", Const (Int 5))))
    ; LetDecl (false, "a", Apply (Var "n", Const (Int 4)))
    ]
;;

let%test _ =
  test_parse
    "let rec factorial_recursive = fun n -> if n <= 1 then 1 else n * \
     factorial_recursive (n - 1)\n\
    \     let a = factorial_recursive 5\n\
    \     let b = factorial_recursive 6"
    [ LetDecl
        ( true
        , "factorial_recursive"
        , Fun
            ( PVar "n"
            , IfThenElse
                ( BinOp (Lessq, Var "n", Const (Int 1))
                , Const (Int 1)
                , BinOp
                    ( Asterisk
                    , Var "n"
                    , Apply
                        (Var "factorial_recursive", BinOp (Dash, Var "n", Const (Int 1)))
                    ) ) ) )
    ; LetDecl (false, "a", Apply (Var "factorial_recursive", Const (Int 5)))
    ; LetDecl (false, "b", Apply (Var "factorial_recursive", Const (Int 6)))
    ]
;;

let%test _ = test_parse "let rec n = 5" [ LetDecl (true, "n", Const (Int 5)) ]

let%test _ =
  test_parse
    "let n = fun y -> (let x = 5 in x + y)\n let f = n 7"
    [ LetDecl
        ( false
        , "n"
        , Fun
            ( PVar "y"
            , EDecl (LetDecl (false, "x", Const (Int 5)), BinOp (Plus, Var "x", Var "y"))
            ) )
    ; LetDecl (false, "f", Apply (Var "n", Const (Int 7)))
    ]
;;

let%test _ =
  test_parse
    "let n = fun y -> if y > 7 then 5 else 3\n let f = n 7"
    [ LetDecl
        ( false
        , "n"
        , Fun
            ( PVar "y"
            , IfThenElse
                (BinOp (Greater, Var "y", Const (Int 7)), Const (Int 5), Const (Int 3)) )
        )
    ; LetDecl (false, "f", Apply (Var "n", Const (Int 7)))
    ]
;;

let%test _ =
  test_parse
    "let n = fun y -> if y > 7 then 5 else 3\n let f = n 100"
    [ LetDecl
        ( false
        , "n"
        , Fun
            ( PVar "y"
            , IfThenElse
                (BinOp (Greater, Var "y", Const (Int 7)), Const (Int 5), Const (Int 3)) )
        )
    ; LetDecl (false, "f", Apply (Var "n", Const (Int 100)))
    ]
;;

let%test _ =
  test_parse
    "let h = fun h :: tl -> h\n"
    [ LetDecl (false, "h", Fun (PCons (PVar "h", PVar "tl"), Var "h")) ]
;;

let%test _ =
  test_parse
    "let h = fun h :: tl -> h\n\
    \ let tl = fun h :: tl -> tl\n\
    \ let n = h (4 :: 5 :: 6)\n\
    \ let m = tl (4 :: 5 :: 6)"
    [ LetDecl (false, "h", Fun (PCons (PVar "h", PVar "tl"), Var "h"))
    ; LetDecl (false, "tl", Fun (PCons (PVar "h", PVar "tl"), Var "tl"))
    ; LetDecl
        ( false
        , "n"
        , Apply
            ( Var "h"
            , BinOp (Cons, Const (Int 4), BinOp (Cons, Const (Int 5), Const (Int 6))) ) )
    ; LetDecl
        ( false
        , "m"
        , Apply
            ( Var "tl"
            , BinOp (Cons, Const (Int 4), BinOp (Cons, Const (Int 5), Const (Int 6))) ) )
    ]
;;

let%test _ =
  test_parse
    "let sum = fun (a, b) -> a + b\n\n\
    \ let sub = fun (a, b) -> a - b\n\
    \ let mul = fun (a, b) -> a * b\n"
    [ LetDecl
        (false, "sum", Fun (PTuple [ PVar "a"; PVar "b" ], BinOp (Plus, Var "a", Var "b")))
    ; LetDecl
        (false, "sub", Fun (PTuple [ PVar "a"; PVar "b" ], BinOp (Dash, Var "a", Var "b")))
    ; LetDecl
        ( false
        , "mul"
        , Fun (PTuple [ PVar "a"; PVar "b" ], BinOp (Asterisk, Var "a", Var "b")) )
    ]
;;
