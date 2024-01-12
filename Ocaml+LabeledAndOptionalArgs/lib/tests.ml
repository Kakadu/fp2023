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
    " let rec fact ( n : int ) : int = if n < 1 then 1 else n * fact (n - 1) "
    [ LetDecl
        ( true
        , "fact"
        , Fun
            ( PArg ("n", NoLabel, IntType false)
            , IfThenElse
                ( BinOp (Less, Var "n", Const (Int 1))
                , Const (Int 1)
                , BinOp
                    ( Asterisk
                    , Var "n"
                    , Apply (Var "fact", BinOp (Dash, Var "n", Const (Int 1))) ) ) )
        , IntType false )
    ]
;;

let%test _ =
  test_parse
    " let foo = fun n -> n + 1 "
    [ LetDecl
        ( false
        , "foo"
        , Fun (PArg ("n", NoLabel, EmptyType), BinOp (Plus, Var "n", Const (Int 1)))
        , EmptyType )
    ]
;;

let%test _ =
  test_parse
    " let foo ( n: int * string list * bool * char ) = 1 "
    [ LetDecl
        ( false
        , "foo"
        , Fun
            ( PArg
                ( "n"
                , NoLabel
                , TupleType
                    ( [ IntType false
                      ; ListType (StringType false, false)
                      ; BoolType false
                      ; CharType false
                      ]
                    , false ) )
            , Const (Int 1) )
        , EmptyType )
    ]
;;

let%test _ =
  test_parse
    " let foo ( n: int option * string option list option * bool option * char option ) \
     = 1 "
    [ LetDecl
        ( false
        , "foo"
        , Fun
            ( PArg
                ( "n"
                , NoLabel
                , TupleType
                    ( [ IntType true
                      ; ListType (StringType true, true)
                      ; BoolType true
                      ; CharType true
                      ]
                    , false ) )
            , Const (Int 1) )
        , EmptyType )
    ]
;;

let%test _ =
  test_parse
    " let foo ((((n : (((((int))))))))) = 1 "
    [ LetDecl
        (false, "foo", Fun (PArg ("n", NoLabel, IntType false), Const (Int 1)), EmptyType)
    ]
;;

let%test _ =
  test_parse
    " let foo ( n : (int -> int -> int) option) = 1 "
    [ LetDecl
        ( false
        , "foo"
        , Fun
            ( PArg
                ( "n"
                , NoLabel
                , FuncType
                    (IntType false, FuncType (IntType false, IntType false, false), true)
                )
            , Const (Int 1) )
        , EmptyType )
    ]
;;

let%test _ =
  test_parse
    " let foo ?(n = 1) = 1 "
    [ LetDecl
        ( false
        , "foo"
        , Fun (PArg ("n", Optional (Some (Int 1)), EmptyType), Const (Int 1))
        , EmptyType )
    ]
;;

let%test _ =
  test_parse
    " let foo ?(n = (1 : int)) = 1 "
    [ LetDecl
        ( false
        , "foo"
        , Fun (PArg ("n", Optional (Some (Int 1)), IntType false), Const (Int 1))
        , EmptyType )
    ]
;;

let%test _ =
  test_parse
    " let foo ?n = 1 "
    [ LetDecl
        ( false
        , "foo"
        , Fun (PArg ("n", Optional None, EmptyType), Const (Int 1))
        , EmptyType )
    ]
;;

let%test _ =
  test_parse
    " let foo ?( n : int) = 1 "
    [ LetDecl
        ( false
        , "foo"
        , Fun (PArg ("n", Optional None, IntType false), Const (Int 1))
        , EmptyType )
    ]
;;

let%test _ =
  test_parse
    " let foo ~x = 1 "
    [ LetDecl
        (false, "foo", Fun (PArg ("x", Label "x", EmptyType), Const (Int 1)), EmptyType)
    ]
;;

let%test _ =
  test_parse
    " let foo ~x:x1 = 1 "
    [ LetDecl
        (false, "foo", Fun (PArg ("x", Label "x1", EmptyType), Const (Int 1)), EmptyType)
    ]
;;

let%test _ =
  test_parse
    " let foo ~(x:int) = 1 "
    [ LetDecl
        ( false
        , "foo"
        , Fun (PArg ("x", Label "x", IntType false), Const (Int 1))
        , EmptyType )
    ]
;;

let%test _ =
  test_parse
    " let foo ~(x:(x1:int)) = 1 "
    [ LetDecl
        ( false
        , "foo"
        , Fun (PArg ("x", Label "x1", IntType false), Const (Int 1))
        , EmptyType )
    ]
;;

let%test _ =
  test_parse
    " let foo ( n : ( int * string list * bool * char ) list ) = 1 "
    [ LetDecl
        ( false
        , "foo"
        , Fun
            ( PArg
                ( "n"
                , NoLabel
                , ListType
                    ( TupleType
                        ( [ IntType false
                          ; ListType (StringType false, false)
                          ; BoolType false
                          ; CharType false
                          ]
                        , false )
                    , false ) )
            , Const (Int 1) )
        , EmptyType )
    ]
;;

let%test _ =
  test_parse
    " let foo ( n : (( int * (string * bool * char * int) list * bool * char ) list * \
     string * bool ) list ) = 1 "
    [ LetDecl
        ( false
        , "foo"
        , Fun
            ( PArg
                ( "n"
                , NoLabel
                , ListType
                    ( TupleType
                        ( [ ListType
                              ( TupleType
                                  ( [ IntType false
                                    ; ListType
                                        ( TupleType
                                            ( [ StringType false
                                              ; BoolType false
                                              ; CharType false
                                              ; IntType false
                                              ]
                                            , false )
                                        , false )
                                    ; BoolType false
                                    ; CharType false
                                    ]
                                  , false )
                              , false )
                          ; StringType false
                          ; BoolType false
                          ]
                        , false )
                    , false ) )
            , Const (Int 1) )
        , EmptyType )
    ]
;;

let%test _ =
  test_parse
    " let foo n = let helper n = n * 3 in helper n "
    [ LetDecl
        ( false
        , "foo"
        , Fun
            ( PArg ("n", NoLabel, EmptyType)
            , EDecl
                ( LetDecl
                    ( false
                    , "helper"
                    , Fun
                        ( PArg ("n", NoLabel, EmptyType)
                        , BinOp (Asterisk, Var "n", Const (Int 3)) )
                    , EmptyType )
                , Apply (Var "helper", Var "n") ) )
        , EmptyType )
    ]
;;

let%test _ =
  test_parse
    " let foo = fun n -> n + 1 "
    [ LetDecl
        ( false
        , "foo"
        , Fun (PArg ("n", NoLabel, EmptyType), BinOp (Plus, Var "n", Const (Int 1)))
        , EmptyType )
    ]
;;

let%test _ =
  test_parse
    " let foo = fun n -> ( let helper n = n * 3 in helper n ) "
    (*Doesn't work without brackets*)
    [ LetDecl
        ( false
        , "foo"
        , Fun
            ( PArg ("n", NoLabel, EmptyType)
            , EDecl
                ( LetDecl
                    ( false
                    , "helper"
                    , Fun
                        ( PArg ("n", NoLabel, EmptyType)
                        , BinOp (Asterisk, Var "n", Const (Int 3)) )
                    , EmptyType )
                , Apply (Var "helper", Var "n") ) )
        , EmptyType )
    ]
;;

let%test _ =
  test_parse
    " let foo1 = true let foo2 = \"lets'go\""
    [ LetDecl (false, "foo1", Const (Bool true), EmptyType)
    ; LetDecl (false, "foo2", Const (String "lets'go"), EmptyType)
    ]
;;

let%test _ =
  test_parse
    " let foo x = if x = 2 then x - 1 else x / 2"
    [ LetDecl
        ( false
        , "foo"
        , Fun
            ( PArg ("x", NoLabel, EmptyType)
            , IfThenElse
                ( BinOp (Eq, Var "x", Const (Int 2))
                , BinOp (Dash, Var "x", Const (Int 1))
                , BinOp (Slash, Var "x", Const (Int 2)) ) )
        , EmptyType )
    ]
;;

let%test _ =
  test_parse
    " let foo x = if x <> 2 && x >= 3 || x <= 8 then x - 1 else x * 2"
    [ LetDecl
        ( false
        , "foo"
        , Fun
            ( PArg ("x", NoLabel, EmptyType)
            , IfThenElse
                ( BinOp
                    ( Or
                    , BinOp
                        ( And
                        , BinOp (Neq, Var "x", Const (Int 2))
                        , BinOp (Greaterq, Var "x", Const (Int 3)) )
                    , BinOp (Lessq, Var "x", Const (Int 8)) )
                , BinOp (Dash, Var "x", Const (Int 1))
                , BinOp (Asterisk, Var "x", Const (Int 2)) ) )
        , EmptyType )
    ]
;;

let%test _ =
  test_parse
    " let foo l = match l with | [] -> 1 | hd :: tl -> 1 | _ -> helper x"
    [ LetDecl
        ( false
        , "foo"
        , Fun
            ( PArg ("l", NoLabel, EmptyType)
            , Match
                ( Var "l"
                , [ PNil, Const (Int 1)
                  ; PCons (PVar "hd", PVar "tl"), Const (Int 1)
                  ; PEmpty, Apply (Var "helper", Var "x")
                  ] ) )
        , EmptyType )
    ]
;;

let%test _ =
  test_parse
    " let is_empty l = match l with | [] -> true | hd :: tl -> false"
    [ LetDecl
        ( false
        , "is_empty"
        , Fun
            ( PArg ("l", NoLabel, EmptyType)
            , Match
                ( Var "l"
                , [ PNil, Const (Bool true)
                  ; PCons (PVar "hd", PVar "tl"), Const (Bool false)
                  ] ) )
        , EmptyType )
    ]
;;

let%test _ =
  test_parse
    " let foo l = match l with [] -> false | hd :: tl -> true"
    [ LetDecl
        ( false
        , "foo"
        , Fun
            ( PArg ("l", NoLabel, EmptyType)
            , Match
                ( Var "l"
                , [ PNil, Const (Bool false)
                  ; PCons (PVar "hd", PVar "tl"), Const (Bool true)
                  ] ) )
        , EmptyType )
    ]
;;
