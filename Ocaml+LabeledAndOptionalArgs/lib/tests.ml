(** Copyright 2021-2023, LeonidElkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
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
        , [ NoLabel ("n", IntType false) ]
        , IntType false
        , IfThenElse
            ( BinOp (Less, Var "n", Const (Int 1))
            , Const (Int 1)
            , BinOp
                ( Asterisk
                , Var "n"
                , Apply (Var "fact", BinOp (Dash, Var "n", Const (Int 1))) ) ) )
    ]
;;

let%test _ =
  test_parse
    " let fact = fun n -> if n < 1 then 1 else n * fact (n - 1) "
    [ LetDecl
        ( false
        , "fact"
        , []
        , UndefinedType
        , Fun
            ( [ NoLabel ("n", UndefinedType) ]
            , UndefinedType
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
    " let foo ( n: int * string list * bool * char ) = 1 "
    [ LetDecl
        ( false
        , "foo"
        , [ NoLabel
              ( "n"
              , TupleType
                  ( [ IntType false
                    ; ListType (StringType false, false)
                    ; BoolType false
                    ; CharType false
                    ]
                  , false ) )
          ]
        , UndefinedType
        , Const (Int 1) )
    ]
;;

let%test _ =
  test_parse
    " let foo ( n: int option * string option list option * bool option * char option ) \
     = 1 "
    [ LetDecl
        ( false
        , "foo"
        , [ NoLabel
              ( "n"
              , TupleType
                  ( [ IntType true
                    ; ListType (StringType true, true)
                    ; BoolType true
                    ; CharType true
                    ]
                  , false ) )
          ]
        , UndefinedType
        , Const (Int 1) )
    ]
;;

let%test _ =
  test_parse
    " let foo ((((n : (((((int))))))))) = 1 "
    [ LetDecl
        (false, "foo", [ NoLabel ("n", IntType false) ], UndefinedType, Const (Int 1))
    ]
;;

let%test _ =
  test_parse
    " let foo ( n : (int -> int -> int) option) = 1 "
    [ LetDecl
        ( false
        , "foo"
        , [ NoLabel
              ( "n"
              , FuncType
                  (IntType false, FuncType (IntType false, IntType false, false), true) )
          ]
        , UndefinedType
        , Const (Int 1) )
    ]
;;

let%test _ =
  test_parse
    " let foo ?(n = 1) = 1 "
    [ LetDecl
        ( false
        , "foo"
        , [ Optional ("n", Some (Int 1), UndefinedType) ]
        , UndefinedType
        , Const (Int 1) )
    ]
;;

let%test _ =
  test_parse
    " let foo ?(n = (1 : int)) = 1 "
    [ LetDecl
        ( false
        , "foo"
        , [ Optional ("n", Some (Int 1), IntType false) ]
        , UndefinedType
        , Const (Int 1) )
    ]
;;

let%test _ =
  test_parse
    " let foo ?n = 1 "
    [ LetDecl
        ( false
        , "foo"
        , [ Optional ("n", None, UndefinedType) ]
        , UndefinedType
        , Const (Int 1) )
    ]
;;

let%test _ =
  test_parse
    " let foo ?( n : int) = 1 "
    [ LetDecl
        ( false
        , "foo"
        , [ Optional ("n", None, IntType false) ]
        , UndefinedType
        , Const (Int 1) )
    ]
;;

let%test _ =
  test_parse
    " let foo ~x = 1 "
    [ LetDecl
        (false, "foo", [ Label ("x", "x", UndefinedType) ], UndefinedType, Const (Int 1))
    ]
;;

let%test _ =
  test_parse
    " let foo ~x:x1 = 1 "
    [ LetDecl
        (false, "foo", [ Label ("x", "x1", UndefinedType) ], UndefinedType, Const (Int 1))
    ]
;;

let%test _ =
  test_parse
    " let foo ~(x:int) = 1 "
    [ LetDecl
        (false, "foo", [ Label ("x", "x", IntType false) ], UndefinedType, Const (Int 1))
    ]
;;

let%test _ =
  test_parse
    " let foo ~(x:(x1:int)) = 1 "
    [ LetDecl
        (false, "foo", [ Label ("x", "x1", IntType false) ], UndefinedType, Const (Int 1))
    ]
;;

let%test _ =
  test_parse
    " let foo ( n : ( int * string list * bool * char ) list ) = 1 "
    [ LetDecl
        ( false
        , "foo"
        , [ NoLabel
              ( "n"
              , ListType
                  ( TupleType
                      ( [ IntType false
                        ; ListType (StringType false, false)
                        ; BoolType false
                        ; CharType false
                        ]
                      , false )
                  , false ) )
          ]
        , UndefinedType
        , Const (Int 1) )
    ]
;;

let%test _ =
  test_parse
    " let foo ( n : (( int * (string * bool * char * int) list * bool * char ) list * \
     string * bool ) list ) = 1 "
    [ LetDecl
        ( false
        , "foo"
        , [ NoLabel
              ( "n"
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
          ]
        , UndefinedType
        , Const (Int 1) )
    ]
;;

let%test _ =
  test_parse
    " let foo n = let helper n = n * 3 in helper n "
    [ LetDecl
        ( false
        , "foo"
        , [ NoLabel ("n", UndefinedType) ]
        , UndefinedType
        , EDecl
            ( false
            , "helper"
            , [ NoLabel ("n", UndefinedType) ]
            , UndefinedType
            , BinOp (Asterisk, Var "n", Const (Int 3))
            , Apply (Var "helper", Var "n") ) )
    ]
;;

let%test _ =
  test_parse
    " let foo = fun n -> n + 1 "
    [ LetDecl
        ( false
        , "foo"
        , []
        , UndefinedType
        , Fun
            ( [ NoLabel ("n", UndefinedType) ]
            , UndefinedType
            , BinOp (Plus, Var "n", Const (Int 1)) ) )
    ]
;;

let%test _ =
  test_parse
    " let foo = fun n -> ( let helper n = n * 3 in helper n ) "
    (*Doesn't work without brackets*)
    [ LetDecl
        ( false
        , "foo"
        , []
        , UndefinedType
        , Fun
            ( [ NoLabel ("n", UndefinedType) ]
            , UndefinedType
            , EDecl
                ( false
                , "helper"
                , [ NoLabel ("n", UndefinedType) ]
                , UndefinedType
                , BinOp (Asterisk, Var "n", Const (Int 3))
                , Apply (Var "helper", Var "n") ) ) )
    ]
;;

let%test _ =
  test_parse
    " let foo1 = true let foo2 = \"lets'go\""
    [ LetDecl (false, "foo1", [], UndefinedType, Const (Bool true))
    ; LetDecl (false, "foo2", [], UndefinedType, Const (String "lets'go"))
    ]
;;

let%test _ =
  test_parse
    " let foo x = if x = 2 then x - 1 else x / 2"
    [ LetDecl
        ( false
        , "foo"
        , [ NoLabel ("x", UndefinedType) ]
        , UndefinedType
        , IfThenElse
            ( BinOp (Eq, Var "x", Const (Int 2))
            , BinOp (Dash, Var "x", Const (Int 1))
            , BinOp (Slash, Var "x", Const (Int 2)) ) )
    ]
;;

let%test _ =
  test_parse
    " let foo x = if x <> 2 && x >= 3 || x <= 8 then x - 1 else x * 2"
    [ LetDecl
        ( false
        , "foo"
        , [ NoLabel ("x", UndefinedType) ]
        , UndefinedType
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
    ]
;;
