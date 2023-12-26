(** Copyright 2023-2024, David Akhmedov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCaml_ExtensibleVariantTypes_lib

let parse_test s expected_res =
  match Parser.parse_program s with
  | Ok actual -> List.equal Ast.equal_decl expected_res actual
  | Error err ->
    Format.printf "%s\n" err;
    false
;;

let%test _ =
  parse_test
    "let a = 5 + 6"
    [ DLet (Not_recursive, Ident "a", EBinop (EConst (CInt 5), Add, EConst (CInt 6))) ]
;;

let%test _ =
  parse_test
    "let rec f n = if n = 0 then 1 else n * f (n-1)"
    [ DLet
        ( Recursive
        , Ident "f"
        , EFun
            (EApp
               ( EId (Ident "n")
               , EIf
                   ( EBinop (EId (Ident "n"), Eq, EConst (CInt 0))
                   , EConst (CInt 1)
                   , EBinop
                       ( EId (Ident "n")
                       , Mul
                       , EApp
                           ( EId (Ident "f")
                           , EBinop (EId (Ident "n"), Sub, EConst (CInt 1)) ) ) ) )) )
    ]
;;

let%test _ =
  parse_test
    "let f x y = g x + g y"
    [ DLet
        ( Not_recursive
        , Ident "f"
        , EFun
            (EApp
               ( EId (Ident "x")
               , EFun
                   (EApp
                      ( EId (Ident "y")
                      , EBinop
                          ( EApp (EId (Ident "g"), EId (Ident "x"))
                          , Add
                          , EApp (EId (Ident "g"), EId (Ident "y")) ) )) )) )
    ]
;;

let%test _ =
  parse_test
    "let g x y = f x1 x2 (x3*x3) + f x3 x2 (x1)"
    [ DLet
        ( Not_recursive
        , Ident "g"
        , EFun
            (EApp
               ( EId (Ident "x")
               , EFun
                   (EApp
                      ( EId (Ident "y")
                      , EBinop
                          ( EApp
                              ( EApp
                                  ( EApp (EId (Ident "f"), EId (Ident "x1"))
                                  , EId (Ident "x2") )
                              , EBinop (EId (Ident "x3"), Mul, EId (Ident "x3")) )
                          , Add
                          , EApp
                              ( EApp
                                  ( EApp (EId (Ident "f"), EId (Ident "x3"))
                                  , EId (Ident "x2") )
                              , EId (Ident "x1") ) ) )) )) )
    ]
;;

let%test _ =
  parse_test
    "let a = fun (x,y) (z,w) k-> (x,y)"
    [ DLet
        ( Not_recursive
        , Ident "a"
        , EFun
            (EApp
               ( ETuple [ EId (Ident "x"); EId (Ident "y") ]
               , EFun
                   (EApp
                      ( ETuple [ EId (Ident "z"); EId (Ident "w") ]
                      , EFun
                          (EApp
                             (EId (Ident "k"), ETuple [ EId (Ident "x"); EId (Ident "y") ]))
                      )) )) )
    ]
;;

let%test _ =
  parse_test
    "let a = (fun k -> k+2) 3"
    [ DLet
        ( Not_recursive
        , Ident "a"
        , EApp
            ( EFun (EApp (EId (Ident "k"), EBinop (EId (Ident "k"), Add, EConst (CInt 2))))
            , EConst (CInt 3) ) )
    ]
;;

let%test _ =
  parse_test
    "let s = 231, 21, (2323 + 23, 432)"
    [ DLet
        ( Not_recursive
        , Ident "s"
        , ETuple
            [ EConst (CInt 231)
            ; EConst (CInt 21)
            ; ETuple
                [ EBinop (EConst (CInt 2323), Add, EConst (CInt 23)); EConst (CInt 432) ]
            ] )
    ]
;;

let%test _ =
  parse_test
    "let s = -f 3"
    [ DLet
        (Not_recursive, Ident "s", EUnop (Minus, EApp (EId (Ident "f"), EConst (CInt 3))))
    ]
;;

let%test _ =
  parse_test
    "let s = - -f 3"
    [ DLet
        ( Not_recursive
        , Ident "s"
        , EUnop (Minus, EUnop (Minus, EApp (EId (Ident "f"), EConst (CInt 3)))) )
    ]
;;

let%test _ =
  parse_test
    "let s = f -3"
    [ DLet (Not_recursive, Ident "s", EBinop (EId (Ident "f"), Sub, EConst (CInt 3))) ]
;;

let%test _ =
  parse_test
    "let s = f (-3)"
    [ DLet (Not_recursive, Ident "s", EApp (EId (Ident "f"), EConst (CInt (-3)))) ]
;;

let%test _ =
  parse_test
    "let s =  - -f -1"
    [ DLet
        ( Not_recursive
        , Ident "s"
        , EBinop (EUnop (Minus, EUnop (Minus, EId (Ident "f"))), Sub, EConst (CInt 1)) )
    ]
;;
