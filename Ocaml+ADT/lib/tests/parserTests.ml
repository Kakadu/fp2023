(** Copyright 2023-2024, tepa46 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocamladt_lib
open Format

let ptest str expected =
  match Parser.parse str with
  | Ok actual ->
    let is_eq = List.equal Ast.equal_decl expected actual in
    if is_eq
    then ()
    else printf "Expected: %a\nActual: %a\n" Ast.pp_program expected Ast.pp_program actual;
    is_eq
  | Error err ->
    printf "%s\n" err;
    false
;;

(* tests *)

let%test _ = ptest {| let rec abc = "a" |} [ DLet (DRec true, LName "abc", EString "a") ]
let%test _ = ptest {| let aBC = "a" |} [ DLet (DRec false, LName "aBC", EString "a") ]

let%test _ =
  ptest
    {| let aBC = fun x -> (let summ = 5 in b) |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , EFun
            (PVar (LName "x"), ELet ((DRec false, LName "summ", EInt 5), EVar (LName "b")))
        )
    ]
;;

let%test _ =
  ptest
    {| let aBC = fun n -> if n then 5 else 0 |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , EFun (PVar (LName "n"), EIf (EVar (LName "n"), EInt 5, EInt 0)) )
    ]
;;

let%test _ =
  ptest
    {| let aBC = fun n -> if 2 - n * (1 - 1 * 3 + 1)  then 5 else 0 |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , EFun
            ( PVar (LName "n")
            , EIf
                ( EBinop
                    ( Sub
                    , EInt 2
                    , EBinop
                        ( Mul
                        , EVar (LName "n")
                        , EBinop
                            ( Sub
                            , EInt 1
                            , EBinop (Add, EBinop (Mul, EInt 1, EInt 3), EInt 1) ) ) )
                , EInt 5
                , EInt 0 ) ) )
    ]
;;

let%test _ =
  ptest
    {| let aBC = f n e |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , EApp (EApp (EVar (LName "f"), EVar (LName "n")), EVar (LName "e")) )
    ]
;;

let%test _ =
  ptest
    {| let x = fun x -> if true then if true then 1 else 2 else 3 |}
    [ DLet
        ( DRec false
        , LName "x"
        , EFun
            (PVar (LName "x"), EIf (EBool true, EIf (EBool true, EInt 1, EInt 2), EInt 3))
        )
    ]
;;

(* test for factorial *)
let%test _ =
  ptest
    {| let rec factorial_recursive = fun n -> if n <= 1 then 1 else n * factorial_recursive (n - 1) |}
    [ DLet
        ( DRec true
        , LName "factorial_recursive"
        , EFun
            ( PVar (LName "n")
            , EIf
                ( EBinop (Leq, EVar (LName "n"), EInt 1)
                , EInt 1
                , EBinop
                    ( Mul
                    , EVar (LName "n")
                    , EApp
                        ( EVar (LName "factorial_recursive")
                        , EBinop (Sub, EVar (LName "n"), EInt 1) ) ) ) ) )
    ]
;;

let%test _ =
  ptest
    {| let rec fix = fun f -> (fun x -> f (fix f) x)
       let fac = fix (fun self -> (fun n -> if n <= 1 then 1 else n * self (n - 1))) |}
    [ DLet
        ( DRec true
        , LName "fix"
        , EFun
            ( PVar (LName "f")
            , EFun
                ( PVar (LName "x")
                , EApp
                    ( EApp (EVar (LName "f"), EApp (EVar (LName "fix"), EVar (LName "f")))
                    , EVar (LName "x") ) ) ) )
    ; DLet
        ( DRec false
        , LName "fac"
        , EApp
            ( EVar (LName "fix")
            , EFun
                ( PVar (LName "self")
                , EFun
                    ( PVar (LName "n")
                    , EIf
                        ( EBinop (Leq, EVar (LName "n"), EInt 1)
                        , EInt 1
                        , EBinop
                            ( Mul
                            , EVar (LName "n")
                            , EApp
                                ( EVar (LName "self")
                                , EBinop (Sub, EVar (LName "n"), EInt 1) ) ) ) ) ) ) )
    ]
;;

let%test _ =
  ptest
    {| let a = fun x -> match x with | [] :: [] -> (f n) | a ->  false | _ -> true |}
    [ DLet
        ( DRec false
        , LName "a"
        , EFun
            ( PVar (LName "x")
            , EMatch
                ( EVar (LName "x")
                , [ PCons (PNill, PNill), EApp (EVar (LName "f"), EVar (LName "n"))
                  ; PVar (LName "a"), EBool false
                  ; PWild, EBool true
                  ] ) ) )
    ]
;;

let%test _ =
  ptest
    {| type a = | AZAZA of string |}
    [ DType (LName "a", [ UName "AZAZA", DType TString ]) ]
;;

let%test _ =
  ptest
    {| type a = | AZAZA of string | ABABA of int | ACACA of int |}
    [ DType
        ( LName "a"
        , [ UName "AZAZA", DType TString
          ; UName "ABABA", DType TInt
          ; UName "ACACA", DType TInt
          ] )
    ]
;;

let%test _ =
  ptest
    {| type a = | Azaza of string -> string
       let b = 5 |}
    [ DType (LName "a", [ UName "Azaza", DType (TFun (TString, TString)) ])
    ; DLet (DRec false, LName "b", EInt 5)
    ]
;;

let%test _ =
  ptest
    {| type node = | Red of int | Black of int
       let is_black = fun x -> match x with | Black _ -> true | Red _ -> false
       let a = is_black (Black 5) |}
    [ DType (LName "node", [ UName "Red", DType TInt; UName "Black", DType TInt ])
    ; DLet
        ( DRec false
        , LName "is_black"
        , EFun
            ( PVar (LName "x")
            , EMatch
                ( EVar (LName "x")
                , [ PAdt (UName "Black", Some PWild), EBool true
                  ; PAdt (UName "Red", Some PWild), EBool false
                  ] ) ) )
    ; DLet
        ( DRec false
        , LName "a"
        , EApp (EVar (LName "is_black"), EConstr (UName "Black", Some (EInt 5))) )
    ]
;;

let%test _ =
  ptest
    {| let a = fun x -> match x with | Tepa (Kuka 46) -> true |}
    [ DLet
        ( DRec false
        , LName "a"
        , EFun
            ( PVar (LName "x")
            , EMatch
                ( EVar (LName "x")
                , [ ( PAdt (UName "Tepa", Some (PAdt (UName "Kuka", Some (PInt 46))))
                    , EBool true )
                  ] ) ) )
    ]
;;

let%test _ =
  ptest
    {| let a = fun x -> match x with | (a, b, c, d) -> true |}
    [ DLet
        ( DRec false
        , LName "a"
        , EFun
            ( PVar (LName "x")
            , EMatch
                ( EVar (LName "x")
                , [ ( PTuple
                        [ PVar (LName "a")
                        ; PVar (LName "b")
                        ; PVar (LName "c")
                        ; PVar (LName "d")
                        ]
                    , EBool true )
                  ] ) ) )
    ]
;;

let%test _ =
  ptest
    {| let aBC = fun n -> if n then 5 else (0, 1, 2, 3) |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , EFun
            ( PVar (LName "n")
            , EIf (EVar (LName "n"), EInt 5, ETuple [ EInt 0; EInt 1; EInt 2; EInt 3 ]) )
        )
    ]
;;

let%test _ =
  ptest
    {| let aBC = F 5 |}
    [ DLet (DRec false, LName "aBC", EConstr (UName "F", Some (EInt 5))) ]
;;

let%test _ =
  ptest
    {| let aBC= F (5, 4, 3, 2) |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , EConstr (UName "F", Some (ETuple [ EInt 5; EInt 4; EInt 3; EInt 2 ])) )
    ]
;;

let%test _ =
  ptest
    {| let n = fun x -> 5 |}
    [ DLet (DRec false, LName "n", EFun (PVar (LName "x"), EInt 5)) ]
;;

let%test _ =
  ptest
    {| let n = fun x -> x+ 5
       let a = n 5 |}
    [ DLet
        ( DRec false
        , LName "n"
        , EFun (PVar (LName "x"), EBinop (Add, EVar (LName "x"), EInt 5)) )
    ; DLet (DRec false, LName "a", EApp (EVar (LName "n"), EInt 5))
    ]
;;

let%test _ =
  ptest
    {| let n = fun y -> (let x = 5 in x + y)
       let f = n 7 |}
    [ DLet
        ( DRec false
        , LName "n"
        , EFun
            ( PVar (LName "y")
            , ELet
                ( (DRec false, LName "x", EInt 5)
                , EBinop (Add, EVar (LName "x"), EVar (LName "y")) ) ) )
    ; DLet (DRec false, LName "f", EApp (EVar (LName "n"), EInt 7))
    ]
;;

let%test _ =
  ptest
    {| let n = fun y -> if y > 7 then 5 else 3
       let f = n 7 |}
    [ DLet
        ( DRec false
        , LName "n"
        , EFun
            ( PVar (LName "y")
            , EIf (EBinop (Gre, EVar (LName "y"), EInt 7), EInt 5, EInt 3) ) )
    ; DLet (DRec false, LName "f", EApp (EVar (LName "n"), EInt 7))
    ]
;;

let%test _ =
  ptest
    {| let a = fun h :: tl -> h
       let n = a (4 :: 5 :: 6 :: []) |}
    [ DLet
        ( DRec false
        , LName "a"
        , EFun (PCons (PVar (LName "h"), PVar (LName "tl")), EVar (LName "h")) )
    ; DLet
        ( DRec false
        , LName "n"
        , EApp
            ( EVar (LName "a")
            , EBinop
                (Cons, EInt 4, EBinop (Cons, EInt 5, EBinop (Cons, EInt 6, EEmptyList)))
            ) )
    ]
;;

let%test _ =
  ptest {| type a = | Apple |} [ DType (LName "a", [ UName "Apple", DType TEmptyType ]) ]
;;

let%test _ =
  ptest
    {| type color = | White | Green | Yellow | Blue | Red | Black
       type eatable = | Yes | No 
       type thing = | Apple of color * eatable | Chair of color * eatable | Potato of color * eatable
       let a = Apple (Yellow, Yes) |}
    [ DType
        ( LName "color"
        , [ UName "White", DType TEmptyType
          ; UName "Green", DType TEmptyType
          ; UName "Yellow", DType TEmptyType
          ; UName "Blue", DType TEmptyType
          ; UName "Red", DType TEmptyType
          ; UName "Black", DType TEmptyType
          ] )
    ; DType
        (LName "eatable", [ UName "Yes", DType TEmptyType; UName "No", DType TEmptyType ])
    ; DType
        ( LName "thing"
        , [ UName "Apple", DType (TTuple [ TVar (LName "color"); TVar (LName "eatable") ])
          ; UName "Chair", DType (TTuple [ TVar (LName "color"); TVar (LName "eatable") ])
          ; ( UName "Potato"
            , DType (TTuple [ TVar (LName "color"); TVar (LName "eatable") ]) )
          ] )
    ; DLet
        ( DRec false
        , LName "a"
        , EConstr
            ( UName "Apple"
            , Some
                (ETuple [ EConstr (UName "Yellow", None); EConstr (UName "Yes", None) ])
            ) )
    ]
;;

let%test _ =
  ptest
    {| let n = fun h :: tl -> h
  let b = n (5 :: 4)|}
    [ DLet
        ( DRec false
        , LName "n"
        , EFun (PCons (PVar (LName "h"), PVar (LName "tl")), EVar (LName "h")) )
    ; DLet (DRec false, LName "b", EApp (EVar (LName "n"), EBinop (Cons, EInt 5, EInt 4)))
    ]
;;

let%test _ =
  ptest
    {| let n = fun x -> match x with | true -> true | false -> false |}
    [ DLet
        ( DRec false
        , LName "n"
        , EFun
            ( PVar (LName "x")
            , EMatch
                (EVar (LName "x"), [ PBool true, EBool true; PBool false, EBool false ])
            ) )
    ]
;;

let%test _ =
  ptest
    {| let rec fix = fun f -> (fun x -> f (fix x) x) |}
    [ DLet
        ( DRec true
        , LName "fix"
        , EFun
            ( PVar (LName "f")
            , EFun
                ( PVar (LName "x")
                , EApp
                    ( EApp (EVar (LName "f"), EApp (EVar (LName "fix"), EVar (LName "x")))
                    , EVar (LName "x") ) ) ) )
    ]
;;

let%test _ =
  ptest
    {| let rev = fun lst ->
      (let rec helper = fun acc -> (fun lst ->
      match lst with
        | [] -> acc
        | h :: tl -> helper (h :: acc) tl)
      in helper [] lst) |}
    [ DLet
        ( DRec false
        , LName "rev"
        , EFun
            ( PVar (LName "lst")
            , ELet
                ( ( DRec true
                  , LName "helper"
                  , EFun
                      ( PVar (LName "acc")
                      , EFun
                          ( PVar (LName "lst")
                          , EMatch
                              ( EVar (LName "lst")
                              , [ PNill, EVar (LName "acc")
                                ; ( PCons (PVar (LName "h"), PVar (LName "tl"))
                                  , EApp
                                      ( EApp
                                          ( EVar (LName "helper")
                                          , EBinop
                                              (Cons, EVar (LName "h"), EVar (LName "acc"))
                                          )
                                      , EVar (LName "tl") ) )
                                ] ) ) ) )
                , EApp (EApp (EVar (LName "helper"), EEmptyList), EVar (LName "lst")) ) )
        )
    ]
;;

let%test _ =
  ptest
    {| type name = | LName of string | RName of string
    let a = LName "tepa" |}
    [ DType (LName "name", [ UName "LName", DType TString; UName "RName", DType TString ])
    ; DLet (DRec false, LName "a", EConstr (UName "LName", Some (EString "tepa")))
    ]
;;

let%test _ =
  ptest
    {| type color = | Red | Black

       type rbtree =
       | Empty
       | Node of color * int * rbtree * rbtree

       let rec member = fun x -> 
        (fun n -> 
        match n with 
        | Empty -> false
        | Node (_, y, left, right) -> if x = y then true else if x < y then member x left else member x right)

       let node_left_left = Node(Black, 3, Empty, Empty)

       let node_left = Node (Red, 4, node_left_left, Empty)

       let node_right = Node(Red, 10, Empty, Empty)
 
       let node = Node (Black, 5, node_left, node_right)

       let is_member = member 4 node
    |}
    [ DType
        (LName "color", [ UName "Red", DType TEmptyType; UName "Black", DType TEmptyType ])
    ; DType
        ( LName "rbtree"
        , [ UName "Empty", DType TEmptyType
          ; ( UName "Node"
            , DType
                (TTuple
                   [ TVar (LName "color")
                   ; TInt
                   ; TVar (LName "rbtree")
                   ; TVar (LName "rbtree")
                   ]) )
          ] )
    ; DLet
        ( DRec true
        , LName "member"
        , EFun
            ( PVar (LName "x")
            , EFun
                ( PVar (LName "n")
                , EMatch
                    ( EVar (LName "n")
                    , [ PAdt (UName "Empty", None), EBool false
                      ; ( PAdt
                            ( UName "Node"
                            , Some
                                (PTuple
                                   [ PWild
                                   ; PVar (LName "y")
                                   ; PVar (LName "left")
                                   ; PVar (LName "right")
                                   ]) )
                        , EIf
                            ( EBinop (Eq, EVar (LName "x"), EVar (LName "y"))
                            , EBool true
                            , EIf
                                ( EBinop (Les, EVar (LName "x"), EVar (LName "y"))
                                , EApp
                                    ( EApp (EVar (LName "member"), EVar (LName "x"))
                                    , EVar (LName "left") )
                                , EApp
                                    ( EApp (EVar (LName "member"), EVar (LName "x"))
                                    , EVar (LName "right") ) ) ) )
                      ] ) ) ) )
    ; DLet
        ( DRec false
        , LName "node_left_left"
        , EConstr
            ( UName "Node"
            , Some
                (ETuple
                   [ EConstr (UName "Black", None)
                   ; EInt 3
                   ; EConstr (UName "Empty", None)
                   ; EConstr (UName "Empty", None)
                   ]) ) )
    ; DLet
        ( DRec false
        , LName "node_left"
        , EConstr
            ( UName "Node"
            , Some
                (ETuple
                   [ EConstr (UName "Red", None)
                   ; EInt 4
                   ; EVar (LName "node_left_left")
                   ; EConstr (UName "Empty", None)
                   ]) ) )
    ; DLet
        ( DRec false
        , LName "node_right"
        , EConstr
            ( UName "Node"
            , Some
                (ETuple
                   [ EConstr (UName "Red", None)
                   ; EInt 10
                   ; EConstr (UName "Empty", None)
                   ; EConstr (UName "Empty", None)
                   ]) ) )
    ; DLet
        ( DRec false
        , LName "node"
        , EConstr
            ( UName "Node"
            , Some
                (ETuple
                   [ EConstr (UName "Black", None)
                   ; EInt 5
                   ; EVar (LName "node_left")
                   ; EVar (LName "node_right")
                   ]) ) )
    ; DLet
        ( DRec false
        , LName "is_member"
        , EApp (EApp (EVar (LName "member"), EInt 4), EVar (LName "node")) )
    ]
;;
