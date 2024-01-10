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

(* tests for declaration type parsing *)
let%test _ =
  ptest
    {| let a : int -> (int -> string) -> int = "a" |}
    [ DLet
        ( DRec false
        , LName "a"
        , DType (TFun (TInt, TFun (TFun (TInt, TString), TInt)))
        , EString "a" )
    ]
;;

let%test _ =
  ptest
    {| let a : int list  = "a" |}
    [ DLet (DRec false, LName "a", DType (TList TInt), EString "a") ]
;;

let%test _ =
  ptest
    {| let rec abc : ((int list) list) list = "a" |}
    [ DLet (DRec true, LName "abc", DType (TList (TList (TList TInt))), EString "a") ]
;;

let%test _ =
  ptest
    {| let rec abc : int -> (int -> (int -> int)) -> int = "a" |}
    [ DLet
        ( DRec true
        , LName "abc"
        , DType (TFun (TInt, TFun (TFun (TInt, TFun (TInt, TInt)), TInt)))
        , EString "a" )
    ]
;;

let%test _ =
  ptest
    {| let aBC:  (string * int) list = "a" |}
    [ DLet (DRec false, LName "aBC", DType (TList (TTuple [ TString; TInt ])), EString "a")
    ]
;;

let%test _ =
  ptest
    {| let aBC: (int list * (string -> int)) list = "a" |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType (TList (TTuple [ TList TInt; TFun (TString, TInt) ]))
        , EString "a" )
    ]
;;

let%test _ =
  ptest
    {| let aBC: (int list * string * a) -> string list = "a" |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType (TFun (TTuple [ TList TInt; TString; TVar (LName "a") ], TList TString))
        , EString "a" )
    ]
;;

let%test _ =
  ptest
    {| let aBC: (int list * string * a) -> string list = "a" |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType (TFun (TTuple [ TList TInt; TString; TVar (LName "a") ], TList TString))
        , EString "a" )
    ]
;;

let%test _ =
  ptest
    {| let aBC: ((int list -> int) * string * (int * int)) -> string list = "a" |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType
            (TFun
               ( TTuple [ TFun (TList TInt, TInt); TString; TTuple [ TInt; TInt ] ]
               , TList TString ))
        , EString "a" )
    ]
;;

(* tests for expression type parsing *)
let%test _ =
  ptest
    {| let aBC: (int list) list = fun a b -> 5 |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType (TList (TList TInt))
        , EFun (PVar (LName "a"), EFun (PVar (LName "b"), EInt 5)) )
    ]
;;

let%test _ =
  ptest
    {| let aBC: (int list) list = let b: string = 5 in 5 |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType (TList (TList TInt))
        , ELet ((DRec false, LName "b", DType TString, EInt 5), EInt 5) )
    ]
;;

let%test _ =
  ptest
    {| let aBC: int -> int = let b: int -> int = fun x -> x in b |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType (TFun (TInt, TInt))
        , ELet
            ( ( DRec false
              , LName "b"
              , DType (TFun (TInt, TInt))
              , EFun (PVar (LName "x"), EVar (LName "x")) )
            , EVar (LName "b") ) )
    ]
;;

let%test _ =
  ptest
    {| let aBC: int -> int = let b: int -> int = fun x -> (let summ: int = 5 in b) in c |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType (TFun (TInt, TInt))
        , ELet
            ( ( DRec false
              , LName "b"
              , DType (TFun (TInt, TInt))
              , EFun
                  ( PVar (LName "x")
                  , ELet ((DRec false, LName "summ", DType TInt, EInt 5), EVar (LName "b"))
                  ) )
            , EVar (LName "c") ) )
    ]
;;

let%test _ =
  ptest
    {| let aBC: int = fun n -> if n then 5 else 0 |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType TInt
        , EFun (PVar (LName "n"), EIf (EVar (LName "n"), EInt 5, EInt 0)) )
    ]
;;

let%test _ =
  ptest
    {| let aBC: int = fun n -> if 2 - n * (1 - 1 * 3 + 1)  then 5 else 0 |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType TInt
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
    {| let aBC: int -> int = f n e |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType (TFun (TInt, TInt))
        , EApp (EApp (EVar (LName "f"), EVar (LName "n")), EVar (LName "e")) )
    ]
;;

let%test _ =
  ptest
    {| let x = fun x -> if true then if true then 1 else 2 else 3 |}
    [ DLet
        ( DRec false
        , LName "x"
        , DType TEmptyType
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
        , DType TEmptyType
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
        , DType TEmptyType
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
        , DType TEmptyType
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
    {| let a: int -> bool = fun x -> match x with | [] :: [] -> (f n) | a ->  false | _ -> true |}
    [ DLet
        ( DRec false
        , LName "a"
        , DType (TFun (TInt, TBool))
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
    {| let a: int -> bool = fun x -> match x with | _ :: [] -> (f n) | a ->  false | _ -> true |}
    [ DLet
        ( DRec false
        , LName "a"
        , DType (TFun (TInt, TBool))
        , EFun
            ( PVar (LName "x")
            , EMatch
                ( EVar (LName "x")
                , [ PCons (PWild, PNill), EApp (EVar (LName "f"), EVar (LName "n"))
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
       let b: int = 5 |}
    [ DType (LName "a", [ UName "Azaza", DType (TFun (TString, TString)) ])
    ; DLet (DRec false, LName "b", DType TInt, EInt 5)
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
        , DType TEmptyType
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
        , DType TEmptyType
        , EApp (EVar (LName "is_black"), EConstr (UName "Black", Some (EInt 5))) )
    ]
;;

let%test _ =
  ptest
    {| let a: int -> bool = fun x -> match x with | Tepa Kuka 46 -> true |}
    [ DLet
        ( DRec false
        , LName "a"
        , DType (TFun (TInt, TBool))
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
    {| let a: int -> bool = fun x -> match x with | (a, b, c, d) -> true |}
    [ DLet
        ( DRec false
        , LName "a"
        , DType (TFun (TInt, TBool))
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
    {| let aBC: int = fun n -> if n then 5 else (0, 1, 2, 3) |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType TInt
        , EFun
            ( PVar (LName "n")
            , EIf (EVar (LName "n"), EInt 5, ETuple [ EInt 0; EInt 1; EInt 2; EInt 3 ]) )
        )
    ]
;;

let%test _ =
  ptest
    {| let aBC: int -> int = F 5 |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType (TFun (TInt, TInt))
        , EConstr (UName "F", Some (EInt 5)) )
    ]
;;

let%test _ =
  ptest
    {| let aBC: int -> int = F (5, 4, 3, 2) |}
    [ DLet
        ( DRec false
        , LName "aBC"
        , DType (TFun (TInt, TInt))
        , EConstr (UName "F", Some (ETuple [ EInt 5; EInt 4; EInt 3; EInt 2 ])) )
    ]
;;

let%test _ =
  ptest
    {| let n = fun x -> 5 |}
    [ DLet (DRec false, LName "n", DType TEmptyType, EFun (PVar (LName "x"), EInt 5)) ]
;;

let%test _ =
  ptest
    {| let n = fun x -> x+ 5
       let a = n 5 |}
    [ DLet
        ( DRec false
        , LName "n"
        , DType TEmptyType
        , EFun (PVar (LName "x"), EBinop (Add, EVar (LName "x"), EInt 5)) )
    ; DLet (DRec false, LName "a", DType TEmptyType, EApp (EVar (LName "n"), EInt 5))
    ]
;;

let%test _ =
  ptest
    {| let n = fun y -> (let x = 5 in x + y)
       let f = n 7 |}
    [ DLet
        ( DRec false
        , LName "n"
        , DType TEmptyType
        , EFun
            ( PVar (LName "y")
            , ELet
                ( (DRec false, LName "x", DType TEmptyType, EInt 5)
                , EBinop (Add, EVar (LName "x"), EVar (LName "y")) ) ) )
    ; DLet (DRec false, LName "f", DType TEmptyType, EApp (EVar (LName "n"), EInt 7))
    ]
;;

let%test _ =
  ptest
    {| let n = fun y -> if y > 7 then 5 else 3
       let f = n 7 |}
    [ DLet
        ( DRec false
        , LName "n"
        , DType TEmptyType
        , EFun
            ( PVar (LName "y")
            , EIf (EBinop (Gre, EVar (LName "y"), EInt 7), EInt 5, EInt 3) ) )
    ; DLet (DRec false, LName "f", DType TEmptyType, EApp (EVar (LName "n"), EInt 7))
    ]
;;

let%test _ =
  ptest
    {| let a = fun h :: tl -> h
       let n = a (4 :: 5 :: 6 :: []) |}
    [ DLet
        ( DRec false
        , LName "a"
        , DType TEmptyType
        , EFun (PCons (PVar (LName "h"), PVar (LName "tl")), EVar (LName "h")) )
    ; DLet
        ( DRec false
        , LName "n"
        , DType TEmptyType
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
        , DType TEmptyType
        , EConstr
            ( UName "Apple"
            , Some
                (ETuple [ EConstr (UName "Yellow", None); EConstr (UName "Yes", None) ])
            ) )
    ]
;;
