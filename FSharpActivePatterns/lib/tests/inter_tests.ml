(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpActivePatterns_lib
open Ast
open Interpret

(* TESTS INTERPRET *)

(*  7 + 10 + 4 * 50 + 19 / 3 + (10 - 5) *)
let test =
  [ BinExpr
      ( Add
      , BinExpr
          ( Add
          , BinExpr
              ( Add
              , BinExpr (Add, ConstExpr (CInt 7), ConstExpr (CInt 10))
              , BinExpr (Mul, ConstExpr (CInt 4), ConstExpr (CInt 50)) )
          , BinExpr (Div, ConstExpr (CInt 19), ConstExpr (CInt 3)) )
      , BinExpr (Sub, ConstExpr (CInt 10), ConstExpr (CInt 5)) )
  ]
;;

let test = [ BinExpr (Div, ConstExpr (CInt 5), ConstExpr (CInt 0)) ]

let%test _ =
  match eval_program test with
  | Error DivisionByZero -> true
  | _ -> false
;;

let test = [ BinExpr (Mod, ConstExpr (CInt 5), ConstExpr (CInt 0)) ]

let%test _ =
  match eval_program test with
  | Error DivisionByZero -> true
  | _ -> false
;;

(*[1;2;3]*)

let test =
  [ ListExpr
      ( ConstExpr (CInt 1)
      , ListExpr (ConstExpr (CInt 2), ListExpr (ConstExpr (CInt 3), ConstExpr CNil)) )
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VList [ VInt 1; VInt 2; VInt 3 ]) -> true
  | _ -> false
;;

let test = [ TupleExpr [ ConstExpr (CInt 1); ConstExpr (CInt 2); ConstExpr (CInt 3) ] ]

let%test _ =
  match eval_program test with
  | Ok (VTuple [ VInt 1; VInt 2; VInt 3 ]) -> true
  | _ -> false
;;

(* 5 = 5 *)

let test = [ BinExpr (Eq, ConstExpr (CInt 5), ConstExpr (CInt 5)) ]

let%test _ =
  match eval_program test with
  | Ok (VBool true) -> true
  | _ -> false
;;

(*(1, [2;3;4], 5)*)
let test =
  [ TupleExpr
      [ ConstExpr (CInt 1)
      ; ListExpr
          ( ConstExpr (CInt 2)
          , ListExpr (ConstExpr (CInt 3), ListExpr (ConstExpr (CInt 4), ConstExpr CNil))
          )
      ; ConstExpr (CInt 5)
      ]
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VTuple [ VInt 1; VList [ VInt 2; VInt 3; VInt 4 ]; VInt 5 ]) -> true
  | _ -> false
;;

(*
   let f x = x+x

   f 25
*)

let test =
  [ LetExpr (false, "f", FunExpr (Var "x", BinExpr (Add, VarExpr "x", VarExpr "x")))
  ; AppExpr (VarExpr "f", ConstExpr (CInt 25))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 50) -> true
  | _ -> false
;;

let test = [ ConstExpr (CBool true) ]

let%test _ =
  match eval_program test with
  | Ok (VBool true) -> true
  | _ -> false
;;

(* (fun x -> x*x) 5 *)

let test =
  [ AppExpr
      (FunExpr (Var "x", BinExpr (Mul, VarExpr "x", VarExpr "x")), ConstExpr (CInt 5))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 25) -> true
  | _ -> false
;;

let test = [ ConstExpr (CString "pomogite") ]

let%test _ =
  match eval_program test with
  | Ok (VString "pomogite") -> true
  | _ -> false
;;

(*
   let rec fact n = if n = 1 then 1 else n * (fact (n - 1))

   fact 5
*)

let test =
  [ LetExpr
      ( true
      , "fact"
      , FunExpr
          ( Var "n"
          , IfExpr
              ( BinExpr (Eq, VarExpr "n", ConstExpr (CInt 1))
              , ConstExpr (CInt 1)
              , BinExpr
                  ( Mul
                  , VarExpr "n"
                  , AppExpr
                      (VarExpr "fact", BinExpr (Sub, VarExpr "n", ConstExpr (CInt 1))) )
              ) ) )
  ; AppExpr (VarExpr "fact", ConstExpr (CInt 5))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 120) -> true
  | _ -> false
;;

(*
   let x = 5

   (fun y -> y*y) x
*)

let test =
  [ LetExpr (false, "x", ConstExpr (CInt 5))
  ; AppExpr (FunExpr (Var "y", BinExpr (Mul, VarExpr "y", VarExpr "y")), VarExpr "x")
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 25) -> true
  | _ -> false
;;

(*
   let check input = match input with | 2 -> 5 | 5 -> 10

   check 5
*)

let test =
  [ LetExpr
      ( false
      , "check"
      , FunExpr
          ( Var "input"
          , MatchExpr
              ( VarExpr "input"
              , [ Const (CInt 2), ConstExpr (CInt 5)
                ; Const (CInt 5), ConstExpr (CInt 10)
                ] ) ) )
  ; AppExpr (VarExpr "check", ConstExpr (CInt 5))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 10) -> true
  | _ -> false
;;

(*
   let (|Even|_|) v = if v % 2 = 0 then Even else None
   let (|Odd|_|) v = if v % 2 <> 0 then Odd else None

   let myfunc v =
   match v with
   | Even -> 50
   | Odd -> 25
   | _ -> 6

   myfunc 6
*)

let test =
  [ LetActExpr
      ( [ "Even"; "_" ]
      , FunExpr
          ( Var "v"
          , IfExpr
              ( BinExpr
                  (Eq, BinExpr (Mod, VarExpr "v", ConstExpr (CInt 2)), ConstExpr (CInt 0))
              , CaseExpr "Even"
              , CaseExpr "None" ) ) )
  ; LetActExpr
      ( [ "Odd"; "_" ]
      , FunExpr
          ( Var "v"
          , IfExpr
              ( BinExpr
                  (NEq, BinExpr (Mod, VarExpr "v", ConstExpr (CInt 2)), ConstExpr (CInt 0))
              , CaseExpr "Odd"
              , CaseExpr "None" ) ) )
  ; LetExpr
      ( false
      , "myfunc"
      , FunExpr
          ( Var "c"
          , MatchExpr
              ( VarExpr "c"
              , [ Case ("Even", []), ConstExpr (CInt 50)
                ; Case ("Odd", []), ConstExpr (CInt 25)
                ; Wild, ConstExpr (CInt 10)
                ] ) ) )
  ; AppExpr (VarExpr "myfunc", ConstExpr (CInt 9))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 25) -> true
  | _ -> false
;;

(*
   let (|Default|) value =
   match value with
   | value -> (value * value)

   let greet (Default value) = value

   greet 10
*)

let test =
  [ LetActExpr
      ( [ "Square" ]
      , FunExpr
          ( Var "value"
          , MatchExpr
              ( VarExpr "value"
              , [ Var "value", BinExpr (Mul, VarExpr "value", VarExpr "value") ] ) ) )
  ; LetExpr (false, "greet", FunExpr (Case ("Square", [ Var "value" ]), VarExpr "value"))
  ; AppExpr (VarExpr "greet", ConstExpr (CInt 10))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 100) -> true
  | _ -> false
;;

(*
   let (|Even|Odd|) value = if value % 2 = 0 then Even else Odd)

   let check value =
   match value with
   | Even -> 25
   | Odd -> 53

   check 14/check 13
*)

let test =
  [ LetActExpr
      ( [ "Even"; "Odd" ]
      , FunExpr
          ( Var "value"
          , IfExpr
              ( BinExpr
                  ( Eq
                  , BinExpr (Mod, VarExpr "value", ConstExpr (CInt 2))
                  , ConstExpr (CInt 0) )
              , CaseExpr "Even"
              , CaseExpr "Odd" ) ) )
  ; LetExpr
      ( false
      , "check"
      , FunExpr
          ( Var "value"
          , MatchExpr
              ( VarExpr "value"
              , [ Case ("Even", []), ConstExpr (CInt 25)
                ; Case ("Odd", []), ConstExpr (CInt 53)
                ] ) ) )
  ; AppExpr (VarExpr "check", ConstExpr (CInt 14))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 25) -> true
  | _ -> false
;;

let test =
  [ LetActExpr
      ( [ "Even"; "Odd" ]
      , FunExpr
          ( Var "value"
          , IfExpr
              ( BinExpr
                  ( Eq
                  , BinExpr (Mod, VarExpr "value", ConstExpr (CInt 2))
                  , ConstExpr (CInt 0) )
              , CaseExpr "Even"
              , CaseExpr "Odd" ) ) )
  ; LetExpr
      ( false
      , "check"
      , FunExpr
          ( Var "value"
          , MatchExpr
              ( VarExpr "value"
              , [ Case ("Even", []), ConstExpr (CInt 25)
                ; Case ("Odd", []), ConstExpr (CInt 53)
                ] ) ) )
  ; AppExpr (VarExpr "check", ConstExpr (CInt 13))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 53) -> true
  | _ -> false
;;
