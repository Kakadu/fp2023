(** Copyright 2023-2024, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpUnitsOfMeasure_lib
open Ast
open Interpreter

let run_test t =
  match interpreter t with
  | Ok (_, ty) -> Ok ty
  | Error e -> Error e
;;

(* Binary operation *)

(* (3 + 1) * (4 - 2) * (9 / 3) *)

let test =
  [ EApp
      ( EBinaryOp Mul
      , EApp
          ( EApp
              ( EBinaryOp Mul
              , EApp
                  ( EApp (EBinaryOp Add, EApp (EConst (FInt 3), EConst (FInt 1)))
                  , EApp (EBinaryOp Sub, EApp (EConst (FInt 4), EConst (FInt 2))) ) )
          , EApp (EBinaryOp Div, EApp (EConst (FInt 9), EConst (FInt 3))) ) )
  ]
;;

let%test _ =
  match run_test test with
  | Ok (VInt 24) -> true
  | _ -> false
;;

(* Binary operation and fun*)

(* ((fun z v -> z * v)4 )5 *)

let test =
  [ EApp
      ( EApp
          ( EFun
              (PVar "z", EFun (PVar "v", EApp (EBinaryOp Mul, EApp (EVar "z", EVar "v"))))
          , EConst (FInt 4) )
      , EConst (FInt 5) )
  ]
;;

let%test _ =
  match run_test test with
  | Ok (VInt 20) -> true
  | _ -> false
;;

(* ((fun z v -> z / v)4 )5 *)

let test =
  [ EApp
      ( EApp
          ( EFun
              (PVar "z", EFun (PVar "v", EApp (EBinaryOp Div, EApp (EVar "z", EVar "v"))))
          , EConst (FInt 4) )
      , EConst (FInt 5) )
  ]
;;

let%test _ =
  match run_test test with
  | Ok (VInt 0) -> true
  | _ -> false
;;

(* (((((fun a b c d e -> a + b + c + d + e) 1) 2) 3) 4) 5 *)

let test =
  [ EApp
      ( EApp
          ( EApp
              ( EApp
                  ( EApp
                      ( EFun
                          ( PVar "a"
                          , EFun
                              ( PVar "b"
                              , EFun
                                  ( PVar "c"
                                  , EFun
                                      ( PVar "d"
                                      , EFun
                                          ( PVar "e"
                                          , EApp
                                              ( EBinaryOp Add
                                              , EApp
                                                  ( EApp
                                                      ( EBinaryOp Add
                                                      , EApp
                                                          ( EApp
                                                              ( EBinaryOp Add
                                                              , EApp
                                                                  ( EApp
                                                                      ( EBinaryOp Add
                                                                      , EApp
                                                                          ( EVar "a"
                                                                          , EVar "b" ) )
                                                                  , EVar "c" ) )
                                                          , EVar "d" ) )
                                                  , EVar "e" ) ) ) ) ) ) )
                      , EConst (FInt 1) )
                  , EConst (FInt 2) )
              , EConst (FInt 3) )
          , EConst (FInt 4) )
      , EConst (FInt 5) )
  ]
;;

let%test _ =
  match run_test test with
  | Ok (VInt 15) -> true
  | _ -> false
;;

(* (1, 5) *)
let test = [ ETuple [ EConst (FInt 1); EConst (FInt 5) ] ]

let%test _ =
  match run_test test with
  | Ok (VTuple [ VInt 1; VInt 5 ]) -> true
  | _ -> false
;;

(*
   let num x = x
   num 5
*)
let test =
  [ ELet ("NotRec", "num", EFun (PVar "x", EVar "x"))
  ; EApp (EVar "num", EConst (FInt 5))
  ]
;;

let%test _ =
  match run_test test with
  | Ok (VInt 5) -> true
  | _ -> false
;;

(* let increase_by_five z = (fun v -> z + v) 5 *)

let test =
  [ ELet
      ( "NotRec"
      , "increase_by_five"
      , EFun
          ( PVar "z"
          , EApp
              ( EFun (PVar "v", EApp (EBinaryOp Add, EApp (EVar "z", EVar "v")))
              , EConst (FInt 5) ) ) )
  ; EApp (EVar "increase_by_five", EConst (FInt 10))
  ]
;;

let%test _ =
  match run_test test with
  | Ok (VInt 15) -> true
  | _ -> false
;;

(* if true then 2 else 1 *)
let test = [ EIfElse (EConst (FBool true), EConst (FInt 2), EConst (FInt 1)) ]

let%test _ =
  match run_test test with
  | Ok (VInt 2) -> true
  | _ -> false
;;

(*
   let num x =
   match x with
   | 1 -> 1
   | _ -> 2
   num 10
*)
let test =
  [ ELet
      ( "NotRec"
      , "num"
      , EFun
          ( PVar "x"
          , EMatch (EVar "x", [ PConst (FInt 1), EConst (FInt 1); PWild, EConst (FInt 2) ])
          ) )
  ; EApp (EVar "num", EConst (FInt 10))
  ]
;;

let%test _ =
  match run_test test with
  | Ok (VInt 2) -> true
  | _ -> false
;;

(* (fun x -> x) 5 *)
let test = [ EApp (EFun (PVar "x", EVar "x"), EConst (FInt 5)) ]

let%test _ =
  match run_test test with
  | Ok (VInt 5) -> true
  | _ -> false
;;

(* let x = 5 *)
let test = [ ELet ("NotRec", "x", EConst (FInt 5)) ]

let%test _ =
  match run_test test with
  | Ok (VInt 5) -> true
  | _ -> false
;;

(*
   let rec fact n =
   if n = 1
   then 1
   else n * (fact ( n - 1 ))
   fact 6
*)

let test =
  [ ELet
      ( "Rec"
      , "fact"
      , EFun
          ( PVar "n"
          , EIfElse
              ( EApp (EBinaryOp Eq, EApp (EVar "n", EConst (FInt 1)))
              , EConst (FInt 1)
              , EApp
                  ( EBinaryOp Mul
                  , EApp
                      ( EVar "n"
                      , EApp
                          ( EVar "fact"
                          , EApp (EBinaryOp Sub, EApp (EVar "n", EConst (FInt 1))) ) ) )
              ) ) )
  ; EApp (EVar "fact", EConst (FInt 6))
  ]
;;

let%test _ =
  match run_test test with
  | Ok (VInt 720) -> true
  | _ -> false
;;

(*
   let rec fib n =
   if n <= 1
   then n
   else (fib (n - 1)) + (fib (n - 2))
   fib 9
*)
let test =
  [ ELet
      ( "Rec"
      , "fib"
      , EFun
          ( PVar "n"
          , EIfElse
              ( EApp (EBinaryOp Leq, EApp (EVar "n", EConst (FInt 1)))
              , EVar "n"
              , EApp
                  ( EBinaryOp Add
                  , EApp
                      ( EApp
                          ( EVar "fib"
                          , EApp (EBinaryOp Sub, EApp (EVar "n", EConst (FInt 1))) )
                      , EApp
                          ( EVar "fib"
                          , EApp (EBinaryOp Sub, EApp (EVar "n", EConst (FInt 2))) ) ) )
              ) ) )
  ; EApp (EVar "fib", EConst (FInt 9))
  ]
;;

let%test _ =
  match run_test test with
  | Ok (VInt 34) -> true
  | _ -> false
;;

(* measure test*)

(*
   [<Measure>] type m
   7.77 <m> + 7.73 <m>
*)
let test =
  [ EMeasure (SMeasure_init (SMeasure ("m", Pow (FInt 1))))
  ; EApp
      ( EBinaryOp Add
      , EApp
          ( EConst (Measure_float (FFloat 7.77, SMeasure ("m", Pow (FInt 3))))
          , EConst (Measure_float (FFloat 7.73, SMeasure ("m", Pow (FInt 3)))) ) )
  ]
;;

let%test _ =
  match run_test test with
  | Ok (VFloatMeasure (VFloat 15.5, [ "m^3" ])) -> true
  | _ -> false
;;

(*
   [<Measure>] type sec
   [<Measure>] type m
   [<Measure>] type dm
   7.77<m/dm> + 7.73<m/dm>
*)
let test =
  [ EMeasure (SMeasure_init (SMeasure ("sec", Pow (FInt 1))))
  ; EMeasure (SMeasure_init (SMeasure ("m", Pow (FInt 1))))
  ; EMeasure (SMeasure_init (SMeasure ("dm", Pow (FInt 1))))
  ; EApp
      ( EBinaryOp Add
      , EApp
          ( EConst
              (Measure_float
                 ( FFloat 7.77
                 , MMeasure
                     (SMeasure ("m", Pow (FInt 1)), Div, SMeasure ("dm", Pow (FInt 1))) ))
          , EConst
              (Measure_float
                 ( FFloat 7.73
                 , MMeasure
                     (SMeasure ("m", Pow (FInt 1)), Div, SMeasure ("dm", Pow (FInt 1))) ))
          ) )
  ]
;;

let%test _ =
  match run_test test with
  | Ok (VFloatMeasure (VFloat 15.5, [ "m"; "/"; "dm" ])) -> true
  | _ -> false
;;

(*
   [<Measure>] type sec
   [<Measure>] type m
   [<Measure>] type speed = m^3 / sec^-1
*)
let test =
  [ EMeasure (SMeasure_init (SMeasure ("sec", Pow (FInt 1))))
  ; EMeasure (SMeasure_init (SMeasure ("m", Pow (FInt 1))))
  ; EMeasure
      (MMeasure_init
         ( SMeasure ("speed", Pow (FInt 1))
         , MMeasure (SMeasure ("m", Pow (FInt 3)), Div, SMeasure ("sec", Pow (FInt (-1))))
         ))
  ]
;;

let test =
  [ EMeasure (SMeasure_init (SMeasure ("sec", Pow (FInt 1))))
  ; EMeasure (SMeasure_init (SMeasure ("m", Pow (FInt 1))))
  ; EMeasure (SMeasure_init (SMeasure ("dm", Pow (FInt 1))))
  ; EMeasure
      (MMeasure_init
         ( SMeasure ("speed", Pow (FInt 1))
         , MMeasure (SMeasure ("m", Pow (FInt 1)), Div, SMeasure ("sec", Pow (FInt 1))) ))
  ; EMeasure
      (MMeasure_init
         ( SMeasure ("sp", Pow (FInt 1))
         , MMeasure (SMeasure ("speed", Pow (FInt 1)), Mul, SMeasure ("dm", Pow (FInt 1)))
         ))
  ; EApp
      ( EBinaryOp Add
      , EApp
          ( EConst
              (Measure_float
                 ( FFloat 7.
                 , MMeasure
                     ( SMeasure ("m", Pow (FInt 1))
                     , Div
                     , MMeasure
                         ( SMeasure ("sec", Pow (FInt 1))
                         , Mul
                         , SMeasure ("dm", Pow (FInt 1)) ) ) ))
          , EConst (Measure_float (FFloat 7., SMeasure ("sp", Pow (FInt 1)))) ) )
  ]
;;

let%test _ =
  match run_test test with
  | Ok (VFloatMeasure (VFloat 14., [ "m"; "/"; "sec"; "*"; "dm" ])) -> true
  | _ -> false
;;
