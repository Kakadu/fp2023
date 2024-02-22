(* Copyright 2021-2023, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpUnitsOfMeasure_lib
open Ast
open Interpreter
open Interpret_error

let run_test t =
  match interpreter t with
  | Ok (_, ty) -> Ok ty
  | Error e -> Error e
;;

(* 777 *)
let test = [ EConst (FInt 777) ]

let%test _ =
  match run_test test with
  | Ok (VInt 777) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* -777 *)
let test = [ EConst (FInt (-777)) ]

let%test _ =
  match run_test test with
  | Ok (VInt -777) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* 777.777 *)

let test = [ EConst (FFloat 777.777) ]

let%test _ =
  match run_test test with
  | Ok (VFloat 777.777) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* -777.777 *)

let test = [ EConst (FFloat (-777.777)) ]

let%test _ =
  match run_test test with
  | Ok (VFloat -777.777) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* string *)

let test = [ EConst (FString "Test to string") ]

let%test _ =
  match run_test test with
  | Ok (VString "Test to string") -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* () *)

let test = [ EConst FUnit ]

let%test _ =
  match run_test test with
  | Ok VUnit -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* [] *)

let test = [ EConst FNil ]

let%test _ =
  match run_test test with
  | Ok VNil -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* Binary operation *)

(* 1 + 5 *)

let test = [ EApp (EBinaryOp Add, EApp (EConst (FInt 1), EConst (FInt 5))) ]

let%test _ =
  match run_test test with
  | Ok (VInt 6) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* 1 - 2 *)

let test = [ EApp (EBinaryOp Sub, EApp (EConst (FInt 1), EConst (FInt 2))) ]

let%test _ =
  match run_test test with
  | Ok (VInt -1) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

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
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* 7.77 + 7.73*)
let test = [ EApp (EBinaryOp Add, EApp (EConst (FFloat 7.77), EConst (FFloat 7.73))) ]

let%test _ =
  match run_test test with
  | Ok (VFloat 15.5) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
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
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
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
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* ((fun z v -> z / v)5.0 )2.0 *)

let test =
  [ EApp
      ( EApp
          ( EFun
              (PVar "z", EFun (PVar "v", EApp (EBinaryOp Div, EApp (EVar "z", EVar "v"))))
          , EConst (FFloat 5.0) )
      , EConst (FFloat 2.0) )
  ]
;;
  
let%test _ =
  match run_test test with
  | Ok (VFloat 2.5) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* ((fun z v -> z < v)4 )5 *)

let test =
  [ EApp
      ( EApp
          ( EFun
              (PVar "z", EFun (PVar "v", EApp (EBinaryOp Less, EApp (EVar "z", EVar "v"))))
          , EConst (FInt 4) )
      , EConst (FInt 5) )
  ]
;;
  
let%test _ =
  match run_test test with
  | Ok (VBool true) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* ((fun z v -> z > v)4 )5 *)

let test =
  [ EApp
      ( EApp
          ( EFun
              (PVar "z", EFun (PVar "v", EApp (EBinaryOp Gre, EApp (EVar "z", EVar "v"))))
          , EConst (FInt 4) )
      , EConst (FInt 5) )
  ]
;;

  
let%test _ =
  match run_test test with
  | Ok (VBool false) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
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
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* (1, 5) *)
let test = [ ETuple [ EConst (FInt 1); EConst (FInt 5) ] ]

let%test _ =
  match run_test test with
  | Ok ((VTuple [(VInt 1); (VInt 5)])) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
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
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
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
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;; 

(* if true then 2 else 1 *)
let test = [ EIfElse (EConst (FBool true), EConst (FInt 2), EConst (FInt 1)) ]

let%test _ =
  match run_test test with
  | Ok (VInt 2) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
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
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
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
  ; EApp (EVar "num", EConst (FInt 1))
  ]
;;

let%test _ =
  match run_test test with
  | Ok (VInt 1) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;; 

(* (fun x -> x) 5 *)
let test = [ EApp (EFun (PVar "x", EVar "x"), EConst (FInt 5)) ]

let%test _ =
  match run_test test with
  | Ok (VInt 5) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* [1; 5] *)
let test = [ EList [ EConst (FInt 1); EConst (FInt 5) ] ]

let%test _ =
  match run_test test with
  | Ok (VList [ VInt 1; VInt 5 ]) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* (1, 5) *)
let test = [ ETuple [ EConst (FInt 1); EConst (FInt 5) ] ]

let%test _ =
  match run_test test with
  | Ok (VTuple [ VInt 1; VInt 5 ]) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* let x = 5 *)
let test = [ ELet ("NotRec", "x", EConst (FInt 5)) ]

let%test _ =
  match run_test test with
  | Ok (VInt 5) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
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
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
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
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* measure test*)

(* [<Measure>] type m *)
let test = [ EMeasure (SMeasure_init (SMeasure ("m", Pow (FInt 1)))) ]

let%test _ =
  match run_test test with
  | Ok (VMeasureList [ "m" ]) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;; 

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
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
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
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
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
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;; 
