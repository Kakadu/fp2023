(* Copyright 2021-2023, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open FSharpUnitsOfMeasure_lib
open Ast
open Interpreter
open Interpret_error

let run_test t =
  match run t with
  | Ok (_, ty) -> Ok ty
  | Error e -> Error e
;;

(* тесты из парсера *)

(* 777 *)
let test = [(EConst (FInt (Plus, 777)))]

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
let test = [(EConst (FInt (Minus, 777)))]

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

let test = [(EConst (FFloat (Plus, 777.777)))]

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

let test = [(EConst (FFloat (Minus, 777.777)))]

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

(* true *)

let test = [(EConst (FBool true) )]

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

(* false *)

let test = [(EConst (FBool false) )]

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

(* string *)

let test = [(EConst (FString "Test to string"))]

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

let test = [(EConst (FUnit))]

let%test _ =
  match run_test test with
  | Ok (VUnit) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* [] *)

let test = [(EConst (FNil))]

let%test _ =
  match run_test test with
  | Ok (VNil) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* Binary operation *)

(* 1 + 5 *)

let test = [ (EApp ((EBinaryOp Add), (EApp ((EConst (FInt (Plus, 1))), (EConst (FInt (Plus, 5))))))) ]

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

let test = [(EApp ((EBinaryOp Sub), (EApp ((EConst (FInt (Plus, 1)), (EConst (FInt (Plus, 2))))))))]

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

let test = [ 
  (EApp ((EBinaryOp Mul),
        (EApp (
           (EApp ((EBinaryOp Mul),
              (EApp (
                 (EApp ((EBinaryOp Add),
                    (EApp ((EConst (FInt (Plus, 3))), (EConst (FInt (Plus, 1))))))),
                 (EApp ((EBinaryOp Sub),
                    (EApp ((EConst (FInt (Plus, 4))), (EConst (FInt (Plus, 2)))))))
                 ))
              )),
           (EApp ((EBinaryOp Div), (EApp ((EConst (FInt (Plus, 9))), (EConst (FInt (Plus, 3)))))))
           ))
        ))
]

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
let test = [     
  (EApp ((EBinaryOp Add),
    (EApp ((EConst (FFloat (Plus, 7.77))), (EConst (FFloat (Plus, 7.73))))))) 
]

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
[(EApp (
  (EApp (
     (EFun ((PVar "z"),
        (EFun ((PVar "v"),
           (EApp ((EBinaryOp Mul), (EApp ((EVar "z"), (EVar "v")))))))
        )),
     (EConst (FInt (Plus, 4))))),
  (EConst (FInt (Plus, 5)))))]
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
  [(EApp (
    (EApp (
       (EFun ((PVar "z"),
          (EFun ((PVar "v"),
             (EApp ((EBinaryOp Div), (EApp ((EVar "z"), (EVar "v")))))))
          )),
       (EConst (FInt (Plus, 4))))),
    (EConst (FInt (Plus, 5)))))]
  
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
  [(EApp (
    (EApp (
       (EFun ((PVar "z"),
          (EFun ((PVar "v"),
             (EApp ((EBinaryOp Div), (EApp ((EVar "z"), (EVar "v")))))))
          )),
       (EConst (FInt (Plus, 4))))),
    (EConst (FInt (Plus, 5)))))]
  
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

(* ((fun z v -> z % v)4 )5 *)

let test = 
  [(EApp (
    (EApp (
       (EFun ((PVar "z"),
          (EFun ((PVar "v"),
             (EApp ((EBinaryOp Mod), (EApp ((EVar "z"), (EVar "v")))))))
          )),
       (EConst (FInt (Plus, 4))))),
    (EConst (FInt (Plus, 5)))))]

  
let%test _ =
  match run_test test with
  | Ok (VInt 4) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* ((fun z v -> z && v)4 )5 *)

let test = 
  [(EApp (
    (EApp (
       (EFun ((PVar "z"),
          (EFun ((PVar "v"),
             (EApp ((EBinaryOp And), (EApp ((EVar "z"), (EVar "v")))))))
          )),
       (EConst (FBool true)))),
    (EConst (FBool true))))]

  
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
  
(* ((fun z v -> z || v)4 )5 *)

let test = 
  [(EApp (
    (EApp (
       (EFun ((PVar "z"),
          (EFun ((PVar "v"),
             (EApp ((EBinaryOp Or), (EApp ((EVar "z"), (EVar "v")))))))
          )),
       (EConst (FBool true)))),
    (EConst (FBool false))))]

  
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

(* ((fun z v -> z = v)4 )5 *)

let test = 
  [(EApp (
    (EApp (
       (EFun ((PVar "z"),
          (EFun ((PVar "v"),
             (EApp ((EBinaryOp Eq), (EApp ((EVar "z"), (EVar "v")))))))
          )),
       (EConst (FInt (Plus, 4))))),
    (EConst (FInt (Plus, 5)))))]

  
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

(* ((fun z v -> z < v)4 )5 *)

let test = 
  [(EApp (
    (EApp (
       (EFun ((PVar "z"),
          (EFun ((PVar "v"),
             (EApp ((EBinaryOp Less), (EApp ((EVar "z"), (EVar "v")))))))
          )),
       (EConst (FInt (Plus, 4))))),
    (EConst (FInt (Plus, 5)))))]

  
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
  [(EApp (
    (EApp (
       (EFun ((PVar "z"),
          (EFun ((PVar "v"),
             (EApp ((EBinaryOp Gre), (EApp ((EVar "z"), (EVar "v")))))))
          )),
       (EConst (FInt (Plus, 4))))),
    (EConst (FInt (Plus, 5)))))]

  
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

(* ((fun z v -> z <= v)4 )5 *)

let test = 
  [(EApp (
    (EApp (
       (EFun ((PVar "z"),
          (EFun ((PVar "v"),
             (EApp ((EBinaryOp Leq), (EApp ((EVar "z"), (EVar "v")))))))
          )),
       (EConst (FInt (Plus, 4))))),
    (EConst (FInt (Plus, 5)))))]

  
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

(* ((fun z v -> z >= v)4 )5 *)

let test = 
  [(EApp (
    (EApp (
       (EFun ((PVar "z"),
          (EFun ((PVar "v"),
             (EApp ((EBinaryOp Greq), (EApp ((EVar "z"), (EVar "v")))))))
          )),
       (EConst (FInt (Plus, 4))))),
    (EConst (FInt (Plus, 5)))))]

  
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
  [(EApp (
    (EApp (
       (EApp (
          (EApp (
             (EApp (
                (EFun ((PVar "a"),
                   (EFun ((PVar "b"),
                      (EFun ((PVar "c"),
                         (EFun ((PVar "d"),
                            (EFun ((PVar "e"),
                               (EApp ((EBinaryOp Add),
                                  (EApp (
                                     (EApp ((EBinaryOp Add),
                                        (EApp (
                                           (EApp ((EBinaryOp Add),
                                              (EApp (
                                                 (EApp ((EBinaryOp Add),
                                                    (EApp ((EVar "a"),
                                                       (EVar "b")))
                                                    )),
                                                 (EVar "c")))
                                              )),
                                           (EVar "d")))
                                        )),
                                     (EVar "e")))
                                  ))
                               ))
                            ))
                         ))
                      ))
                   )),
                (EConst (FInt (Plus, 1))))),
             (EConst (FInt (Plus, 2))))),
          (EConst (FInt (Plus, 3))))),
       (EConst (FInt (Plus, 4))))),
    (EConst (FInt (Plus, 5)))))]

  
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
let test = [ (ETuple [(EConst (FInt (Plus, 1))); (EConst (FInt (Plus, 5)))]) ]

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
  let myf x = x
  myf 5 
*)
let test = [  (ELet ("NotRec", "myf", (EFun ((PVar "x"), (EVar "x"))))); (EApp ((EVar "myf"), (EConst (FInt (Plus,5)))))  ]

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

let test = [  
  (ELet ("NotRec", "increase_by_five",
       (EFun ((PVar "z"),
          (EApp (
             (EFun ((PVar "v"),
                (EApp ((EBinaryOp Add), (EApp ((EVar "z"), (EVar "v"))))))),
             (EConst (FInt (Plus, 5)))))
          ))
       ));
       
  (EApp ((EVar "increase_by_five"), (EConst (FInt (Plus, 10)))))]

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
let test = [ (EIfElse ((EConst (FBool true)), (EConst (FInt (Plus, 2))), (EConst (FInt (Plus, 1)))))  ]

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
  let myf x = 
    match x with 
      | 1 -> 1
      | _ -> 2  
  myf 10 
*)
let test = [     (ELet ("NotRec", "myf",
(EFun ((PVar "x"),
   (EMatch ((EVar "x"),
      [((PConst (FInt (Plus, 1))), (EConst (FInt (Plus, 1)))); (PWild, (EConst (FInt (Plus, 2))))]
      ))
   ))
)); 
(EApp ((EVar "myf"), (EConst (FInt (Plus, 10))))) 
]

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
  let myf x = 
    match x with 
      | 1 -> 1
      | _ -> 2
  myf 1
*)
let test = [     
(ELet ("NotRec", "myf",
(EFun ((PVar "x"),
   (EMatch ((EVar "x"),
      [((PConst (FInt (Plus, 1))), (EConst (FInt (Plus, 1)))); (PWild, (EConst (FInt (Plus, 2))))]
      ))
   ))
)); 
(EApp ((EVar "myf"), (EConst (FInt (Plus, 1))))) 
]

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
let test = [  (EApp ((EFun ((PVar "x"), (EVar "x"))), (EConst (FInt (Plus, 5))))) ]

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
let test = [  (EList [(EConst (FInt (Plus, 1))); (EConst (FInt (Plus, 5)))])  ]

let%test _ =
  match run_test test with
  | Ok (VList [(VInt 1); (VInt 5)]) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* [1; 5] *)
let test = [(ETuple [(EConst (FInt (Plus, 1))); (EConst (FInt (Plus, 5)))])]

let%test _ =
  match run_test test with
  | Ok (VTuple [(VInt 1); (VInt 5)]) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* let x = 5 *)
let test = [ (ELet ("NotRec", "x", (EConst (FInt (Plus, 5)))))  ]

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
  let increase_by_five z = (fun v -> z + v) 5
  increase_by_five 10
*)

let test = [
  (ELet ("NotRec", "increase_by_five",
       (EFun ((PVar "z"),
          (EApp (
             (EFun ((PVar "v"),
                (EApp ((EBinaryOp Add), (EApp ((EVar "z"), (EVar "v"))))))),
             (EConst (FInt (Plus, 5)))))
          ))
       ));
  (EApp ((EVar "increase_by_five"), (EConst (FInt (Plus, 10)))))
]

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

(* 
  let rec fact n = 
    if n = 1 
      then 1 
    else n * (fact ( n - 1 ))
  fact 6
*)

let test = [   
  (ELet ("Rec", "fact",
    (EFun ((PVar "n"),
      (EIfElse (
          (EApp ((EBinaryOp Eq), (EApp ((EVar "n"), (EConst (FInt (Plus, 1))))))),
          (EConst (FInt (Plus, 1))),
          (EApp ((EBinaryOp Mul),
            (EApp ((EVar "n"),
                (EApp ((EVar "fact"),
                  (EApp ((EBinaryOp Sub),
                      (EApp ((EVar "n"), (EConst (FInt (Plus, 1)))))))
                  ))
                ))
            ))
          ))
      ))
    )) ;

(EApp ((EVar "fact"), (EConst (FInt (Plus, 6))))) 
 ]

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
let test = [   
  (ELet ("Rec", "fib",
       (EFun ((PVar "n"),
          (EIfElse (
             (EApp ((EBinaryOp Leq), (EApp ((EVar "n"), (EConst (FInt (Plus, 1))))))),
             (EVar "n"),
             (EApp ((EBinaryOp Add),
                (EApp (
                   (EApp ((EVar "fib"),
                      (EApp ((EBinaryOp Sub),
                         (EApp ((EVar "n"), (EConst (FInt (Plus, 1)))))))
                      )),
                   (EApp ((EVar "fib"),
                      (EApp ((EBinaryOp Sub),
                         (EApp ((EVar "n"), (EConst (FInt (Plus, 2)))))))
                      ))
                   ))
                ))
             ))
          ))
       ));
  (EApp ((EVar "fib"), (EConst (FInt (Plus, 9)))))]

let%test _ =
  match run_test test with
  | Ok (VInt (34)) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;


(* measure test*)

(* [<Measure>] type m *)
let test = [ (EMeasure (Measure_init (Measure_single ("m", (Pow (FInt (Plus, 1)))))))]

let%test _ =
  match run_test test with
  | Ok (VMeasureList [("m", ["m"])]) -> true
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
let test = [  
  (EMeasure (Measure_init (Measure_single ("m", (Pow (FInt (Plus, 1))))))); 
  (EApp ((EBinaryOp Add),
    (EApp ((EConst (Measure_float ((FFloat (Plus, 7.77)), (Measure_single ("m", (Pow (FInt (Plus, 3)))))))),
      (EConst (Measure_float ((FFloat (Plus, 7.73)), (Measure_single ("m", (Pow (FInt (Plus, 3))))))))))
)) ]

let%test _ =
  match run_test test with
  | Ok (VFloatMeasure ((VFloat 15.5), ["m^3"])) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* [<Measure>] type hz = m / sec * sm * luck / unluck*)
let test = [ 
  (EMeasure (Measure_init (Measure_single ("sec", (Pow (FInt (Plus, 1)))))));
  (EMeasure (Measure_init (Measure_single ("m", (Pow (FInt (Plus, 1)))))));
  (EMeasure (Measure_init (Measure_single ("dm", (Pow (FInt (Plus, 1)))))));
  (EMeasure
       (Measure_multiple_init ((Measure_single( "speed", (Pow (FInt (Plus, 1))))),
          (Measure_multiple ((Measure_single ("m", (Pow (FInt (Plus, 1))))), Div,
             (Measure_multiple ((Measure_single ("sec", (Pow (FInt (Plus, 1))))), Mul,
                (Measure_single ("dm", (Pow (FInt (Plus, 1)))))))
             ))
          )));
          (EApp ((EBinaryOp Add),
       (EApp (
          (EConst
             (Measure_float ((FFloat (Plus, 7.77)),
                (Measure_multiple ((Measure_single ("m", (Pow (FInt (Plus, 1))))), Div,
                   (Measure_single ("dm", (Pow (FInt (Plus, 1)))))))
                ))),
          (EConst
             (Measure_float ((FFloat (Plus, 7.73)),
                (Measure_multiple ((Measure_single ("m", (Pow (FInt (Plus, 1))))), Div,
                   (Measure_single ("dm", (Pow (FInt (Plus, 1)))))))
                )))
          ))
       ))
]

let%test _ =
  match run_test test with
  | Ok (VFloatMeasure ((VFloat 15.5), ["m"; "/"; "dm"])) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;; 


(* [<Measure>] type hz = m / sec * sm * luck / unluck*)
let test = [ 
  (EMeasure (Measure_init (Measure_single ("sec", (Pow (FInt (Plus, 1)))))));
  (EMeasure (Measure_init (Measure_single ("m", (Pow (FInt (Plus, 1)))))));
  (EMeasure
       (Measure_multiple_init ((Measure_single ("speed", (Pow (FInt (Plus, 1))))),
          (Measure_multiple ((Measure_single ("m", (Pow (FInt (Plus, 3))))), Div, (Measure_single ("sec", (Pow (FInt (Minus, 1)))))))
          )))
]

let%test _ =
  match run_test test with
  | Ok (VMeasureList [("speed", ["m^3"; "/"; "sec^-1"]); ("m", ["m"]); ("sec", ["sec"])]) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;; 

(* [<Measure>] type hz = m / sec * sm * luck / unluck*)
let test = [ 
  (EMeasure (Measure_init (Measure_single ("sec", (Pow (FInt (Plus, 1)))))));
  (EMeasure (Measure_init (Measure_single ("m", (Pow (FInt (Plus, 1)))))));
  (EMeasure (Measure_init (Measure_single ("dm", (Pow (FInt (Plus, 1)))))));
  (EMeasure
    (Measure_multiple_init ((Measure_single ("speed", (Pow (FInt (Plus, 1))))),
      (Measure_multiple ((Measure_single ("m", (Pow (FInt (Plus, 1))))), Div, (Measure_single ("sec", (Pow (FInt (Plus, 1)))))))
      )));
  (EMeasure
    (Measure_multiple_init ((Measure_single ("sp", (Pow (FInt (Plus, 1))))),
        (Measure_multiple ((Measure_single ("speed", (Pow (FInt (Plus, 1))))), Mul, (Measure_single ("dm", (Pow (FInt (Plus, 1)))))
          ))
        )));
        (EApp ((EBinaryOp Add),
        (EApp (
           (EConst
              (Measure_float ((FFloat (Plus, 7.)),
                 (Measure_multiple ((Measure_single ("m", (Pow (FInt (Plus, 1))))), Div,
                    (Measure_multiple ((Measure_single ("sec", (Pow (FInt (Plus, 1))))), Mul,
                       (Measure_single ("dm", (Pow (FInt (Plus, 1)))))))
                    ))
                 ))),
           (EConst (Measure_float ((FFloat (Plus, 7.)), (Measure_single ("sp", (Pow (FInt (Plus, 1))))))))))
        ))
]

let%test _ =
  match run_test test with
  | Ok (VFloatMeasure ((VFloat 14.), ["m"; "/"; "sec"; "*"; "dm"])) -> true
  | Error t ->
    Format.printf "%a\n" print_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;; 