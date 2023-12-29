(** Copyright 2023, Kuarni, AlexShmak *)
(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Javascript_lib
open Ast
open Parser
open Print

let eq_ok str exp =
  let result = parse str in
  if Result.is_ok(result) && equal_statement (Result.get_ok(result)) exp then true
  else (
    Format.eprintf "The ast doesn't equal to expected\n"; 
    Format.eprintf "Expected:\n";
    Format.eprintf "%a" pp_statement exp;
    if Result.is_error(result) then (
      Format.eprintf "\nBut the parser ended with an error:\n";
      pp_error result
    ) else (
      Format.eprintf "\nBut the parser returned:\n";
      pp_ok result; 
    );
    Format.eprintf "\n";
    false
 )
;;

let eq_error str res = match Result.get_error(parse str) with
| `ParsingError s -> if s = res then true
else (
  Format.eprintf "The error doesn't equal to expected\n";
  Format.eprintf "Expected:\n";
  Format.eprintf "%s\n" res;
  Format.eprintf "But the parser ended with an error:\n";
  Format.eprintf "%s" s;
  false
)
;;

(** Number parser *)

let%test "number 4" =
  eq_ok
  "4"
  (Programm[Expression(Const(Number 4.))])

let%test "number 4." = 
  eq_ok
  "4."
  (Programm[Expression(Const(Number 4.))])
  
let%test "number .4" = 
  eq_ok
  ".4"
  (Programm[Expression(Const(Number 0.4))])

let%test "number 0.4" = 
  eq_ok
  "0.4"
  (Programm[Expression(Const(Number 0.4))])

let%test "number 10.01" =
  eq_ok
  "10.01"
  (Programm[Expression(Const(Number 10.01))])

let%test "number 1000000" =
  eq_ok
  "1000000"
  (Programm[Expression(Const(Number 1000000.))])

let%test "int only dot fail" =
  eq_error
  "."
  "incorrect statement: there is an invalid keyword: \"\""

(** Expressions *)

let%test "sum of number 1" =
  eq_ok
  "40 + 50"
  (Programm[Expression(BinOp(Add, Const(Number 40.), Const(Number 50.)))])

let%test "sum of number 2" =
  eq_ok
  "40+50"
  (Programm[Expression(BinOp(Add, Const(Number 40.), Const(Number 50.)))])

let%test "priority 1 + 2 + 3" =
  eq_ok
  "1 + 2 + 3"
  (Programm[Expression(BinOp(Add, BinOp(Add, 
  Const(Number 1.), Const(Number 2.)), Const(Number 3.)))])

let%test "priority 4 + 5 * 2 + 3" =
  eq_ok
  "4 + 5 * 2 + 3"
  (Programm[Expression(BinOp(Add, BinOp(Add, 
  Const(Number 4.), BinOp(
    Mul, Const(Number 5.), Const(Number 2.))), Const(Number 3.)))])

let%test "priority 4 + 5 + 2 * 3" =
  eq_ok
  "4 + 5 + 2 * 3"
  (Programm
   [(Expression
       (BinOp (Add, (BinOp (Add, (Const (Number 4.)), (Const (Number 5.)))),
          (BinOp (Mul, (Const (Number 2.)), (Const (Number 3.)))))))
     ])



let%test "if1" =
  eq_ok
  "if (a == 4) let a = b + 6; else let b = 6+7;"
  ((
  Programm
    [(If ((BinOp (Equal, (Var "a"), (Const (Number 4.)))),
        (VarDeck
           { var_identifier = "a"; is_const = false; var_type = VarType;
             value = (Some (BinOp (Add, (Var "b"), (Const (Number 6.))))) }),
        (Some (VarDeck
                 { var_identifier = "b"; is_const = false; var_type = VarType;
                   value =
                   (Some (BinOp (Add, (Const (Number 6.)), (Const (Number 7.))
                            )))
                   }))
        ))
      ]))
;;

let%test "factorial" =
  eq_ok
  "let fact = 4

  function calculateFact(fact) {
      if
      (fact != 0)
          return fact * calculateFact(fact - 1);else return 1;
  }"
  ((Programm
  [(VarDeck
      { var_identifier = "fact"; is_const = false; var_type = VarType;
        value = (Some (Const (Number 4.))) });
    (FunDeck
       { fun_identifier = "calculateFact"; arguments = [(Var "fact")];
         body =
         (Some (Block
                  [(If (
                      (BinOp (NotEqual, (Var "fact"), (Const (Number 0.)))),
                      (Return
                         (BinOp (Mul, (Var "fact"),
                            (FunctionCall ("calculateFact",
                               [(BinOp (Sub, (Var "fact"),
                                   (Const (Number 1.))))
                                 ]
                               ))
                            ))),
                      (Some (Return (Const (Number 1.))))))
                    ]))
         })
    ]))
;;