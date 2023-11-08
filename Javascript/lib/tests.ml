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
      pp_result_error result
    ) else (
      Format.eprintf "\nBut the parser returned:\n";
      pp_ok str; 
    );
    Format.eprintf "\n";
    false
  )
let eq_error str res = match Result.get_error(parse str) with
| `ParsingError s -> s = res
;;

(** Number parser *)

let%test "int number" =
  eq_ok
  "4"
  (Programm[Expression(Const(Number 4.))])

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