(** Copyright 2023, Kuarni, AlexShmak *)
(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Javascript_lib
open Ast
open Parser
open Print

let eq_ok str res =
  if equal_statement (Result.get_ok(parse str)) res then true
  else (
    print_string "The ast doesn't equal to expected\n"; 
    pp_ok str; 
    print_newline (); 
    false)
let eq_error str res = match Result.get_error(parse str) with
| `ParsingError s -> s = res
;;

let%test _ =
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