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

(**---------------Expressions parsers---------------*)

let%expect_test _ =
  pp ~parse:parse_expression 
  "4";
  [%expect{|(Expression (Const (Number 4.)))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression 
  "4.";
  [%expect{|(Expression (Const (Number 4.)))|}]
;;
  
let%expect_test _ =
  pp ~parse:parse_expression 
  ".4";
  [%expect{|(Expression (Const (Number 0.4)))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression 
  "0.4";
  [%expect{|(Expression (Const (Number 0.4)))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression 
  "10.01";
  [%expect{|(Expression (Const (Number 10.01)))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression 
  "1000000";
  [%expect{|(Expression (Const (Number 1000000.)))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression 
  ".";
  [%expect{|Error: incorrect expression > invalid part of expression: no more choices|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression 
  "\"Hello world!\"";
  [%expect{|(Expression (Const (String "Hello world!")))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression 
  "\'Hello world!\'";
  [%expect{|(Expression (Const (String "Hello world!")))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression 
  "var1";
  [%expect{|(Expression (Var "var1"))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression 
  "func1(var1, func2(var1), 4)";
  [%expect{|
    (Expression
       (FunctionCall ("func1",
          [(Var "var1"); (FunctionCall ("func2", [(Var "var1")]));
            (Const (Number 4.))]
          )))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression
  "+ 5";
  [%expect{|(Expression (UnOp (Plus, (Const (Number 5.)))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression
  "- - 5";
  [%expect{|(Expression (UnOp (Minus, (UnOp (Minus, (Const (Number 5.)))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression
  "- + 5";
  [%expect{|(Expression (UnOp (Minus, (UnOp (Plus, (Const (Number 5.)))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression
  "- +";
  [%expect{|Error: incorrect expression > invalid part of expression: no more choices|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression
  "40+50";
  [%expect{|(Expression (BinOp (Add, (Const (Number 40.)), (Const (Number 50.)))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression
  "40 + -50";
  [%expect{|
    (Expression
       (BinOp (Add, (Const (Number 40.)), (UnOp (Minus, (Const (Number 50.)))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression
  "40 + (-50)";
  [%expect{|
    (Expression
       (BinOp (Add, (Const (Number 40.)), (UnOp (Minus, (Const (Number 50.)))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression
  "1 + 2 + 3";
  [%expect{|
    (Expression
       (BinOp (Add, (BinOp (Add, (Const (Number 1.)), (Const (Number 2.)))),
          (Const (Number 3.)))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression
  "1 + 2 - 3 + 4";
  [%expect{|
    (Expression
       (BinOp (Add,
          (BinOp (Sub, (BinOp (Add, (Const (Number 1.)), (Const (Number 2.)))),
             (Const (Number 3.)))),
          (Const (Number 4.)))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression
  "4 + 5 * 2 + 3";
  [%expect{|
    (Expression
       (BinOp (Add,
          (BinOp (Add, (Const (Number 4.)),
             (BinOp (Mul, (Const (Number 5.)), (Const (Number 2.)))))),
          (Const (Number 3.)))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression
  "4 + 5 + 2 * 3";
  [%expect{|
    (Expression
       (BinOp (Add, (BinOp (Add, (Const (Number 4.)), (Const (Number 5.)))),
          (BinOp (Mul, (Const (Number 2.)), (Const (Number 3.)))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression
  "(4 + 5 + 2 * 3)";
  [%expect{|
    (Expression
       (BinOp (Add, (BinOp (Add, (Const (Number 4.)), (Const (Number 5.)))),
          (BinOp (Mul, (Const (Number 2.)), (Const (Number 3.)))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression
  "4 + ;5 + 2 * 3";
  [%expect{|
    Error: incorrect expression > invalid part of expression: no more choices|}]
;;

let%expect_test _ =
  pp
  "4 + 5; + 2 * 3";
  [%expect{|
    (Programm
       [(Expression (BinOp (Add, (Const (Number 4.)), (Const (Number 5.)))));
         (Expression
            (BinOp (Mul, (UnOp (Plus, (Const (Number 2.)))), (Const (Number 3.))
               )))
         ])|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression
  "4 + ; + 2 * 3";
  [%expect{|
    Error: incorrect expression > invalid part of expression: no more choices|}]
;;

let%expect_test _ =
  pp
  "4 + 5 ; 2 * 3";
  [%expect{|
    (Programm
       [(Expression (BinOp (Add, (Const (Number 4.)), (Const (Number 5.)))));
         (Expression (BinOp (Mul, (Const (Number 2.)), (Const (Number 3.)))))])|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression
  "(4 + 5) + 2 * 3";
  [%expect{|
    (Expression
       (BinOp (Add, (BinOp (Add, (Const (Number 4.)), (Const (Number 5.)))),
          (BinOp (Mul, (Const (Number 2.)), (Const (Number 3.)))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression
  "4 + 5 * (2 + 3)";
  [%expect{|
    (Expression
       (BinOp (Add, (Const (Number 4.)),
          (BinOp (Mul, (Const (Number 5.)),
             (BinOp (Add, (Const (Number 2.)), (Const (Number 3.))))))
          )))|}]
;;

let%expect_test _ =
  pp
  "4 + (5 + 2) * 3)";
  [%expect{|
    Error: incorrect statement: there is unexpected symbol: ')'|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression
  "(4 + 5) + (2 * 3)";
  [%expect{|
    (Expression
       (BinOp (Add, (BinOp (Add, (Const (Number 4.)), (Const (Number 5.)))),
          (BinOp (Mul, (Const (Number 2.)), (Const (Number 3.)))))))|}]
;;

(**---------------Statements parsers---------------*)

let%expect_test _ =
  pp
  "let a = 6";
  [%expect{|
    (Programm
       [(VarDeck
           { var_identifier = "a"; is_const = false;
             value = (Some (Const (Number 6.))) })
         ]) |}]
;;

let%expect_test _ =
  pp
  "var a = 6";
  [%expect{|
    (Programm
       [(VarDeck
           { var_identifier = "a"; is_const = false;
             value = (Some (Const (Number 6.))) })
         ]) |}]
;;

let%expect_test _ =
  pp
  "const a = 6";
  [%expect{|
    (Programm
       [(VarDeck
           { var_identifier = "a"; is_const = true;
             value = (Some (Const (Number 6.))) })
         ]) |}]
;;

let%expect_test _ =
  pp
  "let a = function(b1) {return b1+6;}";
  [%expect{|
    (Programm
       [(FunDeck
           { fun_identifier = "a"; arguments = [(Var "b1")];
             body =
             (Block [(Return (BinOp (Add, (Var "b1"), (Const (Number 6.)))))]) })
         ]) |}]
;;

let%expect_test "if1" =
  pp
  "if (a == 4) let a = b + 6; else let b = 6+7;";
  [%expect{|
    (Programm
       [(If ((BinOp (Equal, (Var "a"), (Const (Number 4.)))),
           (Block
              [(VarDeck
                  { var_identifier = "a"; is_const = false;
                    value = (Some (BinOp (Add, (Var "b"), (Const (Number 6.)))))
                    })
                ]),
           (Block
              [(VarDeck
                  { var_identifier = "b"; is_const = false;
                    value =
                    (Some (BinOp (Add, (Const (Number 6.)), (Const (Number 7.)))))
                    })
                ])
           ))
         ])|}]
;;

let%expect_test "factorial" =
  pp
  "let fact = 4

  function calculateFact(fact) {
      if
      (fact != 0)
          return fact * calculateFact(fact - 1);else return 1;
  }";
  [%expect{|
    (Programm
       [(VarDeck
           { var_identifier = "fact"; is_const = false;
             value = (Some (Const (Number 4.))) });
         (FunDeck
            { fun_identifier = "calculateFact"; arguments = [(Var "fact")];
              body =
              (Block
                 [(If ((BinOp (NotEqual, (Var "fact"), (Const (Number 0.)))),
                     (Block
                        [(Return
                            (BinOp (Mul, (Var "fact"),
                               (FunctionCall ("calculateFact",
                                  [(BinOp (Sub, (Var "fact"), (Const (Number 1.))
                                      ))
                                    ]
                                  ))
                               )))
                          ]),
                     (Block [(Return (Const (Number 1.)))])))
                   ])
              })
         ])|}]
;;