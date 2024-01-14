(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Javascript_lib
open Parser
open Print

(**---------------Expressions parsers---------------*)

let%expect_test _ =
  pp ~parse:parse_expression "4";
  [%expect {|(Expression (Const (Number 4.)))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "4.";
  [%expect {|(Expression (Const (Number 4.)))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression ".4";
  [%expect {|(Expression (Const (Number 0.4)))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "0.4";
  [%expect {|(Expression (Const (Number 0.4)))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "10.01";
  [%expect {|(Expression (Const (Number 10.01)))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "1000000";
  [%expect {|(Expression (Const (Number 1000000.)))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "Infinity";
  [%expect {|(Expression (Const (Number infinity)))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "NaN";
  [%expect {|(Expression (Const (Number nan)))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression ".";
  [%expect {|Error: incorrect expression > invalid part of expression: no more choices|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "\"Hello world!\"";
  [%expect {|(Expression (Const (String "Hello world!")))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "\'Hello world!\'";
  [%expect {|(Expression (Const (String "Hello world!")))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "var1";
  [%expect {|(Expression (Var "var1"))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "true";
  [%expect {|(Expression (Const (Bool true)))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "false";
  [%expect {|(Expression (Const (Bool false)))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "false1";
  [%expect {|(Expression (Var "false1"))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "null";
  [%expect {|(Expression (Const Null))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "undefined";
  [%expect {|(Expression (Const Undefined))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "func1(var1, func2(var1), 4)";
  [%expect
    {|
    (Expression
       (FunctionCall ((Var "func1"),
          [(Var "var1"); (FunctionCall ((Var "func2"), [(Var "var1")]));
            (Const (Number 4.))]
          )))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "+ 5";
  [%expect {|(Expression (UnOp (Plus, (Const (Number 5.)))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "- - 5";
  [%expect {|(Expression (UnOp (Minus, (UnOp (Minus, (Const (Number 5.)))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "- + 5";
  [%expect {|(Expression (UnOp (Minus, (UnOp (Plus, (Const (Number 5.)))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "new f()";
  [%expect {|(Expression (UnOp (New, (FunctionCall ((Var "f"), [])))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "typeof 4";
  [%expect {|(Expression (UnOp (TypeOf, (Const (Number 4.)))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "+ 4 ++";
  [%expect {|(Expression (UnOp (Plus, (UnOp (PostInc, (Const (Number 4.)))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "+ 4 ++ --";
  [%expect
    {|
    (Expression
       (UnOp (Plus, (UnOp (PostDec, (UnOp (PostInc, (Const (Number 4.)))))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "- +";
  [%expect {|Error: incorrect expression > invalid part of expression: no more choices|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "40+50";
  [%expect {|(Expression (BinOp (Add, (Const (Number 40.)), (Const (Number 50.)))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "40 + -50";
  [%expect
    {|
    (Expression
       (BinOp (Add, (Const (Number 40.)), (UnOp (Minus, (Const (Number 50.)))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "40 + (-50)";
  [%expect
    {|
    (Expression
       (BinOp (Add, (Const (Number 40.)), (UnOp (Minus, (Const (Number 50.)))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "40 && 50";
  [%expect
    {|
    (Expression (BinOp (LogicalAnd, (Const (Number 40.)), (Const (Number 50.)))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "40 & 50";
  [%expect
    {|
    (Expression (BinOp (BitwiseAnd, (Const (Number 40.)), (Const (Number 50.)))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "1 + 2 + 3";
  [%expect
    {|
    (Expression
       (BinOp (Add, (BinOp (Add, (Const (Number 1.)), (Const (Number 2.)))),
          (Const (Number 3.)))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "a = b = c";
  [%expect
    {|
    (Expression
       (BinOp (Assign, (Var "a"), (BinOp (Assign, (Var "b"), (Var "c"))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "1 + 2 - 3 + 4";
  [%expect
    {|
    (Expression
       (BinOp (Add,
          (BinOp (Sub, (BinOp (Add, (Const (Number 1.)), (Const (Number 2.)))),
             (Const (Number 3.)))),
          (Const (Number 4.)))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "4 + 5 * 2 + 3";
  [%expect
    {|
    (Expression
       (BinOp (Add,
          (BinOp (Add, (Const (Number 4.)),
             (BinOp (Mul, (Const (Number 5.)), (Const (Number 2.)))))),
          (Const (Number 3.)))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "4 + 5 + 2 * 3";
  [%expect
    {|
    (Expression
       (BinOp (Add, (BinOp (Add, (Const (Number 4.)), (Const (Number 5.)))),
          (BinOp (Mul, (Const (Number 2.)), (Const (Number 3.)))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "(4 + 5 + 2 * 3)";
  [%expect
    {|
    (Expression
       (BinOp (Add, (BinOp (Add, (Const (Number 4.)), (Const (Number 5.)))),
          (BinOp (Mul, (Const (Number 2.)), (Const (Number 3.)))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "4 + ;5 + 2 * 3";
  [%expect
    {|
    Error: incorrect expression > invalid part of expression: no more choices|}]
;;

let%expect_test _ =
  pp "4 + 5; + 2 * 3";
  [%expect
    {|
    (Programm
       [(Expression (BinOp (Add, (Const (Number 4.)), (Const (Number 5.)))));
         (Expression
            (BinOp (Mul, (UnOp (Plus, (Const (Number 2.)))), (Const (Number 3.))
               )))
         ])|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "4 + ; + 2 * 3";
  [%expect
    {|
    Error: incorrect expression > invalid part of expression: no more choices|}]
;;

let%expect_test _ =
  pp "4 + 5 ; 2 * 3";
  [%expect
    {|
    (Programm
       [(Expression (BinOp (Add, (Const (Number 4.)), (Const (Number 5.)))));
         (Expression (BinOp (Mul, (Const (Number 2.)), (Const (Number 3.)))))])|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "(4 + 5) + 2 * 3";
  [%expect
    {|
    (Expression
       (BinOp (Add, (BinOp (Add, (Const (Number 4.)), (Const (Number 5.)))),
          (BinOp (Mul, (Const (Number 2.)), (Const (Number 3.)))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "4 + 5 * (2 + 3)";
  [%expect
    {|
    (Expression
       (BinOp (Add, (Const (Number 4.)),
          (BinOp (Mul, (Const (Number 5.)),
             (BinOp (Add, (Const (Number 2.)), (Const (Number 3.))))))
          )))|}]
;;

let%expect_test _ =
  pp "4 + (5 + 2) * 3)";
  [%expect {|
    Error: incorrect statement: there is unexpected symbol: ')'|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "(4 + 5) + (2 * 3)";
  [%expect
    {|
    (Expression
       (BinOp (Add, (BinOp (Add, (Const (Number 4.)), (Const (Number 5.)))),
          (BinOp (Mul, (Const (Number 2.)), (Const (Number 3.)))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "func1(4)+(5+6)";
  [%expect
    {|
    (Expression
       (BinOp (Add, (FunctionCall ((Var "func1"), [(Const (Number 4.))])),
          (BinOp (Add, (Const (Number 5.)), (Const (Number 6.)))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "func1(4)+(5+6)";
  [%expect
    {|
    (Expression
       (BinOp (Add, (FunctionCall ((Var "func1"), [(Const (Number 4.))])),
          (BinOp (Add, (Const (Number 5.)), (Const (Number 6.)))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "{name : 4, [\"name\"+5] : 4 + 9}";
  [%expect
    {|
    (Expression
       (ObjectDef
          [((Const (String "name")), (Const (Number 4.)));
            ((BinOp (Add, (Const (String "name")), (Const (Number 5.)))),
             (BinOp (Add, (Const (Number 4.)), (Const (Number 9.)))))
            ]))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "{name : 4, [\"name\"+5] : 4 + 9,}";
  [%expect
    {|
    (Expression
       (ObjectDef
          [((Const (String "name")), (Const (Number 4.)));
            ((BinOp (Add, (Const (String "name")), (Const (Number 5.)))),
             (BinOp (Add, (Const (Number 4.)), (Const (Number 9.)))))
            ]))|}]
;;

let%expect_test _ =
  pp
    ~parse:parse_expression
    "{\n\
    \      name : \"Kakadu\",\n\
    \      sayName() {\n\
    \        return this.name;\n\
    \      },\n\
    \      like : \"OCaml\",\n\
    \    }";
  [%expect
    {|
    (Expression
       (ObjectDef
          [((Const (String "name")), (Const (String "Kakadu")));
            ((Const (String "sayName")),
             (AnonFunction ([],
                (Block
                   [(Return
                       (BinOp (PropAccs, (Var "this"), (Const (String "name")))))
                     ])
                )));
            ((Const (String "like")), (Const (String "OCaml")))]))|}]
;;

let%expect_test _ =
  pp
    ~parse:parse_expression
    "{\n\
    \      name : \"Kakadu\",\n\
    \      sayName : function () {\n\
    \        return this.name;\n\
    \      },\n\
    \      like,\n\
    \    }";
  [%expect
    {|
    (Expression
       (ObjectDef
          [((Const (String "name")), (Const (String "Kakadu")));
            ((Const (String "sayName")),
             (AnonFunction ([],
                (Block
                   [(Return
                       (BinOp (PropAccs, (Var "this"), (Const (String "name")))))
                     ])
                )));
            ((Const (String "like")), (Var "like"))]))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "hello[\"word\"]";
  [%expect
    {|
    (Expression (BinOp (PropAccs, (Var "hello"), (Const (String "word")))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "hello[a+b]";
  [%expect
    {|
    (Expression
       (BinOp (PropAccs, (Var "hello"), (BinOp (Add, (Var "a"), (Var "b"))))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "hello . word";
  [%expect
    {|
    (Expression (BinOp (PropAccs, (Var "hello"), (Const (String "word")))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "hello.let";
  [%expect
    {|
    (Expression (BinOp (PropAccs, (Var "hello"), (Const (String "let")))))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "hello.word()";
  [%expect
    {|
    (Expression
       (FunctionCall ((BinOp (PropAccs, (Var "hello"), (Const (String "word")))),
          [])))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "hello.word.hi()";
  [%expect
    {|
    (Expression
       (FunctionCall (
          (BinOp (PropAccs,
             (BinOp (PropAccs, (Var "hello"), (Const (String "word")))),
             (Const (String "hi")))),
          [])))|}]
;;

let%expect_test _ =
  pp ~parse:parse_expression "{hello : word}.hello";
  [%expect
    {|
    (Expression
       (BinOp (PropAccs, (ObjectDef [((Const (String "hello")), (Var "word"))]),
          (Const (String "hello")))))|}]
;;

(**---------------Statements parsers---------------*)

let%expect_test _ =
  pp "i;";
  [%expect {|
    (Programm [(Expression (Var "i"))])|}]
;;

let%expect_test _ =
  pp "i = i+ 1;";
  [%expect
    {|
    (Programm
       [(Expression
           (BinOp (Assign, (Var "i"),
              (BinOp (Add, (Var "i"), (Const (Number 1.)))))))
         ])|}]
;;

let%expect_test _ =
  pp "let a = 6";
  [%expect
    {|
    (Programm
       [(VarInit
           { var_identifier = "a"; is_const = false; value = (Const (Number 6.))
             })
         ]) |}]
;;

let%expect_test _ =
  pp "let a;";
  [%expect
    {|
    (Programm
       [(VarInit
           { var_identifier = "a"; is_const = false; value = (Const Undefined) })
         ]) |}]
;;

let%expect_test _ =
  pp "let a";
  [%expect
    {|
    (Programm
       [(VarInit
           { var_identifier = "a"; is_const = false; value = (Const Undefined) })
         ]) |}]
;;

let%expect_test _ =
  pp "let let1 = 6";
  [%expect
    {|
    (Programm
       [(VarInit
           { var_identifier = "let1"; is_const = false;
             value = (Const (Number 6.)) })
         ]) |}]
;;

let%expect_test _ =
  pp "let let1 = 6;";
  [%expect
    {|
    (Programm
       [(VarInit
           { var_identifier = "let1"; is_const = false;
             value = (Const (Number 6.)) })
         ]) |}]
;;

let%expect_test _ =
  pp "var a = 6";
  [%expect
    {|
    (Programm
       [(VarInit
           { var_identifier = "a"; is_const = false; value = (Const (Number 6.))
             })
         ]) |}]
;;

let%expect_test _ =
  pp "const a = 6";
  [%expect
    {|
    (Programm
       [(VarInit
           { var_identifier = "a"; is_const = true; value = (Const (Number 6.)) })
         ]) |}]
;;

let%expect_test _ =
  pp "let a = function(b1) {return b1+6;}";
  [%expect
    {|
    (Programm
       [(VarInit
           { var_identifier = "a"; is_const = false;
             value =
             (AnonFunction (["b1"],
                (Block [(Return (BinOp (Add, (Var "b1"), (Const (Number 6.)))))])
                ))
             })
         ]) |}]
;;

let%expect_test _ =
  pp "let a = (b1) => {return b1+6;}";
  [%expect
    {|
    (Programm
       [(VarInit
           { var_identifier = "a"; is_const = false;
             value =
             (ArrowFunction (["b1"],
                (Block [(Return (BinOp (Add, (Var "b1"), (Const (Number 6.)))))])
                ))
             })
         ]) |}]
;;

let%expect_test _ =
  pp "let a = (b1) => b1+6";
  [%expect
    {|
    (Programm
       [(VarInit
           { var_identifier = "a"; is_const = false;
             value =
             (ArrowFunction (["b1"],
                (Block [(Return (BinOp (Add, (Var "b1"), (Const (Number 6.)))))])
                ))
             })
         ]) |}]
;;

let%expect_test _ =
  pp "let a = function(b1) {return b1+6;}(4)";
  [%expect
    {|
    (Programm
       [(VarInit
           { var_identifier = "a"; is_const = false;
             value =
             (FunctionCall (
                (AnonFunction (["b1"],
                   (Block
                      [(Return (BinOp (Add, (Var "b1"), (Const (Number 6.)))))])
                   )),
                [(Const (Number 4.))]))
             })
         ]) |}]
;;

let%expect_test _ =
  pp "let a = ((b1) => {return b1+6;})(4)";
  [%expect
    {|
    (Programm
       [(VarInit
           { var_identifier = "a"; is_const = false;
             value =
             (FunctionCall (
                (ArrowFunction (["b1"],
                   (Block
                      [(Return (BinOp (Add, (Var "b1"), (Const (Number 6.)))))])
                   )),
                [(Const (Number 4.))]))
             })
         ]) |}]
;;

let%expect_test _ =
  pp "let a = ((b1) => b1+6)(4)";
  [%expect
    {|
    (Programm
       [(VarInit
           { var_identifier = "a"; is_const = false;
             value =
             (FunctionCall (
                (ArrowFunction (["b1"],
                   (Block
                      [(Return (BinOp (Add, (Var "b1"), (Const (Number 6.)))))])
                   )),
                [(Const (Number 4.))]))
             })
         ]) |}]
;;

let%expect_test _ =
  pp "let a = ((b1) => b1+6)(4)+5";
  [%expect
    {|
    (Programm
       [(VarInit
           { var_identifier = "a"; is_const = false;
             value =
             (BinOp (Add,
                (FunctionCall (
                   (ArrowFunction (["b1"],
                      (Block
                         [(Return (BinOp (Add, (Var "b1"), (Const (Number 6.)))))
                           ])
                      )),
                   [(Const (Number 4.))])),
                (Const (Number 5.))))
             })
         ]) |}]
;;

let%expect_test _ =
  pp "function a() {\n    return this1.name;\n  }";
  [%expect
    {|
    (Programm
       [(FunInit
           { fun_identifier = "a"; arguments = [];
             body =
             (Block
                [(Return
                    (BinOp (PropAccs, (Var "this1"), (Const (String "name")))))
                  ])
             })
         ])|}]
;;

let%expect_test _ =
  pp "function a() {\n    return this1;\n  } ;";
  [%expect
    {|
    (Programm
       [(FunInit
           { fun_identifier = "a"; arguments = [];
             body = (Block [(Return (Var "this1"))]) })
         ])|}]
;;

let%expect_test _ =
  pp "let a = { isA: true}\n  let b = {isB: true}\n  b.__proto__ = a";
  [%expect
    {|
    (Programm
       [(VarInit
           { var_identifier = "a"; is_const = false;
             value = (ObjectDef [((Const (String "isA")), (Const (Bool true)))])
             });
         (VarInit
            { var_identifier = "b"; is_const = false;
              value = (ObjectDef [((Const (String "isB")), (Const (Bool true)))])
              });
         (Expression
            (BinOp (Assign,
               (BinOp (PropAccs, (Var "b"), (Const (String "__proto__")))),
               (Var "a"))))
         ])|}]
;;

let%expect_test _ =
  pp "let a = { isA: true}\n  let b = { __proto__: a}";
  [%expect
    {|
    (Programm
       [(VarInit
           { var_identifier = "a"; is_const = false;
             value = (ObjectDef [((Const (String "isA")), (Const (Bool true)))])
             });
         (VarInit
            { var_identifier = "b"; is_const = false;
              value = (ObjectDef [((Const (String "__proto__")), (Var "a"))]) })
         ])|}]
;;

let%expect_test "if1" =
  pp "if (a == 4) let a = b + 6; else let b = 6+7;";
  [%expect
    {|
    (Programm
       [(If ((BinOp (Equal, (Var "a"), (Const (Number 4.)))),
           (Block
              [(VarInit
                  { var_identifier = "a"; is_const = false;
                    value = (BinOp (Add, (Var "b"), (Const (Number 6.)))) })
                ]),
           (Block
              [(VarInit
                  { var_identifier = "b"; is_const = false;
                    value =
                    (BinOp (Add, (Const (Number 6.)), (Const (Number 7.)))) })
                ])
           ))
         ])|}]
;;

let%expect_test "if2" =
  pp "if (a == 4) {let a = b + 6;} else {let b = 6+7;};";
  [%expect
    {|
    (Programm
       [(If ((BinOp (Equal, (Var "a"), (Const (Number 4.)))),
           (Block
              [(VarInit
                  { var_identifier = "a"; is_const = false;
                    value = (BinOp (Add, (Var "b"), (Const (Number 6.)))) })
                ]),
           (Block
              [(VarInit
                  { var_identifier = "b"; is_const = false;
                    value =
                    (BinOp (Add, (Const (Number 6.)), (Const (Number 7.)))) })
                ])
           ))
         ])|}]
;;

let%expect_test _ =
  pp "\n  {\n    let let;\n  }";
  [%expect {|
    Error: incorrect statement: there is unexpected symbol: '{'|}]
;;

let%expect_test _ =
  pp "\n  {\n    let x;\n  }";
  [%expect
    {|
    (Programm
       [(Block
           [(VarInit
               { var_identifier = "x"; is_const = false;
                 value = (Const Undefined) })
             ])
         ])|}]
;;

let%expect_test _ =
  pp "return;";
  [%expect {|
    (Programm [(Return (Const Undefined))])|}]
;;

let%expect_test _ =
  pp "return\n  4";
  [%expect
    {|
    (Programm [(Return (Const Undefined)); (Expression (Const (Number 4.)))])|}]
;;

let%expect_test "factorial" =
  pp
    "let fact = 4\n\n\
    \  function calculateFact(fact) {\n\
    \      if\n\
    \      (fact != 0)\n\
    \          return fact * calculateFact(fact - 1);else return 1;\n\
    \  }";
  [%expect
    {|
    (Programm
       [(VarInit
           { var_identifier = "fact"; is_const = false;
             value = (Const (Number 4.)) });
         (FunInit
            { fun_identifier = "calculateFact"; arguments = ["fact"];
              body =
              (Block
                 [(If ((BinOp (NotEqual, (Var "fact"), (Const (Number 0.)))),
                     (Block
                        [(Return
                            (BinOp (Mul, (Var "fact"),
                               (FunctionCall ((Var "calculateFact"),
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

let%expect_test "array_list" =
  pp "var myArray = [1, 2, 3, 4, 5];";
  [%expect
    {|
(Programm
   [(VarInit
       { var_identifier = "myArray"; is_const = false;
         value =
         (Array_list
            [(Const (Number 1.)); (Const (Number 2.)); (Const (Number 3.));
              (Const (Number 4.)); (Const (Number 5.))])
         })
     ])
    |}]
;;

let%expect_test "while" =
  pp
    "while (a != 0 ) {\n\
    \    var b = a - 1;\n\
    \    if (a == 1) {\n\
    \        return b;\n\
    \    }\n\
    \    else {\n\
    \        return 0;\n\
    \    }\n\
    \    let a = a - 1;\n\
     }";
  [%expect
    {|

(Programm
   [(While ((BinOp (NotEqual, (Var "a"), (Const (Number 0.)))),
       (Block
          [(VarInit
              { var_identifier = "b"; is_const = false;
                value = (BinOp (Sub, (Var "a"), (Const (Number 1.)))) });
            (If ((BinOp (Equal, (Var "a"), (Const (Number 1.)))),
               (Block [(Return (Var "b"))]),
               (Block [(Return (Const (Number 0.)))])));
            (VarInit
               { var_identifier = "a"; is_const = false;
                 value = (BinOp (Sub, (Var "a"), (Const (Number 1.)))) })
            ])
       ))
     ])
             |}]
;;

let%expect_test "for" =
  pp
    "for (let i = 0; i != 10; i = i + 1) {\n\
    \      var a = i;\n\
    \      if (a == 5) {\n\
    \          return a;\n\
    \      }\n\
    \    }";
  [%expect
    {|
      (Programm
         [(For
             { for_init =
               (VarInit
                  { var_identifier = "i"; is_const = false; 
                    value = (Const (Number 0.)) });
               for_condition = 
               (Expression (BinOp (NotEqual, (Var "i"), (Const (Number 10.)))));
               for_change =
               (Expression
                  (BinOp (Assign, (Var "i"), 
                     (BinOp (Add, (Var "i"), (Const (Number 1.)))))));
               for_body =
               (Block
                  [(VarInit
                      { var_identifier = "a"; is_const = false; value = (Var "i") });
                    (If ((BinOp (Equal, (Var "a"), (Const (Number 5.)))),
                       (Block [(Return (Var "a"))]), (Block [])))
                    ])
               })
           ])
    |}]
;;
