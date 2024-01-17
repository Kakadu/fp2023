  $ ./demoParser.exe < ../examples/facfib.rb
  [(Func ((Id "fib"), [(Id "n")],
      [(IfElse ((Less ((Var (LocalVar, (Id "n"))), (Const (Int 2)))),
          [(Return (Var (LocalVar, (Id "n"))))], []));
        (Return
           (Plus (
              (FuncMonoCall ((Id "fib"),
                 [(Minus ((Var (LocalVar, (Id "n"))), (Const (Int 1))))])),
              (FuncMonoCall ((Id "fib"),
                 [(Minus ((Var (LocalVar, (Id "n"))), (Const (Int 2))))]))
              )))
        ]
      ));
    (Func ((Id "fac"), [(Id "n")],
       [(IfElse ((LessOrEqual ((Var (LocalVar, (Id "n"))), (Const (Int 1)))),
           [(Return (Const (Int 1)))], []));
         (Return
            (Mult ((Var (LocalVar, (Id "n"))),
               (FuncMonoCall ((Id "fac"),
                  [(Minus ((Var (LocalVar, (Id "n"))), (Const (Int 1))))]))
               )))
         ]
       ));
    (Puts (FuncMonoCall ((Id "fib"), [(Const (Int 10))])));
    (Puts (FuncMonoCall ((Id "fac"), [(Const (Int 10))])))]

  $ ./demoParser.exe < ../examples/testpars.rb
  [(Expr (Const (Int 1))); (Expr (Const (Int -2))); (Expr (Const (Float 1.)));
    (Expr (Const (Float -2.3))); (Expr (Const (Bool true)));
    (Expr (Const (Str "flower"))); (Expr (Const (Bool false)));
    (Expr (Const Null)); (Expr (Var (LocalVar, (Id "iam_local_var"))));
    (Expr (Var (InstanceVar, (Id "iam_instance_var"))));
    (Expr (Var (ClassVar, (Id "iam_class_var"))));
    (Expr (Var (GlobalVar, (Id "iam_global_var"))));
    (Expr (Plus ((Const (Int 1)), (Const (Int 1)))));
    (Expr (Plus ((Plus ((Const (Int 1)), (Const (Int 1)))), (Const (Int 1)))));
    (Expr (Plus ((Const (Int 1)), (Plus ((Const (Int 1)), (Const (Int 1)))))));
    (Expr (Minus ((Const (Int 1)), (Const (Int 1)))));
    (Expr (Minus ((Minus ((Const (Int 1)), (Const (Int 1)))), (Const (Int 1)))));
    (Expr (Minus ((Const (Int 1)), (Minus ((Const (Int 1)), (Const (Int 1)))))));
    (Expr (Mult ((Const (Int 1)), (Const (Int 1)))));
    (Expr (Mult ((Mult ((Const (Int 1)), (Const (Int 1)))), (Const (Int 1)))));
    (Expr (Mult ((Const (Int 1)), (Mult ((Const (Int 1)), (Const (Int 1)))))));
    (Expr (Div ((Const (Int 1)), (Const (Int 1)))));
    (Expr (Div ((Div ((Const (Int 1)), (Const (Int 1)))), (Const (Int 1)))));
    (Expr (Div ((Const (Int 1)), (Div ((Const (Int 1)), (Const (Int 1)))))));
    (Expr (ModOp ((Const (Int 1)), (Const (Int 1)))));
    (Expr (ModOp ((ModOp ((Const (Int 1)), (Const (Int 1)))), (Const (Int 1)))));
    (Expr (ModOp ((Const (Int 1)), (ModOp ((Const (Int 1)), (Const (Int 1)))))));
    (IfElse ((Const (Bool true)),
       [(Assign ((Var (LocalVar, (Id "x"))), (Const (Int 0))))], []));
    (IfElse ((Const (Bool false)),
       [(Assign ((Var (LocalVar, (Id "x"))), (Const (Int 0))))],
       [(Assign ((Var (LocalVar, (Id "x"))), (Const (Int 1))))]));
    (While ((Const (Bool true)), []));
    (While ((Equal ((Var (LocalVar, (Id "x"))), (Const (Bool true)))), 
       [Break]));
    (While ((GreaterOrEqual ((Var (LocalVar, (Id "x"))), (Const (Int 1)))),
       [Continue]));
    (Expr (FuncMonoCall ((Id "f"), [])));
    (Expr
       (FuncMonoCall ((Id "f"),
          [(Var (LocalVar, (Id "a"))); (Var (LocalVar, (Id "b")));
            (Var (LocalVar, (Id "c")))]
          )));
    (Expr
       (FuncMonoCall ((Id "f"),
          [(Const (Int 1)); (Const (Float 1.5)); (Const (Float -6.8))])));
    (Expr (FuncPolyCall ((Id "item"), (Id "yeet"), [(Const (Bool true))])));
    (Expr (Equal ((Const (Int 1)), (Const (Int 1)))));
    (Expr (NotEqual ((Const (Int 1)), (Const (Int 0)))));
    (Expr (Greater ((Const (Int 1)), (Const (Int 0)))));
    (Expr (Less ((Const (Int 0)), (Const (Int 1)))));
    (Expr (GreaterOrEqual ((Const (Int 1)), (Const (Int 1)))));
    (Expr (LessOrEqual ((Const (Int 2)), (Const (Int 3)))))]

  $ ./demoParser.exe < ../examples/object.rb
  [(Class ((Id "Hope"),
      [(Func ((Id "fib"), [(Id "n")],
          [(IfElse ((Less ((Var (LocalVar, (Id "n"))), (Const (Int 2)))),
              [(Return (Var (LocalVar, (Id "n"))))], []));
            (Return
               (Plus (
                  (FuncMonoCall ((Id "fib"),
                     [(Minus ((Var (LocalVar, (Id "n"))), (Const (Int 1))))])),
                  (FuncMonoCall ((Id "fib"),
                     [(Minus ((Var (LocalVar, (Id "n"))), (Const (Int 2))))]))
                  )))
            ]
          ));
        (Func ((Id "doxx"), [(Id "bool")],
           [(While ((Var (LocalVar, (Id "bool"))),
               [(Return (Var (LocalVar, (Id "bool"))))]))
             ]
           ))
        ]
      ));
    (Assign ((Var (LocalVar, (Id "x"))), (Const (Object (Id "Hope")))));
    (IfElse (
       (FuncPolyCall ((Id "x"), (Id "doxx"),
          [(Equal ((Const (Bool true)),
              (FuncPolyCall ((Id "x"), (Id "doxx"), [(Const (Bool true))]))))
            ]
          )),
       [(Puts (FuncPolyCall ((Id "x"), (Id "fib"), [(Const (Int 7))])))], 
       []));
    (Assign ((Var (LocalVar, (Id "multiplier"))), (Const (Int 666))));
    (Assign ((Var (LocalVar, (Id "not_anonymous"))),
       (Const
          (Lambda ([(Id "x"); (Id "y"); (Id "z")],
             [(Assign ((Var (LocalVar, (Id "t"))),
                 (Mult ((Const (Int 3)), (Var (LocalVar, (Id "multiplier")))))
                 ));
               (Return
                  (Plus (
                     (Mult ((Var (LocalVar, (Id "t"))),
                        (Var (LocalVar, (Id "x"))))),
                     (Div ((Var (LocalVar, (Id "y"))),
                        (Var (LocalVar, (Id "z")))))
                     )))
               ]
             )))
       ));
    (Puts
       (FuncPolyCall ((Id "not_anonymous"), (Id "call"),
          [(Const (Int 1)); (Const (Int 2)); (Const (Int 3))])))
    ]
