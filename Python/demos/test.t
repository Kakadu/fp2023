  $ dune exec demo_fact
  (Function ((Identifier "factorial"), [(Identifier "x")],
     [(IfElse ((BoolOp (Equal, (Variable (Identifier "x")), (Const (Int 1)))),
         [(Return (Const (Int 1)))],
         [(Return
             (ArithOp (Mul, (Variable (Identifier "x")),
                (FunctionCall ((Identifier "factorial"),
                   [(ArithOp (Sub, (Variable (Identifier "x")), (Const (Int 1))
                       ))
                     ]
                   ))
                )))
           ]
         ))
       ]
     ))
