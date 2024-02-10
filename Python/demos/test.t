  $ dune exec demo_fact
  (Function ((Identifier "factorial"), [(Identifier "x")],
     [(IfElse (
         (BoolOp (Equal, (Variable (Global, (Identifier "x"))), (Const (Int 1))
            )),
         [(Return (Const (Int 1)))],
         [(Return
             (ArithOp (Mul, (Variable (Global, (Identifier "x"))),
                (FunctionCall ((Identifier "factorial"),
                   [(ArithOp (Sub, (Variable (Global, (Identifier "x"))),
                       (Const (Int 1))))
                     ]
                   ))
                )))
           ]
         ))
       ]
     ))

  $ dune exec interpreter_fact_test
  3628800

  $ dune exec parse_and_interpret_fact_test
  5040

  $ dune exec change_print_test
  test = 1 

  $ dune exec adding_class_to_env_test
  MyClass 

  $ dune exec method_call_test
  4

  $ dune exec string_interpolation_test
  Hello World!

  $ dune exec anon_func_test
  1

  $ dune exec logic_test
  Passed

  $ dune exec change_func_test
  2

  $ dune exec dynamic_adding_test
  Expected output
