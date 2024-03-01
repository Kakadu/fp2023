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

  $ dune exec interpreter << EOF
  > def factorial(x):
  >      if (x == 1):
  >            return 1
  >      else:
  >            return (x * factorial(x - 1))
  > print(factorial(7))
  > EOF
  5040

  $ dune exec interpreter << EOF
  > class MyClass:
  >      def method1():
  >            return "A"
  > print(MyClass.method1())
  > EOF
  A

  $ dune exec interpreter << EOF
  > a = "World"
  > print(f"Hello {a}!")
  > EOF
  Hello World!

  $ dune exec interpreter << EOF
  > Successor = lambda x : (x + 1)
  > print(Successor(4))
  > EOF
  5

  $ dune exec interpreter << EOF
  > if (1 == 1 or 1/1 == 1 and 1 != 2 and 1 < 2 and 1 <= 2 and 2 > 1 and 2 >= 1 and 1 % 1 != 3):
  >      print("Passed")
  > EOF
  Passed

  $ dune exec interpreter << EOF
  > def funcToAdd():
  >      return "Expected output"
  > class myClass:
  >      def met1():
  >            return 1
  > setattr(myClass, "funco", funcToAdd)        
  > print(myClass.funco())
  > EOF
  Expected output

  $ dune exec interpreter << EOF
  > print(1)
  > EOF
  1

