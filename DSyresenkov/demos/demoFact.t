  $ dune exec demoFact
  (ELet (Rec, "fib",
     (EFun ("x",
        (EBranch ((EBinop (Eq, (EVar "x"), (EConst (CInt 1)))), (EVar "x"),
           (EBinop (Mul, (EVar "x"),
              (EApp ((EVar "fib"),
                 (EBinop (Sub, (EVar "x"), (EConst (CInt 1))))))
              ))
           ))
        )),
     EUnit))
x
