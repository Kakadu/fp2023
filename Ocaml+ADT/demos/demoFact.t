  $ dune exec demoFact
  [(DLet
      ((DRec true), (LName "factorial_recursive"), (DType TEmptyType),
       (EFun (((LName "n"), (DType TInt)),
          (EIf ((EBinop (Leq, (EVar (LName "n")), (EInt 1))), (EInt 1),
             (EBinop (Mul, (EVar (LName "n")),
                (EApp ((EVar (LName "factorial_recursive")),
                   (EBinop (Sub, (EVar (LName "n")), (EInt 1)))))
                ))
             ))
          ))))
    ]
