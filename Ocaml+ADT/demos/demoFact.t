  $ dune exec demoFact
  [(DLet
      ((DRec true), (LName "factorial_recursive"), (DType TEmptyType),
       (EFun ((PVar (LName "n")),
          (EIf ((EBinop (Leq, (EVar (LName "n")), (EInt 1))), (EInt 1),
             (EBinop (Mul, (EVar (LName "n")),
                (EApp ((EVar (LName "factorial_recursive")),
                   (EBinop (Sub, (EVar (LName "n")), (EInt 1)))))
                ))
             ))
          ))))
    ]
