  $ dune exec demoParse
  [(SValue (Rec,
      ((PVar "fac"),
       (EFun ((PVar "n"),
          (EIf ((EBin_op (Lt, (EVar "n"), (EConst (CInt 2)))),
             (EConst (CInt 1)),
             (EBin_op (Mul, (EVar "n"),
                (EApply ((EVar "fac"),
                   (EBin_op (Sub, (EVar "n"), (EConst (CInt 1))))))
                ))
             ))
          )))
      ))
    ]
