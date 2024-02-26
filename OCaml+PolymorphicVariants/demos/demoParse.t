  $ dune exec demoParse << EOF
  > let rec fac n = if n < 1 then 1 else n * fac ( n - 1);;
  > EOF
  [(SValue (Rec,
      ((PVar "fac"),
       (EFun ((PVar "n"),
          (EIf ((EBin_op (Lt, (EVar "n"), (EConst (CInt 1)))),
             (EConst (CInt 1)),
             (EBin_op (Mul, (EVar "n"),
                (EApply ((EVar "fac"),
                   (EBin_op (Sub, (EVar "n"), (EConst (CInt 1))))))
                ))
             ))
          )))
      ))
    ]
