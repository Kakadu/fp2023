  $ dune exec demoParse << EOF
  > let rec factorial_recursive = fun n -> if n <= 1 then 1 else n * factorial_recursive (n - 1)
  > EOF
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
  $ dune exec demoParse << EOF
  > let rec fix = fun f -> (fun x -> f (fix f) x)
  > let fac = fix (fun self -> (fun n -> if n <= 1 then 1 else n * self (n - 1)))
  > EOF
  [(DLet
      ((DRec true), (LName "fix"), (DType TEmptyType),
       (EFun ((PVar (LName "f")),
          (EFun ((PVar (LName "x")),
             (EApp (
                (EApp ((EVar (LName "f")),
                   (EApp ((EVar (LName "fix")), (EVar (LName "f")))))),
                (EVar (LName "x"))))
             ))
          ))));
    (DLet
       ((DRec false), (LName "fac"), (DType TEmptyType),
        (EApp ((EVar (LName "fix")),
           (EFun ((PVar (LName "self")),
              (EFun ((PVar (LName "n")),
                 (EIf ((EBinop (Leq, (EVar (LName "n")), (EInt 1))), (EInt 1),
                    (EBinop (Mul, (EVar (LName "n")),
                       (EApp ((EVar (LName "self")),
                          (EBinop (Sub, (EVar (LName "n")), (EInt 1)))))
                       ))
                    ))
                 ))
              ))
           ))))
    ]
