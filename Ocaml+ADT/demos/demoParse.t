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
  $ dune exec demoParse << EOF
  > type node = | Red of int | Black of int
  > let is_black = fun x -> match x with | Black _ -> true | Red _ -> false
  > let a = is_black (Black 5)
  > EOF
  [(DType
      ((LName "node"),
       [((UName "Red"), (DType TInt)); ((UName "Black"), (DType TInt))]));
    (DLet
       ((DRec false), (LName "is_black"), (DType TEmptyType),
        (EFun ((PVar (LName "x")),
           (EMatch ((EVar (LName "x")),
              [((PAdt ((UName "Black"), (Some PWild))), (EBool true));
                ((PAdt ((UName "Red"), (Some PWild))), (EBool false))]
              ))
           ))));
    (DLet
       ((DRec false), (LName "a"), (DType TEmptyType),
        (EApp ((EVar (LName "is_black")),
           (EConstr ((UName "Black"), (Some (EInt 5))))))))
    ]
  $ dune exec demoParse << EOF
  > type color = | White | Green | Yellow | Blue | Red | Black
  > type eatable = | Yes | No
  > type thing = | Apple of color * eatable | Chair of color * eatable | Potato of color * eatable
  > let a = Apple (Yellow, Yes)
  > EOF
  [(DType
      ((LName "color"),
       [((UName "White"), (DType TEmptyType));
         ((UName "Green"), (DType TEmptyType));
         ((UName "Yellow"), (DType TEmptyType));
         ((UName "Blue"), (DType TEmptyType));
         ((UName "Red"), (DType TEmptyType));
         ((UName "Black"), (DType TEmptyType))]));
    (DType
       ((LName "eatable"),
        [((UName "Yes"), (DType TEmptyType));
          ((UName "No"), (DType TEmptyType))]));
    (DType
       ((LName "thing"),
        [((UName "Apple"),
          (DType (TTuple [(TVar (LName "color")); (TVar (LName "eatable"))])));
          ((UName "Chair"),
           (DType (TTuple [(TVar (LName "color")); (TVar (LName "eatable"))])));
          ((UName "Potato"),
           (DType (TTuple [(TVar (LName "color")); (TVar (LName "eatable"))])))
          ]));
    (DLet
       ((DRec false), (LName "a"), (DType TEmptyType),
        (EConstr ((UName "Apple"),
           (Some (ETuple
                    [(EConstr ((UName "Yellow"), None));
                      (EConstr ((UName "Yes"), None))]))
           ))))
    ]
