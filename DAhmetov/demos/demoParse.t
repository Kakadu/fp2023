  $ ./demoParse.exe << EOF
  > let rec fac = fun n -> if n <= 1 then 1 else n * fac (n - 1)
  > EOF
  [(DeclLet (true, "fac",
      (EFun ((PVar "n"),
         (EIfThenElse ((EBinop (Ltq, (EVar "n"), (EInt 1))), (EInt 1),
            (EBinop (Multi, (EVar "n"),
               (EApp ((EVar "fac"), (EBinop (Minus, (EVar "n"), (EInt 1)))))))
            ))
         ))
      ))
    ]

  $ ./demoParse.exe << EOF
  > let f = 5
  > EOF
  [(DeclLet (false, "f", (EInt 5)))]

  $ ./demoParse.exe << EOF
  > let fff = fun n -> if n then 10 else (1, 2, 3)
  > EOF
  [(DeclLet (false, "fff",
      (EFun ((PVar "n"),
         (EIfThenElse ((EVar "n"), (EInt 10),
            (ETuple [(EInt 1); (EInt 2); (EInt 3)])))
         ))
      ))
    ]

  $ ./demoParse.exe << EOF
  > let x = (false || true) && true
  > EOF
  [(DeclLet (false, "x",
      (EBinop (And, (EBinop (Or, (EBool false), (EBool true))), (EBool true)))
      ))
    ]

  $ ./demoParse.exe << EOF
  > let input = fun x -> match x with | 1 -> "one" | 2 -> "two" | _ -> "other"
  > EOF
  [(DeclLet (false, "input",
      (EFun ((PVar "x"),
         (EMatch ((EVar "x"),
            [((PInt 1), (EString "one")); ((PInt 2), (EString "two"));
              ((PVar "_"), (EString "other"))]
            ))
         ))
      ))
    ]

  $ ./demoParse.exe << EOF
  > let summ = fun x -> x + 8  
  > let res = summ 8
  > EOF
  [(DeclLet (false, "summ",
      (EFun ((PVar "x"), (EBinop (Plus, (EVar "x"), (EInt 8)))))));
    (DeclLet (false, "res", (EApp ((EVar "summ"), (EInt 8)))))]

