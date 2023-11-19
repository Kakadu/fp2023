  $ ./demoParse.exe <<-EOF
  > let rec f n = if n = 0 then 1 else n * f (n-1)
  > EOF
  [(DLet (Recursive, (Ident "f"),
      (EFun
         (EApp ((EId (Ident "n")),
            (EIf ((EBinop ((EId (Ident "n")), Eq, (EConst (CInt 0)))),
               (EConst (CInt 1)),
               (EBinop ((EId (Ident "n")), Mul,
                  (EApp ((EId (Ident "f")),
                     (EBinop ((EId (Ident "n")), Sub, (EConst (CInt 1))))))
                  ))
               ))
            )))
      ))
    ]
