  $ ./haskellParser.exe <<EOF
  > fact n = if (n < 2) then 1 else fact (n - 1) * n
  > EOF
  [(DeclLet
      ((PatVar "fact"),
       (ExprFunc
          ((PatVar "n"),
           (ExprIf ((ExprBinOp (Lt, (ExprVar "n"), (ExprLit (LitInt 2)))),
              (ExprLit (LitInt 1)),
              (ExprBinOp (Mul,
                 (ExprApp ((ExprVar "fact"),
                    (ExprBinOp (Sub, (ExprVar "n"), (ExprLit (LitInt 1)))))),
                 (ExprVar "n")))
              ))))))
    ]

yet another factorial 
  $ ./haskellParser.exe <<EOF
  > fact 0 = 1
  > fact n = n * fact (n - 1)
  > EOF
  [(DeclLet
      ((PatVar "fact"), (ExprFunc ((PatLit (LitInt 0)), (ExprLit (LitInt 1))))));
    (DeclLet
       ((PatVar "fact"),
        (ExprFunc
           ((PatVar "n"),
            (ExprBinOp (Mul, (ExprVar "n"),
               (ExprApp ((ExprVar "fact"),
                  (ExprBinOp (Sub, (ExprVar "n"), (ExprLit (LitInt 1))))))
               ))))))
    ]

  $ ./haskellParser.exe <<EOF
  > x=1
  > EOF
  [(DeclLet ((PatVar "x"), (ExprLit (LitInt 1))))]
