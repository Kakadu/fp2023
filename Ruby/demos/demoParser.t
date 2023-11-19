  $ ./demoParser.exe <<- EOF
  > def fac(n)
  >     if n <= 1
  >         return 1
  >     end
  >     return n * fac(n - 1)
  > end
  >   
  > puts fac(10)
  > EOF
  [(Func ((Id "fac"), [(Id "n")],
      [(IfElse ((LessOrEqual ((Var (Id "n")), (Const (Int 1)))),
          [(Returns (Const (Int 1)))], []));
        (Returns
           (Mult ((Var (Id "n")),
              (FuncCall ((Id "fac"),
                 [(Minus ((Var (Id "n")), (Const (Int 1))))]))
              )))
        ]
      ));
    (Puts (FuncCall ((Id "fac"), [(Const (Int 10))])))]
