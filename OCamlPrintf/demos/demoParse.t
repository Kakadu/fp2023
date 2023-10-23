  $ dune exec demoParse << EOF
  > let rec fac n = if n <= 1 then 1 else n * fac (n - 1)
  > let sum x y = x + y
  > EOF
  [(Let_decl
      (true, (LCIdent "fac"),
       (Fun ((LCIdent "n"),
          (ITE ((Bin_op (Leq, (Expr_val (LCIdent "n")), (Expr_const (Int 1)))),
             (Expr_const (Int 1)),
             (Bin_op (Mul, (Expr_val (LCIdent "n")),
                (App ((Expr_val (LCIdent "fac")),
                   (Bin_op (Sub, (Expr_val (LCIdent "n")), (Expr_const (Int 1))
                      ))
                   ))
                ))
             ))
          ))));
    (Let_decl
       (false, (LCIdent "sum"),
        (Fun ((LCIdent "x"),
           (Fun ((LCIdent "y"),
              (Bin_op (Add, (Expr_val (LCIdent "x")), (Expr_val (LCIdent "y"))
                 ))
              ))
           ))))
    ]

