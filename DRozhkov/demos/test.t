  $ dune exec demo_fact
  Let
  (Rec, "fact",
   Fun
   ("x",
    IfThenElse
    (Binop (Eq, Var ("x"), Const (Intenger (0))), Const (Intenger (1)),
     Binop
     (Mul, Var ("x"),
      App (Var ("fact"), Binop (Sub, Var ("x"), Const (Intenger (1))))))),
   Empty)
