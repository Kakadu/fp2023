  $ ./REPL.exe <<-EOF
  > let s = 2+2
  [(DLet (Not_recursive, (Ident "s"),
      (EBinop ((EConst (CInt 2)), Add, (EConst (CInt 2))))))
    ]
