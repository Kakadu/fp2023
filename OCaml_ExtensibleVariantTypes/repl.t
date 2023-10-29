  $ dune exec ./REPL.exe <<-EOF
  > let s = 2+2
  [((DLet (Not_recursive, (Ident "s"), (EParams []))),
    (EBinop ((EConst (CInt 2)), Add, (EConst (CInt 2)))))]
