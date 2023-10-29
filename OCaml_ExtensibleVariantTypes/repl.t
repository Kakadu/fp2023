
  $ ./REPL.exe
  OCaml-style toplevel (ocamlc, utop) is not implemented
  $ ./REPL.exe -help
  Read-Eval-Print-Loop for Utyped Lambda Calculus
    - Read from stdin single program, instead of running full REPL
    -cbv Call-by-value strategy
    -cbn Call-by-name strategy
    -no Normal Order strategy
    -ao Applicative Order strategy
    -help  Display this list of options
    --help  Display this list of options
  $ ./REPL.exe -cbv - <<EOF
  > \f.x
  Parsed result: (Abs (f, (Var x)))
  Evaluated result: (Abs (f, (Var x)))
  $ ./REPL.exe -no - <<EOF
  > (\x.\y.x)(\u.u)((\x. x x)(\x.x x))
  Parsed result: (App (
                    (App ((Abs (x, (Abs (y, (Var x))))), (Abs (u, (Var u))))),
                    (App ((Abs (x, (App ((Var x), (Var x))))),
                       (Abs (x, (App ((Var x), (Var x)))))))
                    ))
  Evaluated result: (Abs (u, (Var u)))
