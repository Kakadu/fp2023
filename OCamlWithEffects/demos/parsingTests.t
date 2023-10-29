Copyright 2021-2022, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Tests about parsing go here. It's expected that programs parse something and
output a parse tree.
For example, where your test correctness of AST it's recommend to put both
input and output into this file. In this case it will be easier to check that
answer is correct

  $ ./demoParse.exe <<-EOF
  > (λf.λx. f (x x))
  > EOF
  (Abs (f, (Abs (x, (App ((Var f), (App ((Var x), (Var x)))))))))
