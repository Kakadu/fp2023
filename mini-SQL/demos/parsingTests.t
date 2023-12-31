Copyright 2021-2022, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Tests about parsing go here. It's expected that programs parse something and
output a parse tree.
For example, where your test correctness of AST it's recommend to put both
input and output into this file. In this case it will be easier to check that
answer is correct

  $ ./demoParse.exe <<-EOF
  > SELECT * FROM table WHERE True
  Parse result: Select {exprs = [All_Columns]; table = "table";
    condition = (Some (Const (Bool true)))}

  $ ./demoParse.exe <<-EOF
  > SELECT *, *, Name, age, 1 + 1 FROM table WHERE NOT False
  Parse result: Select {
    exprs =
    [All_Columns; All_Columns; (Expr (Const (Name "Name")));
      (Expr (Const (Name "age")));
      (Expr (Binary_operation (Add, (Const (Digit 1)), (Const (Digit 1)))))];
    table = "table";
    condition = (Some (Unary_operation (Not, (Const (Bool false)))))}
