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
  > SELECT *, *, Name, age, 21 - 15 * 2 FROM table WHERE (age = 25 OR age = 27) AND ID > 10
  Parse result: Select {
    exprs =
    [All_Columns; All_Columns; (Expr (Const (Name "Name")));
      (Expr (Const (Name "age")));
      (Expr
         (Binary_operation (Substract, (Const (Digit 21)),
            (Binary_operation (Multiply, (Const (Digit 15)), (Const (Digit 2))
               ))
            )))
      ];
    table = "table";
    condition =
    (Some (Binary_operation (And,
             (Binary_operation (Or,
                (Binary_operation (Equal, (Const (Name "age")),
                   (Const (Digit 25)))),
                (Binary_operation (Equal, (Const (Name "age")),
                   (Const (Digit 27))))
                )),
             (Binary_operation (Greater_Than, (Const (Name "ID")),
                (Const (Digit 10))))
             )))}
