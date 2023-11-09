Copyright 2021-2022, Kakadu and contributors
SPDX-License-Identifier: CC0-1.0

Tests about parsing go here. It's expected that programs parse something and
output a parse tree.
For example, where your test correctness of AST it's recommend to put both
input and output into this file. In this case it will be easier to check that
answer is correct

  $ ./demoParse.exe <<-EOF
  > SELECT * FROM table WHERE True
  Parse result: { select = [Asterisk]; from = (Table "table");
    where = (Some (Const (Bool true))) }

  $ ./demoParse.exe <<-EOF
  > SELECT *, *, Name, age, 21 - 15 * 2 FROM table WHERE (age = 25 OR age = 27) AND ID > 10
  Parse result: { select =
    [Asterisk; Asterisk; (Expression (Const (Name "Name")));
      (Expression (Const (Name "age")));
      (Expression
         (Binary_operation (Substract, (Const (Digit 21)),
            (Binary_operation (Multiply, (Const (Digit 15)), (Const (Digit 2))
               ))
            )))
      ];
    from = (Table "table");
    where =
    (Some (Binary_operation (And,
             (Binary_operation (Or,
                (Binary_operation ((Compare Equal), (Const (Name "age")),
                   (Const (Digit 25)))),
                (Binary_operation ((Compare Equal), (Const (Name "age")),
                   (Const (Digit 27))))
                )),
             (Binary_operation ((Compare Greater_Than), (Const (Name "ID")),
                (Const (Digit 10))))
             )))
    }

  $ ./demoParse.exe <<-EOF
  > SELECT table1.name, table2.age FROM table1 FULL JOIN table2 ON table1.ID = table2.ID WHERE table2.age > 10
  Parse result: { select =
    [(Expression (Const (Name "table1.name")));
      (Expression (Const (Name "table2.age")))];
    from =
    (Join (Full, (Table "table1"), "table2",
       (Binary_operation ((Compare Equal), (Const (Name "table1.ID")),
          (Const (Name "table2.ID"))))
       ));
    where =
    (Some (Binary_operation ((Compare Greater_Than),
             (Const (Name "table2.age")), (Const (Digit 10)))))
    }
