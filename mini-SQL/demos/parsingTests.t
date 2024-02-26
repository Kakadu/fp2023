  $ ./demoParse.exe <<-EOF
  > SELECT * FROM table
  Parse result: { select = [Asterisk]; from = (Table "table"); where = None }

  $ ./demoParse.exe <<-EOF
  > SELECT * FROM table WHERE True
  Parse result: { select = [Asterisk]; from = (Table "table");
    where = (Some (Const (Bool true))) }

  $ ./demoParse.exe <<-EOF
  > SELECT ID, name, age, money FROM table WHERE NOT ID = 10 AND age < 100 OR NOT age % 10 = 5
  Parse result: { select =
    [(Expression (Const (Name "ID"))); (Expression (Const (Name "name")));
      (Expression (Const (Name "age"))); (Expression (Const (Name "money")))];
    from = (Table "table");
    where =
    (Some (Binary_operation (Or,
             (Binary_operation (And,
                (Unary_operation (Not,
                   (Binary_operation (Equal, (Const (Name "ID")),
                      (Const (Digit 10))))
                   )),
                (Binary_operation (Less_Than, (Const (Name "age")),
                   (Const (Digit 100))))
                )),
             (Unary_operation (Not,
                (Binary_operation (Equal,
                   (Binary_operation (Modulo, (Const (Name "age")),
                      (Const (Digit 10)))),
                   (Const (Digit 5))))
                ))
             )))
    }

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
                (Binary_operation (Equal, (Const (Name "age")),
                   (Const (Digit 25)))),
                (Binary_operation (Equal, (Const (Name "age")),
                   (Const (Digit 27))))
                )),
             (Binary_operation (Greater_Than, (Const (Name "ID")),
                (Const (Digit 10))))
             )))
    }

  $ ./demoParse.exe <<-EOF
  > SELECT table1.name, table2.age FROM table1 FULL JOIN table2 ON table1.ID = table2.ID WHERE table2.age > 10
  Parse result: { select =
    [(Expression (Const (Name "table1.name")));
      (Expression (Const (Name "table2.age")))];
    from =
    Join {jtype = Full; left = (Table "table1"); table = "table2";
      on =
      (Binary_operation (Equal, (Const (Name "table1.ID")),
         (Const (Name "table2.ID"))))};
    where =
    (Some (Binary_operation (Greater_Than, (Const (Name "table2.age")),
             (Const (Digit 10)))))
    }

  $ ./demoParse.exe <<-EOF
  > SELECT (Company.name), (Trip.town_from) , Trip.town_to, Pass_in_trip.place FROM ((Company INNER JOIN Trip ON Company.id=Trip.company) INNER JOIN Pass_in_trip ON Trip.id = Pass_in_trip.trip) WHERE Company.id > 0
  Parse result: { select =
    [(Expression (Const (Name "Company.name")));
      (Expression (Const (Name "Trip.town_from")));
      (Expression (Const (Name "Trip.town_to")));
      (Expression (Const (Name "Pass_in_trip.place")))];
    from =
    Join {jtype = Inner;
      left =
      Join {jtype = Inner; left = (Table "Company"); table = "Trip";
        on =
        (Binary_operation (Equal, (Const (Name "Company.id")),
           (Const (Name "Trip.company"))))};
      table = "Pass_in_trip";
      on =
      (Binary_operation (Equal, (Const (Name "Trip.id")),
         (Const (Name "Pass_in_trip.trip"))))};
    where =
    (Some (Binary_operation (Greater_Than, (Const (Name "Company.id")),
             (Const (Digit 0)))))
    }
