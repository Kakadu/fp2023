(* --- PARSER --- *)

module Parse_test = struct
  open Parser
  open Ast

  (** Runs parser on a string *)
  let run p = Angstrom.parse_string ~consume:All p

  let assert_equal parser input expected =
    match run parser input with
    | Ok res when res = expected -> true
    | _ -> false
  ;;

  let assert_eq_output f parser input expected =
    let res = run parser input in
    match res with
    | Ok res when res <> expected ->
      Format.printf "Parsing result: %s!!!\n" (f res);
      false
    | Ok _ -> true
    | Error x ->
      Format.printf "Parsing error: %s!!!\n" x;
      false
  ;;

  let assert_raise parser input =
    let tryRun p i =
      match run p i with
      | Ok _ -> true
      | _ -> false
    in
    try not (tryRun parser input) with
    | _ -> true
  ;;

  (* String *)
  let%test _ = assert_equal value_p "\'1a2b3c 7\'" (String "1a2b3c 7")
  let%test _ = assert_equal value_p "\"1a2b3c 7\"" (String "1a2b3c 7")
  let%test _ = assert_raise value_p "\"1a2b3c 7\'"

  (* Name *)
  let%test _ = assert_eq_output show_value value_p "User" (Name "User")
  let%test _ = assert_equal value_p "table1.age" (Name "table1.age")
  let%test _ = assert_raise value_p "1name"

  (* Bool *)
  let%test _ = assert_equal value_p "True" (Bool true)
  let%test _ = assert_equal value_p "true" (Bool true)
  let%test _ = assert_equal value_p "False" (Bool false)
  let%test _ = assert_equal value_p "false" (Bool false)

  (* Digit *)
  let%test _ = assert_equal value_p "10" (Digit 10)
  let%test _ = assert_equal value_p "+10" (Digit 10)
  let%test _ = assert_equal value_p "-10" (Digit (-10))
  let%test _ = assert_equal value_p "+10.5" (Float_Digit 10.5)
  let%test _ = assert_equal value_p "-10.0000" (Float_Digit (-10.0))
  let%test _ = assert_equal value_p "10.015" (Float_Digit 10.015)
  let%test _ = assert_equal value_p "8." (Float_Digit 8.)
  let%test _ = assert_raise value_p "-12a3"

  (* Join *)

  let%test _ =
    assert_equal
      join_p
      "table1 FULL OUTER JOIN table2 ON table1.column_name >= table2.column_name"
      (Join
         { jtype = Full
         ; left = Table "table1"
         ; table = "table2"
         ; on =
             Binary_operation
               ( Greater_Than_Or_Equal
               , Const (Name "table1.column_name")
               , Const (Name "table2.column_name") )
         })
  ;;

  let%test _ = assert_raise on_p "ON table1+table2"

  let%test _ =
    assert_equal
      join_p
      "(table1 FULL OUTER JOIN table2 ON table1.column_name >= table2.column_name) INNER \
       JOIN table3 ON table2.column_name = table3.column_name"
      (Join
         { jtype = Inner
         ; left =
             Join
               { jtype = Full
               ; left = Table "table1"
               ; table = "table2"
               ; on =
                   Binary_operation
                     ( Greater_Than_Or_Equal
                     , Const (Name "table1.column_name")
                     , Const (Name "table2.column_name") )
               }
         ; table = "table3"
         ; on =
             Binary_operation
               ( Equal
               , Const (Name "table2.column_name")
               , Const (Name "table3.column_name") )
         })
  ;;

  let%test _ =
    assert_equal
      on_p
      "ON table1=table2"
      (Binary_operation (Equal, Const (Name "table1"), Const (Name "table2")))
  ;;

  (* arithm_p *)

  let%test _ =
    assert_equal arithm_p "2+2" (Binary_operation (Add, Const (Digit 2), Const (Digit 2)))
  ;;

  let%test _ =
    assert_equal
      arithm_p
      "( 2 + 2)"
      (Binary_operation (Add, Const (Digit 2), Const (Digit 2)))
  ;;

  let%test _ =
    assert_equal
      arithm_p
      "2 / -2"
      (Binary_operation (Divide, Const (Digit 2), Const (Digit (-2))))
  ;;

  let%test _ =
    assert_equal
      arithm_p
      "-2 - -2"
      (Binary_operation (Substract, Const (Digit (-2)), Const (Digit (-2))))
  ;;

  let%test _ =
    assert_equal
      arithm_p
      "-2 * +2"
      (Binary_operation (Multiply, Const (Digit (-2)), Const (Digit 2)))
  ;;

  let%test _ =
    assert_equal
      arithm_p
      "123 % 10"
      (Binary_operation (Modulo, Const (Digit 123), Const (Digit 10)))
  ;;

  let%test _ =
    assert_equal
      arithm_p
      "(1 + 1) * 2"
      (Binary_operation
         ( Multiply
         , Binary_operation (Add, Const (Digit 1), Const (Digit 1))
         , Const (Digit 2) ))
  ;;

  let%test _ =
    assert_equal
      arithm_p
      "(1 + 1) * (123 % 10)"
      (Binary_operation
         ( Multiply
         , Binary_operation (Add, Const (Digit 1), Const (Digit 1))
         , Binary_operation (Modulo, Const (Digit 123), Const (Digit 10)) ))
  ;;

  (* logic *)

  let%test _ =
    assert_equal
      cmp_p
      "1 + 1 != 2.5 + 2"
      (Binary_operation
         ( Not_Equal
         , Binary_operation (Add, Const (Digit 1), Const (Digit 1))
         , Binary_operation (Add, Const (Float_Digit 2.5), Const (Digit 2)) ))
  ;;

  let%test _ =
    assert_equal
      cmp_p
      "1 = 2 - 1 = 0 + 1"
      (Binary_operation
         ( Equal
         , Binary_operation
             ( Equal
             , Const (Digit 1)
             , Binary_operation (Substract, Const (Digit 2), Const (Digit 1)) )
         , Binary_operation (Add, Const (Digit 0), Const (Digit 1)) ))
  ;;

  let%test _ =
    assert_equal
      logic_p
      "1 = 2 AND 0 = 1"
      (Binary_operation
         ( And
         , Binary_operation (Equal, Const (Digit 1), Const (Digit 2))
         , Binary_operation (Equal, Const (Digit 0), Const (Digit 1)) ))
  ;;

  let%test _ = assert_equal logic_p "NOT true" (Unary_operation (Not, Const (Bool true)))

  let%test _ =
    assert_equal
      logic_p
      "NOT true AND NOT false"
      (Binary_operation
         ( And
         , Unary_operation (Not, Const (Bool true))
         , Unary_operation (Not, Const (Bool false)) ))
  ;;

  let%test _ =
    assert_equal
      logic_p
      "NOT 1 = 1 AND 1 + 1 = 2"
      (Binary_operation
         ( And
         , Unary_operation
             (Not, Binary_operation (Equal, Const (Digit 1), Const (Digit 1)))
         , Binary_operation
             ( Equal
             , Binary_operation (Add, Const (Digit 1), Const (Digit 1))
             , Const (Digit 2) ) ))
  ;;

  let%test _ =
    assert_equal
      logic_p
      "1 + 1 = 2"
      (Binary_operation
         (Equal, Binary_operation (Add, Const (Digit 1), Const (Digit 1)), Const (Digit 2)))
  ;;

  let%test _ =
    assert_equal
      logic_p
      "\'123\' = 2 AND ID > 1 OR 1 + 1 = 2"
      (Binary_operation
         ( Or
         , Binary_operation
             ( And
             , Binary_operation (Equal, Const (String "123"), Const (Digit 2))
             , Binary_operation (Greater_Than, Const (Name "ID"), Const (Digit 1)) )
         , Binary_operation
             ( Equal
             , Binary_operation (Add, Const (Digit 1), Const (Digit 1))
             , Const (Digit 2) ) ))
  ;;

  let%test _ =
    assert_equal
      request_p
      "SELECT name, age, phone_number FROM User WHERE age > 18"
      { select =
          [ Expression (Const (Name "name"))
          ; Expression (Const (Name "age"))
          ; Expression (Const (Name "phone_number"))
          ]
      ; from = Table "User"
      ; where =
          Some (Binary_operation (Greater_Than, Const (Name "age"), Const (Digit 18)))
      }
  ;;

  let%test _ = assert_raise arithm_p "-2 x 2"
end

(* --- TYPES --- *)

module Types_test = struct
  open Types

  let assert_equal res exp show =
    match exp = res with
    | true -> true
    | false ->
      Format.printf "%s\n" (show res);
      false
  ;;

  let%test _ =
    assert_equal
      (Row.init
         [ String_Column; Numeric_Column; Real_Column; Boolean_Column ]
         [ "wow"; "123"; "10.0"; "true" ])
      [| String "wow"; Numeric 123; Real 10.0; Bool true |]
      Row.show_row
  ;;

  let%test _ =
    assert_equal
      (Sheet.init
         [ String_Column; Numeric_Column; Real_Column; Boolean_Column ]
         [ [ "wow"; "123"; "10.0"; "true" ]; [ "wt"; "1000"; "15.9"; "false" ] ])
      [| [| String "wow"; Numeric 123; Real 10.0; Bool true |]
       ; [| String "wt"; Numeric 1000; Real 15.9; Bool false |]
      |]
      Sheet.show_sheet
  ;;

  (* let%test _ =
     Format.printf
     "%s\n\n"
     (Sheet.show_sheet
     (Environment.load_table "/home/dmitriy/Desktop/test_data/database1/table1.csv")
     .data);
     true
     ;; *)

  (* let%test _ =
     Format.printf
     "%s\n\n"
     (Database.show_database base2);
     true
     ;; *)

  (* let%test _ =
     Format.printf
     "%s\n\n"
     (Database.show_database base2);
     true
     ;; *)

  open Interpreter.Eval (Utils.Result)

  let assert_equal res exp show =
    match res with
    | Ok x ->
      (match exp = x with
       | true -> true
       | false ->
         Format.printf "%s\n" (show x);
         false)
    | Error e ->
      Format.printf "Interpret error: %s\n" (Utils.show_error e);
      false
  ;;

  open Typecheck.Exec (Utils.Result)

  let%test _ =
    assert_equal
      (Types.Numeric 15) #+ (Types.Numeric 15)
      (Types.Numeric 30)
      Types.show_item
  ;;

  (* typecheck *)

  let%test _ =
    assert_equal (Types.Numeric 15) #+ (Types.Real 15.) (Types.Real 30.) Types.show_item
  ;;

  let%test _ =
    assert_equal (Types.Numeric 15) #- (Types.Real 15.) (Types.Real 0.) Types.show_item
  ;;

  let%test _ =
    assert_equal (Types.Bool true) #* (Types.Real 5.) (Types.Real 5.) Types.show_item
  ;;

  let%test _ =
    assert_equal (Types.Bool true) #* (Types.Real 5.) (Types.Real 5.) Types.show_item
  ;;

  let%test _ =
    assert_equal (Types.Numeric 10) #/ (Types.Real 5.) (Types.Real 2.) Types.show_item
  ;;

  let%test _ =
    assert_equal
      (Types.String "15") #+ (Types.Real 5.)
      (Types.String "155.")
      Types.show_item
  ;;

  let parse inp = Result.get_ok (Parser.parse inp)
  let transform base inp = transform_request base (parse inp)

  let exec base req =
    match eval base req with
    | Ok table -> Format.printf "Result:\n%s\n" (Types.Table.show_table table)
    | Error e -> Format.printf "Execute error: %s\n" (Utils.show_error e)
  ;;

  (* let%test _ =
    assert_equal
      (transform base1 "SELECT name FROM table1")
      (Project
         ( Restrict
             ( Load
                 { data =
                     [| [| String "Rachel Klein"; Numeric 65; Bool false |]
                      ; [| String "Frank James"; Numeric 24; Bool false |]
                      ; [| String "Barbara Fletcher"; Numeric 38; Bool true |]
                      ; [| String "Polly Drake"; Numeric 33; Bool false |]
                      ; [| String "Howard Glover"; Numeric 42; Bool false |]
                      ; [| String "Alice Houston"; Numeric 50; Bool true |]
                      ; [| String "Gregory Cunningham"; Numeric 62; Bool true |]
                      ; [| String "Jim Gregory"; Numeric 27; Bool false |]
                      ; [| String "Eleanor Johnston"; Numeric 37; Bool true |]
                      ; [| String "Eliza Norton"; Numeric 43; Bool false |]
                     |]
                 ; meta =
                     { table_name = "table1"
                     ; table_header =
                         [| { column_name = "name"; column_type = String_Column }
                          ; { column_name = "age"; column_type = Numeric_Column }
                          ; { column_name = "is_gay"; column_type = Boolean_Column }
                         |]
                     }
                 }
             , Const (Bool true) )
         , [ Col
               { column_index = 0
               ; meta = { column_name = "name"; column_type = String_Column }
               }
           ] ))
      Interpreter.show_qot_node
  ;; *)

  (* let%test _ =
     exec
     "/home/dmitriy/Desktop/fp2023/mini-SQL/test_data/database1"
     (parse "SELECT table1.name, age, table1.is_gay FROM table1 WHERE (age > 40 OR is_gay) AND (age < 60)");
     true
     ;; *)

  (* let%test _ =
     exec
     "/home/dmitriy/Desktop/fp2023/mini-SQL/test_data/two_files_database"
     (parse
     "SELECT table0.id, table0.email, table2.id, table1.id, table1.firstname FROM ((table1 INNER JOIN table2 ON table1.email = table2.email) INNER JOIN table0 ON table0.id = table2.id) WHERE table0.id > 110");
     false
     ;; *)

  (* let%test _ =
     exec
     "/home/dmitriy/Desktop/fp2023/mini-SQL/test_data/two_files_database"
     (parse
     "SELECT table0.id, table0.firstname, table1.id, table1.firstname FROM \
     table0 RIGHT JOIN table1 ON table0.id = table1.id");
     false
     ;; *)

  let%test _ =
    exec
      "/home/dmitriy/Desktop/fp2023/mini-SQL/test_data/data"
      (parse
         "SELECT table0.id, table0.firstname, table0.profession, table0.age, table1.id, \
          table1.firstname, table1.profession, table1.age FROM table0 INNER JOIN table1 \
          ON table0.profession = table1.profession");
    false
  ;;
end
