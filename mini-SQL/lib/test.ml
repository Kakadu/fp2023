(** Copyright 2023-2024, Zaytsev Dmitriy *)

(** SPDX-License-Identifier: CC0-1.0 *)

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
    match run parser input with
    | Error _ -> true
    | Ok _ -> false
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

  let test_sheet1 =
    Sheet.init
      [ String_Column; Numeric_Column; Real_Column; Boolean_Column ]
      [ [ "wow"; "123"; "10.0"; "true" ]; [ "wt"; "1000"; "15.9"; "false" ] ]
  ;;

  let test_sheet2 =
    Sheet.init
      [ String_Column; Numeric_Column; Real_Column; Boolean_Column ]
      [ [ "Hello"; "123"; "10.0"; "false" ]; [ "World"; "321"; "15.9"; "false" ] ]
  ;;

  let%test _ =
    assert_equal
      test_sheet1
      [| [| String "wow"; Numeric 123; Real 10.0; Bool true |]
       ; [| String "wt"; Numeric 1000; Real 15.9; Bool false |]
      |]
      Sheet.show_sheet
  ;;

  let%test _ =
    assert_equal
      (Sheet.join test_sheet1 test_sheet2)
      [| [| String "wow"
          ; Numeric 123
          ; Real 10.0
          ; Bool true
          ; String "Hello"
          ; Numeric 123
          ; Real 10.0
          ; Bool false
         |]
       ; [| String "wt"
          ; Numeric 1000
          ; Real 15.9
          ; Bool false
          ; String "World"
          ; Numeric 321
          ; Real 15.9
          ; Bool false
         |]
      |]
      Sheet.show_sheet
  ;;
end

(* ### I'll keep it here in case I come up with tests for the interpreter other than cram ###

   module Interpreter_test = struct
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
   Format.printf "Interpret error: %s\n" (Utils.error_to_string e);
   false
   ;;

   end *)

module Typecheck_test = struct
  open Typecheck.Exec (Utils.Result)

  let assert_equal t1 op t2 exp =
    match op t1 t2 with
    | Ok x ->
      (match exp = x with
       | true -> true
       | false ->
         Format.printf "%s\n" (Types.show_item x);
         false)
    | Error e ->
      Format.printf "Interpret error: %s\n" (Utils.error_to_string e);
      false
  ;;

  let assert_raise t1 op t2 =
    match op t1 t2 with
    | Ok _ -> false
    | Error _ -> true
  ;;

  (* ### below there will be a lot of similar checks (for every arithm op), you don’t have to look at it ### *)

  (* + *)

  let%test _ =
    (* int + int *)
    assert_equal (Types.Numeric 15) ( #+ ) (Types.Numeric 15) (Types.Numeric 30)
  ;;

  let%test _ =
    (* int + real *)
    assert_equal (Types.Numeric 15) ( #+ ) (Types.Real 15.) (Types.Real 30.)
  ;;

  let%test _ =
    (* real + int *)
    assert_equal (Types.Real 15.) ( #+ ) (Types.Numeric 15) (Types.Real 30.)
  ;;

  let%test _ =
    (* real + real *)
    assert_equal (Types.Real 15.) ( #+ ) (Types.Real 15.) (Types.Real 30.)
  ;;

  let%test _ =
    (* string + string *)
    assert_equal
      (Types.String "Hello ")
      ( #+ )
      (Types.String "World")
      (Types.String "Hello World")
  ;;

  let%test _ =
    (* string + real *)
    assert_equal
      (Types.String "10 + 5 = ")
      ( #+ )
      (Types.Real 15.)
      (Types.String "10 + 5 = 15.")
  ;;

  let%test _ =
    (* real + string *)
    assert_equal
      (Types.Real 10.)
      ( #+ )
      (Types.String " = 10.")
      (Types.String "10. = 10.")
  ;;

  let%test _ =
    (* string + int *)
    assert_equal
      (Types.String "10 + 5 = ")
      ( #+ )
      (Types.Numeric 15)
      (Types.String "10 + 5 = 15")
  ;;

  let%test _ =
    (* int + string *)
    assert_equal (Types.Numeric 10) ( #+ ) (Types.String " = 10") (Types.String "10 = 10")
  ;;

  let%test _ =
    (* bool + string *)
    assert_equal (Types.Bool true) ( #+ ) (Types.String " = 1") (Types.String "true = 1")
  ;;

  let%test _ =
    (* bool + string *)
    assert_equal
      (Types.String "0 = ")
      ( #+ )
      (Types.Bool false)
      (Types.String "0 = false")
  ;;

  let%test _ =
    (* bool + int *)
    assert_equal (Types.Bool false) ( #+ ) (Types.Numeric 10) (Types.Numeric 10)
  ;;

  let%test _ =
    (* int + bool *)
    assert_equal (Types.Numeric 10) ( #+ ) (Types.Bool false) (Types.Numeric 10)
  ;;

  let%test _ =
    (* bool + real *)
    assert_equal (Types.Bool true) ( #+ ) (Types.Real 1.) (Types.Real 2.)
  ;;

  let%test _ =
    (* real + bool *)
    assert_equal (Types.Real 1.) ( #+ ) (Types.Bool true) (Types.Real 2.)
  ;;

  (* - *)

  let%test _ =
    (* int - int *)
    assert_equal (Types.Numeric 15) ( #- ) (Types.Numeric 15) (Types.Numeric 0)
  ;;

  let%test _ =
    (* int - real *)
    assert_equal (Types.Numeric 15) ( #- ) (Types.Real 15.) (Types.Real 0.)
  ;;

  let%test _ =
    (* real - int *)
    assert_equal (Types.Real 15.) ( #- ) (Types.Numeric 15) (Types.Real 0.)
  ;;

  let%test _ =
    (* real - real *)
    assert_equal (Types.Real 15.) ( #- ) (Types.Real 15.) (Types.Real 0.)
  ;;

  let%test _ =
    (* string - string *)
    assert_raise (Types.String "10") ( #- ) (Types.String "9")
  ;;

  let%test _ =
    (* string - real *)
    assert_raise (Types.String "15") ( #- ) (Types.Real 15.)
  ;;

  let%test _ =
    (* real - string *)
    assert_raise (Types.Real 10.) ( #- ) (Types.String "1")
  ;;

  let%test _ =
    (* string - int *)
    assert_raise (Types.String "15") ( #- ) (Types.Numeric 15)
  ;;

  let%test _ =
    (* int - string *)
    assert_raise (Types.Numeric 10) ( #- ) (Types.String "3")
  ;;

  let%test _ =
    (* bool + string *)
    assert_raise (Types.Bool true) ( #- ) (Types.String "4")
  ;;

  let%test _ =
    (* bool - string *)
    assert_raise (Types.String "10") ( #- ) (Types.Bool false)
  ;;

  let%test _ =
    (* bool - int *)
    assert_equal (Types.Bool false) ( #- ) (Types.Numeric 10) (Types.Numeric (-10))
  ;;

  let%test _ =
    (* int - bool *)
    assert_equal (Types.Numeric 10) ( #- ) (Types.Bool true) (Types.Numeric 9)
  ;;

  let%test _ =
    (* bool - real *)
    assert_equal (Types.Bool true) ( #- ) (Types.Real 1.) (Types.Real 0.)
  ;;

  let%test _ =
    (* real + bool *)
    assert_equal (Types.Real 1.) ( #- ) (Types.Bool false) (Types.Real 1.)
  ;;

  (* * *)

  let%test _ =
    (* int * int *)
    assert_equal (Types.Numeric 2) ( #* ) (Types.Numeric 2) (Types.Numeric 4)
  ;;

  let%test _ =
    (* int * real *)
    assert_equal (Types.Numeric 2) ( #* ) (Types.Real 2.) (Types.Real 4.)
  ;;

  let%test _ =
    (* real * int *)
    assert_equal (Types.Real 2.) ( #* ) (Types.Numeric 2) (Types.Real 4.)
  ;;

  let%test _ =
    (* real * real *)
    assert_equal (Types.Real 2.) ( #* ) (Types.Real 2.) (Types.Real 4.)
  ;;

  let%test _ =
    (* string * string *)
    assert_raise (Types.String "1") ( #* ) (Types.String "2")
  ;;

  let%test _ =
    (* string * real *)
    assert_raise (Types.String "15") ( #* ) (Types.Real 15.)
  ;;

  let%test _ =
    (* real * string *)
    assert_raise (Types.Real 10.) ( #* ) (Types.String "a")
  ;;

  let%test _ =
    (* string * int *)
    assert_raise (Types.String "a") ( #* ) (Types.Numeric 15)
  ;;

  let%test _ =
    (* int * string *)
    assert_raise (Types.Numeric 10) ( #* ) (Types.String "a")
  ;;

  let%test _ =
    (* bool + string *)
    assert_raise (Types.Bool true) ( #* ) (Types.String "a")
  ;;

  let%test _ =
    (* bool * string *)
    assert_raise (Types.String "a") ( #* ) (Types.Bool false)
  ;;

  let%test _ =
    (* bool * int *)
    assert_equal (Types.Bool false) ( #* ) (Types.Numeric 10) (Types.Numeric 0)
  ;;

  let%test _ =
    (* int * bool *)
    assert_equal (Types.Numeric 10) ( #* ) (Types.Bool true) (Types.Numeric 10)
  ;;

  let%test _ =
    (* bool * real *)
    assert_equal (Types.Bool true) ( #* ) (Types.Real 1.) (Types.Real 1.)
  ;;

  let%test _ =
    (* real * bool *)
    assert_equal (Types.Real 1.) ( #* ) (Types.Bool false) (Types.Real 0.)
  ;;

  (* / *)

  let%test _ =
    (* int / int *)
    assert_equal (Types.Numeric 2) ( #/ ) (Types.Numeric 2) (Types.Numeric 1)
  ;;

  let%test _ =
    (* int / real *)
    assert_equal (Types.Numeric 2) ( #/ ) (Types.Real 2.) (Types.Real 1.)
  ;;

  let%test _ =
    (* real / int *)
    assert_equal (Types.Real 2.) ( #/ ) (Types.Numeric 2) (Types.Real 1.)
  ;;

  let%test _ =
    (* real / real *)
    assert_equal (Types.Real 2.) ( #/ ) (Types.Real 2.) (Types.Real 1.)
  ;;

  let%test _ =
    (* int / int=0 *)
    assert_raise (Types.Numeric 10) ( #/ ) (Types.Numeric 0)
  ;;

  let%test _ =
    (* int / real=0 *)
    assert_raise (Types.Numeric 10) ( #/ ) (Types.Real 0.)
  ;;

  let%test _ =
    (* real / int=0 *)
    assert_raise (Types.Real 10.) ( #/ ) (Types.Numeric 0)
  ;;

  let%test _ =
    (* real / real=0 *)
    assert_raise (Types.Real 10.) ( #/ ) (Types.Bool false)
  ;;

  let%test _ =
    (* string / string *)
    assert_raise (Types.String "1") ( #/ ) (Types.String "1")
  ;;

  let%test _ =
    (* string / real *)
    assert_raise (Types.String "a") ( #/ ) (Types.Real 15.)
  ;;

  let%test _ =
    (* real / string *)
    assert_raise (Types.Real 10.) ( #/ ) (Types.String "2")
  ;;

  let%test _ =
    (* string / int *)
    assert_raise (Types.String "15") ( #/ ) (Types.Numeric 15)
  ;;

  let%test _ =
    (* int / string *)
    assert_raise (Types.Numeric 10) ( #/ ) (Types.String "2")
  ;;

  let%test _ =
    (* bool / string *)
    assert_raise (Types.Bool true) ( #/ ) (Types.String "1")
  ;;

  let%test _ =
    (* bool / string *)
    assert_raise (Types.String "15") ( #/ ) (Types.Bool false)
  ;;

  let%test _ =
    (* bool / int *)
    assert_raise (Types.Bool false) ( #/ ) (Types.Numeric 10)
  ;;

  let%test _ =
    (* int / bool *)
    assert_raise (Types.Numeric 10) ( #/ ) (Types.Bool true)
  ;;

  let%test _ =
    (* bool / real *)
    assert_raise (Types.Bool true) ( #/ ) (Types.Real 1.)
  ;;

  let%test _ =
    (* real / bool *)
    assert_raise (Types.Real 1.) ( #/ ) (Types.Bool true)
  ;;

  (* % *)

  let%test _ =
    (* int % int *)
    assert_equal (Types.Numeric 10) ( #% ) (Types.Numeric 2) (Types.Numeric 0)
  ;;

  let%test _ =
    (* int % real *)
    assert_equal (Types.Numeric 10) ( #% ) (Types.Real 3.) (Types.Real 1.)
  ;;

  let%test _ =
    (* real % int *)
    assert_equal (Types.Real 10.) ( #% ) (Types.Numeric 2) (Types.Real 0.)
  ;;

  let%test _ =
    (* real % real *)
    assert_equal (Types.Real 10.) ( #% ) (Types.Real 2.) (Types.Real 0.)
  ;;

  let%test _ =
    (* int % int=0 *)
    assert_raise (Types.Numeric 10) ( #% ) (Types.Numeric 0)
  ;;

  let%test _ =
    (* int % real=0 *)
    assert_raise (Types.Numeric 10) ( #% ) (Types.Real 0.)
  ;;

  let%test _ =
    (* real % int=0 *)
    assert_raise (Types.Real 10.) ( #% ) (Types.Numeric 0)
  ;;

  let%test _ =
    (* real % real=0 *)
    assert_raise (Types.Real 10.) ( #% ) (Types.Bool false)
  ;;

  let%test _ =
    (* string % string *)
    assert_raise (Types.String "1") ( #% ) (Types.String "1")
  ;;

  let%test _ =
    (* string % real *)
    assert_raise (Types.String "a") ( #% ) (Types.Real 15.)
  ;;

  let%test _ =
    (* real % string *)
    assert_raise (Types.Real 10.) ( #% ) (Types.String "2")
  ;;

  let%test _ =
    (* string % int *)
    assert_raise (Types.String "15") ( #% ) (Types.Numeric 15)
  ;;

  let%test _ =
    (* int % string *)
    assert_raise (Types.Numeric 10) ( #% ) (Types.String "2")
  ;;

  let%test _ =
    (* bool % string *)
    assert_raise (Types.Bool true) ( #% ) (Types.String "1")
  ;;

  let%test _ =
    (* bool % string *)
    assert_raise (Types.String "15") ( #% ) (Types.Bool false)
  ;;

  let%test _ =
    (* bool % int *)
    assert_raise (Types.Bool false) ( #% ) (Types.Numeric 10)
  ;;

  let%test _ =
    (* int % bool *)
    assert_raise (Types.Numeric 10) ( #% ) (Types.Bool true)
  ;;

  let%test _ =
    (* bool % real *)
    assert_raise (Types.Bool true) ( #% ) (Types.Real 1.)
  ;;

  let%test _ =
    (* real % bool *)
    assert_raise (Types.Real 1.) ( #% ) (Types.Bool true)
  ;;

  (* = *)

  let%test _ =
    (* int = int *)
    assert_equal (Types.Numeric 15) ( #= ) (Types.Numeric 15) (Types.Bool true)
  ;;

  let%test _ =
    (* int = real *)
    assert_equal (Types.Numeric 15) ( #= ) (Types.Real 5.) (Types.Bool false)
  ;;

  let%test _ =
    (* real = int *)
    assert_equal (Types.Real 15.) ( #= ) (Types.Numeric 15) (Types.Bool true)
  ;;

  let%test _ =
    (* real = real *)
    assert_equal (Types.Real 15.) ( #= ) (Types.Real 15.) (Types.Bool true)
  ;;

  let%test _ =
    (* string = string *)
    assert_equal (Types.String "abc") ( #= ) (Types.String "abc") (Types.Bool true)
  ;;

  let%test _ =
    (* string = real *)
    assert_equal (Types.String "15.") ( #= ) (Types.Real 15.) (Types.Bool true)
  ;;

  let%test _ =
    (* real = string *)
    assert_equal (Types.Real 10.) ( #= ) (Types.String "11.") (Types.Bool false)
  ;;

  let%test _ =
    (* string = int *)
    assert_equal (Types.String "15") ( #= ) (Types.Numeric 15) (Types.Bool true)
  ;;

  let%test _ =
    (* int = string *)
    assert_equal (Types.Numeric 10) ( #= ) (Types.String "10") (Types.Bool true)
  ;;

  let%test _ =
    (* bool = string *)
    assert_equal (Types.Bool true) ( #= ) (Types.String "true") (Types.Bool true)
  ;;

  let%test _ =
    (* bool = string *)
    assert_equal (Types.String "false") ( #= ) (Types.Bool true) (Types.Bool false)
  ;;

  let%test _ =
    (* bool = int *)
    assert_equal (Types.Bool false) ( #= ) (Types.Numeric 1) (Types.Bool false)
  ;;

  let%test _ =
    (* int = bool *)
    assert_equal (Types.Numeric 1) ( #= ) (Types.Bool false) (Types.Bool false)
  ;;

  let%test _ =
    (* bool = real *)
    assert_equal (Types.Bool true) ( #= ) (Types.Real 1.) (Types.Bool true)
  ;;

  let%test _ =
    (* real = bool *)
    assert_equal (Types.Real 0.) ( #= ) (Types.Bool false) (Types.Bool true)
  ;;

  (* > *)

  let%test _ =
    (* int > int *)
    assert_equal (Types.Numeric 15) ( #> ) (Types.Numeric 10) (Types.Bool true)
  ;;

  let%test _ =
    (* int > real *)
    assert_equal (Types.Numeric 1) ( #> ) (Types.Real 5.) (Types.Bool false)
  ;;

  let%test _ =
    (* real > int *)
    assert_equal (Types.Real 15.) ( #> ) (Types.Numeric 10) (Types.Bool true)
  ;;

  let%test _ =
    (* real > real *)
    assert_equal (Types.Real 15.) ( #> ) (Types.Real 10.) (Types.Bool true)
  ;;

  let%test _ =
    (* string > string *)
    assert_equal (Types.String "A") ( #> ) (Types.String "b") (Types.Bool false)
  ;;

  let%test _ =
    (* string > real *)
    assert_equal (Types.String "9") ( #> ) (Types.Real 1.) (Types.Bool true)
  ;;

  let%test _ =
    (* real > string *)
    assert_equal (Types.Real 10.) ( #> ) (Types.String "99.") (Types.Bool false)
  ;;

  let%test _ =
    (* string > int *)
    assert_equal (Types.String "15") ( #> ) (Types.Numeric 13) (Types.Bool true)
  ;;

  let%test _ =
    (* int > string *)
    assert_equal (Types.Numeric 15) ( #> ) (Types.String "10") (Types.Bool true)
  ;;

  let%test _ =
    (* bool > string *)
    assert_equal (Types.Bool true) ( #> ) (Types.String "a") (Types.Bool true)
  ;;

  let%test _ =
    (* bool > string *)
    assert_equal (Types.String "false") ( #> ) (Types.Bool true) (Types.Bool false)
  ;;

  let%test _ =
    (* bool > int *)
    assert_equal (Types.Bool false) ( #> ) (Types.Numeric 1) (Types.Bool false)
  ;;

  let%test _ =
    (* int > bool *)
    assert_equal (Types.Numeric 1) ( #> ) (Types.Bool false) (Types.Bool true)
  ;;

  let%test _ =
    (* bool > real *)
    assert_equal (Types.Bool true) ( #> ) (Types.Real 1.) (Types.Bool false)
  ;;

  let%test _ =
    (* real > bool *)
    assert_equal (Types.Real 1.) ( #> ) (Types.Bool false) (Types.Bool true)
  ;;

  (* < *)

  let%test _ =
    (* int < int *)
    assert_equal (Types.Numeric 15) ( #< ) (Types.Numeric 10) (Types.Bool false)
  ;;

  let%test _ =
    (* int < real *)
    assert_equal (Types.Numeric 1) ( #< ) (Types.Real 5.) (Types.Bool true)
  ;;

  let%test _ =
    (* real < int *)
    assert_equal (Types.Real 15.) ( #< ) (Types.Numeric 10) (Types.Bool false)
  ;;

  let%test _ =
    (* real < real *)
    assert_equal (Types.Real 15.) ( #< ) (Types.Real 10.) (Types.Bool false)
  ;;

  let%test _ =
    (* string < string *)
    assert_equal (Types.String "A") ( #< ) (Types.String "b") (Types.Bool true)
  ;;

  let%test _ =
    (* string < real *)
    assert_equal (Types.String "9") ( #< ) (Types.Real 1.) (Types.Bool false)
  ;;

  let%test _ =
    (* real < string *)
    assert_equal (Types.Real 10.) ( #< ) (Types.String "99.") (Types.Bool true)
  ;;

  let%test _ =
    (* string < int *)
    assert_equal (Types.String "15") ( #< ) (Types.Numeric 13) (Types.Bool false)
  ;;

  let%test _ =
    (* int < string *)
    assert_equal (Types.Numeric 15) ( #< ) (Types.String "10") (Types.Bool false)
  ;;

  let%test _ =
    (* bool < string *)
    assert_equal (Types.Bool true) ( #< ) (Types.String "a") (Types.Bool false)
  ;;

  let%test _ =
    (* bool < string *)
    assert_equal (Types.String "false") ( #< ) (Types.Bool true) (Types.Bool true)
  ;;

  let%test _ =
    (* bool < int *)
    assert_equal (Types.Bool false) ( #< ) (Types.Numeric 1) (Types.Bool true)
  ;;

  let%test _ =
    (* int < bool *)
    assert_equal (Types.Numeric 1) ( #< ) (Types.Bool false) (Types.Bool false)
  ;;

  let%test _ =
    (* bool < real *)
    assert_equal (Types.Bool true) ( #< ) (Types.Real 5.) (Types.Bool true)
  ;;

  let%test _ =
    (* real < bool *)
    assert_equal (Types.Real 1.) ( #< ) (Types.Bool false) (Types.Bool false)
  ;;

  (* other *)
  let%test _ =
    assert_equal (Types.Numeric 10) ( #!= ) (Types.Numeric 20) (Types.Bool true)
  ;;

  let%test _ =
    assert_equal (Types.Numeric 30) ( #>= ) (Types.Numeric 20) (Types.Bool true)
  ;;

  let%test _ =
    assert_equal (Types.Numeric 30) ( #<= ) (Types.Numeric 30) (Types.Bool true)
  ;;

  let%test _ = assert_equal (Types.Bool true) ( #&& ) (Types.Bool true) (Types.Bool true)
  let%test _ = assert_equal (Types.Bool true) ( #|| ) (Types.Bool false) (Types.Bool true)
end
