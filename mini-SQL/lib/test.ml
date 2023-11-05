open Parser
open Ast

(* Запускает парсер на строке *)
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
  | Error x -> Format.printf "Parsing error: %s!!!\n" x; false
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

let%test _ = assert_equal value "\'1a2b3c 7\'" (String "1a2b3c 7")
let%test _ = assert_equal value "\"1a2b3c 7\"" (String "1a2b3c 7")
let%test _ = assert_equal value "User" (Name "User")
let%test _ = assert_equal value "True" (Bool true)
let%test _ = assert_equal value "false" (Bool false)
let%test _ = assert_equal value "10" (Digit 10)
let%test _ = assert_equal value "+10" (Digit 10)
let%test _ = assert_equal value "-10" (Digit (-10))
(**)
let%test _ = assert_raise value "-12a3"
let%test _ = assert_raise value "1name" 

(*Expr parser*)
let%test _ = assert_equal expr_parser "NOT 1" (Unary_operation(Not, Const (Digit 1)))
let%test _ = assert_equal expr_parser "2+2" (Binary_operation (Add, Const (Digit 2), Const (Digit 2))) 
let%test _ = assert_eq_output show_expr expr_parser "2 + 2" (Binary_operation (Add, Const (Digit 2), Const (Digit 2)))
let%test _ = assert_equal expr_parser "2 / -2" (Binary_operation (Divide, Const (Digit 2), Const (Digit (-2))))
let%test _ = assert_equal expr_parser "-2 - -2" (Binary_operation (Substract, Const (Digit (-2)), Const (Digit (-2))))
let%test _ = assert_equal expr_parser "-2 * +2" (Binary_operation (Multiply, Const (Digit (-2)), Const (Digit 2)))
let%test _ = assert_equal expr_parser "123 % 10" (Binary_operation (Modulo, Const (Digit 123), Const (Digit 10)))

let%test _ = assert_raise expr_parser "-2 x 2"

let%test _ = assert_eq_output show_expr expr_parser "4=4" (Binary_operation (Equal, Const (Digit 4), Const (Digit 4)))
let%test _ = assert_equal expr_parser "10 >4" (Binary_operation (GreaterThan, Const (Digit 10), Const (Digit 4)))
let%test _ = assert_equal expr_parser "2 < 4" (Binary_operation (LessThan, Const (Digit 2), Const (Digit 4)))
let%test _ = assert_equal expr_parser "2 != 4" (Binary_operation (NotEqual, Const (Digit 2), Const (Digit 4)))
let%test _ = assert_equal expr_parser "5<> 4" (Binary_operation (NotEqual, Const (Digit 5), Const (Digit 4)))
let%test _ = assert_equal expr_parser "4 <= 4" (Binary_operation (LessThanOrEqual, Const (Digit 4), Const (Digit 4)))
let%test _ = assert_equal expr_parser "7>=4" (Binary_operation (GreaterThanOrEqual, Const (Digit 7), Const (Digit 4)))
(**)
let%test _ = assert_raise expr_parser "2 + 2 eq 4"
let%test _ = assert_equal expr_parser "ID>= 0" (Binary_operation (GreaterThanOrEqual, Const (Name "ID"), Const (Digit 0)))

(* Logic expr parser *)

let%test _ = assert_equal expr_parser "True AND age" (Binary_operation (And, Const (Bool true), Const (Name "age")))
let%test _ = assert_equal expr_parser "False OR True" (Binary_operation (Or, Const (Bool false), Const (Bool true)))
let%test _ = assert_equal expr_parser "NOT true" (Unary_operation (Not, Const (Bool true)))

(* SELECT exprs pars*)

let%test _ = assert_equal select_expr_parser "*" [All_Columns]
let%test _ = assert_equal select_expr_parser "*, ID" [All_Columns; Expr (Const ( Name "ID"))]

(* SELECT - FROM - WHERE*)

let%test _ = assert_equal statement_parser "SELECT ID, name FROM User" (Select {exprs = [Expr (Const ( Name "ID")); Expr (Const ( Name "name"))]; table = "User"; condition = None})
let%test _ = assert_equal statement_parser "SELECT * FROM User WHERE age >25" (Select {exprs = [All_Columns]; table = "User"; condition = Some (Binary_operation (GreaterThan, Const (Name "age"), Const (Digit 25)))})