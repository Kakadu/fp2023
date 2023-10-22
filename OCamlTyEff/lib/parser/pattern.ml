open! Base
open Angstrom
open Ast
open Common

(* ======= Single patterns parsing ======= *)

let parse_pat_any = char '_' *> return Pat_any

let parse_pat_var = parse_value_name >>| fun name -> Pat_var name

let parse_pat_const = parse_const >>| fun const -> Pat_constant const

let parse_single_pat ppat =
  ws
  *> choice
       [ parse_pat_any
       ; parse_pat_var
       ; parse_pat_const
       ; char '(' *> ppat <* ws <* char ')' ]

(* ======= Operators parsing ======= *)

type pat_infix_op = OpOr | OpTuple

let peek_infix_op =
  peek_char_fail
  >>= function
  | '|' ->
      return {op= OpOr; op_length= 1}
  | ',' ->
      return {op= OpTuple; op_length= 1}
  | _ ->
      fail "not a pattern infix operator"

let get_infix_binding_power = function OpOr -> (1, 2) | OpTuple -> (5, 4)

let parse_pattern =
  let infix_fold_fun acc (op, rhs) =
    match op with
    | OpOr ->
        Pat_or (acc, rhs)
    | OpTuple -> (
      match rhs with
      | Pat_tuple tl ->
          Pat_tuple (acc :: tl)
      | _ ->
          Pat_tuple [acc; rhs] )
  in
  fix (fun ppat ->
      parse_infix_prefix ~parse_operand:(parse_single_pat ppat) ~peek_infix_op
        ~get_infix_binding_power ~infix_fold_fun
        ~parse_prefix_op:(fail "no prefix ops in patterns")
        ~get_prefix_binding_power:(fun _ -> assert false)
        ~apply_prefix_op:(fun _ -> assert false) )

(* ======= Tests ======= *)

let%expect_test "parse_pat_var" =
  pp pp_pattern parse_pattern "a" ;
  [%expect {| (Pat_var "a") |}]

let%expect_test "parse_pat_any" =
  pp pp_pattern parse_pattern "_" ;
  [%expect {| Pat_any |}]

let%expect_test "parse_pat_const" =
  pp pp_pattern parse_pattern "5" ;
  [%expect {| (Pat_constant (Const_integer 5)) |}]

let%expect_test "parse_pat_or" =
  pp pp_pattern parse_pattern "a | (b | c) | d" ;
  [%expect
    {|
    (Pat_or ((Pat_or ((Pat_var "a"), (Pat_or ((Pat_var "b"), (Pat_var "c"))))),
       (Pat_var "d"))) |}]

let%expect_test "parse_pat_tuple" =
  pp pp_pattern parse_pattern "a, (b, c), d" ;
  [%expect
    {|
    (Pat_tuple
       [(Pat_var "a"); (Pat_tuple [(Pat_var "b"); (Pat_var "c")]); (Pat_var "d")]) |}]

let%expect_test "parse_pat_or_tuple" =
  pp pp_pattern parse_pattern "a, b | c, d" ;
  [%expect
    {|
    (Pat_or ((Pat_tuple [(Pat_var "a"); (Pat_var "b")]),
       (Pat_tuple [(Pat_var "c"); (Pat_var "d")]))) |}]
