(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Angstrom
open Ast
open Common

(* ======= Single patterns parsing ======= *)

let parse_pat_any = char '_' *> return Pat_any

let parse_pat_var = parse_value_name >>| fun name -> Pat_var name

let parse_pat_const = parse_const >>| fun const -> Pat_constant const

(* [Cons (hd, tl)] *)
let parse_pat_constr ppat_single =
  let* name = parse_constr_name in
  let* arg = option None (ppat_single <* ws >>| Option.some) in
  return (Pat_construct (Ident name, arg))

let parse_single_pat ppat =
  fix (fun ppat_single ->
      ws
      *> choice
           [ parse_pat_any
           ; parse_pat_var
           ; parse_pat_const
           ; parse_pat_constr ppat_single
           ; char '(' *> ppat <* ws <* char ')' ] )

(* ======= Operators parsing ======= *)

type pat_infix_op = OpOr | OpTuple | OpList

let peek_infix_op =
  let peek_tuple_or_ops =
    peek_char_fail
    >>= function
    | '|' ->
        return {op= OpOr; op_length= 1}
    | ',' ->
        return {op= OpTuple; op_length= 1}
    | _ ->
        fail "not a pattern infix operator"
  in
  let peek_list_op =
    peek_string 2
    >>= fun s ->
    if String.equal s "::" then return {op= OpList; op_length= 2}
    else fail "not a pattern list operator"
  in
  peek_tuple_or_ops <|> peek_list_op

let get_infix_binding_power = function
  | OpOr ->
      (1, 2)
  | OpTuple ->
      (5, 4)
  | OpList ->
      (11, 10)

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
    | OpList ->
        Pat_construct (Ident "::", Some (Pat_tuple [acc; rhs]))
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

let%expect_test "parse_pat_constr1" =
  pp pp_pattern parse_pattern "C" ;
  [%expect {| (Pat_construct ((Ident "C"), None)) |}]

let%expect_test "parse_pat_constr2" =
  pp pp_pattern parse_pattern "C a" ;
  [%expect {| (Pat_construct ((Ident "C"), (Some (Pat_var "a")))) |}]

let%expect_test "parse_pat_constr2" =
  pp pp_pattern parse_pattern "Cons (hd, tl)" ;
  [%expect
    {|
    (Pat_construct ((Ident "Cons"),
       (Some (Pat_tuple [(Pat_var "hd"); (Pat_var "tl")])))) |}]

let%expect_test "parse_pat_constr_or_tuple" =
  pp pp_pattern parse_pattern "C _ | a, b" ;
  [%expect
    {|
    (Pat_or ((Pat_construct ((Ident "C"), (Some Pat_any))),
       (Pat_tuple [(Pat_var "a"); (Pat_var "b")]))) |}]

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

let%expect_test "parse_pat_list" =
  pp pp_pattern parse_pattern "a::(b::c)::d" ;
  [%expect
    {|
    (Pat_construct ((Ident "::"),
       (Some (Pat_tuple
                [(Pat_var "a");
                  (Pat_construct ((Ident "::"),
                     (Some (Pat_tuple
                              [(Pat_construct ((Ident "::"),
                                  (Some (Pat_tuple [(Pat_var "b"); (Pat_var "c")]))
                                  ));
                                (Pat_var "d")]))
                     ))
                  ]))
       )) |}]

let%expect_test "parse_pat_list_or_tuple" =
  pp pp_pattern parse_pattern "a::b::c,d|e" ;
  [%expect
    {|
    (Pat_or (
       (Pat_tuple
          [(Pat_construct ((Ident "::"),
              (Some (Pat_tuple
                       [(Pat_var "a");
                         (Pat_construct ((Ident "::"),
                            (Some (Pat_tuple [(Pat_var "b"); (Pat_var "c")]))))
                         ]))
              ));
            (Pat_var "d")]),
       (Pat_var "e"))) |}]
