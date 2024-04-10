(** Copyright 2023-2024, Alexandr Lekomtsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open! Base
open Angstrom
open Ast
open Common
open Expr

(** ======= Function declaration, assign, if, while parsers ======= *)

let parse_stat_function pblock =
  let parse_idents =
    string "(" *> sep_by (ws *> string "," *> ws) parse_ident <* string ")"
  in
  lift4
    (fun local fun_name idents bl ->
      Stat_assign (local, fun_name, Exp_function (idents, bl)))
    (option Nonlocal (string "local" *> return Local))
    (ws *> string "function" *> ws *> parse_ident)
    (ws *> parse_idents)
    (pblock <* string "end")
;;

let parse_assign pblock =
  (let parse_flag = option Nonlocal (string "local" *> return Local) in
   lift3
     (fun flag lhs expr -> Stat_assign (flag, lhs, expr))
     parse_flag
     (ws *> parse_ident)
     (ws *> char '=' *> ws *> parse_expr pblock))
  <|> parse_stat_function pblock
;;

let parse_if pblock =
  let parse_case_first =
    both (string "if" *> parse_expr pblock) (ws *> string "then" *> pblock)
  in
  let parse_case_rest =
    many
      (both (ws *> string "elseif" *> parse_expr pblock) (ws *> string "then" *> pblock))
  in
  let parse_else_block =
    option None (ws *> string "else" >>| Option.some)
    >>= function
    | None -> return None
    | Some _ -> pblock >>| Option.some
  in
  lift3
    (fun case_first case_rest else_block -> Stat_if (case_first :: case_rest, else_block))
    parse_case_first
    parse_case_rest
    parse_else_block
  <* ws
  <* string "end"
;;

let parse_while pblock =
  let parse_while1 = string "while" *> parse_expr pblock in
  let parse_pblock = ws *> string "do" *> pblock <* ws <* string "end" in
  lift2 (fun exp block -> Stat_while (exp, block)) parse_while1 parse_pblock
;;

(** Parse single function call, example: print(10) *)
let parse_st_call pexpr = parse_apply pexpr >>| fun e1 -> Stat_call e1

let parse_do pblock =
  string "do" *> pblock <* ws <* string "end" >>| fun block -> Stat_do block
;;

let parse_return pblock =
  string "return" *> sep_by (string ",") (parse_expr pblock)
  >>| fun exprs -> Stat_return exprs
;;

let parse_break = string "break" *> return Stat_break

let parse_stat pblock =
  ws
  *> choice
       [ parse_assign pblock
       ; parse_if pblock
       ; parse_do pblock
       ; parse_while pblock
       ; parse_return pblock
       ; parse_st_call (parse_expr pblock)
       ; parse_break
       ]
;;

let parse_block : block t = fix (fun pblock -> many (parse_stat pblock) <* ws)

(* ======= Tests ======= *)

let%expect_test "parse_do" =
  pp pp_block parse_block "do a = 1 b = 2 if true then c = 3 end end";
  [%expect
    {|
    [(Stat_do
        [(Stat_assign (Nonlocal, "a", (Exp_number 1.)));
          (Stat_assign (Nonlocal, "b", (Exp_number 2.)));
          (Stat_if (
             [(Exp_true, [(Stat_assign (Nonlocal, "c", (Exp_number 3.)))])], None
             ))
          ])
      ] |}]
;;

let%expect_test "parse_assign" =
  pp pp_block parse_block "x = 42 < 15";
  [%expect
    {|
    [(Stat_assign (Nonlocal, "x",
        (Exp_op (Op_lt, (Exp_number 42.), (Exp_number 15.)))))
      ] |}]
;;

let%expect_test "parse_decl_as_assign" =
  pp pp_block parse_block "local function abc (x) return x end";
  [%expect
    {|
    [(Stat_assign (Local, "abc",
        (Exp_function (["x"], [(Stat_return [(Exp_lhs "x")])]))))
      ] |}]
;;

let%expect_test "parse_fact" =
  pp
    pp_block
    parse_block
    {| function fact(n)
           if n == 0 then return 1 else return n * fact(n - 1) end
       end |};
  [%expect
    {|
    [(Stat_assign (Nonlocal, "fact",
        (Exp_function (["n"],
           [(Stat_if (
               [((Exp_op (Op_eq, (Exp_lhs "n"), (Exp_number 0.))),
                 [(Stat_return [(Exp_number 1.)])])],
               (Some [(Stat_return
                         [(Exp_op (Op_mul, (Exp_lhs "n"),
                             (Exp_call
                                (Call ((Exp_lhs "fact"),
                                   [(Exp_op (Op_sub, (Exp_lhs "n"),
                                       (Exp_number 1.)))
                                     ]
                                   )))
                             ))
                           ])
                       ])
               ))
             ]
           ))
        ))
      ] |}]
;;

let%expect_test "parse_while" =
  pp pp_block parse_block "while 9 do x = 1 b = 2 end";
  [%expect
    {|
    [(Stat_while ((Exp_number 9.),
        [(Stat_assign (Nonlocal, "x", (Exp_number 1.)));
          (Stat_assign (Nonlocal, "b", (Exp_number 2.)))]
        ))
      ] |}]
;;

let%expect_test "parse_while1" =
  pp pp_block parse_block "x = 15 while x > 10 do x = x - 4 end";
  [%expect
    {|
    [(Stat_assign (Nonlocal, "x", (Exp_number 15.)));
      (Stat_while ((Exp_op (Op_lt, (Exp_number 10.), (Exp_lhs "x"))),
         [(Stat_assign (Nonlocal, "x",
             (Exp_op (Op_sub, (Exp_lhs "x"), (Exp_number 4.)))))
           ]
         ))
      ] |}]
;;

let%expect_test "parse_if" =
  pp pp_block parse_block "if true then a = 1 b = 2 end";
  [%expect
    {|
    [(Stat_if (
        [(Exp_true,
          [(Stat_assign (Nonlocal, "a", (Exp_number 1.)));
            (Stat_assign (Nonlocal, "b", (Exp_number 2.)))])
          ],
        None))
      ] |}]
;;

let%expect_test "parse_if1" =
  pp pp_block parse_block "x = 100 if x == 100 then x = 10 end";
  [%expect
    {|
    [(Stat_assign (Nonlocal, "x", (Exp_number 100.)));
      (Stat_if (
         [((Exp_op (Op_eq, (Exp_lhs "x"), (Exp_number 100.))),
           [(Stat_assign (Nonlocal, "x", (Exp_number 10.)))])],
         None))
      ] |}]
;;

let%expect_test "parse_if2" =
  pp
    pp_block
    parse_block
    {| if false then a = 1 
           elseif true then b = 2 c = 3
           elseif 42 then d = 4
           else e = 5 end |};
  [%expect
    {|
    [(Stat_if (
        [(Exp_false, [(Stat_assign (Nonlocal, "a", (Exp_number 1.)))]);
          (Exp_true,
           [(Stat_assign (Nonlocal, "b", (Exp_number 2.)));
             (Stat_assign (Nonlocal, "c", (Exp_number 3.)))]);
          ((Exp_number 42.), [(Stat_assign (Nonlocal, "d", (Exp_number 4.)))])],
        (Some [(Stat_assign (Nonlocal, "e", (Exp_number 5.)))])))
      ] |}]
;;

let%expect_test "parse_if3" =
  pp pp_block parse_block "if nil then a = 1 else a = 2 end";
  [%expect
    {|
    [(Stat_if ([(Exp_nil, [(Stat_assign (Nonlocal, "a", (Exp_number 1.)))])],
        (Some [(Stat_assign (Nonlocal, "a", (Exp_number 2.)))])))
      ] |}]
;;

let%expect_test "parse_if4" =
  pp pp_block parse_block "if a >= 100 then a = 1 elseif true then a = 2 end";
  [%expect
    {|
    [(Stat_if (
        [((Exp_op (Op_le, (Exp_number 100.), (Exp_lhs "a"))),
          [(Stat_assign (Nonlocal, "a", (Exp_number 1.)))]);
          (Exp_true, [(Stat_assign (Nonlocal, "a", (Exp_number 2.)))])],
        None))
      ] |}]
;;

let%expect_test "parse_return" =
  pp pp_block parse_block "return 10, true, a";
  [%expect {| [(Stat_return [(Exp_number 10.); Exp_true; (Exp_lhs "a")])] |}]
;;

let%expect_test "parse_statcall" =
  pp pp_block parse_block "fact(1)";
  [%expect {| [(Stat_call (Call ((Exp_lhs "fact"), [(Exp_number 1.)])))] |}]
;;

let%expect_test "parse_statcall2" =
  pp pp_expression (parse_expr parse_block) "fact(n-1)";
  [%expect
    {|
    (Exp_call
       (Call ((Exp_lhs "fact"),
          [(Exp_op (Op_sub, (Exp_lhs "n"), (Exp_number 1.)))]))) |}]
;;

let%expect_test "parse_expfun" =
  pp pp_expression (parse_function parse_block) "function (x,y,z,v) return x    end";
  [%expect {| (Exp_function (["x"; "y"; "z"; "v"], [(Stat_return [(Exp_lhs "x")])])) |}]
;;

let%expect_test "parse_arith" =
  pp pp_expression (parse_expr parse_block) "1 - nil * 3 + false / 6";
  [%expect
    {|
    (Exp_op (Op_add,
       (Exp_op (Op_sub, (Exp_number 1.),
          (Exp_op (Op_mul, Exp_nil, (Exp_number 3.))))),
       (Exp_op (Op_div, Exp_false, (Exp_number 6.))))) |}]
;;

let%expect_test "parse_arith2" =
  pp pp_expression (parse_expr parse_block) "abc <= cba";
  [%expect {| (Exp_op (Op_le, (Exp_lhs "abc"), (Exp_lhs "cba"))) |}]
;;

let%expect_test "parse_arith3" =
  pp pp_expression (parse_expr parse_block) "abc % 10 ~= 3";
  [%expect
    {|
    (Exp_op (Op_neq, (Exp_op (Op_mod, (Exp_lhs "abc"), (Exp_number 10.))),
       (Exp_number 3.))) |}]
;;

let%expect_test "parse_arith4" =
  pp pp_expression (parse_expr parse_block) "a and b or c";
  [%expect
    {|
    (Exp_op (Op_or, (Exp_op (Op_and, (Exp_lhs "a"), (Exp_lhs "b"))),
       (Exp_lhs "c"))) |}]
;;

let%expect_test "parse_expcall" =
  pp pp_expression (parse_expr parse_block) "n * fact(n-1)";
  [%expect
    {|
    (Exp_op (Op_mul, (Exp_lhs "n"),
       (Exp_call
          (Call ((Exp_lhs "fact"),
             [(Exp_op (Op_sub, (Exp_lhs "n"), (Exp_number 1.)))])))
       )) |}]
;;

let%expect_test "parse_expstr" =
  pp pp_expression (parse_expr parse_block) {|"hello world"|};
  [%expect {| (Exp_string "hello world") |}]
;;
