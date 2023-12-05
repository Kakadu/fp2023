(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base

(** Start parse func *)

let start_parsing parser string = parse_string ~consume:All parser string

(* Base *)

let is_char = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_bchar = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_keyword = function
  | "let"
  | "rec"
  | "fun"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false"
  | "match"
  | "with"
  | "in" -> true
  | _ -> false
;;

let is_whitespace = function
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false
;;

let is_underscore = function
  | c -> Char.equal c '_'
;;

(* S1mple parsers *)

let parse_white_space = take_while is_whitespace
let parse_white_space1 = take_while1 is_whitespace
let parse_token s = parse_white_space *> s
let parse_token1 s = parse_white_space1 *> s
let pstrtoken s = parse_white_space *> string s
let pstrtoken1 s = parse_white_space1 *> string s
let parens p = pstrtoken "(" *> p <* pstrtoken ")"

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

(* Const parsers *)

let parse_bool =
  parse_white_space
  *> ((fun _ -> CBool true)
      <$> string "true"
      <|> ((fun _ -> CBool true) <$> string "false"))
;;

let parse_int =
  let ps = parse_token (option "" (pstrtoken "-" <|> pstrtoken "+")) in
  let pd = take_while1 is_digit in
  lift2 (fun sign digit -> CInt (Int.of_string @@ sign ^ digit)) ps pd
;;

let parse_str =
  char '"' *> take_while (fun a -> a != '"') <* char '"' >>| fun a -> CString a
;;

let parse_const = choice [ parse_int; parse_bool; parse_str ]

(* Parse var *)

let check_var varname =
  if is_keyword varname
  then fail ("You can not use" ^ varname ^ "keywords as vars")
  else if Char.is_digit @@ String.get varname 0
  then fail "Identifier first sumbol is letter, not digit"
  else return varname
;;

let var cond =
  parse_white_space *> take_while1 cond
  >>= fun v -> if String.length v == 0 then fail "Not identifier" else check_var v
;;

let p_var =
  let is_entry = function
    | c -> is_char c || is_underscore c || is_digit c
  in
  var is_entry
;;

let p_var_case =
  let is_constr_entry = function
    | c -> is_bchar c || is_char c
  in
  var is_constr_entry
;;

(* Pattern parsers*)

let parse_wild = (fun _ -> Wild) <$> pstrtoken "_"
let parse_Const = (fun v -> Const v) <$> parse_const
let parse_var = (fun v -> Var v) <$> p_var

let parse_tuple parser =
  lift2
    (fun a b -> Tuple (a :: b))
    (parse_token @@ parser)
    (many1 (pstrtoken "," *> parser))
;;

let parse_list ps =
  (fun v -> List v) <$> (pstrtoken "[" *> sep_by1 (pstrtoken ";") ps <* pstrtoken "]")
;;

let parse_cases ps =
  lift2
    (fun id args -> Case (id, args))
    (parse_token p_var_case)
    (sep_by (pstrtoken "|") ps)
;;

type pdispatch =
  { value : pdispatch -> pattern t
  ; tuple : pdispatch -> pattern t
  ; list : pdispatch -> pattern t
  ; pattern : pdispatch -> pattern t
  ; case : pdispatch -> pattern t
  }

let pack =
  let pattern pack =
    choice [ pack.tuple pack; pack.case pack; pack.list pack; pack.value pack ]
  in
  let parse pack =
    choice [ pack.value pack; pack.list pack; parens @@ pack.tuple pack ]
  in
  let value pack =
    fix @@ fun _ -> parse_wild <|> parse_Const <|> parse_var <|> parens @@ pack.value pack
  in
  let tuple pack =
    fix @@ fun _ -> parse_tuple (parse pack <|> parens @@ pack.tuple pack)
  in
  let case pack = fix @@ fun _ -> parse_cases (parse pack <|> parens @@ pack.case pack) in
  let list pack = fix @@ fun _ -> parse_list (parse pack <|> parens @@ pack.list pack) in
  { value; tuple; list; pattern; case }
;;

let pat = pack.pattern pack

(* Parse expr *)

let p_op char_op op = pstrtoken char_op *> return (fun e1 e2 -> BinExpr (op, e1, e2))
let pmulti = p_op "*" Mul <|> p_op "/" Div <|> p_op "%" Mod
let padd = p_op "+" Add <|> p_op "-" Sub
let pcomp = p_op ">=" GEq <|> p_op ">" Gre <|> p_op "<=" LEq <|> p_op "<" Less
let peq = p_op "=" Eq <|> p_op "<>" NEq
let pconj = p_op "&&" And
let pdisj = p_op "||" Or
let constr_case pat expr = pat, expr
let constr_efun pl e = List.fold_right ~init:e ~f:(fun p e -> FunExpr (p, e)) pl
let parse_econst = (fun v -> ConstExpr v) <$> parse_const
let parse_evar = (fun v -> VarExpr v) <$> p_var

let parse_fun_args =
  fix @@ fun p -> many1 (pack.list pack <|> pack.value pack) <|> parens p
;;

let parse_list_expr ps =
  (fun v -> ListExpr v) <$> pstrtoken "[" *> sep_by1 (pstrtoken ";") ps <* pstrtoken "]"
;;

let parse_tuple_expr parser =
  lift2
    (fun a b -> TupleExpr (a :: b))
    (parser <* pstrtoken ",")
    (sep_by1 (pstrtoken ",") parser)
;;

let parse_cases_expr ps =
  lift2
    (fun id args -> CaseExpr (id, args))
    (parse_token p_var_case)
    (sep_by (pstrtoken "|") ps)
;;

let plet_body pargs pexpr =
  parse_token1 pargs
  >>= fun args -> pstrtoken "=" *> pexpr >>| fun expr -> constr_efun args expr
;;

let parse_fun_args =
  fix @@ fun p -> many1 (pack.list pack <|> pack.value pack) <|> parens p
;;

type edispatch =
  { list_e : edispatch -> expr t
  ; tuple_e : edispatch -> expr t
  ; fun_e : edispatch -> expr t
  ; let_e : edispatch -> expr t
  ; app_e : edispatch -> expr t
  ; if_e : edispatch -> expr t
  ; matching_e : edispatch -> expr t
  ; bin_e : edispatch -> expr t
  ; expr_parsers : edispatch -> expr t
  ; case_e : edispatch -> expr t
  }

let pack =
  let expr_parsers pack = pack.bin_e pack <|> pack.matching_e pack in
  let value_e = fix @@ fun _ -> parse_evar <|> parse_econst in
  let op_parsers pack =
    choice
      [ pack.fun_e pack
      ; pack.if_e pack
      ; pack.list_e pack
      ; pack.let_e pack
      ; pack.app_e pack
      ; value_e
      ; pack.case_e pack
      ; parens @@ choice [ pack.tuple_e pack; pack.bin_e pack ]
      ]
  in
  let app_args_parsers pack =
    choice
      [ pack.list_e pack
      ; parens
        @@ choice
             [ pack.expr_parsers pack
             ; pack.tuple_e pack
             ; pack.fun_e pack
             ; pack.app_e pack
             ]
      ; value_e
      ]
  in
  let bin_e pack =
    fix
    @@ fun _ ->
    let multi = chainl1 (op_parsers pack) pmulti in
    let add = chainl1 multi padd in
    let comp = chainl1 add pcomp in
    let eq = chainl1 comp peq in
    let conj = chainl1 eq pconj in
    chainl1 conj pdisj <* parse_white_space
  in
  let matching_e pack =
    fix
    @@ fun _ ->
    lift2
      (fun e cases -> MatchExpr (e, cases))
      (pstrtoken "match" *> op_parsers pack <* pstrtoken1 "with")
      (let case2 =
         lift2 constr_case (pstrtoken "|" *> pat <* pstrtoken "->") (op_parsers pack)
       in
       let case1 = lift2 constr_case (pat <* pstrtoken "->") (op_parsers pack) in
       let cases = lift2 (fun h tl -> h :: tl) (case1 <|> case2) (many case2) in
       cases)
  in
  let case_e pack =
    fix @@ fun _ -> parse_cases_expr (expr_parsers pack <|> parens @@ pack.case_e pack)
  in
  let if_e pack =
    fix
    @@ fun _ ->
    lift3
      (fun e1 e2 e3 -> IfExpr (e1, e2, e3))
      (pstrtoken "if" *> expr_parsers pack)
      (pstrtoken "then" *> expr_parsers pack)
      (pstrtoken "else" *> expr_parsers pack)
  in
  let list_e pack = fix @@ fun _ -> parse_list_expr (expr_parsers pack) in
  let tuple_e pack = fix @@ fun _ -> parse_tuple_expr (expr_parsers pack) in
  let let_with = pstrtoken "(|" *> sep_by (pstrtoken "|") p_var_case <* pstrtoken "|)" in
  let let_e pack =
    fix
    @@ fun _ ->
    lift3
      (fun a b c -> LetExpr (a, b, c))
      (pstrtoken "let"
       *> option false (parse_token (string "rec") <* parse_white_space1 >>| fun _ -> true)
      )
      (p_var >>| (fun a -> Name a) <|> (let_with >>| fun a -> ActivePaterns a))
      (plet_body parse_fun_args (expr_parsers pack))
  in
  let fun_e pack =
    fix
    @@ fun _ ->
    lift2
      constr_efun
      (pstrtoken "fun" *> parse_fun_args)
      (pstrtoken "->" *> expr_parsers pack)
  in
  let app_e pack =
    fix
    @@ fun _ ->
    lift2
      (fun f args -> List.fold_left ~init:f ~f:(fun f arg -> AppExpr (f, arg)) args)
      (value_e <|> parens @@ choice [ fun_e pack; pack.app_e pack ])
      (many1 (parse_token1 @@ app_args_parsers pack))
  in
  { list_e; tuple_e; fun_e; let_e; app_e; if_e; expr_parsers; matching_e; bin_e; case_e }
;;

let parse = pack.expr_parsers pack

(* Parser program *)

let pprogram = many1 (parse_token parse <* parse_token (many1 (pstrtoken ";;")))
let main_parse str = start_parsing pprogram (String.strip str)

(* TESTS  PARSER*)

let start_test parser show input =
  let res = start_parsing parser input in
  match res with
  | Ok res -> Format.printf "%s" (show res)
  | Error err -> Format.printf "%s" err
;;

(* Test const parser *)

let%expect_test _ =
  let test = "true" in
  start_test parse_const show_const test;
  [%expect {| (CBool true) |}]
;;

let%expect_test _ =
  let test = "\"itsastring\"" in
  start_test parse_const show_const test;
  [%expect {| (CString "itsastring") |}]
;;

let%expect_test _ =
  let test = "951753" in
  start_test parse_const show_const test;
  [%expect {| (CInt 951753) |}]
;;

let%expect_test _ =
  let test = "-951753" in
  start_test parse_const show_const test;
  [%expect {| (CInt -951753) |}]
;;

(* Test pattern parser *)

let%expect_test _ =
  let test = "varname1" in
  start_test parse_var show_pattern test;
  [%expect {| (Var "varname1") |}]
;;

let%expect_test _ =
  let test = "1varname" in
  start_test parse_var show_pattern test;
  [%expect {| : Identifier first sumbol is letter, not digit |}]
;;

let%expect_test _ =
  let test = "1,(2),3,[1;2;3]" in
  start_test pat show_pattern test;
  [%expect
    {|
    (Tuple
       [(Const (CInt 1)); (Const (CInt 2)); (Const (CInt 3));
         (List [(Const (CInt 1)); (Const (CInt 2)); (Const (CInt 3))])]) |}]
;;

let%expect_test _ =
  let test = "a,b,c" in
  start_test pat show_pattern test;
  [%expect {| (Tuple [(Var "a"); (Var "b"); (Var "c")]) |}]
;;

let%expect_test _ =
  let test = "[((1));2;3]" in
  start_test pat show_pattern test;
  [%expect {| (List [(Const (CInt 1)); (Const (CInt 2)); (Const (CInt 3))]) |}]
;;

(* Test expr *)

let%expect_test _ =
  let test = "[1;2; 3]" in
  start_test parse show_expr test;
  [%expect
    {| (ListExpr [(ConstExpr (CInt 1)); (ConstExpr (CInt 2)); (ConstExpr (CInt 3))]) |}]
;;

let%expect_test _ =
  let test = "[1;2; 3]" in
  start_test parse show_expr test;
  [%expect
    {| (ListExpr [(ConstExpr (CInt 1)); (ConstExpr (CInt 2)); (ConstExpr (CInt 3))]) |}]
;;

let%expect_test _ =
  let test = "(1, [2;((3));4], 5)" in
  start_test parse show_expr test;
  [%expect
    {|
      (TupleExpr
         [(ConstExpr (CInt 1));
           (ListExpr
              [(ConstExpr (CInt 2)); (ConstExpr (CInt 3)); (ConstExpr (CInt 4))]);
           (ConstExpr (CInt 5))]) |}]
;;

let%expect_test _ =
  let test = " (1  +  2)+ (1 *  3 /  3) <= v  " in
  start_test parse show_expr test;
  [%expect
    {|
    (BinExpr (LEq,
       (BinExpr (Add,
          (BinExpr (Add, (ConstExpr (CInt 1)), (ConstExpr (CInt 2)))),
          (BinExpr (Div,
             (BinExpr (Mul, (ConstExpr (CInt 1)), (ConstExpr (CInt 3)))),
             (ConstExpr (CInt 3))))
          )),
       (VarExpr "v"))) |}]
;;

let%expect_test _ =
  let test = "let f x = x+x" in
  start_test parse show_expr test;
  [%expect
    {|
    (LetExpr (false, (Name "f"),
       (FunExpr ((Var "x"), (BinExpr (Add, (VarExpr "x"), (VarExpr "x"))))))) |}]
;;

let%expect_test _ =
  let test = "let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd " in
  start_test parse show_expr test;
  [%expect
    {|
      (LetExpr (false, (ActivePaterns ["Even"; "Odd"]),
         (FunExpr ((Var "input"),
            (IfExpr (
               (BinExpr (Eq,
                  (BinExpr (Mod, (VarExpr "input"), (ConstExpr (CInt 2)))),
                  (ConstExpr (CInt 0)))),
               (CaseExpr ("Even", [])), (CaseExpr ("Odd", []))))
            ))
         )) |}]
;;

let%expect_test _ =
  let test = "let recognize input = a  " in
  start_test parse show_expr test;
  [%expect
    {|
    (LetExpr (false, (Name "recognize"), (FunExpr ((Var "input"), (VarExpr "a")))
       )) |}]
;;

let%expect_test _ =
  let test = "let rec f x = f * x" in
  start_test parse show_expr test;
  [%expect
    {|
    (LetExpr (true, (Name "f"),
       (FunExpr ((Var "x"), (BinExpr (Mul, (VarExpr "f"), (VarExpr "x"))))))) |}]
;;

let%expect_test _ =
  let test = "let rec fact n = if n = 1 then 1 else n * (fact (n - 1))" in
  start_test parse show_expr test;
  [%expect
    {|
    (LetExpr (true, (Name "fact"),
       (FunExpr ((Var "n"),
          (IfExpr ((BinExpr (Eq, (VarExpr "n"), (ConstExpr (CInt 1)))),
             (ConstExpr (CInt 1)),
             (BinExpr (Mul, (VarExpr "n"),
                (AppExpr ((VarExpr "fact"),
                   (BinExpr (Sub, (VarExpr "n"), (ConstExpr (CInt 1))))))
                ))
             ))
          ))
       )) |}]
;;

let%expect_test _ =
  let test = "g x 1 [1;2] y z" in
  start_test parse show_expr test;
  [%expect
    {|
      (AppExpr (
         (AppExpr (
            (AppExpr (
               (AppExpr ((AppExpr ((VarExpr "g"), (VarExpr "x"))),
                  (ConstExpr (CInt 1)))),
               (ListExpr [(ConstExpr (CInt 1)); (ConstExpr (CInt 2))]))),
            (VarExpr "y"))),
         (VarExpr "z"))) |}]
;;

let%expect_test _ =
  let test = "match 2 with | 1 -> 2 | _ -> 5" in
  start_test parse show_expr test;
  [%expect
    {|
    (MatchExpr ((ConstExpr (CInt 2)),
       [((Const (CInt 1)), (ConstExpr (CInt 2))); (Wild, (ConstExpr (CInt 5)))])) |}]
;;

let%expect_test _ =
  let test = "match x with Some x -> x | None  -> 0" in
  start_test parse show_expr test;
  [%expect
    {|
    (MatchExpr ((VarExpr "x"),
       [((Case ("Some", [(Var "x")])), (VarExpr "x"));
         ((Case ("None", [])), (ConstExpr (CInt 0)))]
       )) |}]
;;

let%expect_test _ =
  let test = "match x with Some x -> true | None  -> false" in
  start_test parse show_expr test;
  [%expect
    {|
      (MatchExpr ((VarExpr "x"),
         [((Case ("Some", [(Var "x")])), (ConstExpr (CBool true)));
           ((Case ("None", [])), (ConstExpr (CBool true)))]
         )) |}]
;;
