(** Copyright 2021-2023, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base
module Format = Caml.Format

(* start parse func *)

let start_parsing parser string = parse_string ~consume:All parser string

(* base *)
let is_lchar = function
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
  | "function"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false"
  | "match"
  | "with" -> true
  | _ -> false
;;

let is_whitespace = function
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false
;;

let is_var_symbol = function
  | c -> is_lchar c || is_bchar c || is_digit c || Char.equal c '_'
;;

let is_underscore = function
  | c -> Char.equal c '_'
;;

(* S1mple parsers *)

let parse_white_space = take_while is_whitespace
let parse_white_space1 = take_while1 is_whitespace
let parse_token s = parse_white_space *> s
let parse_token1 s = parse_white_space1 *> s
let parse_trim s = parse_white_space *> s <* parse_white_space
let pstrtoken s = parse_white_space *> string s
let pstrtoken1 s = parse_white_space1 *> string s
let parens p = pstrtoken "(" *> p <* pstrtoken ")"
let verticalbar p = pstrtoken "|" *> p
let parens_or_not p = p <|> parens p

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

(* Const parsers *)

let parse_bool =
  string "true" >>| (fun _ -> CBool true) <|> (string "false" >>| fun _ -> CBool false)
;;

let parse_pos_int = take_while1 is_digit >>| fun b -> CInt (int_of_string b)
let parse_neg_int = char '-' *> take_while1 is_digit >>| fun b -> CInt (-int_of_string b)
let parse_int = parse_pos_int <|> parse_neg_int

let parse_str =
  char '"' *> take_while (fun a -> a != '"') <* char '"' >>| fun a -> CString a
;;

let parse_const = parse_white_space *> choice [ parse_int; parse_bool; parse_str ]

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
    | c -> is_lchar c || is_underscore c || is_digit c
  in
  var is_entry
;;

(* Pattern parsers*)

let parse_wild = (fun _ -> Wild) <$> pstrtoken "_"
let parse_Const = (fun v -> Const v) <$> parse_const
let parse_var = (fun v -> Var v) <$> p_var

let parse_tuple parser =
  lift2
    (fun a b -> Tuple (a :: b))
    (parser <* pstrtoken ",")
    (sep_by1 (pstrtoken ",") parser)
;;

let parse_list ps =
  (fun v -> List v) <$> (pstrtoken "[" *> sep_by1 (pstrtoken ";") ps <* pstrtoken "]")
;;

type pdispatch =
  { value : pdispatch -> pattern t
  ; tuple : pdispatch -> pattern t
  ; list : pdispatch -> pattern t
  ; pattern : pdispatch -> pattern t
  }

let pack =
  let pattern pack = choice [ pack.tuple pack; pack.list pack; pack.value pack ] in
  let parse pack =
    choice [ pack.value pack; pack.list pack; parens @@ pack.tuple pack ]
  in
  let value pack =
    fix
    @@ fun _ ->
    parse_white_space
    *> (parse_wild <|> parse_Const <|> parse_var <|> parens @@ pack.value pack)
  in
  let tuple pack =
    fix @@ fun _ -> parse_tuple (parse pack <|> parens @@ pack.tuple pack)
  in
  let list pack = fix @@ fun _ -> parse_list (parse pack <|> parens @@ pack.list pack) in
  { value; tuple; list; pattern }
;;

let pat = pack.pattern pack
let pargs = choice [ pack.list pack; pack.value pack ]

(* Parse expr *)

let pop ch op = pstrtoken ch *> return (fun e1 e2 -> BinExpr (op, e1, e2))
let pmulti = pop "*" Mul <|> pop "/" Div <|> pop "%" Mod
let padd = pop "+" Add <|> pop "-" Sub
let pcomp = pop ">=" GEq <|> pop ">" Gre <|> pop "<=" LEq <|> pop "<" Less
let peq = pop "=" Eq <|> pop "<>" NEq
let pconj = pop "&&" And
let pdisj = pop "||" Or
let constr_econst e = ConstExpr e
let constr_ebinop op e1 e2 = BinExpr (op, e1, e2)
let constr_evar id = VarExpr id
let constr_elist l = ListExpr l
let constr_etuple t = TupleExpr t
let constr_eif e1 e2 e3 = IfExpr (e1, e2, e3)
let constr_efun pl e = List.fold_right ~init:e ~f:(fun p e -> FunExpr (p, e)) pl
let constr_elet r f = LetExpr (r, f)
let constr_ereclet r f = LetRecExpr (r, f)
let constr_eapp f args = List.fold_left ~init:f ~f:(fun f arg -> AppExpr (f, arg)) args
let parse_econst = (fun v -> ConstExpr v) <$> parse_const
let parse_evar = (fun v -> VarExpr v) <$> p_var
let pfun_args = fix @@ fun p -> many (pack.list pack <|> pack.value pack) <|> parens p
let pfun_args1 = fix @@ fun p -> many1 (pack.list pack <|> pack.value pack) <|> parens p

let parse_list_expr ps =
  (fun v -> ListExpr v) <$> (pstrtoken "[" *> sep_by1 (pstrtoken ";") ps <* pstrtoken "]")
;;

let parse_tuple_expr ps =
  (fun v -> TupleExpr v)
  <$> (pstrtoken "(" *> sep_by1 (pstrtoken ",") ps
       <* pstrtoken ")"
       <|> sep_by1 (pstrtoken ",") ps)
;;

let plet_body pargs pexpr =
  parse_token1 pargs
  >>= fun args -> pstrtoken "=" *> pexpr >>| fun expr -> constr_efun args expr
;;

type edispatch =
  { value_e : edispatch -> expr t
  ; const_e : edispatch -> expr t
  ; var_e : edispatch -> expr t
  ; list_e : edispatch -> expr t
  ; tuple_e : edispatch -> expr t
  ; op_e : edispatch -> expr t
  ; fun_e : edispatch -> expr t
  ; let_e : edispatch -> expr t
  ; app_e : edispatch -> expr t
  ; if_e : edispatch -> expr t
  ; expr : edispatch -> expr t
  }

let parens_only ps = parens @@ choice ps

let pack =
  let expr_parsers pack =
    choice
      [ pack.op_e pack
      ; pack.tuple_e pack
      ; pack.list_e pack
      ; pack.app_e pack
      ; pack.fun_e pack
      ; pack.let_e pack
      ; pack.if_e pack
      ]
  in
  let expr pack = expr_parsers pack in
  let const_e pack = fix @@ fun _ -> parse_econst <|> parens @@ pack.const_e pack in
  let var_e pack = fix @@ fun _ -> parse_evar <|> parens @@ pack.var_e pack in
  let value_e pack = fix @@ fun _ -> pack.const_e pack <|> pack.var_e pack in
  let op_parsers pack =
    choice
      [ pack.list_e pack
      ; pack.app_e pack
      ; parens_only [ pack.tuple_e pack; pack.op_e pack ]
      ; pack.var_e pack
      ; pack.const_e pack
      ]
  in
  let parse_if_state pack =
    pack.op_e pack <|> pack.let_e pack <|> pack.app_e pack <|> pack.if_e pack
  in
  let list_parsers pack = pack.value_e pack <|> pack.list_e pack <|> pack.tuple_e pack in
  let tuple_parsers pack =
    pack.value_e pack <|> pack.list_e pack <|> parens @@ pack.tuple_e pack
  in
  let app_args_parsers pack =
    choice
      [ pack.list_e pack
      ; parens_only
          [ pack.op_e pack; pack.tuple_e pack; pack.fun_e pack; pack.app_e pack ]
      ; pack.var_e pack
      ; pack.const_e pack
      ]
  in
  let op_e pack =
    fix
    @@ fun _ ->
    let multi = chainl1 (op_parsers pack) pmulti in
    let add = chainl1 multi padd in
    let comp = chainl1 add pcomp in
    let eq = chainl1 comp peq in
    let conj = chainl1 eq pconj in
    chainl1 conj pdisj <|> parens (pack.op_e pack)
  in
  let if_e pack =
    fix
    @@ fun _ ->
    lift3
      constr_eif
      (pstrtoken "if" *> parse_if_state pack)
      (pstrtoken1 "then" *> expr pack)
      (pstrtoken1 "else" *> expr pack)
    <|> parens @@ pack.if_e pack
  in
  let list_e pack =
    fix @@ fun _ -> parse_list_expr (list_parsers pack <|> parens @@ pack.list_e pack)
  in
  let tuple_e pack =
    fix @@ fun _ -> parse_tuple_expr (tuple_parsers pack <|> parens @@ pack.tuple_e pack)
  in
  let let_e_without_rec pack =
    fix
    @@ fun _ ->
    lift2 constr_elet (pstrtoken "let" *> p_var) (plet_body pfun_args (expr pack))
  in
  let let_e_with_rec pack =
    fix
    @@ fun _ ->
    lift2 constr_ereclet (pstrtoken "let rec" *> p_var) (plet_body pfun_args (expr pack))
  in
  let fun_e pack =
    fix
    @@ fun _ ->
    lift2 constr_efun (pstrtoken "fun" *> pfun_args1) (pstrtoken "->" *> expr pack)
    <|> parens @@ pack.fun_e pack
  in
  let app_e pack =
    fix
    @@ fun _ ->
    lift2
      constr_eapp
      (pack.var_e pack <|> parens_only [ pack.fun_e pack; pack.app_e pack ])
      (many1 (parse_token1 @@ app_args_parsers pack))
    <|> parens @@ pack.app_e pack
  in
  let let_e pack =
    fix
    @@ fun _ ->
    let_e_without_rec pack <|> let_e_with_rec pack <|> parens @@ pack.let_e pack
  in
  { var_e; const_e; op_e; list_e; tuple_e; fun_e; let_e; app_e; value_e; if_e; expr }
;;

let parse = pack.expr pack

(* TESTS *)
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
  let test = "\"a\"" in
  start_test parse_const show_const test;
  [%expect {| (CString "a") |}]
;;

let%expect_test _ =
  let test = "\"123?/=_asdf\"" in
  start_test parse_const show_const test;
  [%expect {| (CString "123?/=_asdf") |}]
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
  let test = "\"notvarname\"" in
  start_test parse_Const show_pattern test;
  [%expect {| (Const (CString "notvarname")) |}]
;;

let%expect_test _ =
  let test = "1234" in
  start_test parse_Const show_pattern test;
  [%expect {| (Const (CInt 1234)) |}]
;;

let%expect_test _ =
  let test = "-1234" in
  start_test parse_Const show_pattern test;
  [%expect {| (Const (CInt -1234)) |}]
;;

let%expect_test _ =
  let test = "varname" in
  start_test parse_var show_pattern test;
  [%expect {| (Var "varname") |}]
;;

let%expect_test _ =
  let test = "varname1" in
  start_test parse_var show_pattern test;
  [%expect {| (Var "varname1") |}]
;;

let%expect_test _ =
  let test = "varn1ame" in
  start_test parse_var show_pattern test;
  [%expect {| (Var "varn1ame") |}]
;;

let%expect_test _ =
  let test = "1varname" in
  start_test parse_var show_pattern test;
  [%expect {| : Identifier first sumbol is letter, not digit |}]
;;

let%expect_test _ =
  let test = "1,(2),3" in
  start_test pat show_pattern test;
  [%expect {| (Tuple [(Const (CInt 1)); (Const (CInt 2)); (Const (CInt 3))]) |}]
;;

let%expect_test _ =
  let test = "[((1));2;3]" in
  start_test pat show_pattern test;
  [%expect {| (List [(Const (CInt 1)); (Const (CInt 2)); (Const (CInt 3))]) |}]
;;

let%expect_test _ =
  let test = "a,\"b\",c" in
  start_test pat show_pattern test;
  [%expect {| (Tuple [(Var "a"); (Const (CString "b")); (Var "c")]) |}]
;;

let%expect_test _ =
  let test = "[(\"1\",2,3); (2,3,4,5)]" in
  start_test pat show_pattern test;
  [%expect
    {|
    (List
       [(Tuple [(Const (CString "1")); (Const (CInt 2)); (Const (CInt 3))]);
         (Tuple
            [(Const (CInt 2)); (Const (CInt 3)); (Const (CInt 4));
              (Const (CInt 5))])
         ]) |}]
;;

let%expect_test _ =
  let test = "(((5)))" in
  start_test pat show_pattern test;
  [%expect {| (Const (CInt 5)) |}]
;;

let%expect_test _ =
  let test = "[1;2;3], 3" in
  start_test pat show_pattern test;
  [%expect
    {|
    (Tuple
       [(List [(Const (CInt 1)); (Const (CInt 2)); (Const (CInt 3))]);
         (Const (CInt 3))]) |}]
;;

let%expect_test _ =
  let test = "3, 1234, [1;2;3]" in
  start_test pat show_pattern test;
  [%expect
    {|
    (Tuple
       [(Const (CInt 3)); (Const (CInt 1234));
         (List [(Const (CInt 1)); (Const (CInt 2)); (Const (CInt 3))])]) |}]
;;

(* Test expr *)

let%expect_test _ =
  let test = "[1;2; 3]" in
  start_test parse show_expr test;
  [%expect
    {| (ListExpr [(ConstExpr (CInt 1)); (ConstExpr (CInt 2)); (ConstExpr (CInt 3))]) |}]
;;

let%expect_test _ =
  let test = "1       *      3" in
  start_test parse show_expr test;
  [%expect {| (BinExpr (Mul, (ConstExpr (CInt 1)), (ConstExpr (CInt 3)))) |}]
;;

let%expect_test _ =
  let test = "1 + 3 * 4" in
  start_test parse show_expr test;
  [%expect
    {|
    (BinExpr (Add, (ConstExpr (CInt 1)),
       (BinExpr (Mul, (ConstExpr (CInt 3)), (ConstExpr (CInt 4)))))) |}]
;;

let%expect_test _ =
  let test = "(3, 1234, [1;2;3])" in
  start_test parse show_expr test;
  [%expect
    {|
    (TupleExpr
       [(ConstExpr (CInt 3)); (ConstExpr (CInt 1234));
         (ListExpr
            [(ConstExpr (CInt 1)); (ConstExpr (CInt 2)); (ConstExpr (CInt 3))])
         ]) |}]
;;

let%expect_test _ =
  let test = "let f x = x+x" in
  start_test parse show_expr test;
  [%expect
    {|
    (LetExpr ("f",
       (FunExpr ((Var "x"), (BinExpr (Add, (VarExpr "x"), (VarExpr "x"))))))) |}]
;;

let%expect_test _ =
  let test = "let rec f x = f * x" in
  start_test parse show_expr test;
  [%expect
    {|
    (LetRecExpr ("f",
       (FunExpr ((Var "x"), (BinExpr (Mul, (VarExpr "f"), (VarExpr "x"))))))) |}]
;;

let%expect_test _ =
  let test = "if x=1  then x+1 else x-1" in
  start_test parse show_expr test;
  [%expect
    {|
    (IfExpr ((BinExpr (Eq, (VarExpr "x"), (ConstExpr (CInt 1)))),
       (BinExpr (Add, (VarExpr "x"), (ConstExpr (CInt 1)))),
       (BinExpr (Sub, (VarExpr "x"), (ConstExpr (CInt 1)))))) |}]
;;

let%expect_test _ =
  let test = "let rec fact n = if n = 1 then 1 else n * (fact (n - 1))" in
  start_test parse show_expr test;
  [%expect
    {|
    (LetRecExpr ("fact",
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
