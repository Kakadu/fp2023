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
      <|> ((fun _ -> CBool false) <$> string "false"))
;;

let parse_int =
  let ps = parse_token (option "" (pstrtoken "-" <|> pstrtoken "+")) in
  let pd = take_while1 is_digit in
  lift2 (fun sign digit -> CInt (Int.of_string @@ sign ^ digit)) ps pd
;;

let parse_str =
  char '"' *> take_while (fun a -> a != '"') <* char '"' >>| fun a -> CString a
;;

let parse_nil = parse_white_space *> ((fun _ -> CNil) <$> string "[]")
let parse_const = choice [ parse_nil; parse_int; parse_bool; parse_str ]

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
  lift2 (fun a b -> Tuple (a :: b)) (parse_token parser) (many1 (pstrtoken "," *> parser))
;;

let rec constr_con = function
  | [] -> Const CNil
  | hd :: [] when equal_pattern hd (Const CNil) -> Const CNil
  | [ f; s ] -> PCon (f, s)
  | hd :: tl -> PCon (hd, constr_con tl)
;;

let parser_con c =
  lift2
    (fun a b -> constr_con @@ (a :: b))
    (c <* pstrtoken "::" <|> (parens c <* pstrtoken "::"))
    (sep_by (pstrtoken "::") (c <|> parens c))
;;

let parse_con_2 parser constructor =
  constructor <$> (pstrtoken "[" *> sep_by1 (pstrtoken ";") parser <* pstrtoken "]")
;;

let parse_cases ps =
  lift2
    (fun id args -> Case (id, args))
    p_var_case
    (sep_by1 (pstrtoken "|") ps <|> sep_by (pstrtoken " ") ps)
;;

let parse_let_case = sep_by (pstrtoken "|") (p_var_case <|> p_var) <* pstrtoken "|)"

type pdispatch =
  { value : pdispatch -> pattern t
  ; tuple : pdispatch -> pattern t
  ; con : pdispatch -> pattern t
  ; pattern : pdispatch -> pattern t
  ; case : pdispatch -> pattern t
  }

let pack =
  let pattern pack =
    choice [ pack.con pack; pack.tuple pack; pack.value pack; pack.case pack ]
  in
  let parse pack =
    choice [ pack.value pack; pack.case pack; parens @@ pack.tuple pack ]
  in
  let con pack =
    fix
    @@ fun _ ->
    parser_con (parse pack <|> parens @@ pack.con pack)
    <|> parse_con_2 (parse pack <|> parens @@ pack.con pack) constr_con
  in
  (*
     let list pack = fix @@ fun _ -> parse_list (parse pack <|> parens @@ pack.list pack) in
  *)
  let value pack =
    fix @@ fun _ -> parse_wild <|> parse_Const <|> parse_var <|> parens @@ pack.value pack
  in
  let tuple pack =
    fix @@ fun _ -> parse_tuple (parse pack <|> parens @@ pack.tuple pack)
  in
  let case pack = fix @@ fun _ -> parse_cases (parse pack <|> parens @@ pack.case pack) in
  { value; tuple; pattern; case; con }
;;

let pat = pack.pattern pack

(* Parse expr *)

let p_op char_op op = pstrtoken char_op *> return (fun e1 e2 -> BinExpr (op, e1, e2))
let pmulti = p_op "*" Mul <|> p_op "/" Div <|> p_op "%" Mod
let pcons = p_op "::" Con
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
  fix
  @@ fun p -> many1 (pack.con pack <|> pack.case pack <|> pack.value pack) <|> parens p
;;

let parse_cons_semicolon_expr parser constructor =
  constructor <$> (pstrtoken "[" *> sep_by1 (pstrtoken ";") parser <* pstrtoken "]")
;;

let parse_tuple_expr parser =
  lift2
    (fun a b -> TupleExpr (a :: b))
    (parser <* pstrtoken ",")
    (sep_by1 (pstrtoken ",") parser)
;;

let parse_cases_expr = lift (fun id -> CaseExpr id) (parse_token p_var_case)
let parse_let_case = sep_by (pstrtoken "|") (p_var <|> p_var_case)
let parse_let_name = sep_by (pstrtoken " ") p_var

let plet_body pargs pexpr =
  parse_token1 pargs
  >>= fun args -> pstrtoken "=" *> pexpr >>| fun expr -> constr_efun args expr
;;

let parse_fun_args =
  fix
  @@ fun p ->
  many1 (pack.con pack <|> pack.value pack <|> pack.case pack <|> pack.value pack)
  <|> parens p
;;

type edispatch =
  { list_e : edispatch -> expr t
  ; tuple_e : edispatch -> expr t
  ; fun_e : edispatch -> expr t
  ; let_e : edispatch -> expr t
  ; app_e : edispatch -> expr t
  ; if_e : edispatch -> expr t
  ; let_in_e : edispatch -> expr t
  ; matching_e : edispatch -> expr t
  ; bin_e : edispatch -> expr t
  ; expr_parsers : edispatch -> expr t
  ; act_pat_e : edispatch -> expr t
  }

let pack =
  let expr_parsers pack = pack.bin_e pack <|> pack.matching_e pack in
  let value_e = fix @@ fun _ -> parse_evar <|> parse_econst <|> parse_cases_expr in
  let op_parsers pack =
    choice
      [ pack.if_e pack
      ; pack.let_in_e pack
      ; pack.let_e pack
      ; pack.fun_e pack
      ; pack.app_e pack <|> parens @@ pack.app_e pack
      ; parens @@ choice [ pack.tuple_e pack; pack.bin_e pack ]
      ; pack.act_pat_e pack
      ; value_e
      ; pack.list_e pack
      ]
  in
  let app_args_parsers pack =
    choice
      [ pack.list_e pack
      ; parens
        @@ choice
             [ pack.expr_parsers pack
             ; pack.let_in_e pack
             ; pack.let_e pack
             ; pack.app_e pack
             ; pack.fun_e pack
             ; pack.tuple_e pack
             ]
      ; value_e
      ]
  in
  let bin_e pack =
    fix
    @@ fun _ ->
    let multi = chainl1 (op_parsers pack) pmulti in
    let add = chainl1 multi padd in
    let cons = chainl1 add pcons in
    let comp = chainl1 cons pcomp in
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
  let if_e pack =
    fix
    @@ fun _ ->
    lift3
      (fun e1 e2 e3 -> IfExpr (e1, e2, e3))
      (pstrtoken "if" *> expr_parsers pack)
      (pstrtoken "then" *> expr_parsers pack)
      (pstrtoken "else" *> expr_parsers pack)
  in
  let list_e pack =
    fix
    @@ fun _ ->
    let rec create_cons_sc = function
      | [] -> ConstExpr CNil
      | hd :: [] when equal_expr hd (ConstExpr CNil) -> ConstExpr CNil
      | hd :: tl -> ListExpr (hd, create_cons_sc tl)
    in
    parse_cons_semicolon_expr (expr_parsers pack) create_cons_sc
  in
  let tuple_e pack = fix @@ fun _ -> parse_tuple_expr (expr_parsers pack) in
  let act_pat_e pack =
    fix
    @@ fun _ ->
    lift2
      (fun a d -> LetActExpr (a, d))
      (pstrtoken "let" *> pstrtoken "(|" *> (parse_let_case <|> parse_let_name)
       <* pstrtoken "|)")
      (plet_body parse_fun_args (expr_parsers pack <|> parens @@ expr_parsers pack))
  in
  let let_e pack =
    fix
    @@ fun _ ->
    lift4
      (fun flag name args body ->
        let body = constr_efun args body in
        LetExpr (flag, name, body))
      (pstrtoken "let"
       *> option false (parse_token (string "rec") <* parse_white_space1 >>| fun _ -> true)
      )
      p_var
      (parse_white_space *> many (pat <|> parens pat))
      (pstrtoken "=" *> expr_parsers pack)
    (*
       let eletfun pexpr =
       empty
       *> lift4
       (fun flag name args body ->
       let body = construct_efun args body in
       if flag then eletrec name body else elet name body)
       parse_rec_or_not
       psIdent
       pargs
       (pstoken "=" *> pexpr)
       ;;
    *)
  in
  let let_in_e pack =
    let lift5 f p1 p2 p3 p4 p5 = f <$> p1 <*> p2 <*> p3 <*> p4 <*> p5 in
    fix
    @@ fun _ ->
    lift5
      (fun is_rec name args expr1 expr2 ->
        let expr = constr_efun args expr1 in
        LetInExpr (is_rec, name, expr, expr2))
      (pstrtoken "let"
       *> option false (parse_token (string "rec") <* parse_white_space1 >>| fun _ -> true)
      )
      p_var
      (many pat)
      (pstrtoken1 "=" *> expr_parsers pack)
      (pstrtoken "in" *> expr_parsers pack)
    (*
       let eletdecl pexpr =
       let lift5 f p1 p2 p3 p4 p5 = f <$> p1 <*> p2 <*> p3 <*> p4 <*> p5 in
       empty
       *> lift5
       (fun flag name args body1 body2 ->
       let body1 = construct_efun args body1 in
       if flag then eletrecin name body1 body2 else eletin name body1 body2)
       parse_rec_or_not
       psIdent
       pargs
       (pstoken1 "=" *> pexpr)
       (pstoken1 "in" *> pexpr)
       ;;
    *)
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
  { list_e
  ; tuple_e
  ; fun_e
  ; let_in_e
  ; let_e
  ; app_e
  ; if_e
  ; expr_parsers
  ; matching_e
  ; bin_e
  ; act_pat_e
  }
;;

let parse = pack.expr_parsers pack

(* Parser program *)

let program = many1 (parse_token parse <* parse_token (many1 (pstrtoken ";;")))
let main_parse str = start_parsing program (String.strip str)
