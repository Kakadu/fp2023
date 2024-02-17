(** Copyright 2023-2024, David Akhmedov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let is_space = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false
;;

let char_bool (c1 : char) = function
  | c2 -> c1 = c2
;;

let parse_space = take_while is_space
let parse_space1 = take_while1 is_space
let parse_token p = parse_space *> p
let parse_token1 p = parse_space1 *> p
let parse_stoken s = parse_token @@ string s
let parse_stoken1 s = parse_token1 @@ string s
let parse_parens p = parse_stoken "(" *> p <* parse_stoken ")"
let spaces = skip_while is_space

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_letter c = is_lower c || is_upper c
let is_ident_char c = is_digit c || is_letter c
let is_sign c = c = '+' || c = '-'

let keywords =
  [ "if"
  ; "then"
  ; "else"
  ; "let"
  ; "rec"
  ; "true"
  ; "false"
  ; "match"
  ; "with"
  ; "in"
  ; "fun"
  ; "type"
  ; "int"
  ; "bool"
  ; "when"
  ; "function"
  ]
;;

let is_keyword s = List.mem s keywords

(****************************************************** Consts ******************************************************)
let parse_int =
  parse_token
  @@ lift2
       (fun hd tl -> hd, tl)
       (satisfy (fun ch -> ch = '-' || ch = '+' || is_digit ch))
       (take_while (fun ch -> is_digit ch))
  >>= fun (hd, tl) ->
  if is_sign hd && tl = ""
  then fail "Can't parse int"
  else return @@ CInt (int_of_string @@ String.make 1 hd ^ tl)
;;

let parse_string =
  parse_token
  @@ choice
       ~failure_msg:"Can't parse string"
       [ char '"' *> take_while (fun ch -> ch != '"') <* char '"' ]
  >>= fun res_string -> return @@ CString res_string
;;

let parse_bool =
  choice [ parse_stoken "true" *> return true; parse_stoken "false" *> return false ]
  >>= fun res_bool -> return @@ Ast.CBool res_bool
;;

let parse_nil = parse_stoken "Nil" *> return CNil
let parse_const = choice [ parse_bool; parse_int; parse_string; parse_nil ]
let parse_const_to_expr = parse_const >>= fun res -> return @@ EConst res

(****************************************************** Operators ******************************************************)
let parse_str_bin_op =
  choice
    ~failure_msg:"Can't parse binary op"
    [ parse_stoken "+" *> return Add
    ; parse_stoken "-" *> return Sub
    ; parse_stoken "*" *> return Mul
    ; parse_stoken "/" *> return Div
    ; parse_stoken "<=" *> return Leq
    ; parse_stoken "<" *> return Less
    ; parse_stoken ">=" *> return Geq
    ; parse_stoken ">" *> return Gre
    ; parse_stoken "==" *> return Eq
    ; parse_stoken "!=" *> return Neq
    ; parse_stoken "&&" *> return And
    ; parse_stoken "||" *> return Or
    ]
;;

let parse_unary_op =
  choice
    [ parse_stoken "+" *> return Plus
    ; parse_stoken "-" *> return Minus
    ; parse_stoken "not" *> return Not
    ]
;;

let parse_satisfy_ops op parser =
  parse_space
  *>
  let* chars =
    take_while (fun c -> not (is_digit c or is_ident_char c or c = '(' or is_space c))
  in
  if chars = op then parser else fail "The operators don't match"
;;

(****************************************************** Tuple ******************************************************)

let parse_tuple parse_expr =
  let* expr_list_res = sep_by1 (parse_stoken ",") parse_expr in
  match expr_list_res with
  | [ e ] -> return e
  | _ -> return @@ etuple expr_list_res
;;

(****************************************************** Identifiers, let-bindings, anonymous functions ******************************************************)

let parse_identifier =
  parse_token
  @@ lift2
       (fun hd tl -> String.make 1 hd ^ tl)
       (satisfy (fun ch -> ch = '_' || is_lower ch))
       (take_while (fun ch -> ch = '_' || is_ident_char ch))
  >>= fun str_res ->
  if is_keyword str_res
  then fail "invalid syntax"
  else if str_res = "_"
  then fail "_ is not supported"
  else return @@ Ident str_res
;;

let parse_identifier_to_expr = parse_identifier >>= fun id -> return @@ eid id

let parse_params_and_expr parse_expr sep =
  let parse_param =
    parse_parens (parse_tuple parse_identifier_to_expr) <|> parse_identifier_to_expr
  in
  fix
  @@ fun parse_params_and_expr ->
  let* parameter = parse_param in
  let* expr =
    choice [ parse_space1 *> parse_params_and_expr; parse_satisfy_ops sep parse_expr ]
  in
  return @@ efun (eapp parameter expr)
;;

let parse_bind_let parse_expr =
  parse_stoken "let"
  *> lift3
       dlet
       (parse_stoken1 "rec" *> return Recursive <|> return Not_recursive)
       (parse_token1 parse_identifier)
       (parse_params_and_expr parse_expr "=" <|> parse_satisfy_ops "=" parse_expr)
;;

let parse_anon_fun parse_expr =
  parse_stoken "fun" *> parse_space1 *> parse_params_and_expr parse_expr "->"
;;

(****************************************************** Branching ******************************************************)

let parse_branching parse_expr =
  lift3
    eif
    (parse_stoken "if" *> parse_token1 parse_expr)
    (parse_stoken1 "then" *> parse_token1 parse_expr)
    (parse_stoken1 "else" *> parse_token1 parse_expr)
;;

(****************************************************** Application ******************************************************)

let binop_binder str_op bin_op =
  let helper bin_op x y = ebinop x bin_op y in
  parse_satisfy_ops str_op (return @@ helper bin_op)
;;

let add = binop_binder "+" Add
let sub = binop_binder "-" Sub
let mul = binop_binder "*" Mul
let div = binop_binder "/" Div
let geq = binop_binder ">=" Geq
let gre = binop_binder ">" Gre
let leq = binop_binder "<=" Leq
let less = binop_binder "<" Less
let eq = binop_binder "=" Eq
let neq = binop_binder "!=" Neq
let and_l = binop_binder "&&" And
let or_l = binop_binder "||" Or

let chainl1 parse_e op =
  let rec go acc = lift2 (fun f x -> f acc x) op parse_e >>= go <|> return acc in
  parse_e >>= fun init -> go init
;;

let rec chainr1 e op = e >>= fun a -> op >>= (fun f -> chainr1 e op >>| f a) <|> return a

let parse_bin_op_app parse_expr =
  let term = chainl1 parse_expr (mul <|> div) in
  let term = chainl1 term (add <|> sub) in
  let term = chainl1 term (choice [ geq; gre; leq; less; eq; neq ]) in
  chainr1 term (choice [ or_l; and_l ])
;;

let rec parse_un_op_app parse_expr =
  let* unop = parse_unary_op in
  let* res =
    take_while (fun c -> not (is_digit c or is_ident_char c or c = '(' or is_space c))
  in
  if res = ""
  then
    let* expr =
      choice
        [ parse_un_op_app parse_expr
        ; parse_parens (parse_un_op_app parse_expr)
        ; parse_fun_app parse_expr
        ; parse_parens (parse_fun_app parse_expr)
        ]
    in
    return @@ eunop unop expr
  else fail "This is not un op"

and parse_fun_app parse_expr =
  let rec parse_fun_app_currying app1 =
    let* app2 =
      choice
        [ parse_str_bin_op *> return EEmpty
        ; parse_expr
        ; parse_parens (parse_app parse_expr)
        ]
    in
    if app2 = EEmpty
    then fail "Can't proccess bin op, only fun app"
    else parse_fun_app_currying (eapp app1 app2) <|> return (eapp app1 app2)
  in
  let* app1 = parse_expr in
  parse_fun_app_currying app1 <|> return app1

and parse_app parse_expr =
  let parser_ehelper = choice [ parse_fun_app parse_expr; parse_expr ] in
  parse_bin_op_app parser_ehelper
;;

let parse_expr =
  fix (fun parse_expr ->
    let parser_ehelper =
      choice
        [ parse_parens parse_expr
        ; parse_const_to_expr
        ; parse_identifier_to_expr
        ; parse_anon_fun parse_expr
        ; parse_branching parse_expr
        ]
    in
    let parser_ehelper = choice [ parser_ehelper; parse_un_op_app parser_ehelper ] in
    let parser_ehelper = parse_app parser_ehelper in
    let parser_ehelper = parse_tuple parser_ehelper in
    choice ~failure_msg:"Can't parse expr" [ parser_ehelper ])
;;

let parse_decls =
  lift prog (many (parse_bind_let parse_expr <* (parse_stoken ";;" <|> parse_space)))
;;

let parse_program s =
  match Angstrom.parse_string ~consume:Consume.All parse_decls s with
  | Ok v -> Ok v
  | Error msg ->
    (match msg with
     | ": end_of_input" -> Error "Can't parse decl"
     | _ -> Error msg)
;;
