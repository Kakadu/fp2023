(** Copyright 2023-2024, David Akhmedov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let is_space = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false
;;

let parse_space = take_while is_space
let parse_space1 = take_while1 is_space
let parse_token p = parse_space *> p
let parse_token1 p = parse_space1 *> p
let parse_next_token p = parse_token1 p < parse_space1
let parse_stoken s = parse_token @@ string s
let parse_stoken1 s = parse_token1 @@ string s
let parse_next_stoken p = parse_stoken1 p <* parse_space1
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
    (lift (fun hd -> CInt (int_of_string hd)) (take_while1 (fun ch -> is_digit ch)))
;;

let parse_bool =
  choice [ parse_stoken "true" *> return true; parse_stoken "false" *> return false ]
  >>= fun res_bool -> return @@ CBool res_bool
;;

let parse_const = choice [ parse_bool; parse_int ]
let parse_const_expr = parse_const >>= fun res -> return @@ EConst res

(****************************************************** Operators ******************************************************)
let parse_satisfy_op_and_run op parser =
  parse_space
  *>
  let* chars =
    take_while (fun c ->
      not (is_digit c or is_ident_char c or c = '(' or is_space c or c = '"' or c = '['))
  in
  if chars = op then parser else fail "The operators don't match"
;;

let parse_unary_op =
  choice
    [ parse_satisfy_op_and_run "+" (return Plus)
    ; parse_satisfy_op_and_run "-" (return Minus)
    ; parse_satisfy_op_and_run "not" (return Not)
    ]
;;

(****************************************************** Tuple ******************************************************)

let parse_tuple parser wrap =
  let* list_res = sep_by1 (parse_stoken ",") parser in
  match list_res with
  | [ e ] -> return e
  | _ -> return (wrap list_res)
;;

let parse_tuple_expr parse_expr = parse_tuple parse_expr etuple
let parse_tuple_pat parse_pat = parse_tuple parse_pat ptuple

(****************************************************** Branching ******************************************************)

let parse_branching parse_expr =
  lift3
    eif
    (parse_stoken "if" *> parse_token1 parse_expr)
    (parse_stoken1 "then" *> parse_token1 parse_expr)
    (parse_stoken1 "else" *> parse_token1 parse_expr)
;;

(****************************************************** Application, unary, binary ops ******************************************************)

let binop_binder str_op bin_op =
  let helper bin_op x y = ebinop x bin_op y in
  parse_satisfy_op_and_run str_op (return @@ helper bin_op)
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
let app_binder = return eapp

let rec parse_un_op_app parse_expr =
  let* unop = parse_token parse_unary_op in
  let* expr =
    choice
      [ parse_expr
      ; parse_parens (parse_un_op_app parse_expr)
      ; parse_space1 *> parse_un_op_app parse_expr
      ]
  in
  return @@ eunop unop expr
;;

let parse_bin_op_app parse_expr =
  fix (fun parse_bin_op_app ->
    let parse_bin_op parse_expr =
      let term = chainr1 parse_expr and_l in
      let term = chainr1 term or_l in
      let term = chainl1 term (mul <|> div) in
      let term = chainl1 term (add <|> sub) in
      chainr1
        (choice [ term; parse_bin_op_app ])
        (choice [ geq; gre; leq; less; eq; neq ])
    and parse_app parse_expr =
      let term = chainl1 parse_expr app_binder in
      let term = choice [ parse_un_op_app term; term ] in
      let cons = parse_satisfy_op_and_run "::" (return elist) in
      chainr1 term cons
    in
    parse_bin_op (parse_app parse_expr))
;;

(****************************************************** List ******************************************************)

let parse_list parser wrap init =
  let* expr_list =
    choice
      [ parse_stoken "[" *> parse_space *> parse_stoken "]" *> return []
      ; parse_stoken "[" *> sep_by (parse_stoken ";") parser <* parse_stoken "]"
      ]
  in
  return (List.fold_right wrap expr_list init)
;;

let parse_list_expr parse_expr = parse_list parse_expr elist (econst CNil)
let parse_list_pat parse_pat = parse_list parse_pat plist (pconst CNil)

(****************************************************** Identifiers, let-bindings, anonymous functions ******************************************************)

let parse_letters =
  lift2
    (fun hd tl -> String.make 1 hd ^ tl)
    (satisfy (fun ch -> ch = '_' || is_lower ch))
    (take_while (fun ch -> ch = '_' || is_ident_char ch))
;;

let parse_identifier =
  parse_token parse_letters
  >>= fun ident -> if is_keyword ident then fail "invalid syntax" else return ident
;;

let parse_identifier_expr = parse_identifier >>| eid

let parse_pattern =
  fix (fun parse_pattern ->
    let parse_pattern =
      choice
        [ parse_identifier >>| pid
        ; parse_parens (parse_tuple_pat parse_pattern)
        ; parse_const >>| pconst
        ]
    in
    choice [ parse_parens parse_pattern; parse_pattern ])
;;

let parse_pattern_with_list =
  fix (fun parse_pattern_with_list ->
    let parse_list_pattern = parse_satisfy_op_and_run "::" (return plist) in
    chainr1
      (choice
         [ parse_parens parse_pattern_with_list
         ; parse_pattern
         ; parse_list_pat parse_pattern_with_list
         ])
      parse_list_pattern)
;;

let parse_params =
  let parse_param =
    choice
      [ parse_pattern
      ; parse_parens parse_pattern_with_list
      ; parse_list_pat parse_pattern_with_list
      ]
  in
  sep_by parse_space1 parse_param
;;

let parse_fun op parse_expr =
  let* params = parse_params in
  let* expr = parse_satisfy_op_and_run op parse_expr in
  return @@ List.fold_right (fun parameter acc -> efun parameter acc) params expr
;;

let parse_fun_decl = parse_fun "="
let parse_fun_anon = parse_fun "->"

let parse_closure parse_expr =
  parse_stoken "let"
  *> lift4
       (fun rec_flag ident expr_clsr expr -> eclsr (dlet rec_flag ident expr_clsr) expr)
       (parse_next_stoken "rec" *> return Recursive <|> return Not_recursive)
       (parse_token parse_identifier)
       (parse_fun_decl parse_expr <* parse_stoken "in")
       parse_expr
;;

let parse_fun_anon_expr parse_expr =
  parse_stoken "fun" *> parse_space1 *> parse_fun_anon parse_expr
;;

(****************************************************** List matching ******************************************************)

let parse_match parse_expr =
  let parse_case =
    let* case =
      choice
        [ parse_pattern_with_list; parse_pattern; parse_list_pat parse_pattern_with_list ]
    in
    let* value = parse_stoken "->" *> parse_expr in
    return (case, value)
  in
  parse_stoken "match"
  *> lift2
       ematch
       (parse_token1 parse_expr
        <* (parse_stoken "with" <* choice [ parse_stoken1 "|"; parse_space ]))
       (sep_by1 (parse_stoken "|") parse_case)
;;

(****************************************************** Expression parsing ******************************************************)
let parse_expr =
  fix (fun parse_expr ->
    let parser_ehelper =
      choice
        [ parse_stoken "(" *> parse_stoken ")" *> return CUnit >>| econst
        ; parse_parens parse_expr
        ; parse_const_expr
        ; parse_identifier_expr
        ; parse_fun_anon_expr parse_expr
        ; parse_branching parse_expr
        ; parse_closure parse_expr
        ; parse_match parse_expr
        ; parse_list_expr parse_expr
        ]
    in
    let parser_ehelper = parse_bin_op_app parser_ehelper in
    let parser_ehelper = parse_tuple_expr parser_ehelper in
    choice ~failure_msg:"Can't parse expr" [ parser_ehelper ])
;;

let rec postprocess_expr =
  let postprocess_list el = List.map postprocess_expr el in
  function
  | EBinop (e1, bop, e2) ->
    let e1 = postprocess_expr e1 in
    let e2 = postprocess_expr e2 in
    (match bop with
     | Geq -> ebinop e2 Leq e1
     | Gre -> ebinop e2 Less e1
     | Neq -> eunop Not (ebinop e1 Eq e2)
     | Or -> eunop Not (ebinop (eunop Not e1) And (eunop Not e2))
     | _ -> ebinop e1 bop e2)
  | EUnop (uop, e) -> eunop uop (postprocess_expr e)
  | EFun (id, e) -> efun id (postprocess_expr e)
  | EApp (EId "not", e2) -> eunop Not (postprocess_expr e2)
  | EApp (e1, e2) -> eapp (postprocess_expr e1) (postprocess_expr e2)
  | EIf (i, t, e) -> eif (postprocess_expr i) (postprocess_expr t) (postprocess_expr e)
  | EList (hd, tl) -> elist (postprocess_expr hd) (postprocess_expr tl)
  | ETuple e -> etuple @@ postprocess_list e
  | EClsr (DLet (rec_flag, id, e_let), e) ->
    eclsr (DLet (rec_flag, id, postprocess_expr e_let)) (postprocess_expr e)
  | EMatch (e1, e2) ->
    ematch (postprocess_expr e1) (List.map (fun (x, y) -> x, postprocess_expr y) e2)
  | a -> a
;;

let parse_expr =
  let* expr = parse_expr in
  return @@ postprocess_expr expr
;;

(****************************************************** Declaration ******************************************************)

let parse_decl_let parse_expr =
  parse_stoken "let"
  *> lift3
       (fun rec_flag ident expr -> dlet rec_flag ident expr)
       (parse_next_stoken "rec" *> return Recursive <|> return Not_recursive)
       (parse_token parse_identifier)
       (parse_fun_decl parse_expr)
;;

let parse_decls =
  lift prog (many (parse_decl_let parse_expr <* (parse_stoken ";;" <|> parse_space)))
;;

let parse_program s =
  match Angstrom.parse_string ~consume:Consume.All parse_decls s with
  | Ok v -> Ok v
  | Error msg ->
    (match msg with
     | ": end_of_input" -> Error "Syntax error"
     | _ -> Error msg)
;;
