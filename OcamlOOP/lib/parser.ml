(** Copyright 2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Base
open Ast
open Errors

(*=====================Constructors=====================*)

let id s = s
let cint n = Const_int n
let cbool b = Const_bool b
let cnil = Const_nil
let pconst c = Pat_const c
let pcons a b = Pat_cons (a, b)
let pany = Pat_any
let pnil = pconst cnil
let pval c = Pat_var c
let ptuple ps = Pat_tuple ps
let econst c = Exp_constant c
let eval c = Exp_ident c
let eapp f a = Exp_apply (f, a)
let ematch v ptrns = Exp_match (v, ptrns)
let esend s m = Exp_send (s, m)
let eobj o_self o_fields = Exp_object { o_self; o_fields }
let efun i e = Exp_function (i, e)
let eite b t e = Exp_ifthenelse (b, t, e)
let eunop o e = Exp_unary_op (o, e)
let ebinop o l r = Exp_bin_op (o, l, r)
let elet d e = Exp_let (d, e)
let etuple es = Exp_tuple es
let econs a b = Exp_list (a, b)
let edecl d_rec d_pat d_expr = { d_rec; d_pat; d_expr }
let eoverr es = Exp_override es
let oval p e = Obj_val (p, e)
let omthd f p e = Obj_method (f, p, e)
let streval e = Str_eval e
let strval d = Str_value d

(*=====================Check conditions=====================*)
let is_whitespace = Char.is_whitespace
let is_digit = Char.is_digit

let is_upper = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_lower = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_keyword = function
  | "let"
  | "if"
  | "else"
  | "fun"
  | "function"
  | "then"
  | "rec"
  | "true"
  | "false"
  | "match"
  | "with"
  | "object"
  | "end"
  | "val"
  | "not"
  | "method"
  | "in" -> true
  | _ -> false
;;

let is_alpha c = is_upper c || is_lower c
let is_ident c = is_alpha c || Char.equal '_' c

(*=====================Control characters=====================*)
let skip_whitespace = take_while is_whitespace
let skip_whitespace1 = take_while1 is_whitespace
let ptoken p = skip_whitespace *> p
let token p = skip_whitespace *> string p
let lp = token "("
let rp = token ")"
let parens p = lp *> p <* rp
let sbrcts p = token "[" *> p <* token "]"
let dsmcln = token ";;"

(*=====================Fold infix operators=====================*)
let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op =
  e >>= fun sub_e -> op >>= (fun f -> chainr1 e op >>| f sub_e) <|> return sub_e
;;

(*=====================Constants=====================*)

let c_int =
  ptoken @@ take_while1 is_digit
  >>= fun whole ->
  let num = Stdlib.int_of_string_opt whole in
  match num with
  | Some n -> return @@ cint n
  | None -> fail "Integer literal exceeds the range of representable integers of type int"
;;

let c_bool =
  ptoken @@ take_while1 is_alpha
  >>= function
  | "true" -> return @@ cbool true
  | "false" -> return @@ cbool false
  | _ -> fail "not a bool"
;;

let c_nil = token "[]" *> return cnil
let const = choice [ c_int; c_bool; c_nil ]

(*=====================Identifiers=====================*)
let check_ident i =
  if is_keyword i
  then fail "keyword"
  else if String.equal "_" i
  then fail "wildcard not expected"
  else return (id i)
;;

let ident =
  ptoken peek_char
  >>= (function
         | Some x when Char.equal x '_' || is_lower x -> return x
         | _ -> fail "not an identifier")
  >>= fun _ -> take_while is_ident >>= fun s -> check_ident s
;;

(*=====================Patterns=====================*)

let p_cons = token "::" *> return pcons
let p_any = token "_" *> skip_whitespace *> return pany
let p_const = const >>| fun p -> pconst p
let p_val = ident >>| pval
let fold_plist = List.fold_right ~f:(fun p1 p2 -> pcons p1 p2) ~init:pnil

let p_list =
  let item = p_const <|> p_val in
  sbrcts @@ sep_by (token ";") item >>| fold_plist
;;

let tuple ident f = lift2 (fun h tl -> f @@ (h :: tl)) ident (many1 (token "," *> ident))
let p_tuple pat = parens (tuple pat ptuple)

let pattern =
  fix (fun pattern ->
    let term = choice [ parens pattern; p_const; p_any; p_val; p_tuple pattern ] in
    let cons = parens (chainr1 term p_cons) in
    cons <|> term)
;;

(*=====================Expressions=====================*)

let e_const = const >>| fun c -> econst c
let e_val = ident >>| eval
let e_cons = token "::" *> return econs

let e_list expr =
  let rec create_cons = function
    | [] -> econst cnil
    | h :: tl -> econs h (create_cons tl)
  in
  let basic_list = sbrcts @@ sep_by (token ";") expr >>| create_cons in
  let cons_list = chainr1 (expr <|> basic_list) e_cons in
  basic_list <|> cons_list
;;

let e_tuple expr = tuple expr etuple
let e_app expr = chainl1 expr (return eapp)
let e_ite b t e = lift3 eite (token "if" *> b) (token "then" *> t) (token "else" *> e)

let e_fun pexpr =
  token "fun" *> many1 pattern
  >>= fun args ->
  token "->" *> pexpr
  >>= fun e ->
  match List.rev args with
  | h :: tl -> return (List.fold_left ~init:(efun h e) ~f:(fun acc x -> efun x acc) tl)
  | _ -> fail "The function must have at least one argument"
;;

let e_decl pexpr =
  let exp =
    skip_whitespace *> many pattern
    >>= fun args ->
    token "=" *> pexpr
    >>| fun e ->
    match List.rev args with
    | h :: tl -> List.fold_left ~init:(efun h e) ~f:(fun acc x -> efun x acc) tl
    | _ -> e
  in
  token "let"
  *> lift3
       edecl
       (token "rec" *> return Ast.Recursive <|> return Ast.Nonrecursive)
       (ptoken pattern)
       exp
;;

let e_ptrn_matching pexpr = lift2 (fun k v -> k, v) (pattern <* token "->") pexpr

let e_match pexpr =
  token "match"
  *> lift2
       ematch
       (pexpr <* token "with")
       (e_ptrn_matching pexpr
        <|> token "|" *> e_ptrn_matching pexpr
        >>= fun p -> many (token "|" *> e_ptrn_matching pexpr) >>| fun ps -> p :: ps)
;;

let e_let pexpr = lift2 elet (e_decl pexpr) (token "in" *> pexpr)

let e_sinvk pexpr =
  let iter = token "#" *> ident in
  let rec helper acc =
    iter >>= fun sub -> helper (esend acc sub) <|> return (esend acc sub)
  in
  let acc = lift2 esend pexpr (token "#" *> ident) in
  acc >>= helper <|> acc
;;

(* let e_sinvk pexpr = lift2 esend pexpr (token "#" *> ident) *)

let e_override pexpr =
  token "{<"
  *> lift
       eoverr
       (sep_by (token ";") (ident >>= fun id -> token "=" *> pexpr >>| fun e -> id, e))
  <* token ">}"
;;

let e_obj pexpr =
  let ov = lift2 oval (token "val" *> ident) (token "=" *> pexpr) in
  let helper =
    skip_whitespace *> many pattern
    >>= fun args ->
    token "=" *> pexpr
    >>| fun e ->
    match List.rev args with
    | h :: tl -> List.fold_left ~init:(efun h e) ~f:(fun acc x -> efun x acc) tl
    | _ -> e
  in
  let om =
    lift3
      omthd
      (token "method" *> token "private" *> return Private
       <|> token "method" *> return Public)
      ident
      helper
  in
  let self_pat = token "self" *> return (pval "self") in
  token "object"
  *> lift2
       eobj
       (option Ast.Pat_any (parens (p_any <|> self_pat)))
       (many (ov <|> om) <* token "end")
;;

(*=====================Binary/Unary operators=====================*)

let bin_op chain1 e ops = chain1 e (ops >>| fun o l r -> ebinop o l r)
let lbo = bin_op chainl1
let rbo = bin_op chainr1
let op l = choice (List.map ~f:(fun (o, n) -> token o *> return n) l)
let mul_div = op [ "*", Asterisk; "/", Divider ]
let add_sub = op [ "+", Plus; "-", Sub ]
let cmp = op [ "<=", Ltq; "<", Lt; ">=", Gtq; ">", Gt; "=", Eq; "!=", Neq ]
let andop = op [ "&&", And ]
let orop = op [ "||", Or ]
let neg = op [ "not", Not; "-", Minus ]

let expr =
  fix (fun pexpr ->
    let sube = choice [ parens pexpr; e_const; e_val ] in
    let send = e_sinvk sube in
    let term = e_app (send <|> sube) in
    let term = lbo (term <|> lift2 eunop neg term) mul_div in
    let term = lbo term add_sub in
    let term = e_list term <|> term in
    let term = lbo term cmp in
    let term = rbo term andop in
    let term = rbo term orop in
    let term = e_tuple term <|> term in
    choice
      [ e_ite pexpr pexpr pexpr
      ; e_let pexpr
      ; e_match pexpr
      ; e_fun pexpr
      ; e_obj pexpr
      ; e_override pexpr
      ; term
      ])
;;

let del = (dsmcln <|> skip_whitespace) *> skip_whitespace
let decl = ptoken (e_decl expr)
let str_item = expr >>| streval <* dsmcln <|> (decl >>| strval)
let program = del *> many1 (str_item <* del)
let parse_syntax_err msg = Parser (Syntax_error msg)

let parse s =
  match parse_string ~consume:All program s with
  | Ok v -> Ok v
  | Error _ -> Error (parse_syntax_err "Syntax error")
;;

let parse_prefix s =
  match parse_string ~consume:Prefix program s with
  | Ok v -> Ok v
  | Error _ -> Error (parse_syntax_err "Syntax error")
;;

module PP = struct
  let pp_error ppf = function
    | Syntax_error msg -> Format.fprintf ppf "%s" msg
  ;;
end
