(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Base
open Ast

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
  | "method" -> true
  | _ -> false
;;

let is_alpha c = is_upper c || is_lower c
let is_ident c = is_alpha c || Char.equal '_' c
let skip_whitespace = take_while is_whitespace
let skip_whitespace1 = take_while1 is_whitespace
let ptoken p = skip_whitespace *> p
let ptoken1 p = skip_whitespace1 *> p
let token p = skip_whitespace *> string p
let token1 p = skip_whitespace1 *> string p
let lp = token "("
let rp = token ")"
let nil = token "[]" *> return nil
let pcons = token "::" *> return pcons
let pany = token1 "_" *> skip_whitespace1 *> return pany
let parens p = lp *> p <* rp
let sbrcts p = token "[" *> p <* token "]"
let dsmcln = token ";;"

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

let rec chainr1 e op =
  e >>= fun sub_e -> op >>= (fun f -> chainr1 e op >>| f sub_e) <|> return sub_e
;;

let sign =
  peek_char
  >>= function
  | Some '-' -> advance 1 >>| fun () -> "-"
  | Some '+' -> advance 1 >>| fun () -> "+"
  | Some c when is_digit c -> return "+"
  | _ -> fail "Sign or digit expected"
;;

let integer =
  ptoken sign
  >>= fun sign ->
  take_while1 is_digit
  >>= fun whole ->
  let num = Stdlib.int_of_string_opt (sign ^ whole) in
  match num with
  | Some n -> return @@ c_int n
  | None -> fail "Integer literal exceeds the range of representable integers of type int"
;;

let boolean =
  ptoken @@ take_while1 is_alpha
  >>= function
  | "true" -> return @@ c_bool true
  | "false" -> return @@ c_bool false
  | _ -> fail "not a bool"
;;

let const = choice [ integer; boolean; nil ]
let pconst = const >>| fun p -> pconst p

let econst =
  choice
    [ (integer >>| fun x -> econst x)
    ; (boolean >>| fun b -> econst b)
    ; (nil >>| fun n -> econst n)
    ]
;;

let ident =
  ptoken peek_char
  >>= (function
         | Some x when Char.equal x '_' || is_lower x -> return x
         | _ -> fail "fail")
  >>= fun _ ->
  take_while is_ident
  >>= fun s -> if is_keyword s then fail "keyword" else return @@ p_id s
;;

let eval = ident >>| eval
let pval = ident >>| pval
let fold_plist = List.fold_right ~f:(fun p1 p2 -> Ast.pcons p1 p2) ~init:Ast.pnil

(* support @ operator ??*)
let plist =
  let item = pconst <|> pval in
  sbrcts @@ sep_by (token ";") item >>| fold_plist
;;

let patern =
  fix (fun _patern ->
    let pat = plist <|> pval <|> pconst <|> pany in
    chainl1 pat pcons)
;;

let pfun pexpr =
  token "fun" *> many1 patern
  >>= fun args ->
  token "->" *> pexpr
  >>| fun e ->
  match List.rev args with
  | h :: tl -> List.fold_left ~init:(pfun h e) ~f:(fun acc x -> pfun x acc) tl
  | _ -> failwith "unreachable"
;;

let decl kws pexpr =
  let exp =
    skip_whitespace *> many patern
    >>= fun args ->
    token "=" *> pexpr
    >>| fun e ->
    match List.rev args with
    | h :: tl -> List.fold_left ~init:(Ast.pfun h e) ~f:(fun acc x -> Ast.pfun x acc) tl
    | _ -> e
  in
  match kws with
  | _ :: [] -> token "let" *> lift2 pdecl (ptoken patern) exp
  | _ -> token "let" *> token "rec" *> lift2 pdecl (ptoken patern) exp
;;

let nrec_decl = decl ["let"]
let rec_decl = decl ["let"; "rec"]

let plet pexpr =
  lift2 plet (nrec_decl pexpr) (option (Ast.econst Unit) (token "in" *> pexpr))
;;

let prlet pexpr =
  lift2 prlet (rec_decl pexpr) (option (Ast.econst Unit) (token "in" *> pexpr))
;;

(* can there be only one pattern matching? *)
let ptrn pexpr = lift2 (fun k v -> k, v) (patern <* token "->") pexpr

let ematch pexpr =
  token "match"
  *> lift2
       ematch
       (pexpr <* token "with")
       (ptrn pexpr
        <|> token "|" *> ptrn pexpr
        >>= fun p -> many (token "|" *> ptrn pexpr) >>| fun ps -> p :: ps)
;;

let ite b t e =
  lift3
    ite
    (token "if" *> b)
    (token "then" *> t)
    (option (Ast.econst Unit) (token "else" *> e))
;;

let eapp expr = chainl1 expr (skip_whitespace1 *> return Ast.eapp)
let bin_op chain1 e ops = chain1 e (ops >>| fun o l r -> bin_op o l r)
let lbo = bin_op chainl1
let rbo = bin_op chainr1
let op l = choice (List.map ~f:(fun (o, n) -> token o *> return n) l)
let mul_div = op [ "*", Asterisk; "/", Divider ]
let add_sub = op [ "+", Plus; "-", Sub ]
let cmp = op [ "<=", Ltq; "<", Lt; ">=", Gtq; ">", Gt; "=", Eq; "!=", Neq ]
let andop = op [ "&&", And ] (* & ?*)
let orop = op [ "||", Or ] (* or ?*)
let neg = op [ "not", Not; "-", Minus ]

let pobj pexpr =
  let ov = lift2 oval (token "val" *> patern) (token "=" *> pexpr) in
  let helper =
    skip_whitespace *> many patern
    >>= fun args ->
    token "=" *> pexpr
    >>| fun e ->
    match List.rev args with
    | h :: tl -> List.fold_left ~init:(Ast.pfun h e) ~f:(fun acc x -> Ast.pfun x acc) tl
    | _ -> e
  in
  let om =
    lift3
      omthd
      (token "method" *> token "private" *> return Private <|> token "method" *> return Public)
      patern
      helper
  in
  token "object"
  *> lift2 eobj (option Ast.Pany (parens patern)) (many (ov <|> om) <* token "end")
;;

(* s#{some_mthd} *)
let sinvk pexpr = lift2 esend pexpr (token "#" *> patern)

let pexpr =
  fix (fun pexpr ->
    let sube = choice [ parens pexpr; econst; eval ] in
    let send = sinvk sube in
    let eapp = eapp (send <|> sube) in
    let term = send <|> eapp <|> sube in
    let term = lbo (term <|> lift2 un_op neg term) mul_div in
    let term = lbo term add_sub in
    let term = lbo term cmp in
    let term = rbo term andop in
    let term = rbo term orop in
    choice
      [ ite pexpr pexpr pexpr
      ; plet pexpr
      ; prlet pexpr
      ; ematch pexpr
      ; pfun pexpr
      ; pobj pexpr
      ; term
      ])
;;

let del = (dsmcln <|> skip_whitespace) *> skip_whitespace
let prog = del *> many1 (pexpr <* del)
let parse = parse_string ~consume:All prog
