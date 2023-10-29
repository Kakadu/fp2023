(** Copyright 2021-2023, Nikita Lukonenko and Furetur *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)
open Angstrom
open Ast

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = take_while is_space
let space1 = take_while1 is_space

let varname =
  satisfy (function
    | 'a' .. 'z' -> true
    | _ -> false)
;;

let conde = function
  | [] -> fail "empty conde"
  | h :: tl -> List.fold_left ( <|> ) h tl
;;

type error = [ `ParsingError of string ]

let pp_error ppf = function
  | `ParsingError s -> Format.fprintf ppf "%s" s
;;

let string_of_char c = String.make 1 c
let str_char = peek_char_fail >>= fun c -> return (string_of_char c)
let str_body = many str_char >>= fun r -> return (List.fold_left ( ^ ) "" r)
let str_const = char '"' *> str_body <* char '"' >>= fun s -> return (String s)

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let parse_int = take_while1 is_digit >>= fun r -> return (Int (int_of_string r))
let parse_int_not_const = take_while is_digit >>= fun r -> return (int_of_string r)

let word_parser =
  let is_ident_start = function
    | 'a' .. 'z' -> true
    | '_' -> true
    | _ -> false
  in
  let is_ident_mid = function
    | 'a' .. 'z' -> true
    | '0' .. '9' | '_' -> true
    | _ -> false
  in
  let* first = satisfy is_ident_start in
  let* rest = take_while is_ident_mid in
  return (string_of_char first ^ rest)
;;

let parens p = char '(' *> spaces *> p <* spaces <* char ')'
let braces p = char '{' *> p <* char '}'
let keyword kw = string kw
let parse1 p s = parse_string ~consume:Prefix p s
(* let parse_func_name = keyword "func" *> spaces *> word_parser >>= fun s -> return s *)

let ident = word_parser >>= fun c -> return (Identifier c)

let spend_char c =
  let* _ = option ' ' (char c) in
  return ()
;;

let comma_separated p =
  let* ps = sep_by (char ',' <* spaces) (p <* spaces) in
  let* _ = spend_char ',' in
  return ps
;;

(* ***********************************TYPES*********************************** *)

let type_name =
  word_parser
  >>= function
  | "int" -> return IntegerType
  | "string" -> return StringType
  | "bool" -> return BooleanType
;;

let parse_typ = space1 *> type_name >>= fun r -> return (One r)
let parse_option_res = option Void parse_typ

let arg_decl =
  let* name = word_parser <* space1 in
  let* typ = type_name in
  return (name, typ)
;;

let parse_args = parens (comma_separated arg_decl)

let signature =
  let* args = parse_args in
  let* ret = parse_option_res in
  return { args; ret }
;;

type expr_declar_dispatch =
  { (*expressions*)
    expression : expr_declar_dispatch -> expression t
  ; (* declarations *)
    top_level_declaration : expr_declar_dispatch -> top_level_declaration t
  ; func_declaration : expr_declar_dispatch -> func_declaration t
  ; (* statements *)
    block : expr_declar_dispatch -> block t
  ; statements : expr_declar_dispatch -> statements t
  }

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

type helper = Args of expression list

(* ***********************************EXPRESSIONS*********************************** *)
let ex_decl_disp =
  let parse_const_int = parse_int >>= fun r -> return (Const r) in
  let take_bin_op op x1 x2 = BinaryOp (x1, op, x2) in
  let sub = spaces *> char '-' *> spaces *> return (take_bin_op Minus) in
  let mul = spaces *> char '*' *> spaces *> return (take_bin_op Asterisk) in
  let eq = spaces *> string "==" *> spaces *> return (take_bin_op Equal) in
  let expression d =
    fix (fun expression ->
      let parse_fun_call =
        ident
        >>= fun n ->
        parens (comma_separated expression) >>= fun a -> return (FunCall (n, a))
      in
      let factor = parse_const_int <|> parse_fun_call <|> ident <|> parens expression in
      let term = chainl1 factor mul in
      let term = chainl1 term eq in
      chainl1 term sub)
  in
  (* ***********************************DECLARATIONS*********************************** *)
  let func_declaration d =
    keyword "func" *> spaces *> word_parser
    >>= fun n ->
    spaces *> signature >>= fun s -> spaces *> d.block d >>= fun b -> return (n, s, b)
  in
  let top_level_declaration d =
    fix @@ fun _ -> func_declaration d >>= fun r -> return (FuncTopLevelDeclaration r)
  in
  (* ***********************************STATEMENTS*********************************** *)
  let semi = char ';' in
  let ret_stmt d =
    let ret_void =
      let* _ = keyword "return" *> spaces *> semi in
      return (ReturnStatement None)
    in
    let ret_expr =
      let* expression = keyword "return" *> space1 *> expression d <* spaces <* semi in
      return (ReturnStatement (Some expression))
    in
    ret_expr <|> ret_void <?> "ReturnStatement"
  in
  let block d = braces (many (spaces *> d.statements d <* spaces)) in
  let block_stmt d =
    let* b = block d in
    return (StatementsBlock b)
  in
  let if_stmt d =
    let* cond = keyword "if" *> space1 *> expression d <* space1 in
    let* then_block = block d <* spaces in
    return (Ifstmt (cond, then_block, []))
  in
  let statements d = fix @@ fun _ -> ret_stmt d <|> block_stmt d <|> if_stmt d in
  { expression; func_declaration; top_level_declaration; block; statements }
;;

let expression = ex_decl_disp.expression ex_decl_disp
let func_decl = ex_decl_disp.func_declaration ex_decl_disp
let top_level_decl = ex_decl_disp.top_level_declaration ex_decl_disp
let block = ex_decl_disp.block ex_decl_disp
let stmt = ex_decl_disp.statements ex_decl_disp

let%test _ =
  Ok
    ( "fact"
    , { args = [ "x", IntegerType ]; ret = One IntegerType }
    , [ Ifstmt
          ( BinaryOp (Identifier "x", Equal, Const (Int 0))
          , [ ReturnStatement (Some (Const (Int 1))) ]
          , [] )
      ; ReturnStatement
          (Some
             (BinaryOp
                ( Identifier "x"
                , Asterisk
                , FunCall
                    (Identifier "f", [ BinaryOp (Identifier "x", Minus, Const (Int 1)) ])
                )))
      ] )
  = parse1
      func_decl
      "func fact(x int) int { \n\
      \    if x == 0 {\n\
      \        return 1;\n\
      \    }\n\
      \    return x * f(x-1);}"
;;
