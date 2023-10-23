(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)
open Angstrom
open Ast
(* open Base *)

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space

let varname =
  satisfy (function
    | 'a' .. 'z' -> true
    | _ -> false)
;;

let conde = function
  | [] -> fail "empty conde"
  | h :: tl -> List.fold_left ( <|> ) h tl
;;

type dispatch =
  { apps : dispatch -> string Ast.t Angstrom.t
  ; single : dispatch -> string Ast.t Angstrom.t
  }

type error = [ `ParsingError of string ]

let pp_error ppf = function
  | `ParsingError s -> Format.fprintf ppf "%s" s
;;

let parse_lam =
  let single pack =
    fix (fun _ ->
      conde
        [ char '(' *> pack.apps pack <* char ')'
        ; ((string "λ" <|> string "\\") *> spaces *> varname
           <* spaces
           <* char '.'
           >>= fun var ->
           pack.apps pack >>= fun b -> return (Ast.Abs (String.make 1 var, b)))
        ; (varname <* spaces >>= fun c -> return (Ast.Var (String.make 1 c)))
        ])
  in
  let apps pack =
    many1 (spaces *> pack.single pack <* spaces)
    >>= function
    | [] -> fail "bad syntax"
    | x :: xs -> return @@ List.fold_left (fun l r -> Ast.App (l, r)) x xs
  in
  { single; apps }
;;

let parse str =
  match
    Angstrom.parse_string (parse_lam.apps parse_lam) ~consume:Angstrom.Consume.All str
  with
  | Result.Ok x -> Result.Ok x
  | Error er -> Result.Error (`ParsingError er)
;;

let string_of_char c = String.make 1 c

let word_parser =
  let is_head = function
    | 'a' .. 'z' -> true
    | '_' -> true
    | _ -> false
  in
  let is_tale = function
    | 'a' .. 'z' -> true
    | '0' .. '9' | '_' -> true
    | _ -> false
  in
  let* first = satisfy is_head in
  let* rest = take_while is_tale in
  return (string_of_char first ^ rest)
;;

let parens p = char '(' *> p <* char ')'
let braces p = char '{' *> p <* char '}'
let keyword kw = string kw
let parse1 p s = parse_string ~consume:All p s
let parser_func_name = keyword "func" *> spaces *> word_parser >>= fun s -> return s

let parse_type = function
  | String "int" -> IntegerType
  | String "string" -> StringType
  | String "bool" -> BooleanType
;;

let parse_func_args =
  parens
    (word_parser
     >>= fun a ->
     return a
     >>= fun t -> spaces *> word_parser >>= fun r -> return (t, parse_type (String r)))
;;

let parse_func_ret_type =
  spaces *> word_parser >>= fun r -> return (parse_type (String r))
;;

let parse_signature =
  parse_func_args
  >>= fun args1 ->
  parse_func_ret_type >>= fun type' -> return { args = [ args1 ]; ret = One type' }
;;

let digit_c =
  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  in
  satisfy is_digit
;;

let digit = digit_c >>= fun c -> return (Char.code c - Char.code '0')
let ident = word_parser >>= fun c -> return (Ident c)

let parse_cond =
  ident
  >>= fun l ->
  spaces *> string "==" *> spaces *> digit
  >>= fun r -> return (BinOp (l, Equal, Const (Int r)))
;;

let parse_func_call =
  spaces *>
  word_parser
  >>= fun n ->
  parens
    (spaces *> word_parser
     >>= fun par ->
     spaces *> char '-' *> spaces *> digit
     >>= fun dig -> return (BinOp (Ident par, Minus, Const (Int dig))))
  >>= fun bin -> return (Call (Ident n, [ bin ]))
;;

let parse_if =
  spaces *> keyword "if" *> spaces *> parse_cond
  >>= fun cond ->
  spaces *> braces (spaces *> keyword "return" *> spaces *> digit <* spaces)
  >>= fun ret1 ->
  return
    (Ifstmt
       ( cond
       , [ ReturnStatement (Some (Const (Int ret1))) ]
       , []))
;;

let parse_func_body =
  spaces *> braces (parse_if >>= fun i -> spaces *> keyword "return" *> spaces *> ident
  >>= fun id1 -> spaces *> char '*' *> spaces *> parse_func_call >>= fun ret2 -> return [StatementsBlock[i; ReturnStatement (Some (BinOp(id1, Asterisk, ret2)))]])
;;

let parse_func_declar =
  parser_func_name
  >>= fun n ->
  parse_signature
  >>= fun s -> parse_func_body >>= fun b -> return (FuncTopLevelDeclaration (n, s, b))
;;


let%test _ =
    Ok (
      FuncTopLevelDeclaration ("test", {args =["test", IntegerType]; ret=One IntegerType}, 
       
      [StatementsBlock
      [
        Ifstmt(
          BinOp(Ident"test", Equal, Const (Int 0)), 
          [ReturnStatement (Some (Const(Int 1)))], []
          );
      ReturnStatement(Some (BinOp(Ident"test", Asterisk, (Call(Ident"test", [BinOp(Ident "test", Minus, Const (Int 1))])))))
      ]
      ]
      )) =

  parse1 parse_func_declar 
  "func test(test int) int {
    if test == 0{
      return 1
    }
    return test * test(test-1)}" 