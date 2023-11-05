(** Copyright 2021-2023, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Angstrom

let empty_space = function

  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let lower_letter = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let upper_letter = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let ident_symbol = function
  | c -> lower_letter c || upper_letter c || digit c || Char.equal c '_'
;;

let keywords = function
  | "let"
  | "rec"
  | "fun"
  | "in"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false"
  | "match"
  | "with"
  | "function" -> true
  | _ -> false
;;

let take_empty = take_while empty_space
let take_empty1 = take_while1 empty_space
let token s = take_empty *> s
let token1 s = take_empty1 *> s
let stoken s = take_empty *> string s
let stoken1 s = take_empty1 *> string s
let brackets p = stoken "(" *> p <* stoken ")"
let quotes p = stoken "\"" *> p <* stoken "\""
let parens_or_not p = p <|> brackets p
let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc 
  in e >>= fun init -> go init
;;

(** Types constructor *)

let fint x =  FInt x
let fbool x = FBool x
let fstring x = FString x
let fnil = FNil
let funit = FUnit

(** Types parse *)

let parse_sign =
  choice [ stoken "+" *> return 1; stoken "-" *> return (-1); stoken "" *> return 1 ]
;;

let parse_fint =
  let parse_digit = take_while1 digit 
  in lift2 (fun s v -> fint (s * Int.of_string v)) parse_sign parse_digit
;;

let parse_fbool =
  lift (fun b -> fbool @@ Bool.of_string b) 
  (stoken "false" <|> stoken "true")
;;

let parse_fstring =
  lift
    (fun s -> fstring s)
    (quotes @@ take_while (fun c -> not (Char.equal c '"')))
;;

let parse_fnil = stoken "[]" *> return fnil
let parse_funit = stoken "()" *> return funit
let parse_types = parse_fint <|> parse_fbool <|> parse_fstring <|> parse_fnil <|> parse_funit

(** Ident parse *)

let parse_id =
  take_empty *> take_while1 ident_symbol
  >>= fun res ->
  if String.length res == 0
  then fail "Not identifier"
  else if keywords res
  then fail "You can not use keywords as vars"
  else if Char.is_digit @@ String.get res 0
  then fail "Identifier first sumbol is letter, not digit"
  else return res
;;

(** Expression constructors *)

let eifelse i t e = EIfElse (i, t, e)
let elet name body = ELet (name, body)
let eletrec name body = ELetRec (name, body)
let efun id body = EFun (id, body)
let eapp f a = EApp (f, a)
let evar x = EVar x

(** Expression parse *)

let parse_evar = evar <$> parse_id
let parse_etypes = parse_types >>| fun x -> ETypes x
let parse_arg = many @@ parens_or_not parse_evar
let parse_arg1 = many1 @@ parens_or_not parse_evar

type edispatch =
  {
    evar : edispatch -> expression t;
    etypes : edispatch -> expression t;
    eifelse : edispatch -> expression t;
    elet: edispatch -> expression t;
    eletrec: edispatch -> expression t;
    ebinaryop: edispatch -> expression t;
    eapp: edispatch -> expression t;
    expression: edispatch -> expression t
  }

let eifelse i expr = 
  take_empty *> lift3 eifelse
      (stoken "if" *> i)
      (stoken "then" *> expr)
      (stoken "else" *> expr)
;;

let construct_efun arg body =
  let rec helper = function 
    | [] -> body
    | hd :: tl -> efun hd (helper tl)
  in helper arg
;;

let parse_rec =
  stoken "let" *> option "false" (stoken1 "rec") >>| fun x -> x != "false"
;;

let eletfun parse_expr =
  take_empty
  *> lift4
        (fun r name arg body ->
          let body = construct_efun arg body 
          in if r then eletrec name body else elet name body)
        parse_rec
        parse_id
        parse_arg
        (stoken "=" *> parse_expr)
;;

(** Binary operations constructors *)

let ebinop op e1 e2 = eapp op (eapp e1 e2)
let ediv = ebinop @@ EBinaryOp Div
let emul = ebinop @@ EBinaryOp Mul
let eadd = ebinop @@ EBinaryOp Add
let esub = ebinop @@ EBinaryOp Sub
let eless = ebinop @@ EBinaryOp Less
let eleq = ebinop @@ EBinaryOp Leq 
let egre = ebinop @@ EBinaryOp Gre 
let egreq = ebinop @@ EBinaryOp Greq 
let emod = ebinop @@ EBinaryOp Mod
let eand = ebinop @@ EBinaryOp And
let eor = ebinop @@ EBinaryOp Or
let eeq = ebinop @@ EBinaryOp Eq

let parse_binaryop expr =
  let lvl1 = take_empty *> choice
    [ 
      string "*" *> return emul;
      string "/" *> return ediv; 
      string "%" *> return emod
    ]
  in let lvl2 =
    take_empty *> choice 
      [ 
        string "+" *> return eadd; 
        string "-" *> return esub
      ]
  in let lvl3 =
    take_empty *> choice
      [ 
        string ">=" *> return egreq;
        string ">" *> return egre; 
        string "<=" *> return eleq; 
        string "<" *> return eless
      ]
  in let lvl4 = take_empty *> string "=" *> return eeq; 
  in let lvl5 = take_empty *> string "&&" *> return eand 
  in let lvl6 = take_empty *> string "||" *> return eor  
  in let expr = chainl1 expr lvl1
  in let expr = chainl1 expr lvl2
  in let expr = chainl1 expr lvl3
  in let expr = chainl1 expr lvl4
  in let expr = chainl1 expr lvl5 
  in chainl1 expr lvl6
;;

let parse_eapp parse_expr =
  take_empty *> lift2
    (fun expr l -> let res = List.fold_left ~f:eapp ~init:expr l 
    in res)
    parse_expr (many (token1 parse_expr))
;;

let pack =
  let etypes _ = parse_etypes 
  in let evar _ = parse_evar 
  in let lets pack = choice [ pack.elet pack; pack.eletrec pack ] <* take_empty
  in let expression pack = choice 
    [
      lets pack;
      pack.eifelse pack;
      pack.eapp pack;
    ]
  in
  let eifelse pack = 
    fix @@ fun _ ->
      let parse_eifelse =
        parens_or_not @@ choice
          [
            pack.eifelse pack;
            pack.eapp pack;
          ]
      in eifelse parse_eifelse (pack.expression pack)
  in 
  let ebinaryop pack =
    fix @@ fun _ -> 
      let parse_ebinaryop = choice
        [
          brackets @@ pack.eifelse pack;
          brackets @@ pack.ebinaryop pack;
          brackets @@ pack.eapp pack;
          pack.etypes pack;
          pack.evar pack
        ]
      in parens_or_not @@ parse_binaryop parse_ebinaryop
  in let eapp pack =
    fix @@ fun _ -> 
      let parse_eapp_pack = choice
        [
          pack.ebinaryop pack;
          brackets @@ pack.eifelse pack;
          brackets @@ pack.eapp pack
        ]
      in parse_eapp parse_eapp_pack 
  in 
  let parse_lets pack = choice
    [
      pack.eapp pack;
      pack.eifelse pack
    ]
  in let elet pack = fix @@ fun _ -> eletfun @@ parse_lets pack
  in let eletrec pack = fix @@ fun _ -> eletfun @@ parse_lets pack
  in 
  {
    evar;
    etypes;
    eifelse;
    elet;
    eletrec;
    ebinaryop;
    eapp;
    expression
  }
;;

let parse_expression = pack.expression pack
let parse_program = many1 (token parse_expression <* token (many1 (stoken ";;")))
let parse_str p s = parse_string ~consume:All p s
let parse str = parse_str parse_program (String.strip str)


(** Tests *)

let parsed_result str parser show =
  match parse_str parser str with
  | Ok res -> Format.printf "%s" (show res)
  | Error e -> Format.printf "%s" e
;;

(** Types test*)

let%expect_test _ =
  parsed_result "777" parse_types show_types;
  [%expect {| (FInt 777) |}]
;;

let%expect_test _ =
  parsed_result "-777" parse_types show_types;
  [%expect {| (FInt -777) |}]
;;

let%expect_test _ =
  parsed_result "true" parse_types show_types;
  [%expect {| (FBool true) |}]
;;

let%expect_test _ =
  parsed_result "false" parse_types show_types;
  [%expect {| (FBool false) |}]
;;

let%expect_test _ =
  parsed_result "\"May the power be with us\"" parse_types show_types;
  [%expect {| (FString "May the power be with us") |}]
;;

let%expect_test _ =
  parsed_result "let rec fact n = if n = 1 then 1 else n * (fact (n - 1))" parse_expression show_expression;
  [%expect
  {|
  (ELetRec ("fact",
     (EFun ((EVar "n"),
        (EIfElse (
           (EApp ((EBinaryOp Eq), (EApp ((EVar "n"), (ETypes (FInt 1)))))),
           (ETypes (FInt 1)),
           (EApp ((EBinaryOp Mul),
              (EApp ((EVar "n"),
                 (EApp ((EVar "fact"),
                    (EApp ((EBinaryOp Sub),
                       (EApp ((EVar "n"), (ETypes (FInt 1))))))
                    ))
                 ))
              ))
           ))
        ))
     )) |}]
;;
;;

let%expect_test _ =
  parsed_result "()" parse_types show_types;
  [%expect {| FUnit |}]
;;
