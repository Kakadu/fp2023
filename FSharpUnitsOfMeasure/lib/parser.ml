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
  | "function"
  | "Measure" -> true
  | _ -> false
;;

let take_empty = take_while empty_space
let take_empty1 = take_while1 empty_space
let token s = take_empty *> s
let token1 s = take_empty1 *> s
let stoken s = take_empty *> string s
let stoken1 s = take_empty1 *> string s
let brackets p = stoken "(" *> p <* stoken ")"
let square_brackets p = stoken "[" *> p <* stoken "]"
let angle_brackets p = stoken "<" *> p <* stoken ">"
let quotes p = stoken "\"" *> p <* stoken "\""
let parens_or_not p = p <|> brackets p
let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc 
  in e >>= fun init -> go init
;;
 
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
  else if Char.is_uppercase @@ String.get res 0
    then fail "Identifier first sumbol is not small letter"
  else return res
;;

(** Types constructor *)

let fint s x =  FInt (s, x)
let ffloat s x = FFloat (s, x)
let fbool x = FBool x
let fstring x = FString x
let fnil = FNil
let funit = FUnit
let measure_type s = Measure_init s
let measure_float f m = Measure_float (f, m)

(** Types parse *)

let parse_fint =
  let parse_sign = choice [ stoken "+" *> return Plus; stoken "-" *> return Minus; stoken "" *> return Plus ] <* take_empty
  in let parse_digit = take_while1 digit 
  in lift2 (fun s v -> fint s (Int.of_string v)) parse_sign parse_digit
;;

let parse_fbool =
  lift (fun b -> fbool @@ Bool.of_string b) 
  (stoken "false" <|> stoken "true")
;;

let parse_fstring =
  lift (fun s -> fstring s)
  (quotes @@ take_while (fun c -> not (Char.equal c '"')))
;;


let parse_ffloat = 
  let parse_sign_float = choice [ stoken "+" *> return Plus; stoken "-" *> return Minus; stoken "" *> return Plus ]
  in let parse_digit = take_empty *> take_while digit
  in let parse_decimal = stoken "." *> take_while digit 
  in lift3 (fun s int_part fraction_part ->
    let float_value = float_of_string (int_part ^ "." ^ fraction_part)
    in ffloat s float_value)
  parse_sign_float
  parse_digit
  parse_decimal
;;

let parse_fnil = stoken "[]" *> return fnil
let parse_funit = stoken "()" *> return funit

(* Parsing initialization measure single: [<Measure>] type m*)

let parse_measure_single = 
  parse_id >>| fun x -> Measure_single x
;;

let parse_measure_type = 
  square_brackets (angle_brackets (stoken "Measure")) *> stoken "type" *> parse_measure_single
;;

let parse_init_measure_single = parse_measure_type >>| fun a -> Measure_init a

(* Parsing float + measure single: 7.77 <m>*)

let construct_float_measure = take_empty *> (angle_brackets @@ parse_measure_single)

let parse_float_measure_single = 
  lift2 (fun f m -> (measure_float f m))
  parse_ffloat
  construct_float_measure
;;

(* Parsing initialization measure double: [<Measure>] type speed = m/sec *)


let parse_bin_op = take_empty *> (choice 
  [ 
    string "*" *> return Mul;
    string "/" *> return Div
  ]) <|> return Mul
;;

let parse_measure_multiple =
  fix (fun p ->
    lift3 (fun m1 op m2 -> Measure_multiple (m1, op, m2))
      parse_measure_single
      parse_bin_op
      p <|> parse_measure_single
  )
;;

let parse_init_measure_multiple = 
  lift2 (fun typ meaning -> Measure_multiple_init (typ, meaning)) 
  (parse_measure_type <* stoken "=")
  parse_measure_multiple
;;

(* Parsing float + measure multiple: 7.77 <sec/meters>*)

let parse_float_measure_multiple = 
  lift2 (fun f m -> (Measure_float (f, m)))
  parse_ffloat
  (take_empty *> angle_brackets parse_measure_multiple)
;;

let parse_fmeasure = 
  parse_float_measure_multiple <|> parse_float_measure_single
;;

let parse_meassure_init =
  parse_init_measure_multiple <|> parse_init_measure_single
;;

let parse_types = 
  parse_fmeasure <|> parse_ffloat <|> parse_fint <|> parse_fbool <|> parse_fstring <|> parse_fnil <|> parse_funit 
;;

(** Pattern constructor *)

let pwild = PWild
let pconst x = PConst x
let pvar x = PVar x
let ptuple z = PTuple z
let plist l = PList l
let pcons ht tl = PCons (ht, tl)

(** Pattern parse *)

let parse_pconst = parse_types >>| fun x -> pconst x 
let parse_pvar = parse_id >>| fun x -> pvar x
let parse_pwild = stoken "_" *> return pwild
let parse_plist l = square_brackets (sep_by1 (stoken ";") l)

let rec create_cons = function
  | [] -> pconst fnil
  | hd :: [] when equal_pattern hd (pconst fnil) -> pconst fnil
  | [f; s] -> pcons f s
  | hd :: tl -> pcons hd (create_cons tl)
;;

let parse_cons parser constructor =
  lift2
    (fun a b -> constructor @@ (a :: b))
    (parser <* stoken "::")
    (sep_by1 (stoken "::") parser)
;;

let parse_tuple parser constructor =
  lift2
    (fun a b -> constructor @@ (a :: b))
    (parser <* stoken ",")
    (sep_by1 (stoken ",") parser)
;;

type pdispatch =
  { 
    cons: pdispatch -> pattern t;
    tuple : pdispatch -> pattern t; 
    tuple_brackets : pdispatch -> pattern t;
    list: pdispatch -> pattern t;
    value : pdispatch -> pattern t; 
    pattern : pdispatch -> pattern t
  }

let pack = 
  let pattern pack = choice 
    [
      pack.cons pack;
      pack.tuple pack;
      pack.list pack;
      pack.tuple_brackets pack;
      pack.value pack
    ]
  in 
  let parser pack = choice 
    [
      pack.list pack;
      pack.tuple_brackets pack;
      pack.value pack
    ]  
  in 
  let value _ = parse_pwild <|> parse_pvar <|> parse_pconst
  in let tuple_brackets pack = fix @@ fun _ -> take_empty *> (brackets @@ parse_tuple (parser pack) ptuple)
  in let tuple pack = fix @@ fun _ -> take_empty *> parse_tuple (parser pack) ptuple
  in let list pack = fix @@ fun _ -> plist <$> parse_plist @@ pack.pattern pack <|> brackets @@ pack.list pack
  in let cons pack = fix @@ fun _ -> take_empty *> parse_cons (parser pack) create_cons
  in 
    {
      cons;
      tuple;
      tuple_brackets;
      list;
      value;
      pattern
    } 
;;

let parse_pattern = pack.pattern pack

(** Expression constructor *)

let eifelse i t e = EIfElse (i, t, e)
let elet is_rec name body = ELet (is_rec, name, body)
let etuple z = ETuple z
let elist l = EList l
let efun id body = EFun (id, body)
let eapp f a = EApp (f, a)
let evar x = EVar x
let ematch c pl = EMatch (c, pl)

(** Expression parse *)

let parse_evar = parse_id >>| evar 
let parse_econst = parse_types >>| fun x -> EConst x
let parse_measure = parse_meassure_init >>| fun x -> EMeasure x
let parse_arg = many @@ parens_or_not parse_pattern
let parse_arg1 = many1 @@ parens_or_not parse_pattern

type edispatch =
  {
    evar : edispatch -> expression t;
    econst : edispatch -> expression t;
    eifelse : edispatch -> expression t;
    elet: edispatch -> expression t;
    ebinaryop: edispatch -> expression t;
    etuple: edispatch -> expression t;
    elist: edispatch -> expression t;
    eapp: edispatch -> expression t;
    efun: edispatch -> expression t;
    ematch: edispatch -> expression t;
    emeasure: edispatch -> expression t;
    expression: edispatch -> expression t
  }

let eifelse i expr = 
  take_empty *> lift3 eifelse
    (stoken "if" *> i)
    (stoken "then" *> expr)
    (stoken "else" *> expr)
;;

let construct_efun arg body =
  let helper hd acc = efun hd acc in
  List.fold_right arg ~f:helper ~init:body
;;

let parse_efun expr =
  take_empty
  *> lift2
       (fun arg exp -> construct_efun arg exp)
       (stoken "fun" *> parse_arg1)
       (stoken "->" *> expr)
;;

let parse_rec =
  take_empty *> stoken "let" *> option "false" (stoken1 "rec") >>| fun x -> if x != "false" then "Rec" else "NotRec";;
;;

let eletfun parse_expr =
  take_empty *> lift4
    (fun r name arg body ->
      let body = construct_efun arg body 
      in elet r name body )
    parse_rec
    parse_id
    parse_arg
    (stoken "=" *> parse_expr)
;;

let ematch matching parse_expr =
  let wand = stoken "|" *> parse_pattern in
  let arrow = stoken "->" *> parse_expr in
  let expr = lift2 (fun value arrow_value -> value, arrow_value) wand arrow in
  let exprs = many1 expr in
  let pematch = stoken "match" *> matching <* stoken "with" in
  take_empty *> lift2 ematch pematch exprs
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
let eneq = ebinop @@ EBinaryOp Neq

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
  in let lvl4 = take_empty *> choice [string "=" *> return eeq; string "<>" *> return eneq] 
  in let lvl5 = take_empty *> string "&&" *> return eand 
  in let lvl6 = take_empty *> string "||" *> return eor  
  in let expr = chainl1 expr lvl1
  in let expr = chainl1 expr lvl2
  in let expr = chainl1 expr lvl3
  in let expr = chainl1 expr lvl4
  in let expr = chainl1 expr lvl5 
  in chainl1 expr lvl6 <* take_empty
;;

let parse_eapp parse_expr =
  take_empty *> lift2
    (fun expr l -> let res = List.fold_left ~f:eapp ~init:expr l 
    in res)
    parse_expr (many (token parse_expr))
;;

let pack =
  let emeasure _ = parse_measure 
  in let econst _ = parse_econst
  in let evar _ = parse_evar 
  in let expression pack = choice 
    [
      pack.elet pack;
      pack.eifelse pack;
      pack.eapp pack;
      pack.etuple pack;
      pack.elist pack;
      pack.efun pack;
      pack.ematch pack
    ]
  in
  let eifelse pack = 
    fix @@ fun _ ->
      let parse_eifelse =
        parens_or_not @@ choice
          [
            pack.eifelse pack;
            pack.eapp pack;
            pack.efun pack;
            pack.ematch pack;
          ]
      in eifelse parse_eifelse (pack.expression pack)
  in let efun pack = parens_or_not @@ fix @@ fun _ -> parse_efun @@ pack.expression pack
  in let ematch pack =
    fix @@ fun _ ->
      let ematch_parse =
        parens_or_not @@ choice 
          [ 
            pack.eapp pack; 
            pack.eifelse pack; 
            pack.ematch pack 
          ]
      in
      parens_or_not @@ ematch ematch_parse (pack.expression pack)
  in 
  let ebinaryop pack =
    fix @@ fun _ -> 
      let parse_ebinaryop = choice
        [
          pack.emeasure pack;
          pack.econst pack;
          brackets @@ pack.ematch pack;
          brackets @@ pack.eifelse pack;
          brackets @@ pack.ebinaryop pack;
          brackets @@ pack.eapp pack;
          pack.evar pack
        ]
      in parens_or_not @@ parse_binaryop parse_ebinaryop
  in let eapp pack =
    fix @@ fun _ -> 
      let parse_eapp_pack = choice
        [

          pack.etuple pack;
          pack.ebinaryop pack;
          pack.efun pack; 
          brackets @@ pack.eifelse pack;
          brackets @@ pack.eapp pack;
          brackets @@ pack.ematch pack
        ]
      in parse_eapp parse_eapp_pack 
  in 
  let parse_lets pack = choice
    [
      pack.eapp pack;
      pack.eifelse pack;
      pack.etuple pack;
      pack.elist pack;
      pack.efun pack;
      pack.ematch pack
    ]
  in 
  let value pack = choice
    [ 
      pack.evar pack;
      pack.emeasure pack;
      pack.econst pack;
      pack.etuple pack;
      pack.elist pack
    ]
  in let elet pack = fix @@ fun _ -> eletfun @@ parse_lets pack
  in let etuple pack = fix @@ fun _ -> brackets (parse_tuple (value pack) etuple)
  in let elist pack = fix @@ fun _ -> elist <$> parse_plist @@ pack.expression pack <|> brackets @@ pack.elist pack
  in 
  {
    evar;
    emeasure;
    econst;
    eifelse;
    elet;
    etuple;
    elist;
    ebinaryop;
    eapp;
    efun;
    ematch;
    expression
  }
;;

let parse_expression = pack.expression pack
let parse_program = many1 (token parse_expression <* token (many1 (stoken ";;")))
let parse_str p s = parse_string ~consume:All p s
let parse str = parse_str parse_program (String.strip str)