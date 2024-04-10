(** Copyright 2023-2024, Efim Perevalov *)

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
let angle_brackets p = char '<' *> p <* char '>'
let quotes p = stoken "\"" *> p <* stoken "\""
let brackets_or_not p = p <|> brackets p

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

(* Ident parse *)
let parse_id =
  let length_id res = String.length res = 0 in
  let first_char res =
    (Char.is_digit @@ String.get res 0) || (Char.is_uppercase @@ String.get res 0)
  in
  take_empty *> take_while1 ident_symbol
  >>= fun res ->
  match length_id res, keywords res, first_char res with
  | true, _, _ -> fail "Not identifier"
  | _, true, _ -> fail "You can not use keywords as vars"
  | _, _, true -> fail "The first character of the identifier is a small letter"
  | _ -> return res
;;

(* Types constructor *)

let fint x = FInt x
let ffloat x = FFloat x
let fbool x = FBool x
let fstring x = FString x
let fnil = FNil
let funit = FUnit
let measure_type s = SMeasure_init s
let measure_float f m = Measure_float (f, m)

(** Types parse *)

let parse_fint =
  let parse_sign =
    choice [ stoken "+" *> return 1; stoken "-" *> return (-1); stoken "" *> return 1 ]
  in
  let parse_digit = take_while1 digit in
  lift2 (fun s v -> fint @@ (s * Int.of_string v)) parse_sign parse_digit
;;

let parse_fbool =
  lift (fun b -> fbool @@ Bool.of_string b) (stoken "false" <|> stoken "true")
;;

let parse_fstring =
  lift (fun s -> fstring s) (quotes @@ take_while (fun c -> not (Char.equal c '"')))
;;

let parse_ffloat =
  let parse_sign_float =
    choice
      [ stoken "+" *> return 1.0; stoken "-" *> return (-1.0); stoken "" *> return 1.0 ]
  in
  let parse_digit = take_while1 digit in
  let parse_decimal = stoken "." *> take_while digit in
  lift3
    (fun s int_part fraction_part ->
      let float_value = float_of_string (int_part ^ "." ^ fraction_part) in
      ffloat @@ (s *. float_value))
    parse_sign_float
    parse_digit
    parse_decimal
;;

let parse_fnil = stoken "[]" *> return fnil
let parse_funit = stoken "()" *> return funit

(* Parsing initialization measure single: [<Measure>] type m*)

let parse_pow =
  take_empty *> lift (fun n -> Pow n) (string "^" *> parse_fint <|> return (FInt 1))
;;

let parse_smeasure = lift2 (fun m p -> SMeasure (m, p)) parse_id parse_pow

let parse_measure_type =
  square_brackets (angle_brackets (stoken "Measure")) *> stoken "type" *> parse_smeasure
;;

let parse_init_smeasure = parse_measure_type >>| fun a -> SMeasure_init a

(* Parsing float + measure single: 7.77 <m>*)

let construct_float_measure = angle_brackets parse_smeasure

let parse_float_smeasure =
  lift2 (fun f m -> measure_float f m) parse_ffloat construct_float_measure
;;

(* Parsing initialization measure multiple: [<Measure>] type speed = m/sec *)

let parse_mbin_op =
  take_empty *> choice [ string "*" *> return Mul; string "/" *> return Div ]
  <|> return Mul
;;

let parse_mmeasure =
  fix (fun p ->
    lift3 (fun m1 op m2 -> MMeasure (m1, op, m2)) parse_smeasure parse_mbin_op p
    <|> parse_smeasure)
;;

let parse_init_mmeasure =
  lift2
    (fun typ meaning -> MMeasure_init (typ, meaning))
    (parse_measure_type <* stoken "=")
    parse_mmeasure
;;

(* Parsing float + measure multiple: 7.77 <sec/meters>*)

let parse_float_mmeasure =
  lift2 (fun f m -> Measure_float (f, m)) parse_ffloat (angle_brackets parse_mmeasure)
;;

let parse_fmeasure = parse_float_mmeasure <|> parse_float_smeasure
let parse_init_meassure = parse_init_mmeasure <|> parse_init_smeasure

let parse_types =
  parse_fmeasure
  <|> parse_ffloat
  <|> parse_fint
  <|> parse_fbool
  <|> parse_fstring
  <|> parse_fnil
  <|> parse_funit
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
  | [ f; s ] -> pcons f s
  | hd :: tl -> pcons hd (create_cons tl)
;;

let parse_cons parser constructor =
  lift2
    (fun a b -> constructor @@ (a :: b))
    (parser <* stoken "::")
    (sep_by1 (stoken "::") parser)
;;

let parse_tuple parser constructor =
  take_empty
  *> lift2
       (fun a b -> constructor @@ (a :: b))
       (parser <* stoken ",")
       (sep_by1 (stoken ",") parser)
  <* take_empty
;;

type pdispatch =
  { cons : pdispatch -> pattern t
  ; tuple : pdispatch -> pattern t
  ; tuple_brackets : pdispatch -> pattern t
  ; list : pdispatch -> pattern t
  ; value : pdispatch -> pattern t
  ; pattern : pdispatch -> pattern t
  }

let pack =
  let pattern pack =
    choice
      [ pack.cons pack
      ; pack.tuple pack
      ; pack.list pack
      ; pack.tuple_brackets pack
      ; pack.value pack
      ]
  in
  let parser pack =
    choice [ pack.list pack; pack.tuple_brackets pack; pack.value pack ]
  in
  let value _ = parse_pwild <|> parse_pvar <|> parse_pconst in
  let tuple_brackets pack =
    fix @@ fun _ -> take_empty *> (brackets @@ parse_tuple (parser pack) ptuple)
  in
  let tuple pack = fix @@ fun _ -> take_empty *> parse_tuple (parser pack) ptuple in
  let list pack =
    fix
    @@ fun _ -> plist <$> parse_plist @@ pack.pattern pack <|> brackets @@ pack.list pack
  in
  let cons pack = fix @@ fun _ -> take_empty *> parse_cons (parser pack) create_cons in
  { cons; tuple; tuple_brackets; list; value; pattern }
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
let parse_measure = parse_init_meassure >>| fun x -> EMeasure x
let parse_arg = many @@ brackets_or_not parse_pattern
let parse_arg1 = many1 @@ brackets_or_not parse_pattern

type edispatch =
  { evar : edispatch -> expression t
  ; econst : edispatch -> expression t
  ; eifelse : edispatch -> expression t
  ; elet : edispatch -> expression t
  ; ebinaryop : edispatch -> expression t
  ; etuple : edispatch -> expression t
  ; elist : edispatch -> expression t
  ; eapp : edispatch -> expression t
  ; efun : edispatch -> expression t
  ; ematch : edispatch -> expression t
  ; emeasure : edispatch -> expression t
  ; expression : edispatch -> expression t
  }

let eifelse i expr =
  take_empty
  *> lift3 eifelse (stoken "if" *> i) (stoken "then" *> expr) (stoken "else" *> expr)
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
  take_empty *> stoken "let" *> option "false" (stoken1 "rec")
  >>| fun x -> if String.( <> ) x "false" then "Rec" else "NotRec"
;;

let eletfun parse_expr =
  take_empty
  *> lift4
       (fun r name arg body ->
         let body = construct_efun arg body in
         elet r name body)
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

let parse_binop =
  take_empty
  *> choice
       [ string "=" *> return eeq
       ; string "<>" *> return eneq
       ; string "&&" *> return eand
       ; string "||" *> return eor
       ; string "*" *> return emul
       ; string "/" *> return ediv
       ; string "%" *> return emod
       ; string "+" *> return eadd
       ; string "-" *> return esub
       ; string ">=" *> return egreq
       ; string ">" *> return egre
       ; string "<=" *> return eleq
       ; string "<" *> return eless
       ]
  <* take_empty
;;

let parse_eapp parse_expr =
  take_empty
  *> lift2
       (fun expr l ->
         let res = List.fold ~f:eapp ~init:expr l in
         res)
       parse_expr
       (many (token parse_expr))
;;

let pack =
  let emeasure _ = parse_measure in
  let econst _ = parse_econst in
  let evar _ = parse_evar in
  let expression pack =
    choice
      [ pack.elet pack
      ; pack.eifelse pack
      ; pack.eapp pack
      ; pack.etuple pack
      ; pack.elist pack
      ; pack.efun pack
      ; pack.ematch pack
      ]
  in
  let eifelse pack =
    fix
    @@ fun _ ->
    let parse_eifelse =
      brackets_or_not
      @@ choice [ pack.eifelse pack; pack.eapp pack; pack.efun pack; pack.ematch pack ]
    in
    eifelse parse_eifelse (pack.expression pack)
  in
  let efun pack = brackets_or_not @@ fix @@ fun _ -> parse_efun @@ pack.expression pack in
  let ematch pack =
    fix
    @@ fun _ ->
    let ematch_parse =
      brackets_or_not @@ choice [ pack.eapp pack; pack.eifelse pack; pack.ematch pack ]
    in
    brackets_or_not @@ ematch ematch_parse (pack.expression pack)
  in
  let ebinaryop pack =
    fix
    @@ fun _ ->
    let exp =
      choice
        [ pack.emeasure pack
        ; pack.etuple pack
        ; pack.elist pack
        ; pack.econst pack
        ; brackets @@ pack.ebinaryop pack
        ; brackets @@ pack.ematch pack
        ; brackets @@ pack.eifelse pack
        ; brackets @@ pack.eapp pack
        ; pack.evar pack
        ]
    in
    chainl1 (parse_eapp exp) parse_binop <* take_empty
  in
  let eapp pack =
    fix
    @@ fun _ ->
    let parse_eapp_pack =
      choice
        [ pack.ebinaryop pack
        ; pack.etuple pack
        ; pack.efun pack
        ; brackets @@ pack.eifelse pack
        ; brackets @@ pack.eapp pack
        ; brackets @@ pack.ematch pack
        ]
    in
    parse_eapp parse_eapp_pack
  in
  let parse_let pack =
    choice
      [ pack.eapp pack
      ; pack.eifelse pack
      ; pack.etuple pack
      ; pack.elist pack
      ; pack.efun pack
      ; pack.ematch pack
      ]
  in
  let value pack =
    choice
      [ pack.etuple pack
      ; pack.evar pack
      ; pack.emeasure pack
      ; pack.econst pack
      ; pack.elist pack
      ]
  in
  let elet pack = fix @@ fun _ -> eletfun @@ parse_let pack in
  let etuple pack =
    fix @@ fun _ -> take_empty *> (brackets @@ parse_tuple (value pack) etuple)
  in
  let elist pack =
    fix
    @@ fun _ ->
    elist <$> parse_plist @@ pack.expression pack <|> brackets @@ pack.elist pack
  in
  { expression
  ; evar
  ; emeasure
  ; econst
  ; eifelse
  ; elet
  ; etuple
  ; elist
  ; ebinaryop
  ; eapp
  ; efun
  ; ematch
  }
;;

let parse_expression = pack.expression pack
let parse_program = many1 (token parse_expression <* token (many (stoken ";;")))
let parse_str p s = parse_string ~consume:All p s
let parser str = parse_str parse_program (String.strip str)
