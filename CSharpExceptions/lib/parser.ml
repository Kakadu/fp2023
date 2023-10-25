(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)

(* *_ - parser that don't fail *)
(* *' - parser that don't advance input *)

open Base
open Angstrom
open Ast

(** @see <https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/>
      C# keywords *)

let is_type_as_keyword = function
  | "int" | "char" | "string" | "bool" -> true
  | _ -> false
;;

let is_keyword = function
  | "if"
  | "else"
  | "while"
  | "for"
  | "break"
  (*  *)
  | "class"
  | "new"
  | "return"
  | "base"
  (*  *)
  | "public"
  | "private"
  | "protected"
  | "static"
  | "override"
  (*  *)
  | "try"
  | "catch"
  | "finally"
  | "when"
  (*  *)
  | "null"
  | "void"
  | "true"
  | "false" -> true
  | tp -> is_type_as_keyword tp
;;

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let skip_spaces = take_while is_space
let skip_spaces1 = take_while1 is_space

let is_token = function
  | 'a' .. 'z' | '0' .. '9' | 'A' .. 'Z' | '_' -> true
  | _ -> false
;;

let if_value cond x =
  try cond x |> fun _ -> true with
  | _ -> false
;;

let is_int = if_value int_of_string
let is_bool = if_value bool_of_string

let is_nullable = function
  | '?' -> true
  | _ -> false
;;

(* let foo = fix (fun foo1 -> char 'x' *> foo1 <|> return ()) *)
(* let func = fix (fun foo -> string "x" >>= (fun x -> foo >>| fun t -> x ^ t) <|> return "") *)

let s_token = take_while1 is_token

(* let sign =
   peek_char
   >>= function
   | Some '+' -> advance 1 >>| fun _ -> "+"
   | Some '-' -> advance 1 >>| fun _ -> "-"
   | Some c when is_digit_char c -> return "+"
   | _ -> fail "Not a digit"
   ;; *)

let s_string =
  char '\"'
  *> take_till (function
    | '\"' -> true
    | _ -> false)
  <* char '\"'
  <|> fail "Not a string"
;;

let p_string = s_string >>= fun str -> return (VString str)
let s_char = char '\'' *> any_char <* char '\'' <|> fail "Not a char"
let p_char = s_char >>= fun c -> return (VChar c)

let p_number =
  s_token
  >>= fun str ->
  match is_int str with
  | true -> return (VInt (int_of_string str))
  | false -> fail "Not a number"
;;

let p_bool =
  s_token
  >>= fun str ->
  match is_bool str with
  | true -> return (VBool (bool_of_string str))
  | false -> fail "Not a bool"
;;

let p_ident =
  s_token
  >>= fun str ->
  match not (is_keyword str) with
  | true when not (Char.is_digit str.[0]) -> return (Name str)
  | _ -> fail "Not an ident"
;;

let base_type_converter = function
  | "int" -> return TInt
  | "char" -> return TChar
  | "bool" -> return TBool
  | _ -> fail "Not a keyword type (e.g. int)"
;;

let s_keyword =
  s_token
  >>= fun tp ->
  match is_type_as_keyword tp with
  | true -> return tp
  | false -> fail "Not a keyword type (e.g. int)"
;;

let s_is_nullable = skip_spaces *> char '?' *> return true <|> return false

let p_keyword_type =
  s_keyword
  >>= function
  | "string" -> return (TNullable TString)
  | base_type ->
    s_is_nullable
    >>= (function
    | true -> base_type_converter base_type >>| fun x -> TNullable (TBase x)
    | false -> base_type_converter base_type >>| fun x -> TNot_Nullable x)
;;

let p_var_type = p_keyword_type >>= fun x -> return (TVariable x)
let ep_spaces prs = skip_spaces *> prs
let convert_val_to_expr prs = ep_spaces prs >>| fun x -> EVal x
let ep_number = convert_val_to_expr p_number
let ep_char = convert_val_to_expr p_char
let ep_string = convert_val_to_expr p_string
let ep_bool = convert_val_to_expr p_bool
let ep_identifier = ep_spaces p_ident >>= fun x -> return (EIdentifier x)

let ep_member_ident =
  fix (fun member_foo ->
    ep_identifier
    >>= fun id ->
    skip_spaces *> char '.' *> member_foo
    >>| (fun id2 -> EMember_ident (id, id2))
    <|> return id)
;;

(* let ep_params = *)
(** replace with ep_params (foo stub) *)
let eP_PARAMS_STAB =
  ep_spaces
  @@ (char '('
      *> take_till (function
        | ')' -> true
        | _ -> false)
      *> return (EIdentifier (Name "PARAMS")))
;;

let ep_method_inv meth_ident =
  eP_PARAMS_STAB >>| fun args -> EMethod_invoke (meth_ident, args)
;;

let ep_method_fild = ep_member_ident >>= fun id -> ep_method_inv id <|> return id
let _ep_var_decl tp = skip_spaces1 *> p_ident >>| fun id -> EVar_decl (tp, id)

let ep_var_decl =
  ep_spaces
  @@ choice
       [ p_var_type >>= _ep_var_decl
       ; p_ident >>| (fun cl -> TVariable (TNullable (TClass cl))) >>= _ep_var_decl
       ]
;;

let parse str ~p =
  match parse_string p ~consume:Angstrom.Consume.All str with
  | Ok x -> Some x
  | Error _ -> None
;;

let n_parse p = parse_string p ~consume:Angstrom.Consume.Prefix

(* *******************->  tests  <-******************* *)
let eq_wrap ~eq ans = function
  | Some x when eq x ans -> true
  | _ -> false
;;

let show_wrap form = function
  | Some x -> Format.printf "%a@\n" form x
  | _ -> Format.print_string "Some error during parsing\n"
;;

let test_pars ps eq str ans = eq_wrap ~eq ans (parse ~p:ps str)
let print_pars ps form str = show_wrap form (parse ~p:ps str)

(* let%test_unit _ = print_pars func *)

(* p_num tests: *)
let test_num = test_pars p_number equal_value_

(* true *)
let%test _ = test_num "666" (VInt 666)
let%test _ = test_num "6__6_6" (VInt 666)
let%test _ = test_num "0" (VInt 0)

(* false *)
let%test _ = not (test_num "-0" (VInt 0))
let%test _ = not (test_num "adas" (VInt 0))
let%test _ = not (test_num "_123" (VInt 0))

(* p_str tests: *)
let test_str = test_pars p_string equal_value_

(* true *)
let%test _ = test_str "\"666\"" (VString "666")
let%test _ = test_str "\"6\n6\"" (VString "6\n6")

(* false *)
let%test _ = not (test_str "\n6\"" (VString "6\n6"))
let%test _ = not (test_str "\"6\n" (VString "6\n6"))
let%test _ = not (test_str "6" (VString "6\n6"))

(* p_str tests: *)
let test_chr = test_pars p_char equal_value_

(* true *)
let%test _ = test_chr "\'\"\'" (VChar '\"')
let%test _ = test_chr "\'\'\'" (VChar '\'')
let%test _ = test_chr "\'\n\'" (VChar '\n')
let%test _ = test_chr "\'@\'" (VChar '@')

(* false *)
let%test _ = not (test_chr "\n\'" (VChar '\n'))
let%test _ = not (test_chr "\'@" (VChar '@'))
let%test _ = not (test_chr "@" (VChar '@'))

(* p_bool tests: *)
let test_bool = test_pars p_bool equal_value_

(* true *)
let%test _ = test_bool "true" (VBool true)
let%test _ = test_bool "false" (VBool false)

(* false *)
let%test _ = not (test_bool "@das" (VBool false))
let%test _ = not (test_bool "das" (VBool false))
