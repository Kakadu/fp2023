(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* TODO: implement parser here *)
open Base
open Angstrom
open Ast

let space =
  skip_while (function
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false)
;;

(** @see <https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/>
      C# keywords *)
let is_key_word = function
  | "if"
  | "else"
  | "while"
  | "break"
  (*  *)
  | "int"
  | "double"
  | "char"
  | "string"
  | "null"
  | "void"
  (*  *)
  | "public"
  | "private"
  | "protected"
  | "static"
  (*  *)
  | "class"
  | "return"
  | "base" -> true
  | _ -> false
;;

let is_digit = Char.is_digit

let sign =
  peek_char
  >>= function
  | Some '+' -> advance 1 >>| fun _ -> "+"
  | Some '-' -> advance 1 >>| fun _ -> "-"
  | Some c when is_digit c -> return "+"
  | _ -> fail "Not a digit"
;;

(* let foo = fix (fun foo1 -> char 'x' *> foo1 <|> return ()) *)

let type_literal = function
  | 'd' -> Some Double_lit
  | _ -> None
;;

let next' c = peek_char >>= c
let is_next' c : bool t = next' c

let dot = function
  | Some '.' -> return true
  | _ -> return false
;;

let double_literal = function
  | Some x when type_literal x == Some Double_lit -> return true
  | _ -> return false
;;

let literal = function
  | Some x ->
    (match type_literal x with
     | Some _ -> return true
     | None -> return false)
  | None -> return false
;;

let s_digit = take_while1 is_digit
let s_integer = sign >>= fun sign_part -> s_digit >>| fun d_part -> sign_part ^ d_part

let _d_part_pars =
  is_next' dot
  >>= function
  | true -> char '.' *> s_digit >>| fun double_part -> Some ("." ^ double_part)
  | false -> return None
;;

let _lit_part_pars =
  is_next' literal
  >>= function
  | true ->
    any_char
    >>| (function
    | 'd' -> Some Double_lit
    | _ -> None)
  | false -> return None
;;

let s_number =
  lift3 (fun int_p fp_part lit -> int_p, fp_part, lit) s_integer _d_part_pars _lit_part_pars
;;

let p_number =
  s_number
  >>| function
  | int_p, Some fp_p, Some Double_lit | int_p, Some fp_p, None ->
    VBase (VDouble (float_of_string (int_p ^ fp_p)))
  | int_p, None, Some Double_lit -> VBase (VDouble (float_of_string (int_p ^ ".")))
  | int_p, None, None -> VBase (VInt (int_of_string int_p))
;;

(* let number =
   s_integer >>= (fun int_part ->
   let int_part = sign_part^digit_part in
   let literal = function is_next' doub_literal
   is_next' dot >>= function
   | true -> char '.' *> s_digit >>| (fun fp_part -> int_part^"."^fp_part)
   | false -> is_next' doub_literal >>=
   )) *)

(* let is_next' = *)

(* let integer = *)

(* let int_ = *)

(* let value_p = *)


let parse str =
  match parse_string p_number ~consume:Angstrom.Consume.All str with
  | Ok x -> Format.printf "%a@\n"  pp_value_ x
  | Error er -> prerr_endline er
;;

(* let data_line =
   let some x = Some x in
   lift3 (fun x y z -> (x, y, z)
   (lex key)
   (lex value)
   (lex (option None (comment >>| some)))) *)

(* let line =
   whitespace *>
   choice
   [ lex comment   >>| fun comment    -> `Comment comment
    ; data_line >>| fun (k, v, c)  -> `Data(k, v, c) ] *)

let%test_unit _ = parse "1000" 
let%test_unit _ = parse "-1000" 
let%test_unit _ = parse "-1000.0" 
let%test_unit _ = parse "-1000.012" 
let%test_unit _ = parse "-1.012" 
let%test_unit _ = parse "20.20" 
(*  *)
let%test_unit _ = parse "-" 
let%test_unit _ = parse ".qe" 
