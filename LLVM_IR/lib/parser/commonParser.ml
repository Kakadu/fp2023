(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Angstrom

let test_parse_res p str = Angstrom.parse_string ~consume:Consume.Prefix p str

let test_parse p show str =
  match test_parse_res p str with
  | Result.Error _ -> Format.printf "Error"
  | Result.Ok ast -> Format.printf "%s" (show ast)
;;

let whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let comment =
  char ';'
  *> return ()
  *> skip_while (function
    | '\n' -> false
    | _ -> true)
;;

let whitespaces = many (skip whitespace <|> comment)
let comma = whitespaces *> char ',' <* whitespaces

let str_integer =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
;;

let str_extended_integer =
  take_while1 (function
    | 'a' .. 'f' -> true
    | 'A' .. 'F' -> true
    | '0' .. '9' -> true
    | _ -> false)
;;

let varname_char = function
  | 'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | '.' | '$' | '_' | '-' | '+' -> true
  | _ -> false
;;

let parse_word =
  let satisfy_char = function
    | a when varname_char a -> true
    | '0' .. '9' -> true
    | _ -> false
  in
  take_while1 satisfy_char
;;

let word str =
  parse_word
  >>= fun parsed -> if parsed = str then return str else fail "Parser error: Wrong word"
;;

let parse_integer64 =
  let* prefix = choice [ string "0x"; string "0o"; string "0b"; return "" ] in
  let* int_body = str_extended_integer in
  match Int64.of_string_opt (String.concat "" [ prefix; int_body ]) with
  | Some x -> return x
  | None -> fail "Parser error: can't parse int"
;;

let parse_integer = parse_integer64 >>| Int64.to_int

let parse_named_name =
  lift2
    (fun first_char last_part -> String.make 1 first_char ^ last_part)
    (satisfy varname_char)
    (parse_word <|> return "")
;;

let parse_unnamed_name =
  lift2
    (fun name _ -> name)
    str_integer
    (peek_char
     >>= fun c ->
     match c with
     | Some c when varname_char c -> fail "Parser error: deprecated name"
     | _ -> return ())
;;

let%expect_test _ =
  test_parse parse_unnamed_name (fun f -> f) "543554";
  [%expect {| 543554 |}]
;;

let%expect_test _ =
  test_parse parse_unnamed_name (fun f -> f) "543554d";
  [%expect {| Error |}]
;;

let%expect_test _ =
  test_parse parse_unnamed_name (fun f -> f) "54,";
  [%expect {| 54 |}]
;;

let parse_name = parse_named_name <|> parse_unnamed_name

let parse_local_variable =
  whitespaces *> char '%' *> parse_name >>| fun name -> Ast.LocalVar name
;;

let parse_global_variable =
  whitespaces *> char '@' *> parse_name >>| fun name -> Ast.GlobalVar name
;;
