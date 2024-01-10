(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Angstrom
open Ast

let skip_whitespaces = skip_while Char.is_whitespace

let parse_comments =
  skip_whitespaces *> string "(*"
  *> many_till any_char (string "*)")
  *> return ()

let ws = many parse_comments *> skip_whitespaces

let ws1 =
  let skip_whitespaces1 = take_while1 Char.is_whitespace *> return () in
  (skip_whitespaces1 *> many parse_comments <|> many1 parse_comments)
  *> return ()

(* ======= Operator names ======= *)

let is_core_operator_char = function
  | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '>' | '@' | '^' | '|' ->
      true
  | _ ->
      false

let is_operator_char = function
  | '~' | '!' | '?' | '%' | '<' | ':' | '.' ->
      true
  | _ as x when is_core_operator_char x ->
      true
  | _ ->
      false

let parse_custom_prefix_operator_name =
  let parse_prefix1 =
    (* ! { operator-char } *)
    char '!' *> take_while is_operator_char >>| fun s -> "!" ^ s
  in
  let parse_prefix2 =
    (* (?|~) { operator-char }+ *)
    let parse_first =
      satisfy (fun c -> Char.equal '?' c || Char.equal '~' c) >>| String.of_char
    in
    let parse_rest = take_while1 is_operator_char in
    lift2 String.( ^ ) parse_first parse_rest
  in
  parse_prefix1 <|> parse_prefix2

let peek_custom_infix_operator_name =
  (* (core-operator-char | % | <) { operator-char } *)
  let peek_first =
    peek_char_fail
    >>= fun c ->
    if is_core_operator_char c || Char.equal c '%' || Char.equal c '<' then
      return (String.of_char c)
    else fail "not a infix-symbol"
  in
  let rec peek_rest acc index =
    peek_string index
    >>| (fun s -> String.get s (String.length s - 1)) (* get last char *)
    >>= fun c ->
    if is_operator_char c then peek_rest (acc ^ String.of_char c) (index + 1)
    else return acc
  in
  lift2 String.( ^ ) peek_first (peek_rest "" 2)

let parse_custom_operator_name =
  parse_custom_prefix_operator_name
  <|> ( peek_custom_infix_operator_name
      >>= fun name -> advance (String.length name) *> return name )

(* ======= Value names ======= *)

let is_keyword = function
  (* https://v2.ocaml.org/releases/5.1/htmlman/lex.html#sss:keywords *)
  | "and"
  | "else"
  | "exception"
  | "false"
  | "fun"
  | "function"
  | "if"
  | "in"
  | "let"
  | "match"
  | "try"
  | "rec"
  | "then"
  | "true"
  | "with" ->
      true
  | _ ->
      false

let parse_lowercase_ident =
  let parse_first =
    satisfy (function 'a' .. 'z' | '_' -> true | _ -> false)
    >>| String.of_char
  in
  let parse_rest =
    take_while (function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' ->
          true
      | _ ->
          false )
  in
  lift2 String.( ^ ) parse_first parse_rest
  >>= fun name ->
  if not (is_keyword name) then return name
  else fail (name ^ " keyword can't be used as ident")

let parse_capitalized_ident =
  let parse_first =
    satisfy (function 'A' .. 'Z' -> true | _ -> false) >>| String.of_char
  in
  let parse_rest =
    take_while (function
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' ->
          true
      | _ ->
          false )
  in
  lift2 String.( ^ ) parse_first parse_rest

let parse_constr_name =
  let parse_bool_constr = string "true" <|> string "false" in
  let parse_unit_constr = string "()" in
  choice [parse_capitalized_ident; parse_bool_constr; parse_unit_constr]

let parse_value_name =
  parse_lowercase_ident
  <|> (char '(' *> ws *> parse_custom_operator_name <* ws <* char ')')

(* ======= Constants ======= *)

let parse_int =
  take_while1 (function '0' .. '9' -> true | _ -> false)
  >>| fun i -> Const_integer (Int.of_string i)

let parse_string =
  let parse_string_literal1 =
    (* [{id|hello_world|id}] *)
    let ident = take_while (function 'a' .. 'z' | '_' -> true | _ -> false) in
    char '{' *> ident
    >>= fun id ->
    char '|' *> many_till any_char (string ("|" ^ id ^ "}"))
    >>| fun s -> Const_string (String.of_char_list s)
  in
  let parse_string_literal2 =
    (* ["hello world"] *)
    let open Char in
    char (of_int_exn 34) *> take_till (Char.equal (of_int_exn 34))
    <* char (of_int_exn 34) (* [of_int_exn 34 = '"'] *)
    >>| fun s -> Const_string s
  in
  parse_string_literal2 <|> parse_string_literal1

let parse_char = char '\'' *> any_char <* char '\'' >>| fun c -> Const_char c

let parse_const = choice [parse_char; parse_string; parse_int]

(* ======= Value bindings ======= *)

(**
  [P1 = E1 and P2 = E2 and ...]
  [ValName1 PArg1 = E1 and P1 = E2 and ...]
*)
let parse_value_bindings pexp ppat =
  let parse_binding =
    let parse_fun_binding =
      lift3
        (fun name args exp ->
          {pat= Pat_var (Ident name); expr= Exp_fun (args, exp)} )
        (ws *> parse_value_name)
        (ws *> sep_by1 ws ppat)
        (ws *> char '=' *> pexp)
    in
    parse_fun_binding
    <|> lift2 (fun pat expr -> {pat; expr}) ppat (ws *> char '=' *> pexp)
  in
  sep_by1 (ws *> string "and") parse_binding

let parse_let_binding pexp ppat =
  let parse_rec_flag =
    ws1 *> option Nonrecursive (string "rec" *> ws1 *> return Recursive)
  in
  ws *> string "let" *> both parse_rec_flag (parse_value_bindings pexp ppat)

(* ======= Prefix & infix operators parsing ======= *)

type ('oprnd, 'op) prefix_helpers =
  {parse: 'op t; get_binding_power: 'op -> int; apply: 'op -> 'oprnd -> 'oprnd}

type 'op infix_operator = {op: 'op; op_length: int}

type ('oprnd, 'op) infix_helpers =
  { peek: 'op infix_operator t
  ; get_binding_power: 'op -> int * int
  ; fold: 'oprnd -> 'op * 'oprnd -> 'oprnd }

let parse_operators ?prefix ?infix parse_operand =
  (*
     Pratt parsing
     https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
  *)
  let rec helper min_bp =
    let* lhs =
      (* check if {prefix} supplied *)
      Option.value_map prefix ~default:parse_operand ~f:(fun prefix ->
          option None (ws *> prefix.parse >>| Option.some)
          >>= fun op ->
          (* check if prefix op parsed successfully *)
          Option.value_map op ~default:parse_operand ~f:(fun op ->
              let r_bp = prefix.get_binding_power op in
              let* rhs = helper r_bp in
              return (prefix.apply op rhs) ) )
    in
    (* check if {infix} supplied *)
    Option.value_map infix ~default:(return lhs) ~f:(fun infix ->
        many
          (let* {op; op_length} = ws *> infix.peek in
           let l_bp, r_bp = infix.get_binding_power op in
           if l_bp < min_bp then fail "found op with lower binding power"
           else
             advance op_length
             *> let* rhs = helper r_bp in
                return (op, rhs) )
        >>| fun results -> List.fold_left results ~init:lhs ~f:infix.fold )
  in
  helper 0
