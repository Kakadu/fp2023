(* Requires [1rbmap.ml] and [2parser.ml] *)

(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Arithmetic expressions interpreter *)

type expr =
  | Const of int
  | Var of string
  | Plus of expr * expr
  | Minus of expr * expr
  | Asterisk of expr * expr
  | Slash of expr * expr

let parse =
  let ws = take_while char_is_whitespace *> return () in
  let parens p = char '(' *> p <* char ')' in

  let const =
    take_while1 char_is_digit
    >>| (fun str ->
          try int_of_string str with Invalid_argument -> unreachable () )
    >>| fun x -> Const x
  in
  let var = take_while1 char_is_alpha >>| fun x -> Var x in

  let plus = char '+' *> return (fun x y -> Plus (x, y)) in
  let minus = char '-' *> return (fun x y -> Minus (x, y)) in
  let asterisk = char '*' *> return (fun x y -> Asterisk (x, y)) in
  let slash = char '/' *> return (fun x y -> Slash (x, y)) in

  let expr =
    fix (fun expr ->
        let factor = ws *> choice [parens expr; const; var] <* ws in
        let term = chainl1 factor (asterisk <|> slash) in
        chainl1 term (plus <|> minus) )
  in
  run expr

(* Parsing tests *)
let () =
  assert (parse "1 + 2 + 3" = Some (Plus (Plus (Const 1, Const 2), Const 3))) ;

  assert (
    parse "1 + (2 + x) * 4"
    = Some (Plus (Const 1, Asterisk (Plus (Const 2, Var "x"), Const 4))) ) ;

  assert (
    parse "x + y / z * 2"
    = Some (Plus (Var "x", Asterisk (Slash (Var "y", Var "z"), Const 2))) )

type eval_err = Syntax_error | Div_by_zero | Unbound_variable of string

let eval expr =
  (* Reader-Error monad *)
  let return x st = (st, Ok x) in
  let fail err st = (st, Error err) in

  let ( >>= ) m f st =
    let st, r = m st in
    match r with Error x -> (st, Error x) | Ok x -> f x st
  in

  let read st = (st, Ok st) in
  let run f st = snd (f st) in

  let rec helper = function
    | Const x ->
        return x
    | Var x -> (
        read
        >>= fun env ->
        try return (map_find string_compare x env)
        with Not_found -> fail (Unbound_variable x) )
    | Plus (x, y) ->
        helper x >>= fun x -> helper y >>= fun y -> return (x + y)
    | Minus (x, y) ->
        helper x >>= fun x -> helper y >>= fun y -> return (x - y)
    | Asterisk (x, y) ->
        helper x >>= fun x -> helper y >>= fun y -> return (x * y)
    | Slash (x, y) -> (
        helper x
        >>= fun x ->
        helper y
        >>= fun y ->
        try return (x / y) with Division_by_zero -> fail Div_by_zero )
  in

  run (helper expr)

let interp str env =
  match parse str with None -> Error Syntax_error | Some expr -> eval expr env

(* Interpreter tests *)
let () =
  let map_add = map_add string_compare in

  let env = map_add "x" 5 map_empty in
  let env = map_add "y" 10 env in
  let env = map_add "z" 15 env in
  let env = map_add "zero" 0 env in

  assert (interp "x + y + z" env = Ok 30) ;
  assert (interp "(x + y) * 14 - 42 / z" env = Ok 208) ;

  assert (interp "x ^ y" env = Error Syntax_error) ;
  assert (interp "x / a" env = Error (Unbound_variable "a")) ;
  assert (interp "x / zero" env = Error Div_by_zero)
