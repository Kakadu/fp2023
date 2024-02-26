(** Copyright 2023-2024, Danil S, Andrei *)

(*
 * This software includes
 * https://gitlab.com/Kakadu/fp2020course-materials/-/blob/
 * 7c2ba4bb006d3037dd42f6588fbe287d7b2e2446/code/parsing/parser_lib.ml
 *
 * Copyright 2021-2023, Kakadu
 * SPDX-License-Identifier: LGPL-3.0-or-later
 *)

(* Simple parser-combinator library *)

type 'a parse_result = Failed | Parsed of 'a * char list

let fail _ = Failed
let return x s = Parsed (x, s)

let satisfy cond = function
  | h :: tl ->
      if cond h then return h tl else Failed
  | _ ->
      Failed

let char c = satisfy (( = ) c)

let () =
  assert (char 'a' ['a'; 'b'] = return 'a' ['b']) ;
  assert (char 'b' ['a'; 'b'] = Failed)

let ( >>= ) p f s = match p s with Failed -> Failed | Parsed (h, tl) -> f h tl

let ( *> ) p1 p2 = p1 >>= fun _ -> p2

let ( <* ) p1 p2 = p1 >>= fun h -> p2 >>= fun _ -> return h

let () =
  let p1 = char 'a' *> char 'b' in
  let p2 = char 'a' <* char 'b' in
  assert (p1 ['a'; 'b'] = return 'b' []) ;
  assert (p2 ['a'; 'b'] = return 'a' [])

let ( >>| ) p f s =
  match p s with Failed -> Failed | Parsed (x, tl) -> return (f x) tl

let rec many p s =
  match p s with
  | Failed ->
      return [] s
  | Parsed (x, tl) ->
      (many p >>= fun tl -> return (x :: tl)) tl

let many1 p = p >>= fun x -> many p >>= fun xs -> return (x :: xs)

let () =
  let p = many (char 'a') in
  let input = ['b'; 'a'; 'b'] in
  assert (p input = Parsed ([], input)) ;

  let p = many (char 'b') in
  assert (p ['b'; 'b'; 'a'] = Parsed (['b'; 'b'], ['a'])) ;

  let p = many1 (char 'b') in
  assert (p ['b'; 'b'; 'a'] = Parsed (['b'; 'b'], ['a'])) ;

  let p = many1 (char 'a') in
  assert (p ['b'; 'a'; 'b'] = Failed)

let take_while f = many (satisfy f) >>| string_of_char_list

let take_while1 f = many1 (satisfy f) >>| string_of_char_list

let ( <|> ) p1 p2 s = match p1 s with Failed -> p2 s | ok -> ok

let () =
  let a_or_b = char 'a' <|> char 'b' in
  assert (a_or_b ['a'] = return 'a' []) ;
  assert (a_or_b ['b'] = return 'b' []) ;
  assert (a_or_b ['c'] = Failed)

let choice = function [] -> fail | hd :: tl -> list_fold ( <|> ) hd tl

let lift2 f p1 p2 = p1 >>= fun r1 -> p2 >>= fun r2 -> return @@ f r1 r2

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init

let rec fix f s = f (fix f) s

let run p str =
  match p (string_to_list str) with Parsed (res, []) -> Some res | _ -> None
