(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

let prelude =
  {|
exception Division_by_zero
exception Not_found
exception Invalid_argument
exception Assert_failure

type unit = ()
type bool = true | false
type 'a option = None | Some of 'a
type ('ok, 'err) result = Ok of 'ok | Error of 'err

let id x = x
let ( @@ ) f x = f x

let fst (x, y) = x
let snd (x, y) = y

let incr x = x := !x + 1
let decr x = x := !x - 1

let min x y = if x < y then x else y

let assert x = if x then () else raise Assert_failure
let not x = if x then false else true

let char_is_digit = function
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
      true
  | _ ->
      false

let int_compare = ( - )

let rec list_fold f acc = function
  | [] -> acc
  | hd :: tl -> list_fold f (f acc hd) tl

let list_map f l =
  let rec helper acc = function
    | [] -> acc []
    | x :: xs -> helper (fun ys -> acc (f x :: ys)) xs
  in
  helper (fun ys -> ys) l

let list_rev l =
  let rec helper acc = function
    | [] -> acc
    | hd :: tl -> helper (hd :: acc) tl
  in
  helper [] l

let rec list_iter f = function
  | [] -> ()
  | hd :: tl -> let () = f hd in list_iter f tl

let string_of_char_list = list_fold (fun acc ch -> acc ^ string_of_char ch) ""
|}
