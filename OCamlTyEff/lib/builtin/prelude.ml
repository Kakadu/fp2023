(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

let prelude =
  {|
exception Division_by_zero;;

type unit = ();;
type bool = true | false;;
type 'a option = None | Some of 'a;;

let id x = x;;
let ( @@ ) f x = f x;;

let fst (x, y) = x;;
let snd (x, y) = y;;

let not x = if x then false else true;;

let list_map f l =
  let rec helper acc = function
    | [] -> acc []
    | x :: xs -> helper (fun ys -> acc (f x :: ys)) xs
  in
  helper (fun ys -> ys) l
;;

let list_rev l =
  let rec helper acc = function
    | [] -> acc
    | hd :: tl -> helper (hd :: acc) tl
  in
  helper [] l
;;

let rec list_iter f = function
  | [] -> ()
  | hd :: tl -> let () = f hd in list_iter f tl
;;
|}
