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
|}
