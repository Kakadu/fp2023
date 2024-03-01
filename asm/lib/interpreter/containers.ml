(** Copyright 2023-2024, Vadim Yakshigulov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
module StringMap = Map.Make (String)
module IntMap = Map.Make (Int)

module ListStack = struct
  type 'a t = 'a list

  let empty = []
  let push x s = x :: s

  let peek = function
    | [] -> None
    | h :: _ -> Some h
  ;;

  let pop = function
    | [] -> None
    | _ :: tl -> Some tl
  ;;
end
