(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

let cut x n =
  if n >= 64
  then x
  else (
    let mask = Int64.shift_right_logical Int64.max_int (63 - n) in
    Int64.logand x mask)
;;

let uget x n = cut x n

let sget x n =
  let mag = Int64.shift_left 1L (n - 1) in
  let neg = not (Int64.equal (Int64.logand x mag) 0L) in
  let value = cut x (n - 1) in
  if neg then Int64.sub value mag else value
;;

let create x n = Ast.CInteger (n, cut x n)

let bin_op int_op l r =
  match l, r with
  | Ast.CInteger (sz1, v1), Ast.CInteger (sz2, v2) when sz1 = sz2 ->
    Some (create (int_op (uget v1 sz1) (uget v2 sz2) sz1))
  | _ -> None
;;

let sbin_op int_op l r =
  match l, r with
  | Ast.CInteger (sz1, v1), Ast.CInteger (sz2, v2) when sz1 = sz2 ->
    Some (create (int_op (sget v1 sz1) (sget v2 sz2) sz1))
  | _ -> None
;;
