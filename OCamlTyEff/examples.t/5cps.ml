(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let rec map f = function [] -> [] | hd :: tl -> f hd :: map f tl

let map_cps f l =
  let rec helper l k =
    match l with [] -> k [] | hd :: tl -> helper tl (fun tl -> k (f hd :: tl))
  in
  helper l id

let () =
  assert (map (( * ) 2) [1; 2] = [2; 4]) ;

  assert (map (( + ) 1) [1; 2; 3] = map_cps (( + ) 1) [1; 2; 3])

(* Continuation monad *)
let return x k = k x
let ( >>= ) x f k = x (fun v -> f v k)
let run cont = cont id

let rec fold_right f acc = function
  | [] ->
      acc
  | hd :: tl ->
      f hd (fold_right f acc tl)

let fold_right_cps f acc l =
  let rec helper acc = function
    | [] ->
        return acc
    | hd :: tl ->
        helper acc tl >>= fun acc -> return (f hd acc)
  in
  run (helper acc l)

let () =
  assert (fold_right ( * ) 1 [2; 4] = 8) ;

  assert (fold_right ( + ) 0 [1; 2; 3] = fold_right_cps ( + ) 0 [1; 2; 3])
