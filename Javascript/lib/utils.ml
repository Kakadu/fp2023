(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type 'a t = ('a, string) Result.t

let ( >>= ) = Result.bind
let return = Result.ok
let uerror = Result.error
let ( >>| ) x f = x >>= fun r -> return @@ f r

let ( <?> ) x str =
  match x with
  | Result.Ok _ -> x
  | Result.Error a -> Result.Error (str ^ " > " ^ a)
;;

let rec map f = function
  | [] -> return []
  | h :: tl -> f h >>= fun c -> map f tl >>| fun lst -> c :: lst
;;

let rec fold_left f acc = function
  | [] -> return acc
  | h :: tl -> f acc h >>= fun acc -> fold_left f acc tl
;;

let rec fold_left_map f acc = function
  | [] -> return (acc, [])
  | h :: tl ->
    f acc h >>= fun (acc, c) -> fold_left_map f acc tl >>| fun (acc, lst) -> acc, c :: lst
;;

let rec fold_left_s f stop acc = function
  | [] -> return acc
  | h :: tl ->
    f acc h >>= fun acc -> if stop acc then return acc else fold_left_s f stop acc tl
;;

let both f a b = f a >>= fun x -> f b >>= fun y -> return (x, y)
