(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type 'a t = ('a, string) Result.t

let ( >>= ) = Result.bind
let return = Result.ok
let uerror = Result.error
let ( >>| ) x f = x >>= fun r -> return @@ f r

let rec map f = function
  | [] -> return []
  | h :: tl -> f h >>= fun c -> map f tl >>= fun lst -> return (c :: lst)
;;

let rec fold_left f acc = function
  | [] -> return acc
  | a :: l -> f acc a >>= fun x -> fold_left f x l
;;

let rec fold_left_s f stop acc = function
  | [] -> return acc
  | a :: l -> f acc a >>= fun x -> if stop x then return x else fold_left_s f stop x l
;;
