(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type 'a t

val return : 'a -> 'a t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val error : string -> 'a t
val map : ('a -> ('b, 'c) result) -> 'a list -> ('b list, 'c) result
val fold_left : ('a -> 'b -> ('a, 'c) result) -> 'a -> 'b list -> ('a, 'c) result
