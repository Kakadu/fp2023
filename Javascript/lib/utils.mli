(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type 'a t = ('a, string) Result.t

val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
val ( <?> ) : 'a t -> string -> 'a t
val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
val return : 'a -> 'a t
val uerror : string -> 'a t
val map : ('a -> 'b t) -> 'a list -> 'b list t
val fold_left : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t
val fold_left_map : ('a -> 'b -> ('a * 'c) t) -> 'a -> 'b list -> ('a * 'c list) t
val fold_left_s : ('a -> 'b -> 'a t) -> ('a -> bool) -> 'a -> 'b list -> 'a t
val both : ('a -> 'b t) -> 'a -> 'a -> ('b * 'b) t
val both_ext : ('a -> 'b -> ('a * 'c) t) -> 'a -> 'b -> 'b -> ('a * ('c * 'c)) t

module IntMap : Map.S with type key = int
