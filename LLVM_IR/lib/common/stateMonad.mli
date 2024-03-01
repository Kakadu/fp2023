(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

type ('st, 'a) t = 'st -> 'st * ('a, string) result

val return : 'a -> 'b -> 'b * ('a, 'c) result
val fail : 'a -> 'b -> 'b * ('c, 'a) result
val ( >>= ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
val read : ('st, 'st) t
val write : 'st -> ('st, unit) t
val run : ('st, 'a) t -> 'st -> 'st * ('a, string) result
val ( let* ) : ('a, 'b) t -> ('b -> ('a, 'c) t) -> ('a, 'c) t
val ( *> ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'c) t
val ( >>| ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
val ( <* ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b) t
val map_list : ('a -> ('st, 'b) t) -> 'a list -> ('st, 'b list) t
