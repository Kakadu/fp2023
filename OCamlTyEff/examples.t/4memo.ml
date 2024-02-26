(* Requires [1rbmap.ml] *)

(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let fib, fib_memo =
  let memoize f =
    let memo_table = ref map_empty in
    fun x ->
      try map_find int_compare x !memo_table
      with Not_found ->
        let y = f x in
        memo_table := map_add int_compare x y !memo_table ;
        y
  in

  let memo_rec f_norec =
    let fref = ref (fun _ -> unreachable ()) in
    let f = memoize (fun x -> f_norec !fref x) in
    fref := f ;
    f
  in

  let fib_norec fib i = if i <= 1 then i else fib (i - 1) + fib (i - 2) in
  let rec fib i = fib_norec fib i in
  let fib_memo = memo_rec fib_norec in

  (fib, fib_memo)

let () =
  assert (fib 10 = fib_memo 10) ;
  assert (fib_memo 50 = 12586269025)
