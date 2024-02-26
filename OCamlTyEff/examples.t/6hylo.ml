(* Requires [:rectypes] *)

(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Using hylomorphisms to solve simple math problems *)

(** Catamorphism *)
let rec cata map phi layer = phi (map (cata map phi) layer)

(** Anamorphism *)
let rec ana map psi x = map (ana map psi) (psi x)

(* ======= Factorial ======= *)
type 'self list_layer = Nil | Cons of int * 'self

let map_layer f = function Nil -> Nil | Cons (el, self) -> Cons (el, f self)

let phi_list = function Nil -> 1 | Cons (v, layer) -> v * layer

let () =
  let product = cata map_layer phi_list in
  assert (product (Cons (1, Cons (2, Cons (3, Cons (4, Nil))))) = 24)

let psi_list = function 0 -> Nil | n -> Cons (n, n - 1)

let () =
  let construct = ana map_layer psi_list in
  assert (construct 5 = Cons (5, Cons (4, Cons (3, Cons (2, Cons (1, Nil))))))

let hylo phi psi x = cata map_layer phi (ana map_layer psi x)
let fact = hylo phi_list psi_list

let () = assert (fact 5 = 120)

(* ======= Fibonacci ======= *)
type 'a inter = Zero | One | OfTwo of 'a * 'a

let map_inter f = function
  | Zero ->
      Zero
  | One ->
      One
  | OfTwo (a, b) ->
      OfTwo (f a, f b)

let psi_fib = function 0 -> Zero | 1 -> One | n -> OfTwo (n - 1, n - 2)
let phi_fib = function Zero -> 0 | One -> 1 | OfTwo (a, b) -> a + b

let hylo phi psi x = cata map_inter phi (ana map_inter psi x)
let fib = hylo phi_fib psi_fib

let () = assert (fib 19 = 4181)

(* ======= Binary partition ======= *)
type 'a inter = No | One of 'a | Two of 'a * 'a

let map_inter f = function
  | No ->
      No
  | One x ->
      One (f x)
  | Two (x, y) ->
      Two (f x, f y)

let psi_bp = function
  | 0 ->
      No
  | n ->
      let catch f = try f () with Division_by_zero -> unreachable () in
      if catch (fun () -> mod n 2) = 1 then One (n - 1)
      else Two (n - 1, catch (fun () -> n / 2))

let phi_bp = function No -> 1 | One x -> x | Two (x, y) -> x + y

let hylo phi psi x = cata map_inter phi (ana map_inter psi x)
let bp = hylo phi_bp psi_bp

let () =
  assert (bp 0 = 1) (* 0 = 0 *) ;
  assert (bp 1 = 1) (* 1 = 2^0 *) ;

  (* 2 = 2^1 = 2^0 + 2^0 *)
  assert (bp 2 = 2) ;

  (* 3 = 2^1 + 2^0 = 2^0 + 2^0 + 2^0 *)
  assert (bp 3 = 2)
