(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

(*
 * This software includes
 * https://github.com/bmeurer/ocaml-rbtrees/blob/06c879da5b009025e65b9cbe9a36405bcf301f35/src/rbmap.ml
 *
 * Copyright (c) 2007, Benedikt Meurer <benedikt.meurer@googlemail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *)

type ('key, 'value) map =
  | Black of ('key, 'value) map * 'key * 'value * ('key, 'value) map
  | Red of ('key, 'value) map * 'key * 'value * ('key, 'value) map
  | Empty

let map_empty, map_add, map_find, map_remove, map_mem =
  let empty = Empty in

  let blackify = function
    | Red (l, k, x, r) ->
        (Black (l, k, x, r), false)
    | m ->
        (m, true)
  in

  let balance_left l kx x r =
    match (l, kx, x, r) with
    | Red (Red (a, kx, x, b), ky, y, c), kz, z, d
    | Red (a, kx, x, Red (b, ky, y, c)), kz, z, d ->
        Red (Black (a, kx, x, b), ky, y, Black (c, kz, z, d))
    | l, kx, x, r ->
        Black (l, kx, x, r)
  in

  let balance_right l kx x r =
    match (l, kx, x, r) with
    | a, kx, x, Red (Red (b, ky, y, c), kz, z, d)
    | a, kx, x, Red (b, ky, y, Red (c, kz, z, d)) ->
        Red (Black (a, kx, x, b), ky, y, Black (c, kz, z, d))
    | l, kx, x, r ->
        Black (l, kx, x, r)
  in

  let add cmp kx x m =
    let rec add_aux = function
      | Empty ->
          Red (Empty, kx, x, Empty)
      | Red (l, ky, y, r) ->
          let c = cmp kx ky in
          if c < 0 then Red (add_aux l, ky, y, r)
          else if c > 0 then Red (l, ky, y, add_aux r)
          else Red (l, kx, x, r)
      | Black (l, ky, y, r) ->
          let c = cmp kx ky in
          if c < 0 then balance_left (add_aux l) ky y r
          else if c > 0 then balance_right l ky y (add_aux r)
          else Black (l, kx, x, r)
    in
    fst (blackify (add_aux m))
  in

  let rec find cmp k = function
    | Empty ->
        raise Not_found
    | Red (l, kx, x, r) | Black (l, kx, x, r) ->
        let c = cmp k kx in
        if c < 0 then find cmp k l else if c > 0 then find cmp k r else x
  in

  let unbalanced_left = function
    | Red (Black (a, kx, x, b), ky, y, c) ->
        (balance_left (Red (a, kx, x, b)) ky y c, false)
    | Black (Black (a, kx, x, b), ky, y, c) ->
        (balance_left (Red (a, kx, x, b)) ky y c, true)
    | Black (Red (a, kx, x, Black (b, ky, y, c)), kz, z, d) ->
        (Black (a, kx, x, balance_left (Red (b, ky, y, c)) kz z d), false)
    | _ ->
        unreachable ()
  in

  let unbalanced_right = function
    | Red (a, kx, x, Black (b, ky, y, c)) ->
        (balance_right a kx x (Red (b, ky, y, c)), false)
    | Black (a, kx, x, Black (b, ky, y, c)) ->
        (balance_right a kx x (Red (b, ky, y, c)), true)
    | Black (a, kx, x, Red (Black (b, ky, y, c), kz, z, d)) ->
        (Black (balance_right a kx x (Red (b, ky, y, c)), kz, z, d), false)
    | _ ->
        unreachable ()
  in

  let rec remove_min = function
    | Empty | Black (Empty, _, _, Black _) ->
        unreachable ()
    | Black (Empty, kx, x, Empty) ->
        (Empty, kx, x, true)
    | Black (Empty, kx, x, Red (l, ky, y, r)) ->
        (Black (l, ky, y, r), kx, x, false)
    | Red (Empty, kx, x, r) ->
        (r, kx, x, false)
    | Black (l, kx, x, r) ->
        let l, ky, y, d = remove_min l in
        let m = Black (l, kx, x, r) in
        if d then
          let m, d = unbalanced_right m in
          (m, ky, y, d)
        else (m, ky, y, false)
    | Red (l, kx, x, r) ->
        let l, ky, y, d = remove_min l in
        let m = Red (l, kx, x, r) in
        if d then
          let m, d = unbalanced_right m in
          (m, ky, y, d)
        else (m, ky, y, false)
  in

  let remove cmp k m =
    let rec remove_aux = function
      | Empty ->
          (Empty, false)
      | Black (l, kx, x, r) -> (
          let c = cmp k kx in
          if c < 0 then
            let l, d = remove_aux l in
            let m = Black (l, kx, x, r) in
            if d then unbalanced_right m else (m, false)
          else if c > 0 then
            let r, d = remove_aux r in
            let m = Black (l, kx, x, r) in
            if d then unbalanced_left m else (m, false)
          else
            match r with
            | Empty ->
                blackify l
            | _ ->
                let r, kx, x, d = remove_min r in
                let m = Black (l, kx, x, r) in
                if d then unbalanced_left m else (m, false) )
      | Red (l, kx, x, r) -> (
          let c = cmp k kx in
          if c < 0 then
            let l, d = remove_aux l in
            let m = Red (l, kx, x, r) in
            if d then unbalanced_right m else (m, false)
          else if c > 0 then
            let r, d = remove_aux r in
            let m = Red (l, kx, x, r) in
            if d then unbalanced_left m else (m, false)
          else
            match r with
            | Empty ->
                (l, false)
            | _ ->
                let r, kx, x, d = remove_min r in
                let m = Red (l, kx, x, r) in
                if d then unbalanced_left m else (m, false) )
    in
    fst (remove_aux m)
  in

  let rec mem cmp k = function
    | Empty ->
        false
    | Red (l, kx, _, r) | Black (l, kx, _, r) ->
        let c = cmp k kx in
        if c < 0 then mem cmp k l else if c > 0 then mem cmp k r else true
  in
  (empty, add, find, remove, mem)

(* ======= Tests ======= *)
let () =
  let find key map =
    try Some (map_find string_compare key map) with Not_found -> None
  in
  let add, remove, mem =
    (map_add string_compare, map_remove string_compare, map_mem string_compare)
  in

  let t = add "five" 5 map_empty in
  let t = add "eleven" 11 t in
  let t = add "four" 4 t in
  let t = add "two" 2 t in
  let t = add "seven" 7 t in

  assert (mem "five" t = true) ;
  assert (mem "eleven" t = true) ;
  assert (mem "four" t = true) ;
  assert (mem "two" t = true) ;
  assert (mem "seven" t = true) ;
  assert (mem "twelve" t = false) ;

  assert (find "five" t = Some 5) ;
  assert (find "four" t = Some 4) ;
  assert (find "eleven" t = Some 11) ;
  assert (find "seven" t = Some 7) ;
  assert (find "two" t = Some 2) ;
  assert (find "twelve" t = None) ;

  let t = remove "eleven" t in
  assert (mem "five" t = true) ;
  assert (mem "eleven" t = false) ;
  assert (mem "four" t = true) ;
  assert (mem "two" t = true) ;
  assert (mem "seven" t = true) ;
  assert (mem "twelve" t = false) ;

  assert (find "five" t = Some 5) ;
  assert (find "four" t = Some 4) ;
  assert (find "eleven" t = None) ;
  assert (find "seven" t = Some 7) ;
  assert (find "two" t = Some 2) ;
  assert (find "twelve" t = None) ;

  let t = remove "five" t in
  assert (mem "five" t = false) ;
  assert (mem "eleven" t = false) ;
  assert (mem "four" t = true) ;
  assert (mem "two" t = true) ;
  assert (mem "seven" t = true) ;
  assert (mem "twelve" t = false) ;

  assert (find "five" t = None) ;
  assert (find "four" t = Some 4) ;
  assert (find "eleven" t = None) ;
  assert (find "seven" t = Some 7) ;
  assert (find "two" t = Some 2) ;
  assert (find "twelve" t = None)
