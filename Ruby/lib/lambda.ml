(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Utils

(* Smart constructors *)
let var x = Var x
let abs x y = Abs (x, y)
let app x y = App (x, y)

let replace_name x ~by =
  let rec helper = function
    | Var y when String.equal x y -> Var by
    | Var t -> Var t
    | App (l, r) -> App (helper l, helper r)
    | Abs (y, t) when String.equal x y -> Abs (by, helper t)
    | Abs (z, t) -> Abs (z, helper t)
  in
  helper
;;

let rec next_name s old =
  if List.mem ~equal:String.equal old s then next_name ("_" ^ s) old else s
;;

(*  The call [subst x ~by:v e] means `[x/v]e` or `e[v -> x]` *)
let subst x ~by:v =
  let rec helper e =
    match e with
    | Var y when String.equal y x -> v
    | Var y -> Var y
    | App (l, r) -> app (helper l) (helper r)
    | Abs (y, b) when String.equal y x -> abs y b
    | Abs (y, t) when is_free_in y v ->
      let frees = free_vars v @ free_vars t in
      let w = next_name y frees in
      helper (abs w (replace_name y ~by:w t))
    | Abs (y, b) -> abs y (helper b)
  in
  helper
;;

type strat =
  { on_var : strat -> name -> string Ast.t
  ; on_abs : strat -> name -> string Ast.t -> string Ast.t
  ; on_app : strat -> string Ast.t -> string Ast.t -> string Ast.t
  }

let apply_strat st = function
  | Var name -> st.on_var st name
  | Abs (x, b) -> st.on_abs st x b
  | App (l, r) -> st.on_app st l r
;;

let without_strat =
  let on_var _ = var in
  let on_abs _ = abs in
  let on_app _ = app in
  { on_var; on_abs; on_app }
;;

let cbn_strat =
  let on_app st f arg =
    match apply_strat st f with
    | Abs (x, e) -> apply_strat st (subst x ~by:arg e)
    | f2 -> App (f2, arg)
  in
  { without_strat with on_app }
;;

let under_abstraction st x b = abs x (apply_strat st b)

(* Normal Order Reduction to Normal Form
   Application function reduced as CBN first
   + Reduce under abstractions *)
let nor_strat =
  let on_app st f arg =
    match apply_strat cbn_strat f with
    | Abs (x, e) -> apply_strat st @@ subst x ~by:arg e
    | f1 ->
      let f2 = apply_strat st f1 in
      let arg2 = apply_strat st arg in
      App (f2, arg2)
  in
  { without_strat with on_app; on_abs = under_abstraction }
;;

(* Call-by-Value Reduction to Weak Normal Form *)
let cbv_strat =
  let on_app st f arg =
    match apply_strat st f with
    | Abs (x, e) ->
      let arg2 = apply_strat st arg in
      apply_strat st @@ subst x ~by:arg2 e
    | f2 -> App (f2, apply_strat st arg)
  in
  { without_strat with on_app }
;;

(* Applicative Order Reduction to Normal Form
   As CBV but reduce under abstractions *)
let ao_strat = { cbv_strat with on_abs = under_abstraction }
let a = var "a"
let x = var "x"
let y = var "y"
let z = var "z"
let f = var "f"
let g = var "g"
let h = var "h"
let m = var "m"
let n = var "n"
let p = var "p"
let zero = abs "f" @@ abs "x" x
let one = abs "f" @@ abs "x" @@ app f x
let two = abs "f" @@ abs "x" @@ app f (app f x)
let three = abs "f" @@ abs "x" @@ app f (app f (app f x))
