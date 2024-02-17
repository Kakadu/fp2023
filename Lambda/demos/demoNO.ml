(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Lambda_lib
open Ast
open Lambda
open Utils

let test strat term =
  Format.printf "Evaluating: %a\n%!" Pprintast.pp term;
  let rez = apply_strat strat term in
  Format.printf "Result:     %a\n%!" Pprintast.pp rez;
  rez
;;

module _ = struct
  let zero = abs "g" @@ abs "y" @@ Var "y"
  let one = abs "f" @@ abs "x" @@ app f (Var "x")
  let two = abs "f" @@ abs "x" @@ app f (app f x)
  let three = abs "f" @@ abs "x" @@ app f (app f (app f x))
  let plus = abs "m" @@ abs "n" @@ abs "f" @@ abs "x" @@ app (app m f) (app (app n f) x)
  let mul = abs "x" @@ abs "y" @@ abs "z" @@ app x (app y z)
  let true_ = abs "x" @@ abs "y" @@ Var "x"
  let false_ = abs "x" @@ abs "y" @@ Var "y"
  let isZero = abs "n" @@ app (app n (abs "x" false_)) true_

  (* if-then-else for lazy strategy *)
  let ite cond th el = app (app (app isZero cond) th) el

  let pred =
    let xxx = abs "g" @@ abs "h" @@ app h (app g f) in
    abs "n" @@ abs "f" @@ abs "x" @@ app (app (app n xxx) (abs "u" x)) (abs "u" (Var "u"))
  ;;

  let fact =
    abs "self"
    @@ abs "N"
    @@ ite (Var "N") one (app (app mul (app (var "self") (app pred (var "N")))) (var "N"))
  ;;

  let ygrek =
    let hack = abs "x" (app f (app x x)) in
    abs "f" (app hack hack)
  ;;

  let () = test nor_strat @@ zero |> Format.printf "%a\n%!" Pprintast.pp

  (* 5! = 120 *)
  let () =
    test nor_strat @@ app (app ygrek fact) (app (app plus two) three)
    |> Format.printf "%a\n%!" Pprintast.pp
  ;;
end
