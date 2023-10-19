(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Lambda_lib
open Ast
open Lambda
open Utils

type 'a status =
  | Done of 'a
  | WIP of 'a

let fin x = Done x
let wip x = WIP x

let ao_small_step_strat =
  let rec helper = function
    | Var _ as l -> fin l
    | Abs (x, b) ->
      (match helper b with
       | WIP b2 -> wip (abs x b2)
       | Done b2 -> fin (abs x b2))
    | App (f, arg) ->
      (match helper f with
       | WIP f2 -> wip (app f2 arg)
       | Done (Abs (x, body)) ->
         (match helper arg with
          | Done arg -> wip (subst x ~by:arg body)
          | WIP arg -> wip (app f arg))
       | Done f2 -> fin (App (f2, arg)))
  in
  let rec loop t =
    match helper t with
    | Done x -> x
    | WIP x ->
      Format.printf " -- %a\n%!" Pprintast.pp x;
      loop x
  in
  let on_app _ f arg = loop (app f arg) in
  let on_abs _ f x = loop (abs f x) in
  let on_var _ x = loop (var x) in
  { on_var; on_abs; on_app }
;;

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

  let plus =
    abs "m" @@ abs "n" @@ abs "f" @@ abs "x" @@ app m @@ app f @@ app n @@ app f x
  ;;

  let mul = abs "x" @@ abs "y" @@ abs "z" @@ app x (app y z)
  let true_ = abs "x" @@ abs "y" @@ Var "x"
  let false_ = abs "x" @@ abs "y" @@ Var "y"
  let isZero = abs "n" @@ app (app n (abs "x" false_)) true_

  (* TODO: write the right if-then-else
     by adding thunk around then and else branches to delay the evaluation *)
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

  let zed =
    let hack = abs "x" (app f (abs "y" (app (app x x) y))) in
    abs "f" (app hack hack)
  ;;

  let () = test ao_strat @@ zero |> fun lam -> Format.printf "%a\n%!" Pprintast.pp lam
  let () = test ao_strat @@ one |> fun lam -> Format.printf "%a\n%!" Pprintast.pp lam
  let _ = test ao_strat @@ app plus @@ app one one |> Format.printf "%a\n%!" Pprintast.pp

  let () =
    test ao_strat @@ app isZero @@ app one @@ app two three
    |> Format.printf "%a\n%!" Pprintast.pp
  ;;

  let () =
    test ao_small_step_strat @@ app (app mul one) two
    |> Format.printf "%a\n%!" Pprintast.pp
  ;;

  let () =
    test ao_small_step_strat @@ app (app mul three) two
    |> Format.printf "%a\n%!" Pprintast.pp
  ;;

  (* let () =
       test ao_small_step_strat @@ app zed fact |> fun lam ->
       Format.printf "%a\n%!" pp_lam lam
     in *)

  (* let () =
       test ao_small_step_strat @@ (app isZero zero)  |> fun lam ->
       Format.printf "%a\n%!" pp_lam lam
     in
  *)
  (* let () =
       test ao_strat @@ (app isZero zero)  |> fun lam ->
       Format.printf "%a\n%!" pp_lam lam
     in *)
end
