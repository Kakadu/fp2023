(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* Declaration inference tests *)

let%expect_test _ =
  inference {| let f x = x + 1|};
  [%expect {| val f : int -> int |}]
;;

let%expect_test _ =
  inference {| let f x = x :: [2]|};
  [%expect {| val f : int -> int list |}]
;;

let%expect_test _ =
  inference {| let f g x y = if g x then x * y else y |};
  [%expect {| val f : (int -> bool) -> int -> int -> int |}]
;;

let%expect_test _ =
  inference {| let f = match x with | _ -> 0 |};
  [%expect {| Type error: unbound variable 'x' |}]
;;

let%expect_test _ =
  inference {| let f _ = match [] with | _ -> 0 |};
  [%expect {| val f : 'a -> int |}]
;;

let%expect_test _ =
  inference {| 
    let f x = x ;;
    let g x = f x ;;
    let f = f 5
  |};
  [%expect {|
    val g : 'a -> 'a
    val f : int |}]
;;

(* ---------------- *)

(* Rec declarations inference tests *)

let%expect_test _ =
  inference {| let rec fact n = if n = 1 then 1 else n * fact (n - 1) |};
  [%expect {| val fact : int -> int |}]
;;

let%expect_test _ =
  inference {| let rec f = f 4 |};
  [%expect {| Type error: occurs check failed. |}]
;;

(* ---------------- *)

(* Functions tests*)

let%expect_test _ =
  inference {| fun x -> x |};
  [%expect {| - : 'a -> 'a |}]
;;

let%expect_test _ =
  inference {| fun x -> (x+1) > 3 |};
  [%expect {| - : int -> bool |}]
;;

(* ---------------- *)
