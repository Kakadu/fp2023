(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* Polymorphysm tests *)

let%expect_test _ =
  inference {| let id x = x |};
  [%expect {| val id : 'a -> 'a |}]
;;

let%expect_test _ =
  inference {| let f g x y = g x y |};
  [%expect {| val f : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c |}]
;;

let%expect_test _ =
  inference {| let f x y z= let id x = x in [id x; id y; id z] |};
  [%expect {| val f : 'a -> 'a -> 'a -> 'a list |}]
;;

let%expect_test _ =
  inference {| let f x = let helper a b = (a, b) in helper x |};
  [%expect {| val f : 'a -> 'b -> 'a * 'b |}]
;;

(* ---------------- *)
