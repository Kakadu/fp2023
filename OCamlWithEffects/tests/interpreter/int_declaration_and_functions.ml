(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

let%expect_test _ =
  interpret {| fun x -> x + 1 |};
  [%expect {| - : int -> int = <fun> |}]
;;

let%expect_test _ =
  interpret {| ((fun x -> x + 1) 1) >= 2 |};
  [%expect {| - : bool = true |}]
;;

let%expect_test _ =
  interpret {| (fun x -> x) (fun x -> x + 1) 1 |};
  [%expect {| - : int = 2 |}]
;;

let%expect_test _ =
  interpret {| (fun 5 -> 5) 6 |};
  [%expect {| Error: pattern matching failure - the value does not match any pattern. |}]
;;

let%expect_test _ =
  interpret {| let f x = x in f 4 |};
  [%expect {| - : int = 4 |}]
;;

let%expect_test _ =
  interpret {| let rec f x = x in f 4 |};
  [%expect {| - : int = 4 |}]
;;

let%expect_test _ =
  interpret {| let rec f = 5 |};
  [%expect {| val f : int = 5 |}]
;;
