(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* Constants *)

let%expect_test _ =
  interpret {| 1 |};
  [%expect {|
    - : int = 1 |}]
;;

let%expect_test _ =
  interpret {| 'a' |};
  [%expect {|
    - : char = 'a' |}]
;;

let%expect_test _ =
  interpret {| "str" |};
  [%expect {|
    - : string = "str" |}]
;;

let%expect_test _ =
  interpret {| false |};
  [%expect {|
    - : bool = false |}]
;;

let%expect_test _ =
  interpret {| () |};
  [%expect {|
    - : unit = () |}]
;;

(* ---------------- *)

(* Bynary operations *)

let%expect_test _ =
  interpret {| 5 + 6 |};
  [%expect {|
    - : int = 11 |}]
;;

let%expect_test _ =
  interpret {| 5 - 6 / 3 * 2 |};
  [%expect {|
    - : int = 4 |}]
;;

let%expect_test _ =
  interpret {| 4 / 0 |};
  [%expect {|
    Error: division by zero. |}]
;;

let%expect_test _ =
  interpret
    {|
    let res1 = true && true
    let res2 = true && false
    let res3 = false && false
  |};
  [%expect
    {|
    val res1 : bool = true
    val res2 : bool = false
    val res3 : bool = false |}]
;;

let%expect_test _ =
  interpret
    {|
    let res1 = true || true
    let res2 = true || false
    let res3 = false || false
  |};
  [%expect
    {|
    val res1 : bool = true
    val res2 : bool = true
    val res3 : bool = false |}]
;;

let%expect_test _ =
  interpret {| (1 + 1 <= 1 + 1) && (1 + 1 >= 1 + 1) |};
  [%expect {|
    - : bool = true |}]
;;

let%expect_test _ =
  interpret
    {| (1 + 1 < 1 + 1 + 1) && (1 + 1 + 1 > 1 + 1) && (1 + 1 + 1 != 1 + 1) && (1 + 1 <> 1 + 1 + 1)|};
  [%expect {|
  - : bool = true |}]
;;

let%expect_test _ =
  interpret {| not (5 = 2) && 6 > 1 || 5 <= 1 |};
  [%expect {| - : bool = true |}]
;;

let%expect_test _ =
  interpret {| 'a' < 'b' && 'b' > 'a' |};
  [%expect {|
    - : bool = true |}]
;;

let%expect_test _ =
  interpret {| 'a' <> 'b' && 'b' != 'a' && 'a' = 'a' && 'a' >= 'a' && 'a' <= 'b' |};
  [%expect {|
    - : bool = true |}]
;;

let%expect_test _ =
  interpret
    {| "name1" < "name2" && "name2" > "name1" && "name1" = "name1" && "name1" <> "name2" |};
  [%expect {|
    - : bool = true |}]
;;

let%expect_test _ =
  interpret {| "name1" <= "name2" && "name2" >= "name2"|};
  [%expect {|
    - : bool = true |}]
;;

let%expect_test _ =
  interpret
    {| () = () && not (() != ()) && not (() <= () && () >= () && () < () && () > ())|};
  [%expect {|
    - : bool = true |}]
;;

let%expect_test _ =
  interpret
    {| 'a' = 'a' && 1 = 1 && "name" = "name" && true = true && false = false && () = () |};
  [%expect {|
    - : bool = true |}]
;;

let%expect_test _ =
  interpret
    {| true < true || true > true || not (true <= true && true >= true) || true = false || false <> false |};
  [%expect {|
    - : bool = false |}]
;;

let%expect_test _ =
  interpret {| 1 + (-2) * 5 - 10 / 2 |};
  [%expect {| - : int = -14 |}]
;;

let%expect_test _ =
  interpret {| not (5 = 2) && 6 > 1 || 5 <= 1 |};
  [%expect {| - : bool = true |}]
;;

(* ---------------- *)

(* Unary operations *)

let%expect_test _ =
  interpret {| -5 |};
  [%expect {|
    - : int = -5 |}]
;;

let%expect_test _ =
  interpret {| -(-(+(+(-(+(-(+(-(-(+(+5))))))))))) |};
  [%expect {|
    - : int = 5 |}]
;;

let%expect_test _ =
  interpret {| 
    let res1 = not true
    let res2 = not false
  |};
  [%expect {|
    val res1 : bool = false
    val res2 : bool = true |}]
;;

(* ---------------- *)
