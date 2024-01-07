(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Javascript_lib
open Print

let%expect_test _ =
  pi "return 4";
  [%expect {| Programm return: 4. |}]
;;
