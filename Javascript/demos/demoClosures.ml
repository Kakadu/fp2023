(** Copyright 2023-2024, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Javascript_lib.Print

let closures =
  "let fun1 = function (start_val) {\n\
  \    let counter = start_val;\n\
  \    return ()=>{return ++counter}}\n\
  \    let fun2 = fun1(5)\n\
  \    console.log(fun2(), fun2(), fun2())"
;;

let test = print_stdout closures
