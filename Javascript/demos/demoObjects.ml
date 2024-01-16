(** Copyright 2023-2024, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Javascript_lib.Print

let objects =
  "let obj1 = { prop1: 4, [\"prop\"+2] : \"Hi!\", getProp1() {return this.prop1}}\n\
  \  console.log(obj1)\n\
  \  let obj2 = { prop1 : 10, __proto__ : obj1}\n\
  \  console.log(obj2.prop2, obj2.getProp1(), obj2.prop1)"
;;

let test = print_stdout objects
