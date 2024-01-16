(** Copyright 2023-2024, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Javascript_lib.Print

let factorial =
  "let fact = 8\n\n\
   function calculateFact(fact) {\n\
  \    if (fact !== 0)\n\
  \        return fact * calculateFact(fact - 1);\n\
  \    else return 1;\n\
   }\n\n\
   console.log(calculateFact(fact))\n\
  \   \n"
;;

let test = print_stdout factorial
