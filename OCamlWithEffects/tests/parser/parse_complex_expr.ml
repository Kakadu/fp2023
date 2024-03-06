(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* Factorial *)

let%expect_test _ =
  parse_with_print {| let rec fact n = if n = 1 then 1 else n * fact (n - 1) |};
  [%expect
    {|
    [(SDeclaration
        (DRecDeclaration ("fact",
           (EFun ((PVal "n"),
              (EIfThenElse (
                 (EBinaryOperation (Eq, (EIdentifier "n"), (EConst (Int 1)))),
                 (EConst (Int 1)),
                 (EBinaryOperation (Mul, (EIdentifier "n"),
                    (EApplication ((EIdentifier "fact"),
                       (EBinaryOperation (Sub, (EIdentifier "n"),
                          (EConst (Int 1))))
                       ))
                    ))
                 ))
              ))
           )))
      ] |}]
;;

(* ---------------- *)

(* Safe division by zero. *)

let%expect_test _ =
  parse_with_print
    {|
   effect DevisionByZero : int effect

   let helper x y = 
      match y with
      | 0 -> perform DevisionByZero
      | other -> x / other
   ;;

   let div x y =
      try helper x y with
      | DevisionByZero k -> continue k 0
   ;;
  |};
  [%expect
    {|
    [(SDeclaration (DEffectDeclaration ("DevisionByZero", (AEffect AInt))));
      (SDeclaration
         (DDeclaration ("helper",
            (EFun ((PVal "x"),
               (EFun ((PVal "y"),
                  (EMatchWith ((EIdentifier "y"),
                     [((PConst (Int 0)),
                       (EEffectPerform (EEffectWithoutArguments "DevisionByZero")));
                       ((PVal "other"),
                        (EBinaryOperation (Div, (EIdentifier "x"),
                           (EIdentifier "other"))))
                       ]
                     ))
                  ))
               ))
            )));
      (SDeclaration
         (DDeclaration ("div",
            (EFun ((PVal "x"),
               (EFun ((PVal "y"),
                  (ETryWith (
                     (EApplication (
                        (EApplication ((EIdentifier "helper"), (EIdentifier "x")
                           )),
                        (EIdentifier "y"))),
                     [(EffectHandler ((PEffectWithoutArguments "DevisionByZero"),
                         (EEffectContinue ((Continue "k"), (EConst (Int 0)))),
                         (Continue "k")))
                       ]
                     ))
                  ))
               ))
            )))
      ] |}]
;;

(* ---------------- *)

(* Calculating the sum of a list of numbers represented by symbols using effects. *)

let%expect_test _ =
  parse_with_print
    {| effect NotDigit: char -> int effect

    let int_of_char c = match c with
    | '0' -> 0
    | '1' -> 1 
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | c -> perform (NotDigit c)
    

    let rec sum_up li = match li with
    | [] -> 0
    | h :: tl -> int_of_char h + sum_up tl


    let test_l = ['1'; 'a'; '0'; '1'; '5'; '7'; 'v'; '2'; '9']

    let res = try sum_up test_l with
    | (NotDigit x) k -> continue k 0
    |};
  [%expect
    {|
    [(SDeclaration
        (DEffectDeclaration ("NotDigit", (AArrow (AChar, (AEffect AInt))))));
      (SDeclaration
         (DDeclaration ("int_of_char",
            (EFun ((PVal "c"),
               (EMatchWith ((EIdentifier "c"),
                  [((PConst (Char '0')), (EConst (Int 0)));
                    ((PConst (Char '1')), (EConst (Int 1)));
                    ((PConst (Char '2')), (EConst (Int 2)));
                    ((PConst (Char '3')), (EConst (Int 3)));
                    ((PConst (Char '4')), (EConst (Int 4)));
                    ((PConst (Char '5')), (EConst (Int 5)));
                    ((PConst (Char '6')), (EConst (Int 6)));
                    ((PConst (Char '7')), (EConst (Int 7)));
                    ((PConst (Char '8')), (EConst (Int 8)));
                    ((PConst (Char '9')), (EConst (Int 9)));
                    ((PVal "c"),
                     (EEffectPerform
                        (EEffectWithArguments ("NotDigit", (EIdentifier "c")))))
                    ]
                  ))
               ))
            )));
      (SDeclaration
         (DRecDeclaration ("sum_up",
            (EFun ((PVal "li"),
               (EMatchWith ((EIdentifier "li"),
                  [(PNill, (EConst (Int 0)));
                    ((PListCons ((PVal "h"), (PVal "tl"))),
                     (EBinaryOperation (Add,
                        (EApplication ((EIdentifier "int_of_char"),
                           (EIdentifier "h"))),
                        (EApplication ((EIdentifier "sum_up"), (EIdentifier "tl")
                           ))
                        )))
                    ]
                  ))
               ))
            )));
      (SDeclaration
         (DDeclaration ("test_l",
            (EList
               [(EConst (Char '1')); (EConst (Char 'a')); (EConst (Char '0'));
                 (EConst (Char '1')); (EConst (Char '5')); (EConst (Char '7'));
                 (EConst (Char 'v')); (EConst (Char '2')); (EConst (Char '9'))])
            )));
      (SDeclaration
         (DDeclaration ("res",
            (ETryWith (
               (EApplication ((EIdentifier "sum_up"), (EIdentifier "test_l"))),
               [(EffectHandler ((PEffectWithArguments ("NotDigit", (PVal "x"))),
                   (EEffectContinue ((Continue "k"), (EConst (Int 0)))),
                   (Continue "k")))
                 ]
               ))
            )))
      ] |}]
;;

(* ---------------- *)

(* Fibonacci *)

let%expect_test _ =
  parse_with_print {| let rec fib n = if n > 1 then fib (n - 1) + fib (n - 2) else 1;; |};
  [%expect
    {|
    [(SDeclaration
        (DRecDeclaration ("fib",
           (EFun ((PVal "n"),
              (EIfThenElse (
                 (EBinaryOperation (Gt, (EIdentifier "n"), (EConst (Int 1)))),
                 (EBinaryOperation (Add,
                    (EApplication ((EIdentifier "fib"),
                       (EBinaryOperation (Sub, (EIdentifier "n"),
                          (EConst (Int 1))))
                       )),
                    (EApplication ((EIdentifier "fib"),
                       (EBinaryOperation (Sub, (EIdentifier "n"),
                          (EConst (Int 2))))
                       ))
                    )),
                 (EConst (Int 1))))
              ))
           )))
      ] |}]
;;

(* ---------------- *)
