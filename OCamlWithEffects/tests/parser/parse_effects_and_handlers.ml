(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

let%expect_test _ =
  parse_with_print {|
    effect E1 : int -> int effect
  |};
  [%expect
    {|
    [(SDeclaration (DEffectDeclaration ("E1", (AArrow (AInt, (AEffect AInt))))))] |}]
;;

let%expect_test _ =
  parse_with_print {|
    effect E1 : int -> int -> int effect
  |};
  [%expect {|
    Syntax error. |}]
;;

let%expect_test _ =
  parse_with_print {|
    effect E1 : (int -> int) -> int effect
  |};
  [%expect
    {|
    [(SDeclaration
        (DEffectDeclaration ("E1",
           (AArrow ((AArrow (AInt, AInt)), (AEffect AInt))))))
      ] |}]
;;

let%expect_test _ =
  parse_with_print
    {|
    effect E1 : (int -> (bool -> bool) -> bool -> (string -> (char -> bool) -> string) -> string) -> bool effect
  |};
  [%expect
    {|
    [(SDeclaration
        (DEffectDeclaration ("E1",
           (AArrow (
              (AArrow (AInt,
                 (AArrow ((AArrow (ABool, ABool)),
                    (AArrow (ABool,
                       (AArrow (
                          (AArrow (AString,
                             (AArrow ((AArrow (AChar, ABool)), AString)))),
                          AString))
                       ))
                    ))
                 )),
              (AEffect ABool)))
           )))
      ] |}]
;;

let%expect_test _ =
  parse_with_print {|
    effect E1 : (int -> (char -> bool) -> string) effect
  |};
  [%expect
    {|
    [(SDeclaration
        (DEffectDeclaration ("E1",
           (AEffect (AArrow (AInt, (AArrow ((AArrow (AChar, ABool)), AString)))))
           )))
      ] |}]
;;

let%expect_test _ =
  parse_with_print
    {|
    effect E1 : (int -> string -> char) -> (int -> (char -> bool) -> string) effect
  |};
  [%expect
    {|
    [(SDeclaration
        (DEffectDeclaration ("E1",
           (AArrow ((AArrow (AInt, (AArrow (AString, AChar)))),
              (AEffect
                 (AArrow (AInt, (AArrow ((AArrow (AChar, ABool)), AString)))))
              ))
           )))
      ] |}]
;;

let%expect_test _ =
  parse_with_print
    {|
    try perform E1 x with
    | E (a :: b) k -> continue k 0
    | E (a :: b :: c) k -> continue k 1
    | E x -> continue k 2
  |};
  [%expect {|
    Syntax error. |}]
;;

let%expect_test _ =
  parse_with_print {|
    let f1 = perform E
    let f2 = perform E x
  |};
  [%expect
    {|
    [(SDeclaration
        (DDeclaration ("f1", (EEffectPerform (EEffectWithoutArguments "E")))));
      (SDeclaration
         (DDeclaration ("f2",
            (EEffectPerform (EEffectWithArguments ("E", (EIdentifier "x")))))))
      ] |}]
;;

let%expect_test _ =
  parse_with_print
    {|
    effect E1 : int -> int effect
    effect E2 : int -> int effect

    let f x = 
      try x with
      | E1 k -> continue k 0
      | E2 k -> continue k 5
    ;;

    let res = f (perform (E1)) in res
  |};
  [%expect
    {|
    [(SDeclaration (DEffectDeclaration ("E1", (AArrow (AInt, (AEffect AInt))))));
      (SDeclaration (DEffectDeclaration ("E2", (AArrow (AInt, (AEffect AInt))))));
      (SDeclaration
         (DDeclaration ("f",
            (EFun ((PVal "x"),
               (ETryWith ((EIdentifier "x"),
                  [(EffectHandler ((PEffectWithoutArguments "E1"),
                      (EEffectContinue ((Continue "k"), (EConst (Int 0)))),
                      (Continue "k")));
                    (EffectHandler ((PEffectWithoutArguments "E2"),
                       (EEffectContinue ((Continue "k"), (EConst (Int 5)))),
                       (Continue "k")))
                    ]
                  ))
               ))
            )));
      (SExpression
         (ELetIn ("res",
            (EApplication ((EIdentifier "f"),
               (EEffectPerform (EEffectWithoutArguments "E1")))),
            (EIdentifier "res"))))
      ] |}]
;;
