(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

let%expect_test _ =
  parse_with_print
    {|
  let f x = 
    match x with 
    | 0 -> "zero"
    | _ -> "not zero"
  |};
  [%expect
    {|
    [(SDeclaration
        (DDeclaration ("f",
           (EFun ((PVal "x"),
              (EMatchWith ((EIdentifier "x"),
                 [((PConst (Int 0)), (EConst (String "zero")));
                   (PAny, (EConst (String "not zero")))]
                 ))
              ))
           )))
      ] |}]
;;

let%expect_test _ =
  parse_with_print
    {|
  let f x = 
    (match x with 
    | [] -> 0
    | hd :: tl -> 
      (match hd with
      | x -> x
      | _ -> hd))
  |};
  [%expect
    {|
    [(SDeclaration
        (DDeclaration ("f",
           (EFun ((PVal "x"),
              (EMatchWith ((EIdentifier "x"),
                 [(PNill, (EConst (Int 0)));
                   ((PListCons ((PVal "hd"), (PVal "tl"))),
                    (EMatchWith ((EIdentifier "hd"),
                       [((PVal "x"), (EIdentifier "x"));
                         (PAny, (EIdentifier "hd"))]
                       )))
                   ]
                 ))
              ))
           )))
      ] |}]
;;

let%expect_test _ =
  parse_with_print
    {|
  let f x = 
    (match x with 
    | (x, y) -> x + y
    | (x, y, z) -> x + y + z
    | _ -> 0)
  |};
  [%expect
    {|
    [(SDeclaration
        (DDeclaration ("f",
           (EFun ((PVal "x"),
              (EMatchWith ((EIdentifier "x"),
                 [((PTuple [(PVal "x"); (PVal "y")]),
                   (EBinaryOperation (Add, (EIdentifier "x"), (EIdentifier "y"))));
                   ((PTuple [(PVal "x"); (PVal "y"); (PVal "z")]),
                    (EBinaryOperation (Add,
                       (EBinaryOperation (Add, (EIdentifier "x"),
                          (EIdentifier "y"))),
                       (EIdentifier "z"))));
                   (PAny, (EConst (Int 0)))]
                 ))
              ))
           )))
      ] |}]
;;
