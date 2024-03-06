(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* List *)

let%expect_test _ =
  parse_with_print {| [20; 24; 5] |};
  [%expect
    {|
    [(SExpression
        (EList [(EConst (Int 20)); (EConst (Int 24)); (EConst (Int 5))]))
      ] |}]
;;

let%expect_test _ =
  parse_with_print {| [] |};
  [%expect {|
    [(SExpression (EList []))] |}]
;;

let%expect_test _ =
  parse_with_print {| [20; 'c'] |};
  [%expect {|
    [(SExpression (EList [(EConst (Int 20)); (EConst (Char 'c'))]))] |}]
;;

(* ---------------- *)

(* List construction *)

let%expect_test _ =
  parse_with_print {| 1 :: 2 :: [] |};
  [%expect
    {|
    [(SExpression
        (EListCons ((EConst (Int 1)), (EListCons ((EConst (Int 2)), (EList [])))
           )))
      ] |}]
;;

let%expect_test _ =
  parse_with_print {| (1, 2) :: (3, 4) :: [] |};
  [%expect
    {|
    [(SExpression
        (EListCons ((ETuple [(EConst (Int 1)); (EConst (Int 2))]),
           (EListCons ((ETuple [(EConst (Int 3)); (EConst (Int 4))]), (EList [])
              ))
           )))
      ] |}]
;;

let%expect_test _ =
  parse_with_print {| (1 :: 2 :: 3 :: []) :: (4 :: 5 :: 6 :: 7 :: []) :: [] |};
  [%expect
    {|
    [(SExpression
        (EListCons (
           (EListCons ((EConst (Int 1)),
              (EListCons ((EConst (Int 2)),
                 (EListCons ((EConst (Int 3)), (EList [])))))
              )),
           (EListCons (
              (EListCons ((EConst (Int 4)),
                 (EListCons ((EConst (Int 5)),
                    (EListCons ((EConst (Int 6)),
                       (EListCons ((EConst (Int 7)), (EList [])))))
                    ))
                 )),
              (EList [])))
           )))
      ] |}]
;;

(* ---------------- *)

(* Tuple *)

let%expect_test _ =
  parse_with_print {| (1, 2, 3, 4) |};
  [%expect
    {|
    [(SExpression
        (ETuple
           [(EConst (Int 1)); (EConst (Int 2)); (EConst (Int 3));
             (EConst (Int 4))]))
      ] |}]
;;

let%expect_test _ =
  parse_with_print {| ((1,'c'), "name", true, () :: () :: () :: []) |};
  [%expect
    {|
    [(SExpression
        (ETuple
           [(ETuple [(EConst (Int 1)); (EConst (Char 'c'))]);
             (EConst (String "name")); (EConst (Bool true));
             (EListCons ((EConst Unit),
                (EListCons ((EConst Unit),
                   (EListCons ((EConst Unit), (EList [])))))
                ))
             ]))
      ] |}]
;;

let%expect_test _ =
  parse_with_print {| () |};
  (* An empty tuple is not parsed *)
  [%expect {|
    [(SExpression (EConst Unit))] |}]
;;

(* ---------------- *)
