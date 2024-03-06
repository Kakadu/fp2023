(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

(* Let *)

let%expect_test _ =
  parse_with_print {| let x = (1) |};
  [%expect {|
    [(SDeclaration (DDeclaration ("x", (EConst (Int 1)))))] |}]
;;

let%expect_test _ =
  parse_with_print {| let f x y = x + y 
let main = f 4 6|};
  [%expect
    {|
  [(SDeclaration
      (DDeclaration ("f",
         (EFun ((PVal "x"),
            (EFun ((PVal "y"),
               (EBinaryOperation (Add, (EIdentifier "x"), (EIdentifier "y")))))
            ))
         )));
    (SDeclaration
       (DDeclaration ("main",
          (EApplication ((EApplication ((EIdentifier "f"), (EConst (Int 4)))),
             (EConst (Int 6))))
          )))
    ] |}]
;;

let%expect_test _ =
  parse_with_print {| let continue = (1) |};
  [%expect {|
    Syntax error. |}]
;;

(* ---------------- *)

(* Let Rec *)

let%expect_test _ =
  parse_with_print {| let rec f x = f (x - 1)|};
  [%expect
    {|
    [(SDeclaration
        (DRecDeclaration ("f",
           (EFun ((PVal "x"),
              (EApplication ((EIdentifier "f"),
                 (EBinaryOperation (Sub, (EIdentifier "x"), (EConst (Int 1))))))
              ))
           )))
      ] |}]
;;

(* ---------------- *)

(* Let/Let Rec with IN *)

let%expect_test _ =
  parse_with_print {| let f = 
    let x = 5 in
    1 + x * 3
  |};
  [%expect
    {|
    [(SDeclaration
        (DDeclaration ("f",
           (ELetIn ("x", (EConst (Int 5)),
              (EBinaryOperation (Add, (EConst (Int 1)),
                 (EBinaryOperation (Mul, (EIdentifier "x"), (EConst (Int 3))))))
              ))
           )))
      ] |}]
;;

let%expect_test _ =
  parse_with_print {| let rec f x = f (x-1) ;; let g = f 5 in g |};
  [%expect
    {|
    [(SDeclaration
        (DRecDeclaration ("f",
           (EFun ((PVal "x"),
              (EApplication ((EIdentifier "f"),
                 (EBinaryOperation (Sub, (EIdentifier "x"), (EConst (Int 1))))))
              ))
           )));
      (SExpression
         (ELetIn ("g", (EApplication ((EIdentifier "f"), (EConst (Int 5)))),
            (EIdentifier "g"))))
      ] |}]
;;

(* ---------------- *)

(* Functions and application *)

let%expect_test _ =
  parse_with_print {| fun x -> x + 1 ;;|};
  [%expect
    {|
    [(SExpression
        (EFun ((PVal "x"),
           (EBinaryOperation (Add, (EIdentifier "x"), (EConst (Int 1)))))))
      ] |}]
;;

let%expect_test _ =
  parse_with_print {| (fun x -> x + 1) 10000000 ;;|};
  [%expect
    {|
    [(SExpression
        (EApplication (
           (EFun ((PVal "x"),
              (EBinaryOperation (Add, (EIdentifier "x"), (EConst (Int 1)))))),
           (EConst (Int 10000000)))))
      ] |}]
;;

let%expect_test _ =
  parse_with_print {| fun (a :: b) -> a :: b |};
  [%expect
    {|
    [(SExpression
        (EFun ((PListCons ((PVal "a"), (PVal "b"))),
           (EListCons ((EIdentifier "a"), (EIdentifier "b"))))))
      ] |}]
;;

let%expect_test _ =
  parse_with_print
    {|
   fun x ->
      (match x with
      | [] -> 0
      | hd :: tl -> 1)
  |};
  [%expect
    {|
    [(SExpression
        (EFun ((PVal "x"),
           (EMatchWith ((EIdentifier "x"),
              [(PNill, (EConst (Int 0)));
                ((PListCons ((PVal "hd"), (PVal "tl"))), (EConst (Int 1)))]
              ))
           )))
      ] |}]
;;

let%expect_test _ =
  parse_with_print {| fun x -> match x with | rec -> rec | _ -> 0 |};
  [%expect {|
    Syntax error. |}]
;;

(* ---------------- *)
