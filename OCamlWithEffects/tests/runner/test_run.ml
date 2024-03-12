(** Copyright 2021-2024, DmitryPilyuk and raf-nr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_with_effects_lib.Run

let%expect_test _ =
  inference_expr
    (Ocaml_with_effects_lib.Ast.ELetIn
       ( "f"
       , Ocaml_with_effects_lib.Ast.EFun
           ( Ocaml_with_effects_lib.Ast.PVal "x"
           , Ocaml_with_effects_lib.Ast.EIdentifier "x" )
       , Ocaml_with_effects_lib.Ast.EApplication
           ( Ocaml_with_effects_lib.Ast.EIdentifier "f"
           , Ocaml_with_effects_lib.Ast.EConst (Ocaml_with_effects_lib.Ast.Int 5) ) ));
  [%expect {|
    - : int |}]
;;

let%expect_test _ =
  interpret_expr
    (Ocaml_with_effects_lib.Ast.ELetIn
       ( "f"
       , Ocaml_with_effects_lib.Ast.EFun
           ( Ocaml_with_effects_lib.Ast.PVal "x"
           , Ocaml_with_effects_lib.Ast.EIdentifier "x" )
       , Ocaml_with_effects_lib.Ast.EApplication
           ( Ocaml_with_effects_lib.Ast.EIdentifier "f"
           , Ocaml_with_effects_lib.Ast.EUnaryOperation
               ( Ocaml_with_effects_lib.Ast.Not
               , Ocaml_with_effects_lib.Ast.EConst (Ocaml_with_effects_lib.Ast.Bool true)
               ) ) ));
  [%expect {|
    - : bool = false |}]
;;

let%expect_test _ =
  inference_expr
    (Ocaml_with_effects_lib.Ast.EBinaryOperation
       ( Ocaml_with_effects_lib.Ast.Add
       , Ocaml_with_effects_lib.Ast.EUnaryOperation
           ( Ocaml_with_effects_lib.Ast.Minus
           , Ocaml_with_effects_lib.Ast.EConst (Ocaml_with_effects_lib.Ast.Int 5) )
       , Ocaml_with_effects_lib.Ast.EConst (Ocaml_with_effects_lib.Ast.Int 6) ));
  [%expect {|
    - : int |}]
;;

let%expect_test _ =
  interpret_expr
    (Ocaml_with_effects_lib.Ast.EBinaryOperation
       ( Ocaml_with_effects_lib.Ast.Add
       , Ocaml_with_effects_lib.Ast.EUnaryOperation
           ( Ocaml_with_effects_lib.Ast.Minus
           , Ocaml_with_effects_lib.Ast.EConst (Ocaml_with_effects_lib.Ast.Int 5) )
       , Ocaml_with_effects_lib.Ast.EConst (Ocaml_with_effects_lib.Ast.Int 6) ));
  [%expect {|
    - : int = 1 |}]
;;

let%expect_test _ =
  inference_expr
    (Ocaml_with_effects_lib.Ast.EApplication
       ( Ocaml_with_effects_lib.Ast.EFun
           ( Ocaml_with_effects_lib.Ast.PVal "x"
           , Ocaml_with_effects_lib.Ast.EMatchWith
               ( Ocaml_with_effects_lib.Ast.EIdentifier "x"
               , [ ( Ocaml_with_effects_lib.Ast.PConst
                       (Ocaml_with_effects_lib.Ast.Char '0')
                   , Ocaml_with_effects_lib.Ast.EConst (Ocaml_with_effects_lib.Ast.Int 0)
                   )
                 ; ( Ocaml_with_effects_lib.Ast.PConst
                       (Ocaml_with_effects_lib.Ast.Char '1')
                   , Ocaml_with_effects_lib.Ast.EConst (Ocaml_with_effects_lib.Ast.Int 1)
                   )
                 ; ( Ocaml_with_effects_lib.Ast.PAny
                   , Ocaml_with_effects_lib.Ast.EConst (Ocaml_with_effects_lib.Ast.Int 2)
                   )
                 ] ) )
       , Ocaml_with_effects_lib.Ast.EConst (Ocaml_with_effects_lib.Ast.Int 2) ));
  [%expect
    {|
    Type error: unification failed - type char does not match expected type int |}]
;;

let%expect_test _ =
  interpret_expr
    (Ocaml_with_effects_lib.Ast.EApplication
       ( Ocaml_with_effects_lib.Ast.EFun
           ( Ocaml_with_effects_lib.Ast.PVal "x"
           , Ocaml_with_effects_lib.Ast.EMatchWith
               ( Ocaml_with_effects_lib.Ast.EIdentifier "x"
               , [ ( Ocaml_with_effects_lib.Ast.PConst
                       (Ocaml_with_effects_lib.Ast.Char '0')
                   , Ocaml_with_effects_lib.Ast.EConst (Ocaml_with_effects_lib.Ast.Int 0)
                   )
                 ; ( Ocaml_with_effects_lib.Ast.PConst
                       (Ocaml_with_effects_lib.Ast.Char '1')
                   , Ocaml_with_effects_lib.Ast.EConst (Ocaml_with_effects_lib.Ast.Int 1)
                   )
                 ; ( Ocaml_with_effects_lib.Ast.PAny
                   , Ocaml_with_effects_lib.Ast.EConst (Ocaml_with_effects_lib.Ast.Int 2)
                   )
                 ] ) )
       , Ocaml_with_effects_lib.Ast.EConst (Ocaml_with_effects_lib.Ast.Int 2) ));
  [%expect
    {|
    Type error: unification failed - type char does not match expected type int |}]
;;

let%expect_test _ =
  inference_program
    [ Ocaml_with_effects_lib.Ast.SDeclaration
        (Ocaml_with_effects_lib.Ast.DDeclaration
           ( "f"
           , Ocaml_with_effects_lib.Ast.EFun
               ( Ocaml_with_effects_lib.Ast.PVal "x"
               , Ocaml_with_effects_lib.Ast.EIdentifier "x" ) ))
    ; Ocaml_with_effects_lib.Ast.SDeclaration
        (Ocaml_with_effects_lib.Ast.DDeclaration
           ( "g"
           , Ocaml_with_effects_lib.Ast.EFun
               ( Ocaml_with_effects_lib.Ast.PVal "x"
               , Ocaml_with_effects_lib.Ast.EBinaryOperation
                   ( Ocaml_with_effects_lib.Ast.Add
                   , Ocaml_with_effects_lib.Ast.EApplication
                       ( Ocaml_with_effects_lib.Ast.EIdentifier "f"
                       , Ocaml_with_effects_lib.Ast.EConst
                           (Ocaml_with_effects_lib.Ast.Int 5) )
                   , Ocaml_with_effects_lib.Ast.EIdentifier "x" ) ) ))
    ; Ocaml_with_effects_lib.Ast.SDeclaration
        (Ocaml_with_effects_lib.Ast.DDeclaration
           ( "z"
           , Ocaml_with_effects_lib.Ast.EBinaryOperation
               ( Ocaml_with_effects_lib.Ast.Gt
               , Ocaml_with_effects_lib.Ast.EApplication
                   ( Ocaml_with_effects_lib.Ast.EIdentifier "g"
                   , Ocaml_with_effects_lib.Ast.EConst (Ocaml_with_effects_lib.Ast.Int 5)
                   )
               , Ocaml_with_effects_lib.Ast.EConst (Ocaml_with_effects_lib.Ast.Int 1) ) ))
    ];
  [%expect {|
    val f : 'a -> 'a
    val g : int -> int
    val z : bool |}]
;;

let%expect_test _ =
  interpret_program
    [ Ocaml_with_effects_lib.Ast.SDeclaration
        (Ocaml_with_effects_lib.Ast.DDeclaration
           ( "f"
           , Ocaml_with_effects_lib.Ast.EFun
               ( Ocaml_with_effects_lib.Ast.PVal "x"
               , Ocaml_with_effects_lib.Ast.EIdentifier "x" ) ))
    ; Ocaml_with_effects_lib.Ast.SDeclaration
        (Ocaml_with_effects_lib.Ast.DDeclaration
           ( "g"
           , Ocaml_with_effects_lib.Ast.EFun
               ( Ocaml_with_effects_lib.Ast.PVal "x"
               , Ocaml_with_effects_lib.Ast.EBinaryOperation
                   ( Ocaml_with_effects_lib.Ast.Add
                   , Ocaml_with_effects_lib.Ast.EApplication
                       ( Ocaml_with_effects_lib.Ast.EIdentifier "f"
                       , Ocaml_with_effects_lib.Ast.EConst
                           (Ocaml_with_effects_lib.Ast.Int 5) )
                   , Ocaml_with_effects_lib.Ast.EIdentifier "x" ) ) ))
    ; Ocaml_with_effects_lib.Ast.SDeclaration
        (Ocaml_with_effects_lib.Ast.DDeclaration
           ( "z"
           , Ocaml_with_effects_lib.Ast.EBinaryOperation
               ( Ocaml_with_effects_lib.Ast.Gt
               , Ocaml_with_effects_lib.Ast.EApplication
                   ( Ocaml_with_effects_lib.Ast.EIdentifier "g"
                   , Ocaml_with_effects_lib.Ast.EConst (Ocaml_with_effects_lib.Ast.Int 5)
                   )
               , Ocaml_with_effects_lib.Ast.EConst (Ocaml_with_effects_lib.Ast.Int 1) ) ))
    ];
  [%expect
    {|
    val f : 'a -> 'a = <fun>
    val g : int -> int = <fun>
    val z : bool = true |}]
;;

let%expect_test _ =
  interpret_program
    [ Ocaml_with_effects_lib.Ast.SDeclaration
        (Ocaml_with_effects_lib.Ast.DDeclaration
           ("f", Ocaml_with_effects_lib.Ast.EConst (Ocaml_with_effects_lib.Ast.Int 5)))
    ; Ocaml_with_effects_lib.Ast.SDeclaration
        (Ocaml_with_effects_lib.Ast.DDeclaration
           ( "g"
           , Ocaml_with_effects_lib.Ast.EUnaryOperation
               (Ocaml_with_effects_lib.Ast.Not, Ocaml_with_effects_lib.Ast.EIdentifier "f")
           ))
    ];
  [%expect
    {|
    Type error: unification failed - type int does not match expected type bool |}]
;;

let%expect_test _ =
  interpret {| let f = perform |};
  [%expect {|
    Syntax error. |}]
;;

let%expect_test _ =
  inference {| let f = continue |};
  [%expect {|
    Syntax error. |}]
;;

(* Test ast determine *)

let%expect_test _ =
  inference {| let f x = x ;; let g = f 5 in g |};
  [%expect {|
    Syntax error. |}]
;;

let%expect_test _ =
  interpret {| let f x = x ;; let g = f 5 in g |};
  [%expect {|
    Syntax error. |}]
;;

(* ---------------- *)
