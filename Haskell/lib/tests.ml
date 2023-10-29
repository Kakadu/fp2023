(** Copyright 2023-2024, Danil P *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open HaskellLib
open Ast

let ptest s e =
  match Parser.parse s with
  | Ok a -> List.equal equal_decl e a
  | Error err ->
    Format.printf "%s\n" err;
    false
;;

let%test _ = ptest "x = 2" [ DeclLet (PatVar "x", ExprLit (LitInt 2)) ]

let%test _ =
  ptest
    "fact n = if (n < 2) then 1 else fact (n - 1) * n"
    [ DeclLet
        ( PatVar "fact"
        , ExprFunc
            ( PatVar "n"
            , ExprIf
                ( ExprBinOp (Lt, ExprVar "n", ExprLit (LitInt 2))
                , ExprLit (LitInt 1)
                , ExprBinOp
                    ( Mul
                    , ExprApp
                        (ExprVar "fact", ExprBinOp (Sub, ExprVar "n", ExprLit (LitInt 1)))
                    , ExprVar "n" ) ) ) )
    ]
;;
