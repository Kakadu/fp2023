(** Copyright 2023-2024, Danil P *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open HaskellLib
open Ast
open Parser

let ptest parser printer s =
  match Angstrom.parse_string ~consume:All parser s with
  | Ok a -> Format.printf "%a" printer a
  | Error err -> Format.printf "%s\n" err
;;

let%expect_test _ =
  ptest pexpr pp_expr {|1 + 2 - 3 * 4 + (-5) / 6 * 7 - 8 * 9|};
  [%expect
    {|
      (ExprBinOp (Sub,
         (ExprBinOp (Add,
            (ExprBinOp (Sub,
               (ExprBinOp (Add, (ExprLit (LitInt 1)), (ExprLit (LitInt 2)))),
               (ExprBinOp (Mul, (ExprLit (LitInt 3)), (ExprLit (LitInt 4)))))),
            (ExprBinOp (Mul,
               (ExprBinOp (Div, (ExprUnOp (Neg, (ExprLit (LitInt 5)))),
                  (ExprLit (LitInt 6)))),
               (ExprLit (LitInt 7))))
            )),
         (ExprBinOp (Mul, (ExprLit (LitInt 8)), (ExprLit (LitInt 9)))))) |}]
;;

let%expect_test _ =
  ptest pdecl pp_decl {|x = (1, 2)|};
  [%expect
    {|
    (DeclLet
       ((PatVar "x"), (ExprTuple [(ExprLit (LitInt 1)); (ExprLit (LitInt 2))]))) |}]
;;

let%expect_test _ =
  ptest pdecl pp_decl {|fact n = if (n < 2) then 1 else fact (n - 1) * n|};
  [%expect
    {| 
    (DeclLet
       ((PatVar "fact"),
        (ExprFunc
           ((PatVar "n"),
            (ExprIf ((ExprBinOp (Lt, (ExprVar "n"), (ExprLit (LitInt 2)))),
               (ExprLit (LitInt 1)),
               (ExprBinOp (Mul,
                  (ExprApp ((ExprVar "fact"),
                     (ExprBinOp (Sub, (ExprVar "n"), (ExprLit (LitInt 1)))))),
                  (ExprVar "n")))
               )))))) |}]
;;

let%expect_test _ =
  ptest pdecl pp_decl {|x = [1, 2]|};
  [%expect
    {|
    (DeclLet
       ((PatVar "x"),
        (ExprCons ((ExprLit (LitInt 1)),
           (ExprCons ((ExprLit (LitInt 2)), ExprNil)))))) |}]
;;

let%expect_test _ =
  ptest pdecl pp_decl {|(x, y, z, (a, b)) = (1, 2, 3, (4, 5))|};
  [%expect
    {|
    (DeclLet
       ((PatTuple
           [(PatVar "x"); (PatVar "y"); (PatVar "z");
             (PatTuple [(PatVar "a"); (PatVar "b")])]),
        (ExprTuple
           [(ExprLit (LitInt 1)); (ExprLit (LitInt 2)); (ExprLit (LitInt 3));
             (ExprTuple [(ExprLit (LitInt 4)); (ExprLit (LitInt 5))])]))) |}]
;;

let%expect_test _ =
  ptest pexpr pp_expr {|1 : 2 : 3 : [4, 5, 6]|};
  [%expect
    {|
    (ExprCons ((ExprLit (LitInt 1)),
       (ExprCons ((ExprLit (LitInt 2)),
          (ExprCons ((ExprLit (LitInt 3)),
             (ExprCons ((ExprLit (LitInt 4)),
                (ExprCons ((ExprLit (LitInt 5)),
                   (ExprCons ((ExprLit (LitInt 6)), ExprNil))))
                ))
             ))
          ))
       )) |}]
;;

let%expect_test _ =
  ptest plit pp_lit {|"hello "|};
  [%expect {|
    (LitString "hello ") |}]
;;

let%expect_test _ =
  ptest pexpr pp_expr {|\x y z -> x + y + z + 1|};
  [%expect
    {|
    (ExprFunc
       ((PatVar "x"),
        (ExprFunc
           ((PatVar "y"),
            (ExprFunc
               ((PatVar "z"),
                (ExprBinOp (Add,
                   (ExprBinOp (Add,
                      (ExprBinOp (Add, (ExprVar "x"), (ExprVar "y"))),
                      (ExprVar "z"))),
                   (ExprLit (LitInt 1)))))))))) |}]
;;

let%expect_test _ =
  ptest pdecl pp_decl {|fun a b =\x ->\y -> x + y + 1|};
  [%expect
    {|
    (DeclLet
       ((PatVar "fun"),
        (ExprFunc
           ((PatVar "a"),
            (ExprFunc
               ((PatVar "b"),
                (ExprFunc
                   ((PatVar "x"),
                    (ExprFunc
                       ((PatVar "y"),
                        (ExprBinOp (Add,
                           (ExprBinOp (Add, (ExprVar "x"), (ExprVar "y"))),
                           (ExprLit (LitInt 1)))))))))))))) |}]
;;

let%expect_test _ =
  ptest pdecl pp_decl {|f = let x = 3 in let y = 2 in x + y|};
  [%expect
    {|
    (DeclLet
       ((PatVar "f"),
        (ExprLet ([((PatVar "x"), (ExprLit (LitInt 3)))],
           (ExprLet ([((PatVar "y"), (ExprLit (LitInt 2)))],
              (ExprBinOp (Add, (ExprVar "x"), (ExprVar "y")))))
           )))) |}]
;;

let%expect_test _ =
  ptest pdecl pp_decl {|
  f = 
   let x = 3
       y = 5
       z = 7
   in x + y + z |};
  [%expect
    {|
    (DeclLet
       ((PatVar "f"),
        (ExprLet (
           [((PatVar "x"), (ExprLit (LitInt 3)));
             ((PatVar "y"), (ExprLit (LitInt 5)));
             ((PatVar "z"), (ExprLit (LitInt 7)))],
           (ExprBinOp (Add, (ExprBinOp (Add, (ExprVar "x"), (ExprVar "y"))),
              (ExprVar "z")))
           )))) |}]
;;

let%expect_test _ =
  ptest pdecl pp_decl {|real n = if True || False then false else true|};
  [%expect
    {|
    (DeclLet
       ((PatVar "real"),
        (ExprFunc
           ((PatVar "n"),
            (ExprIf (
               (ExprBinOp (Or, (ExprLit (LitBool true)),
                  (ExprLit (LitBool false)))),
               (ExprVar "false"), (ExprVar "true"))))))) |}]
;;

let%expect_test _ =
  ptest pdecl pp_decl {|x = if True then 45 else 70 + 60|};
  [%expect
    {|
    (DeclLet
       ((PatVar "x"),
        (ExprIf ((ExprLit (LitBool true)), (ExprLit (LitInt 45)),
           (ExprBinOp (Add, (ExprLit (LitInt 70)), (ExprLit (LitInt 60)))))))) |}]
;;

let%expect_test _ =
  ptest pdecl pp_decl {|f [] = 2 + 2|};
  [%expect
    {|
    (DeclLet
       ((PatVar "f"),
        (ExprFunc
           (PatNil, (ExprBinOp (Add, (ExprLit (LitInt 2)), (ExprLit (LitInt 2)))))))) |}]
;;

let%expect_test _ =
  ptest ppat pp_pat {|[1,2,x]|};
  [%expect
    {|
    (PatCons ((PatLit (LitInt 1)),
       (PatCons ((PatLit (LitInt 2)), (PatCons ((PatVar "x"), PatNil)))))) |}]
;;

let%expect_test _ =
  ptest ppat pp_pat {|(x:xs:xsw)|};
  [%expect {|
    (PatCons ((PatVar "x"), (PatCons ((PatVar "xs"), (PatVar "xsw"))))) |}]
;;

let%expect_test _ =
  ptest ppat pp_pat {|(x, y)|};
  [%expect {|
    (PatTuple [(PatVar "x"); (PatVar "y")]) |}]
;;

let%expect_test _ =
  ptest pdecl pp_decl {|f (x:y:xs) = xs|};
  [%expect
    {|
    (DeclLet
       ((PatVar "f"),
        (ExprFunc
           ((PatCons ((PatVar "x"), (PatCons ((PatVar "y"), (PatVar "xs"))))),
            (ExprVar "xs"))))) |}]
;;

let%expect_test _ =
  ptest pdecl pp_decl {|(x:xs)= [52]|};
  [%expect
    {|
    (DeclLet
       ((PatCons ((PatVar "x"), (PatVar "xs"))),
        (ExprCons ((ExprLit (LitInt 52)), ExprNil)))) |}]
;;

let%expect_test _ =
  ptest pdecl pp_decl {| map f (x:xs) = f x : 5|};
  [%expect
    {|
    (DeclLet
       ((PatVar "map"),
        (ExprFunc
           ((PatVar "f"),
            (ExprFunc
               ((PatCons ((PatVar "x"), (PatVar "xs"))),
                (ExprCons ((ExprApp ((ExprVar "f"), (ExprVar "x"))),
                   (ExprLit (LitInt 5)))))))))) |}]
;;

let%expect_test _ =
  ptest pexpr pp_expr {|case x of
1 -> 1 + 1
2 -> 2|};
  [%expect
    {|
    (ExprCase ((ExprVar "x"),
       [((PatLit (LitInt 1)),
         (ExprBinOp (Add, (ExprLit (LitInt 1)), (ExprLit (LitInt 1)))));
         ((PatLit (LitInt 2)), (ExprLit (LitInt 2)))]
       )) |}]
;;

let%expect_test _ =
  ptest
    pdecl
    pp_decl
    {|sieve lst = case lst of
    [] -> []
    (p:xs) -> p : sieve (filter (\x -> mod x p /= 0) xs)|};
  [%expect
    {|
    (DeclLet
       ((PatVar "sieve"),
        (ExprFunc
           ((PatVar "lst"),
            (ExprCase ((ExprVar "lst"),
               [(PatNil, ExprNil);
                 ((PatCons ((PatVar "p"), (PatVar "xs"))),
                  (ExprCons ((ExprVar "p"),
                     (ExprApp ((ExprVar "sieve"),
                        (ExprApp (
                           (ExprApp ((ExprVar "filter"),
                              (ExprFunc
                                 ((PatVar "x"),
                                  (ExprBinOp (Neq,
                                     (ExprApp (
                                        (ExprApp ((ExprVar "mod"), (ExprVar "x")
                                           )),
                                        (ExprVar "p"))),
                                     (ExprLit (LitInt 0))))))
                              )),
                           (ExprVar "xs")))
                        ))
                     )))
                 ]
               )))))) |}]
;;

let%expect_test _ =
  ptest
    pdecl
    pp_decl
    {|index lst n = case (lst, n) of 
([], _) -> []
((x:xs), n) -> (if n == 0 then x else myIndex xs (n - 1))|};
  [%expect
    {|
    (DeclLet
       ((PatVar "index"),
        (ExprFunc
           ((PatVar "lst"),
            (ExprFunc
               ((PatVar "n"),
                (ExprCase ((ExprTuple [(ExprVar "lst"); (ExprVar "n")]),
                   [((PatTuple [PatNil; PatWild]), ExprNil);
                     ((PatTuple
                         [(PatCons ((PatVar "x"), (PatVar "xs"))); (PatVar "n")]),
                      (ExprIf (
                         (ExprBinOp (Eq, (ExprVar "n"), (ExprLit (LitInt 0)))),
                         (ExprVar "x"),
                         (ExprApp (
                            (ExprApp ((ExprVar "myIndex"), (ExprVar "xs"))),
                            (ExprBinOp (Sub, (ExprVar "n"), (ExprLit (LitInt 1))
                               ))
                            ))
                         )))
                     ]
                   )))))))) |}]
;;

let%expect_test _ =
  ptest
    pdecl
    pp_decl
    {|take n lst = case (n, lst) of
  (n, (x:xs)) -> x : take (n - 1) xs
  (1, (x:_)) -> [x]
  (_, []) -> []|};
  [%expect
    {|
    (DeclLet
       ((PatVar "take"),
        (ExprFunc
           ((PatVar "n"),
            (ExprFunc
               ((PatVar "lst"),
                (ExprCase ((ExprTuple [(ExprVar "n"); (ExprVar "lst")]),
                   [((PatTuple
                        [(PatVar "n"); (PatCons ((PatVar "x"), (PatVar "xs")))]),
                     (ExprCons ((ExprVar "x"),
                        (ExprApp (
                           (ExprApp ((ExprVar "take"),
                              (ExprBinOp (Sub, (ExprVar "n"),
                                 (ExprLit (LitInt 1))))
                              )),
                           (ExprVar "xs")))
                        )));
                     ((PatTuple
                         [(PatLit (LitInt 1)); (PatCons ((PatVar "x"), PatWild))]),
                      (ExprCons ((ExprVar "x"), ExprNil)));
                     ((PatTuple [PatWild; PatNil]), ExprNil)]
                   )))))))) |}]
;;

let%expect_test _ =
  ptest
    pdecl
    pp_decl
    {|merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else (if x == y then x : merge xs ys else y : merge (x:xs) ys)|};
  [%expect
    {|
    (DeclLet
       ((PatVar "merge"),
        (ExprFunc
           ((PatCons ((PatVar "x"), (PatVar "xs"))),
            (ExprFunc
               ((PatCons ((PatVar "y"), (PatVar "ys"))),
                (ExprIf ((ExprBinOp (Lt, (ExprVar "x"), (ExprVar "y"))),
                   (ExprCons ((ExprVar "x"),
                      (ExprApp ((ExprApp ((ExprVar "merge"), (ExprVar "xs"))),
                         (ExprCons ((ExprVar "y"), (ExprVar "ys")))))
                      )),
                   (ExprIf ((ExprBinOp (Eq, (ExprVar "x"), (ExprVar "y"))),
                      (ExprCons ((ExprVar "x"),
                         (ExprApp ((ExprApp ((ExprVar "merge"), (ExprVar "xs"))),
                            (ExprVar "ys")))
                         )),
                      (ExprCons ((ExprVar "y"),
                         (ExprApp (
                            (ExprApp ((ExprVar "merge"),
                               (ExprCons ((ExprVar "x"), (ExprVar "xs"))))),
                            (ExprVar "ys")))
                         ))
                      ))
                   )))))))) |}]
;;

let%expect_test _ =
  ptest pdecl pp_decl {|x = (Node 1  Leaf Leaf)|};
  [%expect
    {|
    (DeclLet
       ((PatVar "x"),
        (ExprTree (Node ((ExprLit (LitInt 1)), (ExprTree Leaf), (ExprTree Leaf)))))) |}]
;;

let%expect_test _ =
  ptest pdecl pp_decl {|x = Leaf|};
  [%expect {|
    (DeclLet ((PatVar "x"), (ExprTree Leaf))) |}]
;;

let%expect_test _ =
  ptest pdecl pp_decl {|f (Node x  (Node 1  Leaf Leaf) Leaf) = Leaf|};
  [%expect
    {|
    (DeclLet
       ((PatVar "f"),
        (ExprFunc
           ((PatTree ((PatVar "x"),
               (PatTree ((PatLit (LitInt 1)), PatLeaf, PatLeaf)), PatLeaf)),
            (ExprTree Leaf))))) |}]
;;
