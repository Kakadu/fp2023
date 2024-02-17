(** Copyright 2023-2024, Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_pv
open Ast

module ParserTests = struct
  open Parser

  let test_parse input =
    match parse input with
    | Ok ast -> Format.printf "%a\n" pp_structure ast
    | Error s -> Format.printf "%s\n" s
  ;;

  let%expect_test _ =
    test_parse
      {|
        let rec fix f x = f (fix f) x;;
        let fac_ fac n = if n = 1 then 1 else n * fac (n - 1);;
      |};
    [%expect
      {|
    [(SValue (Rec,
        ((PVar "fix"),
         (EFun ((PVar "f"),
            (EFun ((PVar "x"),
               (EApply (
                  (EApply ((EVar "f"), (EApply ((EVar "fix"), (EVar "f"))))),
                  (EVar "x")))
               ))
            )))
        ));
      (SValue (Nonrec,
         ((PVar "fac_"),
          (EFun ((PVar "fac"),
             (EFun ((PVar "n"),
                (EIf ((EBin_op (Eq, (EVar "n"), (EConst (CInt 1)))),
                   (EConst (CInt 1)),
                   (EBin_op (Mul, (EVar "n"),
                      (EApply ((EVar "fac"),
                         (EBin_op (Sub, (EVar "n"), (EConst (CInt 1))))))
                      ))
                   ))
                ))
             )))
         ))
      ] |}]
  ;;

  let%expect_test _ =
    test_parse {| 
      let n = fun y -> if y > 0 then 1 else 2
    |};
    [%expect
      {|
      [(SValue (Nonrec,
          ((PVar "n"),
           (EFun ((PVar "y"),
              (EIf ((EBin_op (Gt, (EVar "y"), (EConst (CInt 0)))),
                 (EConst (CInt 1)), (EConst (CInt 2))))
              )))
          ))
        ] |}]
  ;;

  let%expect_test _ =
    test_parse {| 
    let rec fac n = if n < 2 then 1 else n * fac(n - 1);; 
    |};
    [%expect
      {|
    [(SValue (Rec,
        ((PVar "fac"),
         (EFun ((PVar "n"),
            (EIf ((EBin_op (Lt, (EVar "n"), (EConst (CInt 2)))),
               (EConst (CInt 1)),
               (EBin_op (Mul, (EVar "n"),
                  (EApply ((EVar "fac"),
                     (EBin_op (Sub, (EVar "n"), (EConst (CInt 1))))))
                  ))
               ))
            )))
        ))
      ] |}]
  ;;

  let%expect_test _ =
    test_parse
      {| 
    let f = let g x = x + 1 in g;;
    let rec len l = 
      match l with
      | [] -> 0
      | _ :: xs -> 1 + len xs
    ;; 
    |};
    [%expect
      {|
      [(SValue (Nonrec,
          ((PVar "f"),
           (ELet (Nonrec,
              ((PVar "g"),
               (EFun ((PVar "x"), (EBin_op (Add, (EVar "x"), (EConst (CInt 1))))))),
              (EVar "g"))))
          ));
        (SValue (Rec,
           ((PVar "len"),
            (EFun ((PVar "l"),
               (EMatch ((EVar "l"),
                  [((PConst CNil), (EConst (CInt 0)));
                    ((PCons (PAny, (PVar "xs"))),
                     (EBin_op (Add, (EConst (CInt 1)),
                        (EApply ((EVar "len"), (EVar "xs"))))))
                    ]
                  ))
               )))
           ))
        ] |}]
  ;;

  let%expect_test _ =
    test_parse
      {| 

    let f = (fun x -> x + 1) 123 in f;;
    let x, y, z = (1, 2, 3);; 

    |};
    [%expect
      {|
      [(SEval
          (ELet (Nonrec,
             ((PVar "f"),
              (EApply (
                 (EFun ((PVar "x"), (EBin_op (Add, (EVar "x"), (EConst (CInt 1))))
                    )),
                 (EConst (CInt 123))))),
             (EVar "f"))));
        (SValue (Nonrec,
           ((PTuple [(PVar "x"); (PVar "y"); (PVar "z")]),
            (ETuple [(EConst (CInt 1)); (EConst (CInt 2)); (EConst (CInt 3))]))
           ))
        ] |}]
  ;;

  let%expect_test _ =
    test_parse {| 



    |};
    [%expect
      {|
    [(SEval
        (ELet (Rec,
           ((PVar "fact"),
            (EFun ((PVar "x"),
               (EIf ((EBin_op (Eq, (EVar "x"), (EConst (CInt 1)))), (EVar "x"),
                  (EBin_op (Mul, (EVar "x"),
                     (EApply ((EVar "fact"),
                        (EBin_op (Sub, (EVar "x"), (EConst (CInt 1))))))
                     ))
                  ))
               ))),
           (EApply ((EVar "fact"), (EConst (CInt 42)))))))
      ] |}]
  ;;
end

module InferTests = struct end
