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
end

module InferTests = struct
  open Infer

  let test_infer s =
    let open Format in
    match Parser.parse s with
    | Ok parsed ->
      (match run_infer parsed with
       | Ok env ->
         Base.Map.iteri env ~f:(fun ~key ~data:(S (_, ty)) ->
           printf "val %s : %a\n" key Typedtree.pp_typ ty)
       | Error e -> Format.printf "%a" pp_error e)
    | Error e -> Format.printf "Parsing error: %s\n" e
  ;;

  let%expect_test _ =
    test_infer {| 
      let rec fix f x = f (fix f) x ;;
     |};
    [%expect {| val fix : (('_2 -> '_3) -> '_2 -> '_3) -> '_2 -> '_3 |}]
  ;;

  let%expect_test _ =
    test_infer
      {| 
   let rec fold_left f acc l =
      match l with
      | [] -> acc
      | h :: tl -> fold_left f (f acc h) tl
   ;;
     |};
    [%expect {| val fold_left : ('_11 -> '_5 -> '_11) -> '_11 -> '_5 list -> '_11 |}]
  ;;

  let%expect_test _ =
    test_infer {| 
    let f x y = (x + y, [x; y])]
     |};
    [%expect {| val f : int -> int -> (int * int list) |}]
  ;;

  let%expect_test _ =
    test_infer {| 
      let fs = ((fun x -> x), (fun x y -> x + y))
     |};
    [%expect {| val fs : (('_0 -> '_0) * (int -> int -> int)) |}]
  ;;

  let%expect_test _ =
    test_infer {| 
      let f x = x + y;;
      let y = 3;;
     |};
    [%expect {| Unbound variable 'y' |}]
  ;;

  let%expect_test _ =
    test_infer {| 
      let f x = 
         let y = 3 in
         x + y;;
     |};
    [%expect {| val f : int -> int |}]
  ;;

  let%expect_test _ =
    test_infer {| 
    let f a b c d e = a b c d e;;
     |};
    [%expect
      {| val f : ('_1 -> '_2 -> '_3 -> '_4 -> '_5) -> '_1 -> '_2 -> '_3 -> '_4 -> '_5 |}]
  ;;

  let%expect_test _ =
    test_infer
      {| 

      let map_cps f l = 
        let rec helper k xs =
          match xs with
          | [] -> k []
          | h :: tl -> helper (fun r -> k ((f h) :: r)) tl
        in
        helper (fun x -> x) l
      ;;
     |};
    [%expect
      {| val map_cps : '_0 -> '_6 list -> '_8 list |}]
  ;;
end
