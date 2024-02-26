(** Copyright 2023-2024, Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_pv
open Parser
open Infer
open Interpreter

(*----------------------------- Parser ---------------------------------------*)

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

(*------------------------------ Inferencer _---------------------------------*)

let%expect_test _ =
  test_infer {| 
      let rec fix f x = f (fix f) x ;;
     |};
  [%expect {| val fix : (('2 -> '3) -> '2 -> '3) -> '2 -> '3 |}]
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
  [%expect {| val fold_left : ('11 -> '5 -> '11) -> '11 -> '5 list -> '11 |}]
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
  [%expect {| val fs : (('0 -> '0) * (int -> int -> int)) |}]
;;

let%expect_test _ =
  test_infer {| 
      let f x = x + y;;
      let y = 3;;
     |};
  [%expect {| Infer error: Unbound variable 'y' |}]
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
  [%expect {| val f : ('1 -> '2 -> '3 -> '4 -> '5) -> '1 -> '2 -> '3 -> '4 -> '5 |}]
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
  [%expect {| val map_cps : ('6 -> '8) -> '6 list -> '8 list |}]
;;

let%expect_test _ =
  test_infer {| 
    let x = [`A 52; `B 52; `C 52];;
     |};
  [%expect {| Infer error: Not implemented |}]
;;

let%expect_test _ =
  test_infer
    {| 
    let f x =
      match x with
      | `A 3 -> 52
      | `B _ -> 53
    ;;
     |};
  [%expect {| Infer error: Pattern matching error |}]
;;

let%expect_test _ =
  test_infer {|
    let l = [`A; `B; `C];;
    ;;
    |};
  [%expect {|
    Infer error: Not implemented |}]
;;

let%expect_test _ =
  test_infer
    {|
    let f x =
      match x with
      | `A x -> x + 1
      | `B _ -> 0
    ;;
    |};
  [%expect {|
    Infer error: Pattern matching error |}]
;;

(*------------------------------ Interpreter ---------------------------------*)

let%expect_test _ =
  test_interpret
    {|
      let rec fac n = if n < 1 then 1 else n * fac (n - 1);;
      let x = fac 5;; 
    |};
  [%expect {|
    val fac : int -> int = <fun>
    val x : int = 120 |}]
;;

let%expect_test _ =
  test_interpret
    {|
    let rec fix f x = f (fix f) x;;
    let fac fac_ n = if n < 1 then 1 else n * fac_ (n - 1);;
    let f = fix fac 5;;
    |};
  [%expect
    {|
    val f : int = 120
    val fac : (int -> int) -> int -> int = <fun>
    val fix : (('2 -> '3) -> '2 -> '3) -> '2 -> '3 = <fun> |}]
;;

let%expect_test _ =
  test_interpret
    {|
    let x = 
      let y = 
        let z = 
          let w = 1
          in w
        in z
      in y
    |};
  [%expect {|
    val x : int = 1 |}]
;;

let%expect_test _ =
  test_interpret
    {|
    let a =
      let b = 
        let rec f = (let x = 3 in x) + 1 
        in f
      in ();;
    let s = "string";;
    |};
  [%expect {|
    val a : unit = ()
    val s : string = string |}]
;;

let%expect_test _ =
  test_interpret
    {|
    let x =
      let rec fac n = if n = 1 then 1 else n * fac (n - 1) in
      fac 5
    ;;
    |};
  [%expect {|
    val x : int = 120 |}]
;;

let%expect_test _ =
  test_interpret
    {|
    let rev l = 
      let rec helper acc xs =
        match xs with
        | [] -> acc
        | h :: tl -> helper (h :: acc) tl
      in
      helper [] l
    ;;

    let reversed = rev [1;2;3;4;5];;
    |};
  [%expect
    {|
    val rev : '12 list -> '12 list = <fun>
    val reversed : int list = [5; 4; 3; 2; 1] |}]
;;

let%expect_test _ =
  test_interpret
    {|
    let f a b c d e = a b c d e;;
    let id x = x;;

    let cmp = f id id id id (fun x -> x + 1);;
    |};
  [%expect
    {|
    val cmp : int -> int = <fun>
    val f : ('1 -> '2 -> '3 -> '4 -> '5) -> '1 -> '2 -> '3 -> '4 -> '5 = <fun>
    val id : '9 -> '9 = <fun> |}]
;;

let%expect_test _ =
  test_interpret
    {|
  let arith x y = (x * y, x / y, x + y, x - y);;
  let prod x y = 
    let fst (a, _, _, _) = a in
    fst (arith x y)
  ;;
  let p = prod 3 0;;
    |};
  [%expect {|
    Interpreter error: Division by zero |}]
;;

let%expect_test _ =
  test_interpret
    {|
    let f l = match l with
      | x :: y :: tl -> x + y
      | x :: y :: z :: tl -> x + y + z
    ;;
    let x = f [1];;
    |};
  [%expect {|
    Interpreter error: Pattern-matching failed |}]
;;

let%expect_test _ =
  test_interpret {|
    let `A x = `A 3;;
    |};
  [%expect {|
    Infer error: Not implemented |}]
;;

let%expect_test _ =
  test_interpret {|
    let (x, y, z) = (1, 2, 3, 4);;
    |};
  [%expect {|
    Infer error: Not implemented |}]
;;
