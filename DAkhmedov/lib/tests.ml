(** Copyright 2023-2024, David Akhmedov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OCaml_ExtensibleVariantTypes_lib

module ParsingTests = struct
  let parse_test s expected_res =
    match Parser.parse_program s with
    | Ok actual -> List.equal Ast.equal_decl expected_res actual
    | Error err ->
      Format.printf "%s\n" err;
      false
  ;;

  let%test _ = parse_test "let a = 3" [ DLet (Not_recursive, "a", EConst (CInt 3)) ]

  let%test _ =
    parse_test
      "let a = let f x = x in 13"
      [ DLet
          ( Not_recursive
          , "a"
          , EClsr (DLet (Not_recursive, "f", EFun (PId "x", EId "x")), EConst (CInt 13))
          )
      ]
  ;;

  let%test _ =
    parse_test
      "let a = let f x = x + 1 in let b = 5 in f (b + 1)"
      [ DLet
          ( Not_recursive
          , "a"
          , EClsr
              ( DLet
                  ( Not_recursive
                  , "f"
                  , EFun (PId "x", EBinop (EId "x", Add, EConst (CInt 1))) )
              , EClsr
                  ( DLet (Not_recursive, "b", EConst (CInt 5))
                  , EApp (EId "f", EBinop (EId "b", Add, EConst (CInt 1))) ) ) )
      ]
  ;;

  let%test _ =
    parse_test
      "let a =([3;3], [4;4]) :: [([5;5],[6;6]);([7;7],[8;8])]"
      [ DLet
          ( Not_recursive
          , "a"
          , EList
              ( ETuple
                  [ EList (EConst (CInt 3), EList (EConst (CInt 3), EConst CNil))
                  ; EList (EConst (CInt 4), EList (EConst (CInt 4), EConst CNil))
                  ]
              , EList
                  ( ETuple
                      [ EList (EConst (CInt 5), EList (EConst (CInt 5), EConst CNil))
                      ; EList (EConst (CInt 6), EList (EConst (CInt 6), EConst CNil))
                      ]
                  , EList
                      ( ETuple
                          [ EList (EConst (CInt 7), EList (EConst (CInt 7), EConst CNil))
                          ; EList (EConst (CInt 8), EList (EConst (CInt 8), EConst CNil))
                          ]
                      , EConst CNil ) ) ) )
      ]
  ;;

  let%test _ =
    parse_test
      "let a = match l with | hd::tl -> hd | _ -> 0"
      [ DLet
          ( Not_recursive
          , "a"
          , EMatch
              (EId "l", [ PList (PId "hd", PId "tl"), EId "hd"; PId "_", EConst (CInt 0) ])
          )
      ]
  ;;

  let%test _ =
    parse_test "let a = - 3" [ DLet (Not_recursive, "a", EUnop (Minus, EConst (CInt 3))) ]
  ;;

  let%test _ =
    parse_test
      "let a = f a + f b"
      [ DLet
          ( Not_recursive
          , "a"
          , EBinop (EApp (EId "f", EId "a"), Add, EApp (EId "f", EId "b")) )
      ]
  ;;

  let%test _ =
    parse_test
      "let a = - f 3 - f 4"
      [ DLet
          ( Not_recursive
          , "a"
          , EBinop
              ( EUnop (Minus, EApp (EId "f", EConst (CInt 3)))
              , Sub
              , EApp (EId "f", EConst (CInt 4)) ) )
      ]
  ;;

  let%test _ =
    parse_test
      "let f (hd::tl) = 3"
      [ DLet (Not_recursive, "f", EFun (PList (PId "hd", PId "tl"), EConst (CInt 3))) ]
  ;;

  let%test _ =
    parse_test
      "let f (x,y) = x + y"
      [ DLet
          ( Not_recursive
          , "f"
          , EFun (PTuple [ PId "x"; PId "y" ], EBinop (EId "x", Add, EId "y")) )
      ]
  ;;

  let%test _ = parse_test "let f  = []" [ DLet (Not_recursive, "f", EConst CNil) ]

  let%test _ =
    parse_test
      "let a = let f x = x in f (f (f (5 + f 5+5+5+f 5) + 5) + 5) + 5"
      [ DLet
          ( Not_recursive
          , "a"
          , EClsr
              ( DLet (Not_recursive, "f", EFun (PId "x", EId "x"))
              , EBinop
                  ( EApp
                      ( EId "f"
                      , EBinop
                          ( EApp
                              ( EId "f"
                              , EBinop
                                  ( EApp
                                      ( EId "f"
                                      , EBinop
                                          ( EBinop
                                              ( EBinop
                                                  ( EBinop
                                                      ( EConst (CInt 5)
                                                      , Add
                                                      , EApp (EId "f", EConst (CInt 5)) )
                                                  , Add
                                                  , EConst (CInt 5) )
                                              , Add
                                              , EConst (CInt 5) )
                                          , Add
                                          , EApp (EId "f", EConst (CInt 5)) ) )
                                  , Add
                                  , EConst (CInt 5) ) )
                          , Add
                          , EConst (CInt 5) ) )
                  , Add
                  , EConst (CInt 5) ) ) )
      ]
  ;;
end

module InferenceTests = struct
  let infer_test s =
    match Parser.parse_program s with
    | Ok actual ->
      let env = OCaml_ExtensibleVariantTypes_lib.Inferencer.run_inference actual in
      OCaml_ExtensibleVariantTypes_lib.Inferencer.print_env env
    | Error err -> Format.printf "%s\n" err
  ;;

  let%expect_test _ =
    infer_test
      {| let f x = x;; let a = match [3;4;5] with | hd::tl -> (2, f true) | hd -> (f 2, false) |};
    [%expect {|
        val a : int * bool
        val f : 'a -> 'a
    |}]
  ;;

  let%expect_test _ =
    infer_test {| let a = let rec f x = if x=0 then 1 else x * f (x-1) in f 4 |};
    [%expect {|
        val a : int
    |}]
  ;;

  let%expect_test _ =
    infer_test {| let a =([3;3], [4;4]) :: [([5;5],[6;6]);([7;7],[8;8])] |};
    [%expect {| val a : (int list * int list) list |}]
  ;;

  let%expect_test _ =
    infer_test {| let a x = x :: [1; 2; 3] |};
    [%expect {| val a : int -> int list |}]
  ;;

  let%expect_test _ =
    infer_test
      {| let check_equal x y = 
          let sum a b = a + b in 
          let sub a b = a - b in 
          let rev x = -x in 
          if rev x = rev y then sub x y else sum x y |};
    [%expect {| val check_equal : int -> int -> int |}]
  ;;

  let%expect_test _ =
    infer_test
      {|   let reverse l = 
            let helper acc list = match list with 
              | [] -> acc
              | (h :: tl) -> (h :: acc) 
            in helper [] l |};
    [%expect {| val reverse : 'a list -> 'a list |}]
  ;;

  let%expect_test _ =
    infer_test {|  let rec fix f = f (fix f) 
      |};
    [%expect {| 
          val fix : ('a -> 'a) -> 'a
    |}]
  ;;

  let%expect_test _ =
    infer_test {|  let rec fix f x = f (fix f) x 
      |};
    [%expect {| 
          val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
    |}]
  ;;

  let%expect_test _ =
    infer_test
      {|
        let a = let f x = x in f (f (f (5 + f 5+5+5+f 5) + 5) + 5) + 5
       |};
    [%expect {| 
        val a : int
      |}]
  ;;

  let%expect_test _ =
    infer_test
      {|
          let rec factorial n =
            if n <= 1
            then 1
            else (
              let rec inner_factorial m acc =
                if m <= 1 then acc else inner_factorial (m - 1) (acc * m)
              in
              inner_factorial n 1)

          let rec sum_of_squares n =
            let rec sum_of_squares_helper i acc =
              if i <= 0
              then acc
              else (
                let square = i * i in
                sum_of_squares_helper (i - 1) (acc + square))
            in
            sum_of_squares_helper n 0

          let rec is_prime n =
            let rec is_divisible_by d =
              if d * d > n
              then false
              else if n - (n / d * d) = 0
              then true
              else is_divisible_by (d + 1)
            in
            if n <= 1 then false else not (is_divisible_by 2)

          let rec fibonacci n =
            let rec fibonacci_helper a b count =
              if count <= 0 then b else fibonacci_helper (a + b) a (count - 1)
            in
            fibonacci_helper 1 0 n

          let rec weird_function x y =
            let z = if x < y then x else y in
            let rec helper a b = if a <= 0 then b else helper (a - 1) (b + z) in
            helper x y
        |};
    [%expect
      {| 
        val factorial : int -> int
        val fibonacci : int -> int
        val is_prime : int -> bool
        val sum_of_squares : int -> int
        val weird_function : int -> int -> int
        |}]
  ;;

  let%expect_test _ =
    infer_test
      {|  
        let rec is_prime n =
          let rec is_divisible_by d =
            if d * d > n then
              false
            else if n - d = 0 then
              true
            else
              is_divisible_by (d + 1)
          in
          if n <= 1 then
            false
          else
            not (is_divisible_by 2)
        |};
    [%expect {| 
        val is_prime : int -> bool
        |}]
  ;;

  let%expect_test _ =
    infer_test
      {|  
        let rec recfun n =
          match n with
          | 0 -> 1
          | m -> 
              let rec inner_rec m acc =
                match m with
                | 1 -> acc
                | p ->
                  let result =
                    match (p < 10, p < 20) with
                    | (true, _) ->
                      inner_rec (m - 1) (acc * n)
                    | (false, true) ->
                      inner_rec (m - 2) (acc * (n + 2))
                    | (_, _) ->
                      inner_rec (m - n) (acc - n)
                  in
                  if result > 100 then
                    result - 10
                  else
                    p * result + 5
              in
              if m > 0 then inner_rec n 1 else 0
        |};
    [%expect {| 
        val recfun : int -> int
        |}]
  ;;

  let%expect_test _ =
    infer_test
      {| let rec map f lst = 
        match lst with 
          | [] -> []
          | (x::xs) -> f x :: map f xs |};
    [%expect {| val map : ('a -> 'b) -> 'a list -> 'b list |}]
  ;;
end

module InterpretTests = struct
  let interpret_test = OCaml_ExtensibleVariantTypes_lib.Interpreter.run_and_pp

  let%expect_test _ =
    interpret_test
      {|
        let a = let add_one x = x + 1 in
        let apply_twice f x = f (f x) in
        apply_twice add_one 3
       |};
    [%expect {| 
        val a : int = 5
      |}]
  ;;

  let%expect_test _ =
    interpret_test
      {|
        let rec power x n =
          if n = 0 then 1
          else x * power x (n - 1)

        let rec sum_of_squares n =
          let rec sum_sq_iter acc x =
            if x = 0 then acc
            else sum_sq_iter (acc + x * x) (x - 1)
          in
          sum_sq_iter 0 n

        let base = 3
        let exponent = 4
        let power_result = power base exponent
        let sum_of_squares_result = sum_of_squares 5

        let res = (base, exponent, power_result, sum_of_squares_result)
       |};
    [%expect
      {| 
        val base : int = 3
        val exponent : int = 4
        val power : int -> int -> int = <fun>
        val power_result : int = 81
        val res : int * int * int * int = (3, 4, 81, 55)
        val sum_of_squares : int -> int = <fun>
        val sum_of_squares_result : int = 55
      |}]
  ;;

  let%expect_test _ =
    interpret_test
      {|
        let fst (x, _) = x
        let snd (_, x) = x
        let a = fst (1,2) + snd (3,4)
       |};
    [%expect
      {| 
        val a : int = 5
        val fst : 'a * 'b -> 'a = <fun>
        val snd : 'a * 'b -> 'b = <fun>
      |}]
  ;;
end
