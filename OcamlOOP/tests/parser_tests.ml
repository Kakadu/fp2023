(** Copyright 2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open OcamlOOP_lib
open Parser

let parse_test s =
  match parse_prefix s with
  | Ok v -> Format.printf "%s\n" Ast.(show_program v)
  | Error _ -> Format.printf "Syntax error\n"
;;

let%expect_test _ =
  let () = parse_test "1 + 2 / 3;;" in
  [%expect
    {|
    [(Str_eval
        (Exp_bin_op (Plus, (Exp_constant (Const_int 1)),
           (Exp_bin_op (Divider, (Exp_constant (Const_int 2)),
              (Exp_constant (Const_int 3))))
           )))
      ] |}]
;;

let%expect_test _ =
  let () = parse_test "1 * 2 / 3;;" in
  [%expect
    {|
    [(Str_eval
        (Exp_bin_op (Divider,
           (Exp_bin_op (Asterisk, (Exp_constant (Const_int 1)),
              (Exp_constant (Const_int 2)))),
           (Exp_constant (Const_int 3)))))
      ] |}]
;;

let%expect_test _ =
  let () = parse_test {|true && true && false;;|} in
  [%expect
    {|
    [(Str_eval
        (Exp_bin_op (And, (Exp_constant (Const_bool true)),
           (Exp_bin_op (And, (Exp_constant (Const_bool true)),
              (Exp_constant (Const_bool false))))
           )))
      ] |}]
;;

let%expect_test _ =
  let () = parse_test {|not vbool ;;|} in
  [%expect {| [(Str_eval (Exp_unary_op (Not, (Exp_ident "vbool"))))] |}]
;;

let%expect_test _ =
  let () = parse_test {|[1; 2; 3; 4];;|} in
  [%expect
    {|
    [(Str_eval
        (Exp_list ((Exp_constant (Const_int 1)),
           (Exp_list ((Exp_constant (Const_int 2)),
              (Exp_list ((Exp_constant (Const_int 3)),
                 (Exp_list ((Exp_constant (Const_int 4)),
                    (Exp_constant Const_nil)))
                 ))
              ))
           )))
      ] |}]
;;

let%expect_test _ =
  let () = parse_test {|let a = stack#pop|} in
  [%expect
    {|
    [(Str_value
        { d_rec = Nonrecursive; d_pat = (Pat_var "a");
          d_expr = (Exp_send ((Exp_ident "stack"), "pop")) })
      ] |}]
;;

let%expect_test _ =
  let () = parse_test {|object (_) method i x = x + 1 end;;|} in
  [%expect
    {|
    [(Str_eval
        (Exp_object
           { o_self = Pat_any;
             o_fields =
             [(Obj_method (Public, "i",
                 (Exp_function ((Pat_var "x"),
                    (Exp_bin_op (Plus, (Exp_ident "x"),
                       (Exp_constant (Const_int 1))))
                    ))
                 ))
               ]
             }))
      ] |}]
;;

let%expect_test _ =
  let () = parse_test {|let f a = fun x -> x + a|} in
  [%expect
    {|
    [(Str_value
        { d_rec = Nonrecursive; d_pat = (Pat_var "f");
          d_expr =
          (Exp_function ((Pat_var "a"),
             (Exp_function ((Pat_var "x"),
                (Exp_bin_op (Plus, (Exp_ident "x"), (Exp_ident "a")))))
             ))
          })
      ] |}]
;;

let%expect_test _ =
  let () = parse_test {|(2, 3, 4);;|} in
  [%expect
    {|
    [(Str_eval
        (Exp_tuple
           [(Exp_constant (Const_int 2)); (Exp_constant (Const_int 3));
             (Exp_constant (Const_int 4))]))
      ] |}]
;;

let%expect_test _ =
  let () = parse_test {|let a = fun (x :: y) -> (1, 5)|} in
  [%expect
    {|
    [(Str_value
        { d_rec = Nonrecursive; d_pat = (Pat_var "a");
          d_expr =
          (Exp_function ((Pat_cons ((Pat_var "x"), (Pat_var "y"))),
             (Exp_tuple
                [(Exp_constant (Const_int 1)); (Exp_constant (Const_int 5))])
             ))
          })
      ] |}]
;;

let%expect_test _ =
  let () = parse_test {|let a = fun ((x, y) :: z) -> 5|} in
  [%expect
    {|
    [(Str_value
        { d_rec = Nonrecursive; d_pat = (Pat_var "a");
          d_expr =
          (Exp_function (
             (Pat_cons ((Pat_tuple [(Pat_var "x"); (Pat_var "y")]), (Pat_var "z")
                )),
             (Exp_constant (Const_int 5))))
          })
      ] |}]
;;

let%expect_test _ =
  let () = parse_test "1, 2;;" in
  [%expect
    {|
    [(Str_eval
        (Exp_tuple [(Exp_constant (Const_int 1)); (Exp_constant (Const_int 2))]))
      ] |}]
;;

let%expect_test _ =
  let () = parse_test {|let a = fun (x, y) -> 1, 2 in x;;|} in
  [%expect
    {|
    [(Str_eval
        (Exp_let (
           { d_rec = Nonrecursive; d_pat = (Pat_var "a");
             d_expr =
             (Exp_function ((Pat_tuple [(Pat_var "x"); (Pat_var "y")]),
                (Exp_tuple
                   [(Exp_constant (Const_int 1)); (Exp_constant (Const_int 2))])
                ))
             },
           (Exp_ident "x"))))
      ] |}]
;;

let%expect_test _ =
  let () = parse_test {|let id x = x in let snd a b = b in snd (id 1) (id 0);;|} in
  [%expect
    {|
    [(Str_eval
        (Exp_let (
           { d_rec = Nonrecursive; d_pat = (Pat_var "id");
             d_expr = (Exp_function ((Pat_var "x"), (Exp_ident "x"))) },
           (Exp_let (
              { d_rec = Nonrecursive; d_pat = (Pat_var "snd");
                d_expr =
                (Exp_function ((Pat_var "a"),
                   (Exp_function ((Pat_var "b"), (Exp_ident "b")))))
                },
              (Exp_apply (
                 (Exp_apply ((Exp_ident "snd"),
                    (Exp_apply ((Exp_ident "id"), (Exp_constant (Const_int 1))))
                    )),
                 (Exp_apply ((Exp_ident "id"), (Exp_constant (Const_int 0))))))
              ))
           )))
      ] |}]
;;

let%expect_test _ =
  let _ = parse_test {|1::2::[3];;|} in
  [%expect
    {|
    [(Str_eval
        (Exp_list ((Exp_constant (Const_int 1)),
           (Exp_list ((Exp_constant (Const_int 2)),
              (Exp_list ((Exp_constant (Const_int 3)), (Exp_constant Const_nil)))
              ))
           )))
      ] |}]
;;

let%expect_test _ =
  let _ = parse_test {|let a (h::tl) = 5|} in
  [%expect
    {|
    [(Str_value
        { d_rec = Nonrecursive; d_pat = (Pat_var "a");
          d_expr =
          (Exp_function ((Pat_cons ((Pat_var "h"), (Pat_var "tl"))),
             (Exp_constant (Const_int 5))))
          })
      ] |}]
;;

(* =============== Objects =============== *)

let%expect_test _ =
  let _ = parse_test {|let a = object val a = 5 end|} in
  [%expect
    {|
    [(Str_value
        { d_rec = Nonrecursive; d_pat = (Pat_var "a");
          d_expr =
          (Exp_object
             { o_self = Pat_any;
               o_fields = [(Obj_val ("a", (Exp_constant (Const_int 5))))] })
          })
      ] |}]
;;

let%expect_test _ =
  let () = parse_test {|let a = object method b x = x end|} in
  [%expect
    {|
    [(Str_value
        { d_rec = Nonrecursive; d_pat = (Pat_var "a");
          d_expr =
          (Exp_object
             { o_self = Pat_any;
               o_fields =
               [(Obj_method (Public, "b",
                   (Exp_function ((Pat_var "x"), (Exp_ident "x")))))
                 ]
               })
          })
      ] |}]
;;

let%expect_test "Overriding__objects_fields" =
  let () = parse_test {|let o = object val increase = {< n = n + 1 >} end|} in
  [%expect
    {|
    [(Str_value
        { d_rec = Nonrecursive; d_pat = (Pat_var "o");
          d_expr =
          (Exp_object
             { o_self = Pat_any;
               o_fields =
               [(Obj_val ("increase",
                   (Exp_override
                      [("n",
                        (Exp_bin_op (Plus, (Exp_ident "n"),
                           (Exp_constant (Const_int 1)))))
                        ])
                   ))
                 ]
               })
          })
      ] |}]
;;

let%expect_test "Overriding" =
  let () =
    parse_test {|let o = object val n = 0 method increase x = {< n = n + x >} end|}
  in
  [%expect
    {|
    [(Str_value
        { d_rec = Nonrecursive; d_pat = (Pat_var "o");
          d_expr =
          (Exp_object
             { o_self = Pat_any;
               o_fields =
               [(Obj_val ("n", (Exp_constant (Const_int 0))));
                 (Obj_method (Public, "increase",
                    (Exp_function ((Pat_var "x"),
                       (Exp_override
                          [("n",
                            (Exp_bin_op (Plus, (Exp_ident "n"), (Exp_ident "x"))))
                            ])
                       ))
                    ))
                 ]
               })
          })
      ] |}]
;;

let%expect_test _ =
  let () =
    parse_test
      {| let a = object (self) method d x = x + 2 method b = self#c method c = self#d 0 end |}
  in
  [%expect
    {|
    [(Str_value
        { d_rec = Nonrecursive; d_pat = (Pat_var "a");
          d_expr =
          (Exp_object
             { o_self = (Pat_var "self");
               o_fields =
               [(Obj_method (Public, "d",
                   (Exp_function ((Pat_var "x"),
                      (Exp_bin_op (Plus, (Exp_ident "x"),
                         (Exp_constant (Const_int 2))))
                      ))
                   ));
                 (Obj_method (Public, "b", (Exp_send ((Exp_ident "self"), "c"))));
                 (Obj_method (Public, "c",
                    (Exp_apply ((Exp_send ((Exp_ident "self"), "d")),
                       (Exp_constant (Const_int 0))))
                    ))
                 ]
               })
          })
      ] |}]
;;

let%expect_test _ =
  let () =
    parse_test
      {| let a = object (self) method d x = x + 2 method b = self#c method c = 4 end |}
  in
  [%expect
    {|
    [(Str_value
        { d_rec = Nonrecursive; d_pat = (Pat_var "a");
          d_expr =
          (Exp_object
             { o_self = (Pat_var "self");
               o_fields =
               [(Obj_method (Public, "d",
                   (Exp_function ((Pat_var "x"),
                      (Exp_bin_op (Plus, (Exp_ident "x"),
                         (Exp_constant (Const_int 2))))
                      ))
                   ));
                 (Obj_method (Public, "b", (Exp_send ((Exp_ident "self"), "c"))));
                 (Obj_method (Public, "c", (Exp_constant (Const_int 4))))]
               })
          })
      ] |}]
;;

let input =
  {|
let a = object (self) val n = 0 method increase = {< n = n + 1>} method get = n end

let b = a#increase#increase#increase

|}
;;

let%expect_test _ =
  let () = parse_test input in
  [%expect
    {|
    [(Str_value
        { d_rec = Nonrecursive; d_pat = (Pat_var "a");
          d_expr =
          (Exp_object
             { o_self = (Pat_var "self");
               o_fields =
               [(Obj_val ("n", (Exp_constant (Const_int 0))));
                 (Obj_method (Public, "increase",
                    (Exp_override
                       [("n",
                         (Exp_bin_op (Plus, (Exp_ident "n"),
                            (Exp_constant (Const_int 1)))))
                         ])
                    ));
                 (Obj_method (Public, "get", (Exp_ident "n")))]
               })
          });
      (Str_value
         { d_rec = Nonrecursive; d_pat = (Pat_var "b");
           d_expr =
           (Exp_send (
              (Exp_send ((Exp_send ((Exp_ident "a"), "increase")), "increase")),
              "increase"))
           })
      ] |}]
;;

let%expect_test _ =
  let () = parse_test "-1::[2];;" in
  [%expect
    {|
      [(Str_eval
          (Exp_list ((Exp_unary_op (Minus, (Exp_constant (Const_int 1)))),
             (Exp_list ((Exp_constant (Const_int 2)), (Exp_constant Const_nil))))))
        ] |}]
;;

let%expect_test _ =
  let () =
    parse_test "let rec fact n k = if n <= 1 then k 1 else fact (n-1) (fun z -> k(z*n))"
  in
  [%expect
    {|
    [(Str_value
        { d_rec = Recursive; d_pat = (Pat_var "fact");
          d_expr =
          (Exp_function ((Pat_var "n"),
             (Exp_function ((Pat_var "k"),
                (Exp_ifthenelse (
                   (Exp_bin_op (Ltq, (Exp_ident "n"),
                      (Exp_constant (Const_int 1)))),
                   (Exp_apply ((Exp_ident "k"), (Exp_constant (Const_int 1)))),
                   (Exp_apply (
                      (Exp_apply ((Exp_ident "fact"),
                         (Exp_bin_op (Sub, (Exp_ident "n"),
                            (Exp_constant (Const_int 1))))
                         )),
                      (Exp_function ((Pat_var "z"),
                         (Exp_apply ((Exp_ident "k"),
                            (Exp_bin_op (Asterisk, (Exp_ident "z"),
                               (Exp_ident "n")))
                            ))
                         ))
                      ))
                   ))
                ))
             ))
          })
      ] |}]
;;
