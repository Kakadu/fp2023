(** Copyright 2023, Arseniy Baytenov *)

(** SPDX-License-Identifier: MIT *)

open Base
open Cypher_lib.Ast
open Cypher_lib.Parser

let parse_and_print s =
  Stdlib.Format.printf
    "%a"
    pp_clause
    (match parse_request s with
     | Ok v -> v
     | Error msg -> failwith msg)
;;

let%expect_test "Simple expr" =
  parse_and_print {| RETURN 120.607 + 400 / 10 ^ 5 |};
  [%expect
    {|
    (Return (None,
       [((Bin_op (Plus, (Const (Float 120.607)),
            (Bin_op (Slash, (Const (Int64 400L)),
               (Bin_op (Caret, (Const (Int64 10L)), (Const (Int64 5L))))))
            )),
         None)],
       [])) |}]
;;

let%expect_test "Simple expr with unary minus" =
  parse_and_print {| RETURN - 120.607 + -400 / 10 ^ -x |};
  [%expect
    {|
    (Return (None,
       [((Bin_op (Plus, (Un_op (Minus, (Const (Float 120.607)))),
            (Bin_op (Slash, (Const (Int64 -400L)),
               (Bin_op (Caret, (Const (Int64 10L)), (Un_op (Minus, (Var "x")))))
               ))
            )),
         None)],
       [])) |}]
;;

let%expect_test "Multiple sequential comparison operators" =
  parse_and_print {| RETURN 1+-1 = 0 <> 10 >= 5*1 <= 5/1 > - -4 <> null |};
  [%expect
    {|
    (Return (None,
       [((List_op ((Bin_op (Plus, (Const (Int64 1L)), (Const (Int64 -1L)))),
            [(Eq, (Const (Int64 0L))); (NEq, (Const (Int64 10L)));
              (GEq, (Bin_op (Asterisk, (Const (Int64 5L)), (Const (Int64 1L)))));
              (LEq, (Bin_op (Slash, (Const (Int64 5L)), (Const (Int64 1L)))));
              (Greater, (Un_op (Minus, (Const (Int64 -4L)))));
              (NEq, (Const Null))]
            )),
         None)],
       [])) |}]
;;

let%expect_test "Null check operators" =
  parse_and_print {| RETURN 4 = (4 + a is null IS NOT NULL) is null |};
  [%expect
    {|
    (Return (None,
       [((List_op ((Const (Int64 4L)),
            [(Eq,
              (Un_op (IS_NULL,
                 (Bin_op (Plus, (Const (Int64 4L)),
                    (Un_op (IS_NOT_NULL, (Un_op (IS_NULL, (Var "a")))))))
                 )))
              ]
            )),
         None)],
       [])) |}]
;;

let%expect_test "Boolean oparators and literals" =
  parse_and_print
    {| RETURN not 1 = 1 or 4 = a + 23.0 and false xor not true or "Hello" = a + "llo" |};
  [%expect
    {|
    (Return (None,
       [((Bin_op (OR,
            (Bin_op (OR,
               (Un_op (NOT,
                  (List_op ((Const (Int64 1L)), [(Eq, (Const (Int64 1L)))])))),
               (Bin_op (AND,
                  (List_op ((Const (Int64 4L)),
                     [(Eq, (Bin_op (Plus, (Var "a"), (Const (Float 23.)))))])),
                  (Bin_op (XOR, (Const (Bool false)),
                     (Un_op (NOT, (Const (Bool true))))))
                  ))
               )),
            (List_op ((Const (String "Hello")),
               [(Eq, (Bin_op (Plus, (Var "a"), (Const (String "llo")))))]))
            )),
         None)],
       [])) |}]
;;

let%expect_test "String specific comparison operators" =
  parse_and_print
    {| RETURN "hola" STARTS WITH "ho", "hola" ENDS WITH "la", "hola" CONTAINS "ola" |};
  [%expect
    {|
    (Return (None,
       [((Bin_op (STARTS_WITH, (Const (String "hola")), (Const (String "ho")))),
         None);
         ((Bin_op (ENDS_WITH, (Const (String "hola")), (Const (String "la")))),
          None);
         ((Bin_op (CONTAINS, (Const (String "hola")), (Const (String "ola")))),
          None)
         ],
       [])) |}]
;;

let%expect_test "RETURN clause test1" =
  parse_and_print {| RETURN * |};
  [%expect {| (Return ((Some All), [], [])) |}]
;;

let%expect_test "RETURN clause test2" =
  parse_and_print {| RETURN *, 4, 5 as five |};
  [%expect
    {|
    (Return ((Some All),
       [((Const (Int64 4L)), None); ((Const (Int64 5L)), (Some "five"))],
       [])) |}]
;;

let%expect_test "RETURN clause test3" =
  parse_and_print {| RETURN 4 as four, 5 as five |};
  [%expect
    {|
    (Return (None,
       [((Const (Int64 4L)), (Some "four")); ((Const (Int64 5L)), (Some "five"))],
       [])) |}]
;;

let%expect_test "WITH clause test1" =
  parse_and_print {| WITH * RETURN * |};
  [%expect {| (With ((Some All), [], [], [], (Return ((Some All), [], [])))) |}]
;;

let%expect_test "WITH clause test2" =
  parse_and_print {| WITH *, 4 as four, 5 as five RETURN * |};
  [%expect
    {|
    (With ((Some All),
       [((Const (Int64 4L)), "four"); ((Const (Int64 5L)), "five")], [],
       [], (Return ((Some All), [], [])))) |}]
;;

let%expect_test "WITH clause test3" =
  parse_and_print {| WITH 4 as four RETURN * |};
  [%expect
    {|
    (With (None, [((Const (Int64 4L)), "four")], [], [],
       (Return ((Some All), [], [])))) |}]
;;

let%expect_test "MATCH clause test1" =
  parse_and_print {|
  WITH *
  WITH 4 as four, 5 as five
  MATCH ()
  RETURN * |};
  [%expect
    {|
    (With ((Some All), [], [], [],
       (With (None, [((Const (Int64 4L)), "four"); ((Const (Int64 5L)), "five")],
          [], [],
          (Match ([((None, ([], [])), [])], [], (Return ((Some All), [], []))))))
       )) |}]
;;

let%expect_test "MATCH clause test2" =
  parse_and_print {|
  WITH 5 as f
  MATCH ()--(), ()-[]-()-->()<--()
  RETURN * |};
  [%expect
    {|
    (With (None, [((Const (Int64 5L)), "f")], [], [],
       (Match (
          [((None, ([], [])), [((None, ([], []), No), (None, ([], [])))]);
            ((None, ([], [])),
             [((None, ([], []), No), (None, ([], [])));
               ((None, ([], []), Right), (None, ([], [])));
               ((None, ([], []), Left), (None, ([], [])))])
            ],
          [], (Return ((Some All), [], []))))
       )) |}]
;;

let%expect_test "MATCH clause test3" =
  parse_and_print
    {|
  MATCH (:L1:L2{name:"Sasha"})-[r1:L1{d:5, f:6}]-(n1{})-[{}]->(n2)
  RETURN * |};
  [%expect
    {|
    (Match (
       [((None, (["L1"; "L2"], [("name", (Const (String "Sasha")))])),
         [(((Some "r1"),
            (["L1"], [("d", (Const (Int64 5L))); ("f", (Const (Int64 6L)))]), No),
           ((Some "n1"), ([], [])));
           ((None, ([], []), Right), ((Some "n2"), ([], [])))])
         ],
       [], (Return ((Some All), [], [])))) |}]
;;

let%expect_test "CREATE clause test1" =
  parse_and_print
    {|
  WITH *
  WITH 4 as four, 5 as five
  CREATE (), ()-->(), ()<-[]-()-->()<--()
  |};
  [%expect
    {|
    (With ((Some All), [], [], [],
       (With (None, [((Const (Int64 4L)), "four"); ((Const (Int64 5L)), "five")],
          [], [],
          (Create (
             [((None, ([], [])), []);
               ((None, ([], [])), [((None, ([], []), Right), (None, ([], [])))]);
               ((None, ([], [])),
                [((None, ([], []), Left), (None, ([], [])));
                  ((None, ([], []), Right), (None, ([], [])));
                  ((None, ([], []), Left), (None, ([], [])))])
               ],
             None))
          ))
       )) |}]
;;

let%expect_test "CREATE clause test2" =
  parse_and_print
    {|
  WITH 5 as f
  CREATE (:L1:L2{name:"Sasha"})<-[r1:L1{d:5, f:6}]-(n1{})-[{}]->(n2)
  |};
  [%expect
    {|
    (With (None, [((Const (Int64 5L)), "f")], [], [],
       (Create (
          [((None, (["L1"; "L2"], [("name", (Const (String "Sasha")))])),
            [(((Some "r1"),
               (["L1"], [("d", (Const (Int64 5L))); ("f", (Const (Int64 6L)))]),
               Left),
              ((Some "n1"), ([], [])));
              ((None, ([], []), Right), ((Some "n2"), ([], [])))])
            ],
          None))
       )) |}]
;;

let%expect_test "DELETE clause test" =
  parse_and_print
    {|
  WITH *
  WITH 4 as four, 5 as five
  MATCH (n1)-[r]-(n2)
  DELETE n1, (r), n2
  |};
  [%expect
    {|
    (With ((Some All), [], [], [],
       (With (None, [((Const (Int64 4L)), "four"); ((Const (Int64 5L)), "five")],
          [], [],
          (Match (
             [(((Some "n1"), ([], [])),
               [(((Some "r"), ([], []), No), ((Some "n2"), ([], [])))])],
             [], (Delete (Nodetach, ["n1"; "r"; "n2"], None))))
          ))
       )) |}]
;;

let%expect_test "many DELETE and CREATE clauses test" =
  parse_and_print
    {|
  CREATE (n1)
  CREATE (n2)
  DELETE n1
  DELETE n2
  CREATE (n3)
  DELETE n3
  CREATE (n4)
  |};
  [%expect
    {|
    (Create ([(((Some "n1"), ([], [])), [])],
       (Some (Create ([(((Some "n2"), ([], [])), [])],
                (Some (Delete (Nodetach, ["n1"],
                         (Some (Delete (Nodetach, ["n2"],
                                  (Some (Create ([(((Some "n3"), ([], [])), [])],
                                           (Some (Delete (Nodetach, ["n3"],
                                                    (Some (Create (
                                                             [(((Some "n4"),
                                                                ([], [])),
                                                               [])],
                                                             None)))
                                                    )))
                                           )))
                                  )))
                         )))
                )))
       )) |}]
;;

let%expect_test "Read-write request test" =
  parse_and_print
    {|
  WITH *
  MATCH ()
  WITH *
  WITH *
  CREATE (n1)
  CREATE (n2)
  WITH *
  WITH *
  DELETE n1
  DELETE n2
  CREATE (n3)
  DELETE n3
  CREATE (n4{name:"hola", id:1})
  RETURN *, n4.name, n4.id as id
  |};
  [%expect
    {|
    (With ((Some All), [], [], [],
       (Match ([((None, ([], [])), [])], [],
          (With ((Some All), [], [], [],
             (With ((Some All), [], [], [],
                (Create ([(((Some "n1"), ([], [])), [])],
                   (Some (Create ([(((Some "n2"), ([], [])), [])],
                            (Some (With ((Some All), [], [], [],
                                     (With ((Some All), [], [], [],
                                        (Delete (Nodetach, ["n1"],
                                           (Some (Delete (Nodetach, ["n2"],
                                                    (Some (Create (
                                                             [(((Some "n3"),
                                                                ([], [])),
                                                               [])],
                                                             (Some (Delete (
                                                                      Nodetach,
                                                                      ["n3"],
                                                                      (Some (
                                                                      Create (
                                                                        [((
                                                                        (Some "n4"),
                                                                        (
                                                                        [],
                                                                        [("name",
                                                                        (Const
                                                                        (String
                                                                        "hola")));
                                                                        ("id",
                                                                        (Const
                                                                        (Int64 1L)))
                                                                        ])),
                                                                        [])],
                                                                        (Some (
                                                                        Return (
                                                                        (Some All),
                                                                        [((
                                                                        Property (
                                                                        "n4",
                                                                        "name")),
                                                                        None);
                                                                        ((
                                                                        Property (
                                                                        "n4",
                                                                        "id")),
                                                                        (Some "id"))
                                                                        ],
                                                                        []))))))
                                                                      )))
                                                             )))
                                                    )))
                                           ))
                                        ))
                                     )))
                            )))
                   ))
                ))
             ))
          ))
       )) |}]
;;
