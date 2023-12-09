open! Base
open Types

let run code =
  match Parse.parse code with
  | Some [str_item] -> (
    match Infer.infer_structure_item StdEnv.env str_item with
    | Ok (ty, _) ->
        Scheme.pp Stdlib.Format.std_formatter ty
    | Error err ->
        Infer.TyError.pp Stdlib.Format.std_formatter err )
  | None ->
      Stdlib.print_endline "syntax error"
  | _ ->
      Stdlib.print_endline "invalid test"

let%expect_test _ =
  run {|
    let id1 = fun x -> x in
    id1 42; id1 "hello"
  |} ;
  [%expect {| (Forall ({}, (Ty_con ((Ident "string"), [])))) |}]

let%expect_test _ =
  run {|
    let f id = id 42; id "hello" in
    f (fun x -> x)
  |} ;
  [%expect
    {|
    (UnificationFail ((Ty_con ((Ident "string"), [])),
       (Ty_con ((Ident "int"), [])))) |}]

let%expect_test _ =
  run {| fun x -> let y = x in y |} ;
  [%expect
    {|
    (Forall ({(Var "gen0")},
       (Ty_arr ((Ty_var (Var "gen0")), (Ty_var (Var "gen0")))))) |}]

let%expect_test _ =
  run {|
    fun x ->
      let y = fun z -> x z in y |} ;
  [%expect
    {|
    (Forall ({(Var "gen2"), (Var "gen5")},
       (Ty_arr ((Ty_arr ((Ty_var (Var "gen2")), (Ty_var (Var "gen5")))),
          (Ty_arr ((Ty_var (Var "gen2")), (Ty_var (Var "gen5"))))))
       )) |}]

let%expect_test _ =
  run {| fun x f -> f x |} ;
  [%expect
    {|
    (Forall ({(Var "gen0"), (Var "gen4")},
       (Ty_arr ((Ty_var (Var "gen0")),
          (Ty_arr ((Ty_arr ((Ty_var (Var "gen0")), (Ty_var (Var "gen4")))),
             (Ty_var (Var "gen4"))))
          ))
       )) |}]

let%expect_test _ =
  run {| fun f -> fun x -> f x |} ;
  [%expect
    {|
    (Forall ({(Var "gen1"), (Var "gen4")},
       (Ty_arr ((Ty_arr ((Ty_var (Var "gen1")), (Ty_var (Var "gen4")))),
          (Ty_arr ((Ty_var (Var "gen1")), (Ty_var (Var "gen4"))))))
       )) |}]

let%expect_test _ =
  run {| fun f -> fun x -> g x |} ;
  [%expect {| (UnboundVariable (Ident "g")) |}]

let%expect_test _ =
  run {|
    fun m -> let y = m in
    let x = y true in x
  |} ;
  [%expect
    {|
    (Forall ({(Var "gen7")},
       (Ty_arr ((Ty_arr ((Ty_con ((Ident "bool"), [])), (Ty_var (Var "gen7")))),
          (Ty_var (Var "gen7"))))
       )) |}]

let%expect_test _ =
  run
    {|
    (fun x -> x + 1)
    ( (fun y -> if y then true else false) false )
  |} ;
  [%expect
    {|
    (UnificationFail ((Ty_con ((Ident "int"), [])), (Ty_con ((Ident "bool"), []))
       )) |}]

let%expect_test _ =
  run {| fun x -> if x then 42 else x |} ;
  [%expect
    {|
    (UnificationFail ((Ty_con ((Ident "int"), [])), (Ty_con ((Ident "bool"), []))
       )) |}]

let%expect_test _ =
  run {| fun f -> (fun x -> f (x x)) (fun x -> f (x x)) |} ;
  [%expect
    {|
    (OccursIn ((Var "gen1"),
       (Ty_arr ((Ty_var (Var "gen1")), (Ty_var (Var "gen5")))))) |}]

let%expect_test _ =
  run {| fun x y (a, _) -> (x + y - a) = 1 |} ;
  [%expect
    {|
    (Forall ({(Var "gen3")},
       (Ty_arr ((Ty_con ((Ident "int"), [])),
          (Ty_arr ((Ty_con ((Ident "int"), [])),
             (Ty_arr (
                (Ty_tuple [(Ty_con ((Ident "int"), [])); (Ty_var (Var "gen3"))]),
                (Ty_con ((Ident "bool"), []))))
             ))
          ))
       )) |}]

let%expect_test _ =
  run {|
    let x, Some f = 1, Some ( ( + ) 4 )
    in f x |} ;
  [%expect {|
    (Forall ({}, (Ty_con ((Ident "int"), [])))) |}]

let%expect_test _ =
  run {| Some (1, "hi") |} ;
  [%expect
    {|
    (Forall ({},
       (Ty_con ((Ident "option"),
          [(Ty_tuple
              [(Ty_con ((Ident "int"), [])); (Ty_con ((Ident "string"), []))])
            ]
          ))
       )) |}]

let%expect_test _ =
  run {| None |} ;
  [%expect
    {|
    (Forall ({(Var "solve0")},
       (Ty_con ((Ident "option"), [(Ty_var (Var "solve0"))])))) |}]

let%expect_test _ =
  run {| Some |} ; [%expect {|
    (ConstructorArityMismatch (Ident "Some")) |}]

let%expect_test _ =
  run {| None 42 |} ; [%expect {| (ConstructorArityMismatch (Ident "None")) |}]

let%expect_test _ =
  run {| None None |} ;
  [%expect {| (ConstructorArityMismatch (Ident "None")) |}]

let%expect_test _ =
  run {| let Some = Some 1 in 0 |} ;
  [%expect {| (ConstructorArityMismatch (Ident "Some")) |}]

let%expect_test _ =
  run {| let x, Some x = 1, Some 2 in x |} ;
  [%expect {| (PatVarBoundSeveralTimes (Ident "x")) |}]

let%expect_test _ =
  run {| fun x x -> x |} ; [%expect {| (PatVarBoundSeveralTimes (Ident "x")) |}]

let%expect_test _ =
  run {| let a, _ = 1, 2, 3 in a |} ;
  [%expect {| UnificationMismatch |}]
