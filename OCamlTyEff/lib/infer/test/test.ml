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

let%expect_test "infer_id" =
  run {| fun x -> x |} ;
  [%expect
    {|
    (Forall ({(Var "gen1")},
       (Ty_arr ((Ty_var (Var "gen1")), (Ty_var (Var "gen1")))))) |}]

let%expect_test "infer_add" =
  run {| fun x y -> x + y |} ;
  [%expect
    {|
    (Forall ({},
       (Ty_arr ((Ty_con ((Ident "int"), [])),
          (Ty_arr ((Ty_con ((Ident "int"), [])), (Ty_con ((Ident "int"), []))))))
       )) |}]

let%expect_test _ =
  run {| let a, a = 1, 2 in a |} ;
  [%expect {| (PatVarBoundSeveralTimes (Ident "a")) |}]

let%expect_test _ =
  run {| Some |} ; [%expect {| (ConstructorArityMismatch (Ident "Some")) |}]

let%expect_test _ =
  run {| None 1 |} ; [%expect {| (ConstructorArityMismatch (Ident "None")) |}]
