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
  [%expect {| string |}]

let%expect_test _ =
  run {|
    let f id = id 42; id "hello" in
    f (fun x -> x)
  |} ;
  [%expect {| (UnificationFail (string, int)) |}]

let%expect_test _ =
  run {| fun x -> let y = x in y |} ;
  [%expect {| 'gen0. 'gen0 -> 'gen0 |}]

let%expect_test _ =
  run {|
    fun x ->
      let y = fun z -> x z in y |} ;
  [%expect {| 'gen2 'gen5. ('gen2 -> 'gen5) -> 'gen2 -> 'gen5 |}]

let%expect_test _ =
  run {| fun x f -> f x |} ;
  [%expect {| 'gen1 'gen4. 'gen1 -> ('gen1 -> 'gen4) -> 'gen4 |}]

let%expect_test _ =
  run {| fun f -> fun x -> f x |} ;
  [%expect {| 'gen1 'gen4. ('gen1 -> 'gen4) -> 'gen1 -> 'gen4 |}]

let%expect_test _ =
  run {| fun f -> fun x -> g x |} ;
  [%expect {| (UnboundVariable (Ident "g")) |}]

let%expect_test _ =
  run {|
    fun m -> let y = m in
    let x = y true in x
  |} ;
  [%expect {| 'gen7. (bool -> 'gen7) -> 'gen7 |}]

let%expect_test _ =
  run
    {|
    (fun x -> x + 1)
    ( (fun y -> if y then true else false) false )
  |} ;
  [%expect {| (UnificationFail (int, bool)) |}]

let%expect_test _ =
  run {| fun x -> if x then 42 else x |} ;
  [%expect {| (UnificationFail (int, bool)) |}]

let%expect_test _ =
  run {| fun f -> (fun x -> f (x x)) (fun x -> f (x x)) |} ;
  [%expect {| (OccursIn ('gen1, 'gen1 -> 'gen5)) |}]

let%expect_test _ =
  run {| fun x y (a, _) -> (x + y - a) = 1 |} ;
  [%expect {| 'gen0. int -> int -> (int * 'gen0) -> bool |}]

let%expect_test _ =
  run {|
    let x, Some f = 1, Some ( ( + ) 4 )
    in f x |} ;
  [%expect {| int |}]

let%expect_test _ =
  run {| Some (1, "hi") |} ; [%expect {| (int * string) option |}]

let%expect_test _ = run {| None |} ; [%expect {| 'solve0. 'solve0 option |}]

let%expect_test _ =
  run {| Some |} ; [%expect {| (ConstructorArityMismatch (Ident "Some")) |}]

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

let%expect_test _ =
  run {| let a = 1, (fun (a, _) -> a), 2 in a|} ;
  [%expect {| 'solve0 'solve1. int * (('solve0 * 'solve1) -> 'solve0) * int |}]

let%expect_test _ =
  run
    {|
    match Some id with
      | Some x -> x "hi"; x 5
      | None -> 1
    |} ;
  [%expect {| int |}]

let%expect_test _ =
  run
    {|
    fun x ->
      match x with
        | Some v -> Some (v + 1)
        | None -> None
    |} ;
  [%expect {| int option -> int option |}]

let%expect_test _ =
  run {| function Some x -> x | None -> 0 |} ;
  [%expect {| int option -> int |}]

let%expect_test _ =
  run {| function Some id -> id "hi"; id 5 | None -> 1 |} ;
  [%expect {| (UnificationFail (string, int)) |}]

let%expect_test _ =
  run {| fun arg -> match arg with Some x -> let y = x in y |} ;
  [%expect {| 'solve0. 'solve0 option -> 'solve0 |}]

let%expect_test _ =
  run {| function [x] -> let y = x in y |} ;
  [%expect {| 'solve1. 'solve1 list -> 'solve1 |}]

let%expect_test _ =
  run {| function 42 -> true | _ -> false |} ;
  [%expect {| int -> bool |}]
