(** Copyright 2023, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Run

let%expect_test _ =
  run {| fun f g -> f (); g (); print_string "42" |} ;
  [%expect
    {| 'a 'b 'e. (unit -[console | 'e]-> 'a) -> (unit -[console | 'e]-> 'b) -[console | 'e]-> unit |}]

let%expect_test _ =
  run
    {|
    let foo f = print_string "hi"; f Exc1;;
    fun _ -> foo raise;;
    fun _ -> foo id |} ;
  [%expect
    {|
    foo: 'a 'e. (_Exc1 exception -[console | 'e]-> 'a) -[console | 'e]-> 'a
    'a 'b. 'a -[console, exn _Exc1]-> 'b
    'a. 'a -[console]-> _Exc1 exception |}]

let%expect_test _ =
  run {|
    let id = fun x -> x in
    id print_string; id raise; id |} ;
  [%expect {| 'a. 'a -> 'a |}]

let%expect_test _ =
  run
    {|
      let foo f g _ = f ""; g Exc1 in
      foo print_string raise
    |} ;
  [%expect {| 'a 'b. 'a -[console, exn _Exc1]-> 'b |}]

let%expect_test _ =
  run
    {| let rec map f = function
         | [] -> []
         | x::xs -> (f x) :: (map f xs)
       ;;

       map id;;
       map print_string |} ;
  [%expect
    {|
    map: 'a 'b 'e. ('a -'e-> 'b) -> 'a list -'e-> 'b list
    'a. 'a list -> 'a list
    string list -[console]-> unit list |}]

let%expect_test _ =
  run
    {|
    let rec filter f = function
      | [] -> []
      | hd::tl -> if f hd then hd::(filter f tl) else filter f tl;;
    filter (fun x -> print_string x; true)|} ;
  [%expect
    {|
    filter: 'a 'e. ('a -'e-> bool) -> 'a list -'e-> 'a list
    string list -[console]-> string list |}]

let%expect_test _ =
  run {| fun f -> f 1 2 |} ;
  [%expect {| 'a 'e. (int -'e-> int -'e-> 'a) -'e-> 'a |}]

let%expect_test _ =
  run {| let rec foo f x = f x; foo f x in foo |} ;
  [%expect {| 'a 'b 'c 'e. ('a -'e-> 'b) -> 'a -'e-> 'c |}]

let%expect_test _ =
  run {| fun x -> print_string "hi"; fun y -> x + y |} ;
  [%expect {| int -[console]-> int -> int |}]

let%expect_test _ =
  run {| fun x -> raise Exc1; fun y -> raise Exc1; x + y |} ;
  [%expect {| int -[exn _Exc1]-> int -[exn _Exc1]-> int |}]

let%expect_test _ =
  run {| fun x -> print_string "hi"; fun y -> raise Exc2; x + y |} ;
  [%expect {| int -[console]-> int -[exn _Exc2]-> int |}]

let%expect_test _ =
  run {| function 1 -> raise Exc1 | 2 -> raise Exc2 | _ -> 0 |} ;
  [%expect {| int -[exn _Exc2, exn _Exc1]-> int |}]

let%expect_test _ =
  run
    {|
    let catch_exc1 f x = try f x with Exc1 -> ();;

    let f1 _ = raise Exc1;;
    catch_exc1 f1;;

    let f2 = function 1 -> raise Exc1 | 2 -> raise Exc2 | _ -> ();;
    catch_exc1 f2;;

    catch_exc1 print_string;;
    fun _ -> catch_exc1 raise Exc1
    |} ;
  [%expect
    {|
    catch_exc1: 'a 'e. ('a -[exn _Exc1 | 'e]-> unit) -> 'a -'e-> unit
    f1: 'a 'b. 'a -[exn _Exc1]-> 'b
    'a. 'a -> unit
    f2: int -[exn _Exc2, exn _Exc1]-> unit
    int -[exn _Exc2]-> unit
    string -[console]-> unit
    'a. 'a -[exn _Exc1]-> unit |}]

let%expect_test _ =
  run
    {|
    let catch f x = try f x with Exc1 -> raise Exc1 | Exc2 -> print_string "exc2";;

    catch (fun _ -> raise Exc1; raise Exc2);;
    catch (fun _ -> raise Exc1);;
    catch id |} ;
  [%expect
    {|
      catch: 'a 'e. ('a -[exn _Exc1, exn _Exc2, console, exn _Exc1 | 'e]-> unit) -> 'a -[console, exn _Exc1 | 'e]-> unit
      'a. 'a -[console, exn _Exc1]-> unit
      'a. 'a -[console, exn _Exc1]-> unit
      unit -[console, exn _Exc1]-> unit |}]

let%expect_test _ =
  run
    {|
    exception My_exc1;;
    exception My_exc2;;
    let catch f x = try print_string "hi"; f x with My_exc1 -> 1 | My_exc2 -> 2 | Exc1 -> 0 |} ;
  [%expect
    {|
    My_exc1: _My_exc1 exception
    My_exc2: _My_exc2 exception
    catch: 'a 'e. ('a -[exn _My_exc1, exn _My_exc2, exn _Exc1, console | 'e]-> int) -> 'a -[console | 'e]-> int |}]

let%expect_test _ =
  run {| print_string |} ; [%expect {| string -[console]-> unit |}]

let%expect_test _ =
  run
    {|
    let foo f x = match f x with
    | Some a -> raise Exc1
    | None -> print_string "42" |} ;
  [%expect
    {| foo: 'a 'b 'e. ('a -[exn _Exc1, console | 'e]-> 'b option) -> 'a -[exn _Exc1, console | 'e]-> unit |}]

let%expect_test _ =
  run
    {|
    let raise_Exc1 () = raise Exc1;;
    let catch_Exc1 f x = try f x with Exc1 -> print_string;;
    catch_Exc1 raise_Exc1 () |} ;
  [%expect
    {|
    raise_Exc1: 'a. unit -[exn _Exc1]-> 'a
    catch_Exc1: 'a 'e 'e1. ('a -[exn _Exc1 | 'e]-> string -[console | 'e1]-> unit) -> 'a -'e-> string -[console | 'e1]-> unit
    string -[console]-> unit |}]

let%expect_test _ =
  run
    {|
    let raise_Exc1 _ = raise Exc1 in
    let baz x = if x then (id raise_Exc1) else (id print_string) in
    baz true |} ;
  [%expect {| string -[console, exn _Exc1]-> unit |}]

let%expect_test _ =
  run
    {|
    fun _ -> (print_string "42", raise Exc1, raise Exc2);;
    fun _ -> [print_string "42"; raise Exc1; raise Exc2] |} ;
  [%expect
    {|
    'a 'b 'c. 'a -[exn _Exc2, console, exn _Exc1]-> unit * 'b * 'c
    'a. 'a -[exn _Exc2, console, exn _Exc1]-> unit list |}]

let%expect_test _ =
  run
    {|
    ref None;;

    let fst (a, b) = a;;
    let snd (a, b) = b;;

    let a = (fun x -> x), ref None;;
    let b = ref None;;
    (fst a) "hi"; (snd a) := Some true; b := Some 1;;

    a;; b|} ;
  [%expect
    {|
    '_weak1 option ref
    fst: 'a 'b. ('a * 'b) -> 'a
    snd: 'a 'b. ('a * 'b) -> 'b
    a: ('_weak2 -> '_weak2) * '_weak3 option ref
    b: '_weak4 option ref
    unit
    (string -> string) * bool option ref
    int option ref |}]

let%expect_test _ =
  run {| let x () = ref None |} ;
  [%expect {| x: 'a. unit -[ref]-> 'a option ref |}]

let%expect_test _ =
  run
    {|
    let const x y = y in
    const ();;

    id (fun x -> [x;x]);;
    (fun _ _ -> 0) () |} ;
  [%expect {|
    'a. 'a -> 'a
    'a. 'a -> 'a list
    'a. 'a -> int |}]

let%expect_test _ =
  run
    {|
    let cache1 =
      let cache = ref None in
      fun x -> match !cache with
        | Some cached -> cached
        | _ -> cache := Some x; x;;
    cache1 5; cache1

    let cache2 _ =
      let cache = ref None in
      fun x -> match !cache with
        | Some cached -> cached
        | _ -> cache := Some x; x;;

    let id = cache2 () in id 5; id
    |} ;
  [%expect
    {|
    cache1: '_weak1 -[ref]-> '_weak1
    int -[ref]-> int
    cache2: 'a. 'a -[ref]-> '_weak2 -[ref]-> '_weak2
    int -[ref]-> int |}]

let%expect_test _ =
  run
    {|
    let a = ref None in a := Some 1; a;;
    let a = ref None in a := Some 1; a := Some true |} ;
  [%expect {|
    int option ref
    (UnificationFailTy (int, bool)) |}]

let%expect_test _ =
  run
    {| fun _ ->
         let id = print_string "42"; fun x -> x
         in ref None; id 1; id "hello" |} ;
  [%expect {| 'a. 'a -[ref, console]-> string |}]

let%expect_test _ =
  run
    {| let a = ref None in let b = a in b := Some 1; b;;
       let a = ref None;;
       let b = a |} ;
  [%expect
    {|
    int option ref
    a: '_weak1 option ref
    b: '_weak1 option ref |}]

let%expect_test _ =
  run {| let f = ref None; id |} ;
  [%expect {| f: '_weak1 -> '_weak1 |}]

let%expect_test _ =
  run {| match Some id with Some f -> ref None; f 1; f "hi"; f |} ;
  [%expect {| '_weak1 -> '_weak1 |}]

let%expect_test _ =
  run
    {| fun () ->
       let _ =
         let _ = print_string "hi" in ()
       in () |} ;
  [%expect {| unit -[console]-> unit |}]
