(** Copyright 2023-2024, Pogorelov Ilya *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser

module Env = Map.Make(String)

type value =
  | VNum of int
  | VAtom of string
  | VList of value list
  | VClosure of string list * term * (value Env.t)

module MonadFail = struct
  type 'a t = Success of 'a | Failure of string

  let return x = Success x
  let bind m f = match m with
    | Success x -> f x
    | Failure msg -> Failure msg
  let fail msg = Failure msg
end

open MonadFail

module Interpreter = struct
  let ( let* ) = bind

  let rec value_to_term = function
    | VNum n -> Const (Num n)
    | VAtom a -> Const (Atom (Name a))
    | VList vs -> Relation { atom = Name "list"; terms = List.map value_to_term vs }
    | VClosure _ -> failwith "Cannot convert closure to term"

  let rec term_to_value = function
    | Const (Num n) -> VNum n
    | Const (Atom (Name a)) -> VAtom a
    | Relation { atom = Name "list"; terms = ts } -> VList (List.map term_to_value ts)
    | Var v -> failwith ("Unexpected variable during term to value conversion: " ^ v)
    | _ -> failwith "Unsupported term type in term_to_value"

  let rec unify env t1 t2 =
    match (t1, t2) with
    | Var v, t | t, Var v ->
      (match Env.find_opt v env with
       | Some t' -> unify env (value_to_term t') t
       | None -> return (Env.add v (term_to_value t) env))
    | Const (Num n1), Const (Num n2) when n1 = n2 -> return env
    | Const (Atom (Name a1)), Const (Atom (Name a2)) when a1 = a2 -> return env
    | Relation { atom = Name n1; terms = ts1 }, Relation { atom = Name n2; terms = ts2 } when n1 = n2 ->
      unify_lists env ts1 ts2
    | _, _ -> fail "Unification failed"

  and unify_lists env ts1 ts2 =
    try
      List.fold_left2 (fun acc t1 t2 -> 
        let* env' = acc in 
        unify env' t1 t2
      ) (return env) ts1 ts2
    with
    | Invalid_argument _ -> fail "Different lengths in list unification"

  let rec eval_term env = function
    | Const (Num n) -> return (VNum n)
    | Const (Atom (Name n)) -> return (VAtom n)
    | Const (Atom (Oper _)) -> fail "Unexpected operator in atomic evaluation"
    | Var v ->
      (match Env.find_opt v env with
       | Some value -> return value
       | None -> return (VAtom "undefined"))  (* Default binding for a new variable *)
    | Relation { atom = Oper op; terms = [t1; t2] } ->
      let* v1 = eval_term env t1 in
      let* v2 = eval_term env t2 in
      eval_op op v1 v2
    | Relation { atom = Oper _; terms = _ } ->
      fail "Operator should be applied to exactly two operands"
    | Relation { atom = Name name; terms = args } ->
      let* arg_values = mapM (eval_term env) args in
      match Env.find_opt name env with
        | Some (VClosure (params, body, closure_env)) when List.length params == List.length arg_values ->
          let* env' = unify_lists closure_env (List.map (fun p -> Var p) params) (List.map value_to_term arg_values) in
          eval_term env' body
        | Some (VClosure (_, _, _)) -> fail ("Mismatched arguments for " ^ name)
        | None | Some _ -> return (VList arg_values)

  and eval_op op v1 v2 = match (op, v1, v2) with
    | ("+", VNum n1, VNum n2) -> return (VNum (n1 + n2))
    | ("-", VNum n1, VNum n2) -> return (VNum (n1 - n2))
    | ("*", VNum n1, VNum n2) -> return (VNum (n1 * n2))
    | ("/", VNum n1, VNum n2) -> if n2 == 0 then fail "Division by zero" else return (VNum (n1 / n2))
    | ("==", VNum n1, VNum n2) -> return (VAtom (if n1 == n2 then "true" else "false"))
    | ("<", VNum n1, VNum n2) -> return (VAtom (if n1 < n2 then "true" else "false"))
    | (">", VNum n1, VNum n2) -> return (VAtom (if n1 > n2 then "true" else "false"))
    | _ -> fail ("Unsupported operation: " ^ op)

  and mapM f = function
    | [] -> return []
    | x :: xs ->
        let* v = f x in
        let* vs = mapM f xs in
        return (v :: vs)

  let rec eval env stmts =
    match stmts with
    | [] -> return env
    | Relation { atom = Oper ":-"; terms = [Relation { atom = Name name; terms = params }; body] } :: rest ->
      let params = List.map (function Var v -> v | _ -> failwith "Expected variable in parameters") params in
      let closure = VClosure (params, body, env) in
      let env' = Env.add name closure env in
      eval env' rest
    | Relation { atom = Name name; terms = params } :: rest when List.for_all (function Var _ -> true | _ -> false) params ->
      let params = List.map (function Var v -> v | _ -> failwith "Expected variable in parameters") params in
      let closure = VClosure (params, Relation { atom = Name "true"; terms = [] }, env) in
      let env' = Env.add name closure env in
      eval env' rest
    | Relation { atom = Oper "is"; terms = [Var v; expr] } :: rest ->
      let* value = eval_term env expr in
      let env' = Env.add v value env in
      eval env' rest
    | _ :: rest -> eval env rest

  let rec print_value = function
    | VNum n -> string_of_int n
    | VAtom a -> a
    | VList l -> "[" ^ (String.concat ", " (List.map print_value l)) ^ "]"
    | VClosure _ -> "<closure>"

  let solve env query =
    match query with
    | Relation { atom = Oper "?-"; terms } ->
      let* results = mapM (eval_term env) terms in
      List.iter (fun res -> print_endline (print_value res)) results;
      return env
    | _ -> fail "Query should start with ?-"

  let interpret (Many_term ast) =
    let rec separate_queries acc = function
      | [] -> (List.rev acc, [])
      | Relation { atom = Oper "?-"; terms = _ } as query :: rest -> (List.rev acc, query :: rest)
      | h :: t -> separate_queries (h :: acc) t
    in
    let (program, queries) = separate_queries [] ast in
    match eval Env.empty program with
    | Success env ->
      (match queries with
       | [] -> Some env
       | q :: _ ->
          match solve env q with
           | Success _ -> Some env
           | Failure msg ->
               print_endline ("Failed to solve query: " ^ msg);
               None
      )
    | Failure msg ->
      print_endline ("Failed to interpret: " ^ msg);
      None

  let print_env env =
    Env.iter (fun key value ->
      Printf.printf "%s = %s\n" key (print_value value)
    ) env
end

open Interpreter

let run prolog_code =
  match parse_program prolog_code with
  | Ok ast -> 
    (match interpret ast with
     | Some env -> print_env env
     | None -> print_endline "Interpretation failed")
  | Error _ -> print_endline "Parsing failed"


let%expect_test _ =
  let test =
    {|factorial(0, 1).
      factorial(N, Fact) :- N > 0, N1 is N - 1, factorial(N1, Fact1), Fact is N * Fact1.
      ?- factorial(0, X).|}
  in
  run test;
  [%expect {| X = 120 |}]
;;

let%expect_test _ =
  let test =
    {|friend(tom, jerry). 
    ?- friend(jerry, tom).|}
  in
  run test;
  [%expect {| false |}]
;;

let%expect_test _ =
  let test =
    {|?- 5 + 5.|}
  in
  run test;
  [%expect {| 10 |}]
;;
