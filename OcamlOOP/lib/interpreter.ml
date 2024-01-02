(** Copyright 2021-2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

type error =
  | Division_by_zero
  | Match_failure
  | Invalid_compare_arg of string
  | Ill_right_side_rec of string
  | Ill_typed
  | Unbound_var of string
  | Not_implemented

type name = string

module type MONAD_ERROR = sig
  include Base.Monad.S2

  val fail : error -> ('a, error) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val ( let+ ) : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t
  val map_error : ('a, 'b) t -> f:('b -> 'c) -> ('a, 'c) t

  module EList : sig
    val fold_left : 'a list -> init:('b, 'c) t -> f:('b -> 'a -> ('b, 'c) t) -> ('b, 'c) t

    val find_map_assoc_list
      :  ('a * 'b) list
      -> f:('a -> ('c, 'e) t)
      -> err:'e
      -> ('c * 'b, 'e) t
  end
end

type var =
  | Var_int of int
  | Var_bool of bool
  | Var_fun of pattern * rec_flag * enviroment * expression
  | Var_list of var list
  | Var_tuple of var list

and enviroment = (name, var, String.comparator_witness) Map.t

let vint i = Var_int i
let vbool b = Var_bool b
let vfun p r env e = Var_fun (p, r, env, e)
let vtuple vs = Var_tuple vs
let vlist vs = Var_list vs

module ENV (M : MONAD_ERROR) = struct
  open M

  let empty = Map.empty (module String)

  let lookup env name =
    match Map.find env name with
    | None -> fail (Unbound_var name)
    | Some v -> return v
  ;;

  let extend env n v = Map.update env n ~f:(fun _ -> v)

  let rec extend_by_apply env = function
    | Pat_const (Const_int i1), Var_int i2 when i1 = i2 -> return env
    | Pat_const (Const_bool b1), Var_bool b2 when Bool.equal b1 b2 -> return env
    | Pat_const Const_nil, Var_list [] -> return env
    | Pat_const _, _ -> fail Match_failure
    | Pat_var (Id name), var -> return @@ extend env name var
    | Pat_any, _ -> return env
    | Pat_cons (p1, p2), Var_list (h :: tl) ->
      let* env = extend_by_apply env (p2, Var_list tl) in
      extend_by_apply env (p1, h)
    | Pat_tuple ps, Var_tuple vs ->
      (match
         List.fold2 ps vs ~init:(return env) ~f:(fun env p v ->
           let* env = env in
           extend_by_apply env (p, v))
       with
       | Ok env -> env
       | _ -> fail Ill_typed)
    | _ -> fail Ill_typed
  ;;
end

module INTERPRETER (M : MONAD_ERROR) : sig
  val run : enviroment -> expression -> (var, error) M.t
  val eval_program : program -> (enviroment, error) M.t
end = struct
  open M
  open ENV (M)

  let specify_unbound = function
    | Unbound_var v -> Ill_right_side_rec v
    | other -> other
  ;;

  let eval_expression =
    let bin_op_helper = function
      | Asterisk, Var_int i1, Var_int i2 -> return (vint (i1 * i2))
      | Divider, Var_int i1, Var_int i2 ->
        if i2 = 0 then fail Division_by_zero else return (vint (i1 / i2))
      | Plus, Var_int i1, Var_int i2 -> return (vint (i1 + i2))
      | Sub, Var_int i1, Var_int i2 -> return (vint (i1 - i2))
      | And, Var_bool b1, Var_bool b2 -> return (vbool (b1 && b2))
      | Or, Var_bool b1, Var_bool b2 -> return (vbool (b1 || b2))
      | Or, _, _ | And, _, _ | Sub, _, _ | Plus, _, _ | Divider, _, _ | Asterisk, _, _ ->
        fail Ill_typed
      | op, v1, v2 ->
        let rec compare = function
          | (Var_int _ as v1), (Var_int _ as v2) | (Var_bool _ as v1), (Var_bool _ as v2)
            -> return (Base.Poly.compare v1 v2)
          | (Var_tuple (v1 :: tl1) as v), Var_tuple (v2 :: tl2)
          | (Var_list (v1 :: tl1) as v), Var_list (v2 :: tl2) ->
            let* cmp_res = compare (v1, v2) in
            (match cmp_res, v with
             | 0, Var_tuple _ -> compare (vtuple tl1, vtuple tl2)
             | 0, Var_list _ -> compare (vlist tl1, vlist tl2)
             | _ -> return cmp_res)
          | Var_fun _, Var_fun _ -> fail (Invalid_compare_arg "functional")
          (* obj *)
          | _ -> fail Ill_typed
        in
        let+ compare_args = compare (v1, v2) in
        (match op with
         | Eq -> vbool (compare_args = 0)
         | Neq -> vbool (compare_args != 0)
         | Lt -> vbool (compare_args = -1)
         | Ltq -> vbool (compare_args < 1)
         | Gt -> vbool (compare_args = 1)
         | _ -> vbool (compare_args > -1))
    in
    let rec helper env = function
      | Exp_constant c ->
        (match c with
         | Const_int i -> return (vint i)
         | Const_bool b -> return (vbool b)
         | Const_nil -> return (vlist [])
         | _ -> fail Not_implemented)
      | Exp_unary_op (op, e) ->
        let* e = helper env e in
        (match op, e with
         | Minus, Var_int i -> return (vint (-i))
         | Not, Var_bool b -> return (vbool (not b))
         | _ -> fail Ill_typed)
      | Exp_bin_op (op, e1, e2) ->
        let* v1 = helper env e1 in
        let* v2 = helper env e2 in
        bin_op_helper (op, v1, v2)
      | Exp_ifthenelse (ei, et, ee) ->
        let* cond = helper env ei in
        (match cond with
         | Var_bool true -> helper env et
         | Var_bool false -> helper env ee
         | _ -> fail Ill_typed)
      | Exp_function (p, e) -> return (vfun p Nonrecursive env e)
      | Exp_ident (Id name) ->
        let+ var = lookup env name in
        (match var with
         | Var_fun (p, Recursive, _, e) -> vfun p Recursive (extend env name var) e
         | _ -> var)
      | Exp_apply (e1, e2) ->
        let* v1 = helper env e1 in
        let* v2 = helper env e2 in
        (match v1 with
         | Var_fun (pat, _, fun_env, exp) ->
           let* new_env = extend_by_apply fun_env (pat, v2) in
           helper new_env exp
         | _ -> fail Ill_typed)
      | Exp_let ({ d_rec = Nonrecursive; d_pat; d_expr }, exp) ->
        let* v = helper env d_expr in
        let* env1 = extend_by_apply env (d_pat, v) in
        helper env1 exp
      | Exp_let ({ d_rec = Recursive; d_pat; d_expr }, exp) ->
        let* v = map_error (helper env d_expr) ~f:specify_unbound in
        let* env1 = extend_by_apply env (d_pat, v) in
        let* v =
          match v with
          | Var_fun (p, _, _, exp) -> return (vfun p Recursive env1 exp)
          | _ -> return v
        in
        let* env2 = extend_by_apply env (d_pat, v) in
        helper env2 exp
      | Exp_tuple es ->
        let+ vs =
          EList.fold_left es ~init:(return []) ~f:(fun acc e ->
            let+ v = helper env e in
            v :: acc)
        in
        vtuple (List.rev vs)
      | Exp_list (h, tl) ->
        let* vh = helper env h in
        let* vtl = helper env tl in
        (match vtl with
         | Var_list vs -> return (vlist (vh :: vs))
         | _ -> fail Ill_typed)
      | Exp_match (e, cases) ->
        let* v = helper env e in
        let* env, exp =
          EList.find_map_assoc_list
            cases
            ~f:(fun pat -> extend_by_apply env (pat, v))
            ~err:Match_failure
        in
        helper env exp
      | Exp_send _ -> fail Not_implemented
      | Exp_object _ -> fail Not_implemented
    in
    helper
  ;;

  let eval_structure_item env = function
    | Str_eval exp ->
      let* _ = eval_expression env exp in
      return env
    | Str_value v ->
      (match v with
       | { d_rec = Nonrecursive; d_pat; d_expr } ->
         let* v = eval_expression env d_expr in
         extend_by_apply env (d_pat, v)
       | { d_rec = Recursive; d_pat; d_expr } ->
         let* v = map_error (eval_expression env d_expr) ~f:specify_unbound in
         let* env1 = extend_by_apply env (d_pat, v) in
         let* v =
           match v with
           | Var_fun (p, _, _, exp) -> return (vfun p Recursive env1 exp)
           | _ -> return v
         in
         extend_by_apply env (d_pat, v))
  ;;

  let eval_program program =
    let env = empty in
    EList.fold_left program ~init:(return env) ~f:(fun env str_item ->
      eval_structure_item env str_item)
  ;;

  let run env = eval_expression env
end

module INTERPR = INTERPRETER (struct
    include Base.Result

    let fail e = Result.Error e
    let ( let* ) x f = bind x ~f
    let ( let+ ) x f = x >>| f

    module EList = struct
      let fold_left xs ~init ~f =
        Base.List.fold_left xs ~init ~f:(fun acc x ->
          let* acc = acc in
          f acc x)
      ;;

      let find_map_assoc_list l ~f ~err =
        let res =
          Base.List.find_map l ~f:(fun (x, y) ->
            match f x with
            | Error _ -> None
            | Ok res -> Some (res, y))
        in
        match res with
        | None -> Error err
        | Some x -> Ok x
      ;;
    end
  end)

module Pretty_printer = struct
  open Format

  let pp_error ppf = function
    | Division_by_zero -> fprintf ppf "Division by zero"
    | Ill_right_side_rec name ->
      fprintf
        ppf
        "This kind of expression is not allowed as right-hand side of `let rec %s'"
        name
    | Invalid_compare_arg v -> fprintf ppf "Invalid argument for compare: %s value" v
    | Match_failure -> fprintf ppf "Match failure"
    | Ill_typed ->
      fprintf
        ppf
        "Ill typed of expression. This error means that type checked worth worked"
    | Unbound_var name -> fprintf ppf "Unound value %s" name
    | Not_implemented -> fprintf ppf "not impl"
  ;;

  let pp_eval ppf ty =
    let () = fprintf ppf "%a = " Pp_infer.pp_type ty in
    let rec helper ppf = function
      | Var_int i -> fprintf ppf "%d" i
      | Var_bool b -> fprintf ppf "%b" b
      | Var_fun _ -> fprintf ppf "<fun>"
      | Var_list vs ->
        fprintf
          ppf
          "[%a]"
          (pp_print_list
             ~pp_sep:(fun ppf () -> fprintf ppf "; ")
             (fun ppf v -> fprintf ppf "%a" helper v))
          vs
      | Var_tuple vs ->
        fprintf
          ppf
          "(%a)"
          (pp_print_list
             ~pp_sep:(fun ppf () -> fprintf ppf ", ")
             (fun ppf v -> fprintf ppf "%a" helper v))
          vs
    in
    helper ppf
  ;;

  let pp_program ppf infer_env interpr_env =
    let open Typedtree in
    Map.iter2 infer_env interpr_env ~f:(fun ~key:_ ~data:el ->
      match el with
      | `Both (S (_, ty), v) ->
        pp_eval ppf ty v;
        print_newline ()
      | _ -> ())
  ;;
end

let run_interpreter s =
  let open Result in
  let open Angstrom in
  let run_parser = parse_string ~consume:Prefix Parser.program s in
  match run_parser with
  | Error e -> print_endline e
  | Ok statements ->
    (match Infer.check_program statements with
     | Error e -> Pp_infer.pp_error Format.std_formatter e
     | Ok st ->
       (match INTERPR.eval_program statements with
        | Error e -> Pretty_printer.pp_error Format.std_formatter e
        | Ok vs -> Pretty_printer.pp_program Format.std_formatter st vs))
;;



let%expect_test _ =
  let () =
    run_interpreter
      "let rec fact x = if x = 0 then 1 else x * fact (x-1) let eval =  fact 5"
  in
  [%expect {|
    int = 120
    int -> int = <fun> |}]
;;

let%expect_test _ =
  let () = run_interpreter "let a x = if x then 1 else 5;; let eval = a false" in
  [%expect {|
    bool -> int = <fun>
    int = 5 |}]
;;

let%expect_test _ =
  let () =
    run_interpreter "let a = ((fun x -> x), (fun y -> y)) < ((fun z -> z), (fun a -> a))"
  in
  [%expect {| Invalid argument for compare: functional value |}]
;;

let%expect_test _ =
  let () = run_interpreter "let a = (5, 6, 10, 34)" in
  [%expect {| int * int * int * int = (5, 6, 10, 34) |}]
;;

let%expect_test _ =
  let () = run_interpreter "let l = [5; 234; 22; -299; 50]" in
  [%expect {| int list = [5; 234; 22; -299; 50] |}]
;;

let%expect_test _ =
  let () = run_interpreter "let l = [324; true]" in
  [%expect {| This expression has type int but an expression was expected of type bool |}]
;;

(* pattern-matching *)

let%expect_test _ =
  let () =
    run_interpreter "let a x = match x with 1 -> true | _ -> false;; let b = a 3"
  in
  [%expect {|
    int -> bool = <fun>
    bool = false |}]
;;

let%expect_test _ =
  let () = run_interpreter "let (5, a) = (4, 3)" in
  [%expect {| Match failure |}]
;;


let%expect_test _ =
  let () =
    run_interpreter
      "let match_list = let l = [1; 2; 3; 4] in match l with (1::b) -> b | _ -> []"
  in
  [%expect {| int list = [2; 3; 4] |}]
;;


let%expect_test _ =
  let () = run_interpreter "let a = []" in
  [%expect {| 'a list = [] |}]
;;

(*
   let%expect_test _ =
   let () = run_interpreter "" in
   [%expect {||}]
   ;;
*)

let%expect_test _ =
  let () =
    run_interpreter "let tuple = ((fun x -> x + 1), (fun b -> if b then 1 else 0))"
  in
  [%expect {| (int -> int) * (bool -> int) = (<fun>, <fun>) |}]
;;

let%expect_test _ =
  let () = run_interpreter "let rec a = a" in
  [%expect {| This kind of expression is not allowed as right-hand side of `let rec a' |}]
;;
