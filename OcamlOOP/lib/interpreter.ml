(** Copyright 2023, Artem-Rzhankoff *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Errors

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
  | Var_object of enviroment

and enviroment = (name, var, String.comparator_witness) Map.t

let vint i = Var_int i
let vbool b = Var_bool b
let vfun p r env e = Var_fun (p, r, env, e)
let vtuple vs = Var_tuple vs
let vlist vs = Var_list vs
let vobj fs = Var_object fs

module ENV (M : MONAD_ERROR) = struct
  open M

  let empty = Map.empty (module String)

  let lookup env name =
    match Map.find env name with
    | None -> fail (unbound_var name)
    | Some v -> return v
  ;;

  let extend env ~k v = Map.update env k ~f:(fun _ -> v)

  let rec extend_by_apply env = function
    | Pat_const (Const_int i1), Var_int i2 when i1 = i2 -> return env
    | Pat_const (Const_bool b1), Var_bool b2 when Bool.equal b1 b2 -> return env
    | Pat_const Const_nil, Var_list [] -> return env
    | Pat_const _, _ -> fail match_failure
    | Pat_var name, var -> return @@ extend env ~k:name var
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
       | _ -> fail ill_typed)
    | _ -> fail ill_typed
  ;;

  let override_fields env new_fields =
    List.fold_left new_fields ~init:env ~f:(fun env (name, v) -> extend env ~k:name v)
  ;;
end

module EVAL (M : MONAD_ERROR) : sig
  val run : enviroment -> expression -> (var, error) M.t
  val eval_program : program -> (enviroment, error) M.t
end = struct
  open M
  open ENV (M)

  let specify_unbound = function
    | Interpreter (Unbound_var v) -> ill_right_side_rec v
    | other -> other
  ;;

  let eval_expression =
    let bin_op_helper = function
      | Asterisk, Var_int i1, Var_int i2 -> return (vint (i1 * i2))
      | Divider, Var_int i1, Var_int i2 ->
        if i2 = 0 then fail division_by_zero else return (vint (i1 / i2))
      | Plus, Var_int i1, Var_int i2 -> return (vint (i1 + i2))
      | Sub, Var_int i1, Var_int i2 -> return (vint (i1 - i2))
      | And, Var_bool b1, Var_bool b2 -> return (vbool (b1 && b2))
      | Or, Var_bool b1, Var_bool b2 -> return (vbool (b1 || b2))
      | Or, _, _ | And, _, _ | Sub, _, _ | Plus, _, _ | Divider, _, _ | Asterisk, _, _ ->
        fail ill_typed
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
          | Var_fun _, Var_fun _ -> fail (invalid_compare_arg "functional")
          | Var_object _, Var_object _ -> return 1
          | _ -> fail ill_typed
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
         | Const_nil -> return (vlist []))
      | Exp_unary_op (op, e) ->
        let* e = helper env e in
        (match op, e with
         | Minus, Var_int i -> return (vint (-i))
         | Not, Var_bool b -> return (vbool (not b))
         | _ -> fail ill_typed)
      | Exp_bin_op (op, e1, e2) ->
        let* v1 = helper env e1 in
        let* v2 = helper env e2 in
        bin_op_helper (op, v1, v2)
      | Exp_ifthenelse (ei, et, ee) ->
        let* cond = helper env ei in
        (match cond with
         | Var_bool true -> helper env et
         | Var_bool false -> helper env ee
         | _ -> fail ill_typed)
      | Exp_function (p, e) -> return (vfun p Nonrecursive env e)
      | Exp_ident name ->
        let+ var = lookup env name in
        (match var with
         | Var_fun (p, Recursive, _, e) -> vfun p Recursive (extend env ~k:name var) e
         | _ -> var)
      | Exp_apply (e1, e2) ->
        let* v1 = helper env e1 in
        let* v2 = helper env e2 in
        (match v1 with
         | Var_fun (pat, _, fun_env, exp) ->
           let* new_env = extend_by_apply fun_env (pat, v2) in
           helper new_env exp
         | _ -> fail ill_typed)
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
         | _ -> fail ill_typed)
      | Exp_match (e, cases) ->
        let* v = helper env e in
        let* env, exp =
          EList.find_map_assoc_list
            cases
            ~f:(fun pat -> extend_by_apply env (pat, v))
            ~err:match_failure
        in
        helper env exp
      | Exp_send (obj, meth) ->
        let* v = helper env obj in
        (match v with
         | Var_object env ->
           let* mval = lookup env meth in
           (match mval with
            | Var_fun (pat, _, _, exp) ->
              let* new_env = extend_by_apply env (pat, v) in
              helper new_env exp
            | _ -> fail ill_typed)
         | _ -> fail ill_typed)
      | Exp_object { o_self; o_fields } ->
        let+ env =
          EList.fold_left o_fields ~init:(return env) ~f:(fun env ->
              function
              | Obj_val (name, exp) ->
                let+ v = helper env exp in
                extend env ~k:name v
              | Obj_method (_, name, exp) ->
                let env = extend env ~k:name (vfun o_self Nonrecursive env exp) in
                return env)
        in
        vobj env
      | Exp_override fields ->
        let* obj = lookup env "self" in
        let* vfields =
          EList.fold_left fields ~init:(return []) ~f:(fun acc (name, exp) ->
            let+ v = helper env exp in
            (name, v) :: acc)
        in
        (match obj with
         | Var_object obj_env ->
           let new_env = override_fields obj_env vfields in
           return (vobj new_env)
         | _ -> fail ill_typed)
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

module Interpreter = EVAL (struct
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

module PP = struct
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
  ;;

  let pp_eval ppf name ty =
    let () = fprintf ppf "val %s : %a = " name Inferencer.PP.pp_type ty in
    let rec helper ppf = function
      | Var_int i -> fprintf ppf "%d" i
      | Var_bool b -> fprintf ppf "%b" b
      | Var_fun _ -> fprintf ppf "<fun>"
      | Var_object _ -> fprintf ppf "<obj>"
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
    Map.iter2 infer_env interpr_env ~f:(fun ~key ~data ->
      match data with
      | `Both (S (_, ty), v) ->
        pp_eval ppf key ty v;
        print_newline ()
      | _ -> ())
  ;;
end

open Result

let eval s =
  Parser.parse_prefix s
  >>= fun statements ->
  Inferencer.check_program statements
  >>= fun ty_env -> Interpreter.eval_program statements >>| fun var_env -> ty_env, var_env
;;

let eval_with_printing s =
  let ppf = Format.std_formatter in
  match eval s with
  | Ok (ty_env, var_env) -> PP.pp_program ppf ty_env var_env
  | Error err ->
    (match err with
     | Parser e -> Parser.PP.pp_error ppf e
     | Infer e -> Inferencer.PP.pp_error ppf e
     | Interpreter e -> PP.pp_error ppf e)
;;
