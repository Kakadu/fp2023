(** Copyright 2023-2024, tepa46 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open InterpreterTypes

module type MONAD = sig
  type ('a, 'e) t

  val return : 'a -> ('a, 'e) t
  val fail : 'e -> ('a, 'e) t
  val all : ('a, 'e) t list -> ('a list, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

module MONAD_RESULT = struct
  include Base.Result

  let ( let* ) m f = m >>= fun x -> f x
end

module Environment (M : MONAD) = struct
  open M

  let find_decl decl_name env =
    match StringMap.find_opt decl_name env with
    | Some decl -> return decl
    | None -> fail @@ UnboundVariable decl_name
  ;;

  let extend_env fun_arg_name app_arg_value fun_env =
    StringMap.add fun_arg_name app_arg_value fun_env
  ;;
end

module PatternMatcher (M : MONAD) : sig
  val match_pattern : pattern -> value -> ((string * value) list, failure) M.t

  val find_match
    :  value
    -> (pattern * decl_exp) list
    -> (decl_exp * (string * value) list, failure) M.t
end = struct
  open M

  let rec match_var pattern value =
    match pattern, value with
    | PVar (LName var1), val1 -> return [ var1, val1 ]
    | _ -> fail PatternMatchingError

  and match_list pattern value =
    match pattern, value with
    | PCons (pattern1, pattern2), VList (h :: tl) ->
      let* pvh = match_pattern pattern1 h in
      let* pvtl = match_pattern pattern2 (VList tl) in
      return (pvh @ pvtl)
    | _ -> fail PatternMatchingError

  and match_tuple pattern value =
    match pattern, value with
    | PTuple pattern_lst, VTuple tuple_lst ->
      (match pattern_lst, tuple_lst with
       | [], [] -> return []
       | ph :: ptl, th :: ttl ->
         let* pvh = match_pattern ph th in
         let* pvtl = match_pattern (PTuple ptl) (VTuple ttl) in
         return (pvh @ pvtl)
       | _ -> fail PatternMatchingError)
    | _ -> fail PatternMatchingError

  and match_case pattern value =
    match pattern, value with
    | PAdt (UName p_name, Some p_pattern), VAdt (v_name, Some v_pattern)
      when p_name = v_name -> match_pattern p_pattern v_pattern
    | PAdt (UName p_name, None), VAdt (v_name, None) when p_name = v_name -> return []
    | _ -> fail PatternMatchingError

  and match_pattern pattern value =
    match pattern, value with
    | PNill, VList [] | PWild, _ -> return []
    | PString str1, VString str2 when str1 = str2 -> return []
    | PBool bool1, VBool bool2 when bool1 = bool2 -> return []
    | PInt num1, VInt num2 when num1 = num2 -> return []
    | PVar _, _ -> match_var pattern value
    | PCons _, VList _ -> match_list pattern value
    | PTuple _, VTuple _ -> match_tuple pattern value
    | PAdt _, VAdt _ -> match_case pattern value
    | _ -> fail PatternMatchingError
  ;;

  let rec check_case pattern value =
    match pattern, value with
    | PAdt (UName p_name, Some p_pattern), VAdt (v_name, Some v_pattern)
      when p_name = v_name -> check_pattern_matching p_pattern v_pattern
    | PAdt (UName p_name, None), VAdt (v_name, None) when p_name = v_name -> true
    | _ -> false

  and check_pattern_matching pattern value =
    match pattern, value with
    | PNill, VList [] | PWild, _ -> true
    | PString str1, VString str2 when str1 = str2 -> true
    | PBool bool1, VBool bool2 when bool1 = bool2 -> true
    | PInt num1, VInt num2 when num1 = num2 -> true
    | PVar (LName _), _ -> true
    | PCons (pattern1, pattern2), VList (h :: tl) ->
      check_pattern_matching pattern1 h && check_pattern_matching pattern2 (VList tl)
    | PTuple pattern_lst, VTuple tuple_lst ->
      (match pattern_lst, tuple_lst with
       | [], [] -> true
       | ph :: ptl, th :: ttl ->
         check_pattern_matching ph th && check_pattern_matching (PTuple ptl) (VTuple ttl)
       | _ -> false)
    | PAdt _, VAdt _ -> check_case pattern value
    | _ -> false
  ;;

  let rec find_match match_exp_value = function
    | [] -> fail PatternMatchingError
    | (pattern, expr) :: tl ->
      (match check_pattern_matching pattern match_exp_value with
       | true ->
         let* new_env_items = match_pattern pattern match_exp_value in
         return (expr, new_env_items)
       | false -> find_match match_exp_value tl)
  ;;
end

module Interpreter (M : MONAD) : sig
  val exec_program : program -> (env, failure) M.t
end = struct
  include Environment (M)
  include PatternMatcher (M)
  open M

  let rec exec_var var env =
    match var with
    | LName var -> find_decl var env
    | UName _ -> fail @@ ExprTypeError "unexpected Uname"

  and exec_tuple tuple env =
    let* lst = all (List.map (fun tuple_item -> exec tuple_item env) tuple) in
    return @@ vtuple lst

  and exec_eq_lst lst1 lst2 =
    let* res =
      List.fold_left2
        (fun acc h1 h2 ->
          let* acc = acc in
          let* res = exec_binop_match (Eq, h1, h2) in
          match res with
          | VBool true -> return acc
          | _ -> return false)
        (return true)
        lst1
        lst2
    in
    return @@ vbool res

  and exec_binop_match = function
    | And, VBool true, VBool bool | Or, VBool false, VBool bool -> return @@ vbool bool
    | Eq, VBool bool1, VBool bool2 -> return @@ vbool (bool1 = bool2)
    | Neq, VBool bool1, VBool bool2 -> return @@ vbool (bool1 <> bool2)
    | Add, VInt num1, VInt num2 -> return @@ vint (num1 + num2)
    | Sub, VInt num1, VInt num2 -> return @@ vint (num1 - num2)
    | Mul, VInt num1, VInt num2 -> return @@ vint (num1 * num2)
    | Div, VInt _, VInt num2 when num2 = 0 -> fail DivisionByZeroError
    | Div, VInt num1, VInt num2 -> return @@ vint (num1 / num2)
    | Eq, VInt num1, VInt num2 -> return @@ vbool (num1 = num2)
    | Neq, VInt num1, VInt num2 -> return @@ vbool (num1 <> num2)
    | Les, VInt num1, VInt num2 -> return @@ vbool (num1 < num2)
    | Leq, VInt num1, VInt num2 -> return @@ vbool (num1 <= num2)
    | Gre, VInt num1, VInt num2 -> return @@ vbool (num1 > num2)
    | Geq, VInt num1, VInt num2 -> return @@ vbool (num1 > num2)
    | Eq, VString str1, VString str2 -> return @@ vbool (str1 = str2)
    | Neq, VString str1, VString str2 -> return @@ vbool (str1 <> str2)
    | Cons, h, VList tl -> return @@ vlist (h :: tl)
    | Eq, VList lst1, VList lst2 -> exec_eq_lst lst1 lst2
    | Eq, VTuple lst1, VTuple lst2 -> exec_eq_lst lst1 lst2
    | Eq, VAdt (name1, val1), VAdt (name2, val2) ->
      if name1 = name2
      then (
        match val1, val2 with
        | Some val1, Some val2 -> exec_binop_match (Eq, val1, val2)
        | None, None -> return @@ vbool true
        | _ -> return @@ vbool false)
      else return @@ vbool false
    | _, val1, val2 -> fail @@ ExecError (val1, val2)

  and exec_arith binop exp1 exp2 env =
    let* val1 = exec exp1 env in
    match binop, val1 with
    | And, VBool false -> return @@ vbool false
    | Or, VBool true -> return @@ vbool true
    | _ ->
      let* val2 = exec exp2 env in
      exec_binop_match (binop, val1, val2)

  and exec_fun pattern decl_exp env = return @@ vfun pattern decl_exp env

  and exec_constr constr exp1 env =
    match constr, exp1 with
    | UName constr, Some exp1 ->
      let* val1 = exec exp1 env in
      return @@ vadt constr (Some val1)
    | UName constr, None -> return @@ vadt constr None
    | LName _, _ -> fail @@ ExprTypeError "unexpected LName"

  and exec_let let_decl exp1 env =
    let is_rec, decl_name, decl_exp = let_decl in
    match is_rec, decl_name with
    | DRec false, LName decl_name ->
      let* exp_val = exec decl_exp env in
      let new_env = extend_env decl_name exp_val env in
      exec exp1 new_env
    | DRec true, LName decl_name ->
      let* exp_val = exec decl_exp env in
      let exp_val =
        match exp_val with
        | VFun (_, _, _) -> vletrec decl_name exp_val
        | _ -> exp_val
      in
      let new_env = extend_env decl_name exp_val env in
      exec exp1 new_env
    | _, _ -> fail @@ ExprTypeError "unexpected Uname"

  and exec_app app_exp_val app_arg env =
    match app_exp_val with
    | VFun (fun_pattern, fun_exp, fun_env) ->
      let* app_arg = exec app_arg env in
      let* new_fun_env_items = match_pattern fun_pattern app_arg in
      let new_fun_env =
        List.fold_left
          (fun fun_env (arg_name, arg_val) -> extend_env arg_name arg_val fun_env)
          fun_env
          new_fun_env_items
      in
      exec fun_exp new_fun_env
    | VLetRec (let_name, let_val) ->
      (match let_val with
       | VFun (fun_pattern, fun_exp, fun_env) ->
         let new_fun_env = extend_env let_name app_exp_val fun_env in
         let new_fun_val = vfun fun_pattern fun_exp new_fun_env in
         exec_app new_fun_val app_arg env
       | _ -> fail @@ ValueTypeError let_val)
    | _ -> fail @@ ValueTypeError app_exp_val

  and exec_match match_exp matching_lst env =
    let* match_exp_value = exec match_exp env in
    let* expr, new_env_items = find_match match_exp_value matching_lst in
    let new_env =
      List.fold_left
        (fun env (arg_name, arg_val) -> extend_env arg_name arg_val env)
        env
        new_env_items
    in
    exec expr new_env

  and exec_if if_exp then_exp else_exp env =
    let* exp_val = exec if_exp env in
    match exp_val with
    | VBool true -> exec then_exp env
    | VBool false -> exec else_exp env
    | _ -> fail @@ ValueTypeError exp_val

  and exec let_expr env =
    match let_expr with
    | EEmptyList -> return @@ vlist []
    | EInt num -> return @@ vint num
    | EString str -> return @@ vstring str
    | EBool bool -> return @@ vbool bool
    | EVar var -> exec_var var env
    | ETuple tuple -> exec_tuple tuple env
    | EBinop (binop, exp1, exp2) -> exec_arith binop exp1 exp2 env
    | EFun (pattern, decl_exp) -> exec_fun pattern decl_exp env
    | EConstr (constr, exp1) -> exec_constr constr exp1 env
    | ELet (let_decl, exp1) -> exec_let let_decl exp1 env
    | EApp (app_exp, app_arg) ->
      let* app_exp_val = exec app_exp env in
      exec_app app_exp_val app_arg env
    | EMatch (exp1, lst) -> exec_match exp1 lst env
    | EIf (if_exp, then_exp, else_exp) -> exec_if if_exp then_exp else_exp env
  ;;

  let exec_let program_decl env =
    let decl_is_rec, decl_name, decl_exp = program_decl in
    match decl_name with
    | LName decl_name ->
      (match decl_is_rec with
       | DRec true ->
         let* exp_val = exec decl_exp env in
         (match exp_val with
          | VFun (_, _, _) -> return (decl_name, vletrec decl_name exp_val)
          | _ -> return (decl_name, exp_val))
       | DRec false ->
         let* exp_val = exec decl_exp env in
         return (decl_name, exp_val))
    | UName _ -> fail @@ ExprTypeError "unexpected Uname"
  ;;

  let exec_program (program : decl list) =
    List.fold_left
      (fun env decl ->
        match decl with
        | DLet decl ->
          let* env = env in
          let* decl_name, exp_val = exec_let decl env in
          let new_env = extend_env decl_name exp_val env in
          return new_env
        | DType _ -> env)
      (return StringMap.empty)
      program
  ;;
end

module InterpreterResult = Interpreter (MONAD_RESULT)
