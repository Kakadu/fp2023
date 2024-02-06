(** Copyright 2021-2023, LeonidElkin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open InterpretTypes

module type MONAD = sig
  include Base.Monad.S2

  val fail : 'e -> (_, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

module MONAD_RESULT = struct
  include Base.Result

  let ( let* ) m f = m >>= fun x -> f x
end

module Interpret (M : MONAD) = struct
  open M

  let find_val key env =
    match IdMap.find_opt key env with
    | Some v -> return v
    | None -> fail (UnboundVariable key)
  ;;

  let add_val key value env = IdMap.add key value env

  let rec update_env env = function
    | (key, value) :: tl ->
      let new_env = add_val key value env in
      update_env new_env tl
    | [] -> env
  ;;

  let check_corretnes_of_const_pattern c v =
    match c, v with
    | String s1, VString s2 when s1 = s2 -> true
    | Bool b1, VBool b2 when b1 = b2 -> true
    | Int i1, VInt i2 when i1 = i2 -> true
    | Char c1, VChar c2 when c1 = c2 -> true
    | _ -> false
  ;;

  let rec check_correctnes_of_matching pattern value =
    match pattern, value with
    | PNill, VList [] | PEmpty, _ | PVar _, _ | PArg _, _ -> true
    | PConst c, v -> check_corretnes_of_const_pattern c v
    | PCons (p1, p2), VList (h :: tl) ->
      check_correctnes_of_matching p1 h && check_correctnes_of_matching p2 (VList tl)
    | PTuple pl, VTuple tl ->
      (match pl, tl with
       | [], [] -> true
       | ph :: ptl, th :: ttl ->
         check_correctnes_of_matching ph th
         && check_correctnes_of_matching (PTuple ptl) (VTuple ttl)
       | _ -> false)
    | _ -> false
  ;;

  let match_const c v =
    match c, v with
    | Int x1, VInt x2 when x1 = x2 -> return []
    | Char x1, VChar x2 when x1 = x2 -> return []
    | String x1, VString x2 when x1 = x2 -> return []
    | Bool x1, VBool x2 when x1 = x2 -> return []
    | _ -> fail (PatternError "Constants are not the same")
  ;;

  let eval_const = function
    | Int i -> return (VInt i)
    | Char c -> return (VChar c)
    | String s -> return (VString s)
    | Bool b -> return (VBool b)
  ;;

  let eval_fun ptrn expr1 env = return (VFun (ptrn, expr1, env))

  let rec eval expr env =
    match expr with
    | Const c -> eval_const c
    | BinOp (b_op, expr1, expr2) -> eval_binop b_op expr1 expr2 env
    | Var v -> find_val v env
    | Apply (expr1, expr2) ->
      let* v1 = eval expr1 env in
      eval_apply v1 expr2 env
    | Fun (ptrn, expr1) -> eval_fun ptrn expr1 env
    | IfThenElse (expr1, expr2, expr3) -> eval_if expr1 expr2 expr3 env
    | EDecl (LetDecl (_, name, expr1), expr2) -> eval_decl name expr1 expr2 env
    | Match (expr1, ptrnl) -> eval_match expr1 ptrnl env
    | LabeledVar (name, expr) -> eval_labeled_var name expr env

  and eval_binop b_op exprl exprr env =
    let* l = eval exprl env in
    let* r = eval exprr env in
    match b_op with
    | Plus ->
      (match l, r with
       | VInt l, VInt r -> return (VInt (l + r))
       | _, _ -> fail (ExprTypeError "Addition does not accept non-int types"))
    | Dash ->
      (match l, r with
       | VInt l, VInt r -> return (VInt (l - r))
       | _, _ -> fail (ExprTypeError "Subtraction does not accept non-int types"))
    | Asterisk ->
      (match l, r with
       | VInt l, VInt r -> return (VInt (l * r))
       | _ -> fail (ExprTypeError "Multiplication does not accept non-int types"))
    | Slash ->
      (match l, r with
       | VInt _, VInt 0 -> fail DivisionByZero
       | VInt l, VInt r -> return (VInt (l / r))
       | _ -> fail (ExprTypeError "Division does not accept non-int types"))
    | Eq ->
      (match l, r with
       | VInt l, VInt r -> return (VBool (l = r))
       | VBool l, VBool r -> return (VBool (l = r))
       | VString l, VString r -> return (VBool (l = r))
       | VChar l, VChar r -> return (VBool (l = r))
       | _, _ -> fail (ExprTypeError "Wrong expression type for equality"))
    | And ->
      (match l, r with
       | VBool l, VBool r -> return (VBool (l & r))
       | _, _ -> fail (ExprTypeError "Wrong expression type for \"and\" bin operation"))
    | Or ->
      (match l, r with
       | VBool l, VBool r -> return (VBool (l || r))
       | _, _ -> fail (ExprTypeError "Wrong expression type for \"or\" bin operation"))
    | Neq ->
      (match l, r with
       | VInt l, VInt r -> return (VBool (l <> r))
       | VBool l, VBool r -> return (VBool (l <> r))
       | VString l, VString r -> return (VBool (l <> r))
       | VChar l, VChar r -> return (VBool (l <> r))
       | _, _ -> fail (ExprTypeError "Wrong expression type for equality"))
    | Less ->
      (match l, r with
       | VInt l, VInt r -> return (VBool (l < r))
       | VString l, VString r -> return (VBool (l < r))
       | VChar l, VChar r -> return (VBool (l < r))
       | _, _ -> fail (ExprTypeError "Wrong expression type for less bin operation"))
    | Lessq ->
      (match l, r with
       | VInt l, VInt r -> return (VBool (l <= r))
       | VString l, VString r -> return (VBool (l <= r))
       | VChar l, VChar r -> return (VBool (l <= r))
       | _, _ -> fail (ExprTypeError "Wrong expression type for less equal bin operation"))
    | Greater ->
      (match l, r with
       | VInt l, VInt r -> return (VBool (l > r))
       | VString l, VString r -> return (VBool (l > r))
       | VChar l, VChar r -> return (VBool (l > r))
       | _, _ -> fail (ExprTypeError "Wrong expression type for grater bin operation"))
    | Greaterq ->
      (match l, r with
       | VInt l, VInt r -> return (VBool (l >= r))
       | VString l, VString r -> return (VBool (l >= r))
       | VChar l, VChar r -> return (VBool (l >= r))
       | _, _ ->
         fail (ExprTypeError "Wrong expression type for greater equal bin operation"))
    | Cons ->
      (match l, r with
       | VInt l, VInt r -> return (VList (VInt l :: [ VInt r ]))
       | VBool l, VBool r -> return (VList (VBool l :: [ VBool r ]))
       | VString l, VString r -> return (VList (VString l :: [ VString r ]))
       | VChar l, VChar r -> return (VList (VChar l :: [ VChar r ]))
       | VTuple l, VTuple r -> return (VList (VTuple l :: [ VTuple r ]))
       | VFun (lp, le, lenv), VFun (rp, re, renv) ->
         return (VList (VFun (lp, le, lenv) :: [ VFun (rp, re, renv) ]))
       | l, VList r -> return (VList (l :: r))
       | _, _ -> fail (ExprTypeError "Wrong expression type for cons bin operation"))

  and eval_if expr1 expr2 expr3 env =
    let* cond = eval expr1 env in
    match cond with
    | VBool true -> eval expr2 env
    | VBool false -> eval expr3 env
    | _ -> fail (ExprTypeError "Condition isn't bool type")

  and eval_apply v1 expr2 env =
    match v1 with
    | VFun (p, e, env) ->
      let* v2 = eval expr2 env in
      let* env_elements = match_patterns p v2 in
      let new_env = update_env env env_elements in
      eval e new_env
    | VLet (let_name, is_rec, let_val) ->
      (match is_rec with
       | true ->
         (match let_val with
          | VFun (fun_pattern, fun_exp, fun_env) ->
            let new_fun_env = add_val let_name v1 fun_env in
            let new_fun_val = VFun (fun_pattern, fun_exp, new_fun_env) in
            eval_apply new_fun_val expr2 env
          | _ -> fail @@ ValueTypeError let_val)
       | false -> fail (ExprTypeError "The application does not occur to a function"))
    | _ -> fail (ExprTypeError "The application does not occur to a function")

  and eval_decl name fun_expr in_expr env =
    let* fun_val = eval fun_expr env in
    eval in_expr (add_val name fun_val env)

  and eval_match match_expr l env =
    let* match_value = eval match_expr env in
    let* expr, env_elements = match_list_of_patterns match_value l in
    let new_env = update_env env env_elements in
    eval expr new_env

  and eval_labeled_var n expr env =
    let* value = eval expr env in
    let _ = add_val n value env in
    return value

  and match_patterns ptrn value =
    match ptrn, value with
    | PNill, VList [] | PEmpty, _ -> return []
    | PArg (n, l), v -> match_arg n l v
    | PConst c, v -> match_const c v
    | PVar s, v -> return [ s, v ]
    | PCons (p1, p2), VList v -> match_cons p1 p2 v
    | PTuple l, VTuple v -> match_tuple l v
    | _ -> fail (PatternError "Incorrect pattern match")

  and match_cons p1 p2 v =
    match v with
    | h :: tl ->
      let* pattern_h = match_patterns p1 h in
      let* pattern_tl = match_patterns p2 (VList tl) in
      return (pattern_h @ pattern_tl)
    | _ -> fail (PatternError "PCons isn't a list")

  and match_tuple l v =
    match l, v with
    | [], [] -> return []
    | ph :: ptl, vh :: vtl ->
      let* pattern_h = match_patterns ph vh in
      let* pattern_tl = match_patterns (PTuple ptl) (VTuple vtl) in
      return (pattern_h @ pattern_tl)
    | _ -> fail (PatternError "PTuple isn't a list")

  and match_arg n l v =
    match l with
    | Label sn ->
      (match sn with
       | None -> return [ n, v ]
       | Some x -> return [ x, v ])
    | Optional expr ->
      (match expr with
       | None -> return [ n, v ]
       | Some x ->
         let* e = eval x IdMap.empty in
         match_patterns (PVar n) e)

  and match_list_of_patterns match_value = function
    | [] -> fail (PatternError "Couldn't match list of patterns")
    | (pattern, expr) :: tl ->
      (match check_correctnes_of_matching pattern match_value with
       | true ->
         let* new_env_items = match_patterns pattern match_value in
         return (expr, new_env_items)
       | false -> match_list_of_patterns match_value tl)
  ;;

  let eval_let (decl : decl) env =
    match decl with
    | LetDecl (is_rec, name, expr) ->
      (match is_rec with
       | true ->
         let* exp_val = eval expr env in
         (match exp_val with
          | VFun (_, _, _) -> return (name, VLet (name, is_rec, exp_val))
          | _ -> return (name, exp_val))
       | false ->
         let* exp_val = eval expr env in
         return (name, exp_val))
  ;;

  let exec_program (program : decl list) =
    List.fold_left
      (fun env decl ->
        let* env = env in
        let* name, exp_val = eval_let decl env in
        let new_env = add_val name exp_val env in
        return new_env)
      (return IdMap.empty)
      program
  ;;
end

module InterpretResult = Interpret (MONAD_RESULT)
