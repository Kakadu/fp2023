(** Copyright 2023-2024, Lesh79 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format
module KeyMap = Map.Make (String)

module type MONAD = sig
  type ('a, 'e) t

  val return : 'a -> ('a, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

module MONAD_RESULT = struct
  include Base.Result

  let ( let* ) m f = m >>= fun x -> f x
end

type value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VTuple of value list
  | VFun of pattern * expr * env
  | VLRec of string * value

and env = value KeyMap.t

let rec value_tuple fmt = function
  | [] -> ()
  | [ h ] -> fprintf fmt "%a" pp_value h
  | h :: tl -> fprintf fmt "%a, %a" pp_value h value_tuple tl

and pp_value fmt = function
  | VInt num -> fprintf fmt "%d" num
  | VString str -> fprintf fmt "%s" str
  | VBool bool -> fprintf fmt "%b" bool
  | VFun (_, _, _) -> fprintf fmt "<fun>"
  | VLRec (_, _) -> fprintf fmt "<rec fun>"
  | VTuple lst -> fprintf fmt "(%a)" value_tuple lst
;;

type failure =
  | Division_by_zero
  | Unknown_Variable of string
  | Type_Error of string
  | ExecErr of value * value
  | PatternMatchErr
  | NotImplemented

let print_failure fmt = function
  | Division_by_zero -> fprintf fmt "Division by zero"
  | Unknown_Variable str -> fprintf fmt "Unknow Variable: %S" str
  | Type_Error typeerr -> fprintf fmt "Type Error: %S" typeerr
  | ExecErr (val1, val2) -> fprintf fmt "ExecErr : %a # %a" pp_value val1 pp_value val2
  | PatternMatchErr -> fprintf fmt "Error in pattern matching"
  | NotImplemented -> fprintf fmt "Not implemented"
;;

module Interpreter (M : MONAD) : sig
  val exec_program : program -> (env, failure) M.t
end = struct
  open M

  let check_mathcing pattern value =
    match pattern, value with
    | PString str1, VString str2 when str1 = str2 -> true
    | PBool bool1, VBool bool2 when bool1 = bool2 -> true
    | PInt num1, VInt num2 when num1 = num2 -> true
    | _ -> false
  ;;

  let fresh_env env bindings =
    List.fold_left (fun acc (key, value) -> KeyMap.add key value acc) env bindings
  ;;

  let rec exec_var id env =
    match KeyMap.find_opt id env with
    | Some decl -> return decl
    | None -> fail @@ Unknown_Variable id

  and exec_binop binop expr1 expr2 env =
    let* val1 = exec expr1 env in
    let* val2 = exec expr2 env in
    match binop with
    | Plus ->
      (match val1, val2 with
       | VInt val1, VInt val2 -> return (VInt (val1 + val2))
       | _, _ -> fail @@ ExecErr (val1, val2))
    | Minus ->
      (match val1, val2 with
       | VInt val1, VInt val2 -> return (VInt (val1 - val2))
       | _, _ -> fail @@ ExecErr (val1, val2))
    | Multi ->
      (match val1, val2 with
       | VInt val1, VInt val2 -> return (VInt (val1 * val2))
       | _, _ -> fail @@ ExecErr (val1, val2))
    | Division ->
      (match val1, val2 with
       | VInt _, VInt 0 -> fail Division_by_zero
       | VInt val1, VInt val2 -> return (VInt (val1 / val2))
       | _, _ -> fail @@ ExecErr (val1, val2))
    | Equal ->
      (match val1, val2 with
       | VInt val1, VInt val2 -> return (VBool (val1 = val2))
       | VBool val1, VBool val2 -> return (VBool (val1 = val2))
       | VString val1, VString val2 -> return (VBool (val1 = val2))
       | _, _ -> fail @@ ExecErr (val1, val2))
    | NotEqual ->
      (match val1, val2 with
       | VInt val1, VInt val2 -> return (VBool (val1 <> val2))
       | VBool val1, VBool val2 -> return (VBool (val1 <> val2))
       | VString val1, VString val2 -> return (VBool (val1 <> val2))
       | _, _ -> fail @@ ExecErr (val1, val2))
    | And ->
      (match val1, val2 with
       | VBool val1, VBool val2 -> return (VBool (val1 & val2))
       | _, _ -> fail @@ ExecErr (val1, val2))
    | Or ->
      (match val1, val2 with
       | VBool val1, VBool val2 -> return (VBool (val1 || val2))
       | _, _ -> fail @@ ExecErr (val1, val2))
    | Lt ->
      (match val1, val2 with
       | VInt val1, VInt val2 -> return (VBool (val1 < val2))
       | VBool val1, VBool val2 -> return (VBool (val1 < val2))
       | _, _ -> fail @@ ExecErr (val1, val2))
    | Ltq ->
      (match val1, val2 with
       | VInt val1, VInt val2 -> return (VBool (val1 <= val2))
       | VBool val1, VBool val2 -> return (VBool (val1 <= val2))
       | _, _ -> fail @@ ExecErr (val1, val2))
    | Gt ->
      (match val1, val2 with
       | VInt val1, VInt val2 -> return (VBool (val1 > val2))
       | VBool val1, VBool val2 -> return (VBool (val1 > val2))
       | _, _ -> fail @@ ExecErr (val1, val2))
    | Gtq ->
      (match val1, val2 with
       | VInt val1, VInt val2 -> return (VBool (val1 >= val2))
       | VBool val1, VBool val2 -> return (VBool (val1 >= val2))
       | _, _ -> fail @@ ExecErr (val1, val2))

  and match_tuple p v =
    match p, v with
    | PTuple p_lst, VTuple t_lst ->
      (match p_lst, t_lst with
       | [], [] -> return []
       | ph :: ptl, vh :: vtl ->
         let* pattern_h = pattern_match ph vh in
         let* pattern_tl = pattern_match (PTuple ptl) (VTuple vtl) in
         return (pattern_h @ pattern_tl)
       | _ -> fail PatternMatchErr)
    | _ -> fail PatternMatchErr

  and pattern_match pattern value =
    match pattern, value with
    | PString str1, VString str2 when str1 = str2 -> return []
    | PBool bool1, VBool bool2 when bool1 = bool2 -> return []
    | PInt num1, VInt num2 when num1 = num2 -> return []
    | PVar var, value -> return [ var, value ]
    | PTuple _, VTuple _ -> match_tuple pattern value
    | _ -> fail PatternMatchErr

  and match_list_of_patterns match_value = function
    | [] -> fail PatternMatchErr
    | (pattern, expr) :: tl ->
      (match check_mathcing pattern match_value with
       | true ->
         let* new_env_items = pattern_match pattern match_value in
         return (expr, new_env_items)
       | false -> match_list_of_patterns match_value tl)

  and exec_match match_expr l env =
    let* match_value = exec match_expr env in
    let* expr, env_elements = match_list_of_patterns match_value l in
    let new_env = fresh_env env env_elements in
    exec expr new_env

  and exec_fun pattern decl_exp env = return @@ VFun (pattern, decl_exp, env)

  and exec_ifThenElse expr1 expr2 expr3 env =
    let* cond_val = exec expr1 env in
    match cond_val with
    | VBool true -> exec expr2 env
    | VBool false -> exec expr3 env
    | _ -> fail @@ Type_Error "Condition expression ,must evaluate to a boolean value"

  and exec_app app_exp_val app_arg env =
    match app_exp_val with
    | VFun (fun_pattern, fun_exp, fun_env) ->
      let* app_arg = exec app_arg env in
      let* new_fun_env_items = pattern_match fun_pattern app_arg in
      let new_fun_env = fresh_env fun_env new_fun_env_items in
      exec fun_exp new_fun_env
    | VLRec (let_name, let_val) ->
      (match let_val with
       | VFun (fun_pattern, fun_exp, fun_env) ->
         let new_fun_env = KeyMap.add let_name app_exp_val fun_env in
         let new_fun_val = VFun (fun_pattern, fun_exp, new_fun_env) in
         exec_app new_fun_val app_arg env
       | _ -> fail @@ Unknown_Variable "Invalid function definition in let rec")
    | _ -> fail @@ Type_Error "Invalid function application"

  and exec_tuple expr_list env =
    match expr_list with
    | [] -> return (VTuple [])
    | expr :: rest ->
      let* expr_val = exec expr env in
      let* rest_val = exec_tuple rest env in
      (match rest_val with
       | VTuple rest_list -> return (VTuple (expr_val :: rest_list))
       | _ -> fail @@ Type_Error "Expected a tuple")

  and exec expr env =
    match expr with
    | EInt int -> return @@ VInt int
    | EString str -> return @@ VString str
    | EBool bool -> return @@ VBool bool
    | EBinop (bin_op, exp1, exp2) -> exec_binop bin_op exp1 exp2 env
    | EFun (ptrn, exp) -> exec_fun ptrn exp env
    | EIfThenElse (expr1, expr2, expr3) -> exec_ifThenElse expr1 expr2 expr3 env
    | EVar var -> exec_var var env
    | ETuple tuple -> exec_tuple tuple env
    | EApp (expr1, arg) ->
      let* expr_val = exec expr1 env in
      exec_app expr_val arg env
    | EMatch (expr, ptrn) -> exec_match expr ptrn env
    | _ -> fail NotImplemented
  ;;

  let exec_let let_decl env =
    match let_decl with
    | DeclLet (decl_rec, id, expr) ->
      (match decl_rec with
       | true ->
         let* exec_val = exec expr env in
         (match exec_val with
          | VFun (_, _, _) -> return (id, VLRec (id, exec_val))
          | _ -> return (id, exec_val))
       | false ->
         let* exec_val = exec expr env in
         return (id, exec_val))
  ;;

  let exec_program (program : Ast.program) =
    List.fold_left
      (fun env decl ->
        let* env = env in
        let* name, value = exec_let decl env in
        let env' = KeyMap.add name value env in
        return env')
      (return KeyMap.empty)
      program
  ;;
end

module InterpretResult = Interpreter (MONAD_RESULT)

let pp_env fmt = KeyMap.iter (fun key data -> fprintf fmt "%s: %a\n" key pp_value data)
