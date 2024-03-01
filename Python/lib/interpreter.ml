(** Copyright 2023-2024, Averin Pavel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  (* A synonym for >>= *)
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val error : string -> 'a t
end

module Result : MONADERROR with type 'a t = ('a, string) result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind
  let return = Result.ok
  let ( let* ) = ( >>= )
  let error = Result.error
end

module Eval (M : MONADERROR) = struct
  open M

  (* Utility functions *)
  let rec fold_left func init = function
    | [] -> return init
    | hd :: tl -> func init hd >>= fun init -> fold_left func init tl
  ;;

  let rec map1 f = function
    | [] -> return []
    | h :: tl -> f h >>= fun c -> map1 f tl >>= fun lst -> return (c :: lst)
  ;;

  (* Environment symbols *)
  type var_symb =
    { identifier : identifier
    ; value : value
    }

  type function_symb =
    { identifier : identifier
    ; params : identifier list
    ; body : statement list
    }

  (* Environments *)

  type environment =
    { id : identifier
    ; local_envs : environment list
    ; classes : environment list
    ; functions : function_symb list
    ; flag : flag
    ; return_v : value
    ; vars : var_symb list
    ; vals_to_print : value list
    }

  let local_env : environment =
    { id = Identifier "local"
    ; local_envs = []
    ; classes = []
    ; functions = []
    ; flag = No
    ; return_v = Nil
    ; vars = []
    ; vals_to_print = []
    }
  ;;

  let temp_class_env =
    { id = Identifier "temp"
    ; local_envs = []
    ; classes = []
    ; functions = []
    ; flag = No
    ; return_v = Nil
    ; vars = []
    ; vals_to_print = []
    }
  ;;

  let global_env =
    { id = Identifier "global"
    ; vars = []
    ; local_envs = [ local_env ]
    ; classes = []
    ; functions = []
    ; flag = No
    ; return_v = Nil
    ; vals_to_print = []
    }
  ;;

  (* Environment related functions for variables *)

  let var_in_env i env = List.exists (fun (x : var_symb) -> x.identifier = i) env.vars

  let change_var i new_value env =
    let new_vars =
      List.map
        (fun (x : var_symb) ->
          if x.identifier = i then { x with value = new_value } else x)
        env.vars
    in
    { env with vars = new_vars }
  ;;

  let change_or_add_var env (x : var_symb) =
    match var_in_env x.identifier env with
    | true -> change_var x.identifier x.value env
    | false -> { env with vars = x :: env.vars }
  ;;

  let change_or_add_var_list = List.fold_left change_or_add_var
  let get_var i env = List.find (fun (x : var_symb) -> x.identifier = i) env.vars

  (* Environment related functions for functions *)

  let func_in_env i env =
    List.exists (fun (a : function_symb) -> i = a.identifier) env.functions
  ;;

  let class_in_env i env = List.exists (fun (a : environment) -> i = a.id) env.classes

  let change_func (new_func : function_symb) env =
    let new_funcs =
      List.map
        (fun (x : function_symb) ->
          if x.identifier = new_func.identifier
          then { x with body = new_func.body; params = new_func.params }
          else x)
        env.functions
    in
    { env with functions = new_funcs }
  ;;

  let change_or_add_func (x : function_symb) env =
    match func_in_env x.identifier env with
    | true -> change_func x env
    | false -> { env with functions = x :: env.functions }
  ;;

  let change_class (new_class : environment) env =
    let new_classes =
      List.map
        (fun (x : environment) ->
          if x.id = new_class.id
          then { x with functions = new_class.functions; vars = new_class.vars }
          else x)
        env.classes
    in
    { env with classes = new_classes }
  ;;

  let change_or_add_class (x : environment) env =
    match func_in_env x.id env with
    | true -> change_class x env
    | false -> { env with classes = x :: env.classes }
  ;;

  let get_func i env =
    List.find (fun (x : function_symb) -> x.identifier = i) env.functions
  ;;

  let get_class i env = List.find (fun (x : environment) -> x.id = i) env.classes

  (* Miscellaneous *)

  let combine_args_and_params args (params : value list) =
    List.map (fun x -> { identifier = fst x; value = snd x }) (List.combine args params)
  ;;

  (* Debugging & Testing functions *)

  let get_str_from_identifier (Identifier i) = i

  let pack_to_string = function
    | String a -> return a
    | Int a -> return @@ Int.to_string a
    | Bool a -> return @@ Bool.to_string a
    | _ -> error "Interpretation Error"
  ;;

  (* Main functions *)

  type dispatch =
    { i_expr : dispatch -> environment -> expression -> value t
    ; i_stmt : dispatch -> environment -> statement -> environment t
    }

  let i_exp_or_stmt =
    let rec i_expr (exp_or_stmt : dispatch) (env : environment) exp =
      let rec apply env body =
        if env.flag = Return_f
        then return env.return_v
        else (
          match body with
          | [] -> return Nil
          | hd :: tl ->
            let* a = exp_or_stmt.i_stmt exp_or_stmt env hd in
            apply a tl)
      in
      match exp with
      | Const a -> return a
      | ArithOp (Add, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        (match a, b with
         | Int a1, Int b1 -> return (Int (a1 + b1))
         | _ -> error "unexpected type")
      | ArithOp (Sub, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        (match a, b with
         | Int a1, Int b1 -> return (Int (a1 - b1))
         | _ -> error "unexpected type")
      | ArithOp (Div, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        (match a, b with
         | _, Int b1 when b1 = 0 -> error "0 Division Error"
         | Int a1, Int b1 -> return (Int (a1 / b1))
         | _ -> error "unexpected type")
      | ArithOp (Mul, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        (match a, b with
         | Int a1, Int b1 -> return (Int (a1 * b1))
         | _ -> error "unexpected type")
      | ArithOp (Mod, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        (match a, b with
         | Int a1, Int b1 -> return (Int (a1 mod b1))
         | _ -> error "unexpected type")
      | BoolOp (Equal, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        return (Bool (a = b))
      | BoolOp (NotEqual, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        return (Bool (a != b))
      | BoolOp (Less, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        return (Bool (a < b))
      | BoolOp (LessOrEqual, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        return (Bool (a <= b))
      | BoolOp (Greater, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        return (Bool (a > b))
      | BoolOp (GreaterOrEqual, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        return (Bool (a >= b))
      | BoolOp (And, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        return (Bool (a = b))
      | BoolOp (Or, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        return (Bool (a = Bool true || b = Bool true))
      | Variable (_, i) ->
        (match var_in_env i env with
         | true -> return (get_var i env).value
         | false -> error "undefined Variable")
      | FunctionCall (identifier, exp_list) ->
        (match identifier with
         | Identifier "print" when not (func_in_env (Identifier "print") env) ->
           let rec print_exp_list = function
             | [] -> return Nil
             | exp :: tl ->
               let* value = i_expr exp_or_stmt env exp in
               let* str_val = pack_to_string value in
               let () = Printf.printf "%s" str_val in
               print_exp_list tl
           in
           (match exp_list with
            | [] -> return Nil
            | _ -> print_exp_list exp_list)
         | _ ->
           (match func_in_env identifier env with
            | false -> error "undefined Function"
            | true ->
              let* x = map1 (fun x -> i_expr exp_or_stmt env x) exp_list in
              apply
                { (change_or_add_var_list
                     env
                     (combine_args_and_params (get_func identifier env).params x))
                  with
                  id = Identifier "local"
                ; local_envs = [ { local_env with vars = env.vars } ]
                ; functions = [ get_func identifier env ]
                }
                (get_func identifier env).body))
      | MethodCall (classId, methodId, e) ->
        let fetchedMethod = get_func methodId (get_class classId env) in
        let temp_env = { env with functions = [ fetchedMethod ] @ env.functions } in
        i_expr exp_or_stmt temp_env (FunctionCall (methodId, e))
      | ListExp exps ->
        let* calcList = map1 (fun exp -> i_expr exp_or_stmt env exp) exps in
        return @@ List calcList
      | FString fStringList ->
        let* strList =
          map1
            (function
              | Str c -> pack_to_string c
              | Var id -> pack_to_string (get_var id env).value)
            fStringList
        in
        i_expr exp_or_stmt env (Const (String (String.concat "" strList)))
      | Lambda _ -> return Nil
      | Field (classId, fieldId) -> return (get_var fieldId (get_class classId env)).value
    in
    let rec i_stmt (i_exp_or_stmt : dispatch) (env : environment) = function
      | Expression exp ->
        let* _ = i_expr i_exp_or_stmt env exp in
        return env
      | Return exp ->
        let* value = i_exp_or_stmt.i_expr i_exp_or_stmt env exp in
        return { env with flag = Return_f; return_v = value }
      | While (e, s) ->
        let rec guard_res ctx =
          i_exp_or_stmt.i_expr i_exp_or_stmt env e
          >>= function
          | Bool false -> return ctx
          | _ -> helper ctx s
        and helper envH s =
          match envH.flag with
          | Return_f -> return envH
          | _ ->
            (match s with
             | [] -> guard_res envH
             | h :: tl ->
               let* newEnv = i_stmt i_exp_or_stmt envH h in
               helper newEnv tl)
        in
        guard_res env
      | IfElse (guard, ifBranch, elseBranch) ->
        let* res = i_expr i_exp_or_stmt env guard in
        let get_env env = fold_left (i_exp_or_stmt.i_stmt i_exp_or_stmt) env in
        (match res with
         | Bool true -> get_env env ifBranch
         | Bool false -> get_env env elseBranch
         | _ -> error "failed to interpred the guard")
      | Assign (l, r) ->
        (match l with
         | Variable (_, identifier) ->
           (match r with
            | Lambda (params, exp) ->
              i_stmt i_exp_or_stmt env (Function (identifier, params, [ Return exp ]))
            | _ ->
              let* value = i_exp_or_stmt.i_expr i_exp_or_stmt env r in
              return (change_or_add_var env { identifier; value }))
         | _ -> error "Left-hand side operator is not a variable")
      | Class (id, statements) ->
        let global_env =
          fold_left (fun x -> i_stmt i_exp_or_stmt x) local_env statements
        in
        let class_env =
          let* global_env = global_env in
          return
            { temp_class_env with
              vars = global_env.vars
            ; functions = global_env.functions
            }
        in
        (match class_in_env id env with
         | true -> return env
         | false ->
           let* class_env = class_env in
           return { env with classes = { class_env with id } :: env.classes })
      | Function (i, some_params, some_body) ->
        let new_func_env = { identifier = i; params = some_params; body = some_body } in
        return (change_or_add_func new_func_env env)
      | Setattr (classDest, methodName, methodItself) ->
        (match methodName with
         | Const (String str) ->
           let fetchedFunc = get_func methodItself env in
           let fetchedClass = get_class classDest env in
           let changedClass =
             change_or_add_func
               { fetchedFunc with identifier = Identifier str }
               fetchedClass
           in
           return @@ change_or_add_class changedClass env
         | _ -> error "string was expected in the place of method name")
      | Else _ -> error "loose else statement"
    in
    { i_expr; i_stmt }
  ;;

  let get_env env = fold_left (i_exp_or_stmt.i_stmt i_exp_or_stmt) env
  let interpret = get_env global_env
end
