(** Copyright 2023-2024, Danil P *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format

module LAZY_RESULT : sig
  type ('a, 'e) t =
    | Result of ('a, 'e) result
    | Thunk of (unit -> ('a, 'e) t)

  val return : 'a -> ('a, 'e) t
  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

  module Syntax : sig
    val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  end

  val fail : 'e -> ('a, 'e) t
  val force : ('a, 'e) t -> ('a, 'e) t
  val thunk : (unit -> ('a, 'b) t) -> ('a, 'b) t
end = struct
  type ('a, 'e) t =
    | Result of ('a, 'e) result
    | Thunk of (unit -> ('a, 'e) t)

  let return x = Result (Ok x)
  let fail err = Result (Error err)
  let thunk t = Thunk t

  let force mx =
    match mx with
    | Result _ -> mx
    | Thunk thunk -> thunk ()
  ;;

  let rec ( >>= ) mx f =
    match mx with
    | Result (Ok x) -> f x
    (* todo: something seems wrong, look up OCaml's result.ml*)
    | Result (Error e) -> Result (Error e)
    | Thunk thunk -> thunk () >>= f
  ;;

  module Syntax = struct
    let ( let* ) = ( >>= )
  end
end

module EnvTypes = struct
  type res = (value, err) LAZY_RESULT.t
  and environment = (string, res) Hashtbl.t

  and value =
    | ValInt of int
    | ValBool of bool
    | ValString of string
    | ValChar of char
    | ValNil
    | ValList of res * res
    | ValTuple of res list
    | ValFun of pat * expr * environment

  and err =
    | NotInScopeError of string
    | ValueTypeError of value
    | TypeError of string
    | RuntimeError of string
    | DivisionByZeroError
    | EvalError of value * value
    | NonExhaustivePatterns
    | TypeMismatch of value * value
end

module Env : sig
  include module type of EnvTypes

  val pp_err : formatter -> err -> unit
  val pp_value : formatter -> value -> unit
  val pp_environment : formatter -> environment -> unit
  val pp_value_t : formatter -> (value, err) LAZY_RESULT.t -> unit
end = struct
  open LAZY_RESULT
  include EnvTypes

  let rec pp_value fmt = function
    | ValInt n -> fprintf fmt "%d" n
    | ValBool b -> fprintf fmt "%b" b
    | ValString s -> fprintf fmt "\"%s\"" s
    | ValChar c -> fprintf fmt "'%c'" c
    | ValNil -> printf "[]"
    | ValList (hd, tl) ->
      fprintf fmt "[";
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt ", ")
        pp_value_t
        fmt
        (force hd :: transform tl);
      fprintf fmt "]"
    | ValTuple es ->
      fprintf fmt "(";
      pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_value_t fmt es;
      fprintf fmt ")"
    | ValFun (_param, _body, _env) -> fprintf fmt "<fun>"

  (* very inefficient and abhorrent *)
  and transform = function
    | Result (Ok (ValList (hd, nil))) when force nil = Result (Ok ValNil) ->
      force hd :: []
    | Result (Ok (ValList (hd, tl))) -> force hd :: transform tl
    | Result (Ok ValNil) -> []
    | Thunk t -> transform (t ())
    | _ -> failwith "error"

  and pp_err fmt = function
    | NotInScopeError str -> fprintf fmt "Not in scope: %S" str
    | ValueTypeError err_val -> fprintf fmt "ValueTypeError: %a" pp_value err_val
    | TypeError err_expr -> fprintf fmt "TypeError: %S" err_expr
    | RuntimeError str -> fprintf fmt "RuntimeError: %S" str
    | DivisionByZeroError -> fprintf fmt "DivisionByZeroError"
    | EvalError (val1, val2) ->
      fprintf fmt "EvalError: %a # %a" pp_value val1 pp_value val2
    | NonExhaustivePatterns -> printf "Non-exhausitve patterns"
    | TypeMismatch (val1, val2) ->
      fprintf fmt "Couldn't match %a with %a" pp_value val1 pp_value val2

  and pp_value_t fmt = function
    | Result (Ok x) -> pp_value fmt x
    | Result (Error err) -> pp_err fmt err
    | Thunk t -> t () |> pp_value_t fmt

  and pp_environment fmt env =
    Hashtbl.iter (fun key value -> fprintf fmt "%s => %a \n" key pp_value_t value) env
  ;;
end

module Eval : sig
  val interpret : prog -> unit
  val run_prog : prog -> ((string, Env.res) Hashtbl.t, Env.err) LAZY_RESULT.t
end = struct
  open LAZY_RESULT
  open LAZY_RESULT.Syntax
  open Env

  (*TODO: too coupled, very cringe*)
  let rec eval env expr =
    match expr with
    | ExprBinOp (op, e1, e2) ->
      thunk (fun () ->
        let* e1' = eval env e1 in
        let* e2' = eval env e2 in
        force_binop e1' e2' op)
    | ExprVar x ->
      (match Hashtbl.find_opt env x with
       | Some v -> v
       | None -> fail @@ NotInScopeError x)
    | ExprLit lit ->
      (match lit with
       | LitInt n -> thunk (fun () -> return (ValInt n))
       | LitBool b -> thunk (fun () -> return (ValBool b))
       | LitString s -> thunk (fun () -> return (ValString s))
       | LitChar s -> thunk (fun () -> return (ValChar s)))
    | ExprIf (cond, then_expr, else_expr) ->
      thunk (fun () ->
        let* cond_val = eval env cond in
        let then' = eval env then_expr in
        let else' = eval env else_expr in
        match cond_val with
        | ValBool true -> then'
        | ValBool false -> else'
        | _ -> fail @@ TypeError "Condition in if-expression is not a boolean")
    | ExprApp (f, arg) ->
      thunk (fun () ->
        let* f_val = eval env f in
        match f_val with
        | ValFun (pat, body, f_env) ->
          let local_env = return (Hashtbl.copy f_env) in
          let* local_env =
            match_pattern
              local_env
              pat
              (* cringe but sometimes it is not in WHNF due to bad design
                 so we have to make this abomination *)
              (thunk (fun () ->
                 let* arg = eval env arg in
                 return arg))
          in
          eval local_env body
        | _ -> fail @@ TypeError "Application to a non-function")
    | ExprNil -> thunk (fun () -> return ValNil)
    | ExprCons (hd, tl) ->
      let hd_t = eval env hd in
      let tl_t = eval env tl in
      thunk (fun () -> return (ValList (hd_t, tl_t)))
    | ExprFunc (pat, expr) ->
      thunk (fun () -> return (ValFun (pat, expr, Hashtbl.copy env)))
    | ExprCase (expr, branches) ->
      thunk (fun () ->
        let e_val = eval env expr in
        eval_case (return env) e_val branches)
    | ExprTuple es ->
      thunk (fun () ->
        let es = List.map (fun e -> eval env e) es in
        return (ValTuple es))
    | ExprLet (bindings, expr) ->
      thunk (fun () ->
        (* todo: refactor *)
        let helper env (pat, expr) =
          let* env' = env in
          let value = eval env' expr in
          match_pattern env pat value
        in
        let* local_env = List.fold_left helper (return env) bindings in
        eval local_env expr)
    | _ -> fail @@ TypeError "DSdasda"

  and eval_case env res branches =
    match branches with
    | (pat, expr) :: rest ->
      (match match_pattern env pat res with
       | Result (Ok env) -> eval env expr
       | _ -> eval_case env res rest)
    | [] -> fail @@ TypeError "No matching pattern"

  and force_binop l r op =
    match l, r, op with
    | ValInt x, ValInt y, Add -> return (ValInt (x + y))
    | ValInt x, ValInt y, Sub -> return (ValInt (x - y))
    | ValInt x, ValInt y, Mul -> return (ValInt (x * y))
    | ValInt x, ValInt y, Div ->
      if y = 0 then fail DivisionByZeroError else return (ValInt (x / y))
    | ValBool x, ValBool y, And -> return (ValBool (x && y))
    | ValBool x, ValBool y, Or -> return (ValBool (x || y))
    | l, r, op ->
      let rec compare l r =
        match l, r with
        | ValInt _, ValInt _
        | ValBool _, ValBool _
        | ValString _, ValString _
        | ValChar _, ValChar _ -> return (Base.Poly.compare l r)
        | ValList (hd1, tl1), ValList (hd2, tl2) ->
          let* hd1 = hd1 in
          let* hd2 = hd2 in
          let* res = compare hd1 hd2 in
          (match res, l with
           | 0, ValList _ ->
             let* tl1 = tl1 in
             let* tl2 = tl2 in
             compare tl1 tl2
           | _ -> return res)
      in
      let* compare_args = compare l r in
      (match op with
       | Eq -> return (ValBool (compare_args = 0))
       | Neq -> return (ValBool (compare_args != 0))
       | Lt -> return (ValBool (compare_args = -1))
       | Leq -> return (ValBool (compare_args < 1))
       | Gt -> return (ValBool (compare_args = 1))
       | _ -> return (ValBool (compare_args > -1)))
    | _ -> fail @@ TypeMismatch (l, r)
  (* | Undefined, _, _ | _, Undefined, _ -> fail @@ Unbound "name" *)
  (* | _ -> fail (Incorrect_force (EBinOp (op, l, r))) *)

  and eval_decl env (DeclLet (pat, e)) =
    let* env' = env in
    let val_e = eval env' e in
    match_pattern env pat val_e

  and match_pattern env pat value =
    match pat with
    | PatVar x ->
      let* env = env in
      (match Hashtbl.find_opt env x with
       | Some _ ->
         (* todo: need to compare types *)
         (* replace bad *)
         Hashtbl.replace env x value;
         return env
       | None ->
         (* todo: прочиттать про add, мб поезно *)
         Hashtbl.add env x value;
         return env)
    | PatCons (hd1, tl1) ->
      (match force value with
       | Result (Ok (ValList (hd2, tl2))) ->
         let env = match_pattern env hd1 hd2 in
         match_pattern env tl1 tl2
       | _ -> fail NonExhaustivePatterns)
    | PatNil ->
      (match force value with
       | Result (Ok ValNil) -> env
       | _ -> fail NonExhaustivePatterns)
    | PatWild -> env
    | PatTuple pats ->
      (match force value with
       | Result (Ok (ValTuple values)) ->
         List.fold_left2
           (fun env pat_elem val_elem -> match_pattern env pat_elem val_elem)
           env
           pats
           values
       | _ -> fail NonExhaustivePatterns)
    | PatLit lit ->
      force value
      >>= fun value ->
      (match lit, value with
       | LitInt n, ValInt m when n = m -> env
       | LitBool b, ValBool bv when b = bv -> env
       | LitChar c, ValChar cv when c = cv -> env
       | LitString s, ValString sv when s = sv -> env
       (*
          | LitFloat f, ValFloat fv when f = fv -> env
       *)
       | _ -> fail NonExhaustivePatterns)
  ;;

  let eval_prog p = List.fold_left eval_decl (return (Hashtbl.create 69)) p

  and pp_environment fmt env =
    match env with
    | Result (Ok env) ->
      Hashtbl.iter (fun key value -> fprintf fmt "%s => %a \n" key pp_value_t value) env
    | Result (Error err) -> fprintf fmt "%a" pp_err err
    | _ -> ()
  ;;

  let run_prog p = eval_prog p
  let interpret p = Format.printf "%a" pp_environment (eval_prog p)
end

module Interpret = Eval
