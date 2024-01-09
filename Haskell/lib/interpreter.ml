open Parser
open Ast
open Format

module type MONAD = sig
  type ('a, 'e) t

  val return : 'a -> ('a, 'e) t
  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

  module Syntax : sig
    val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  end
end

module type MONAD_ERROR = sig
  include MONAD

  val fail : 'e -> ('a, 'e) t
  val thunk : (unit -> ('a, 'b) t) -> ('a, 'b) t
end

(* TODO: change names*)

module LAZY_RESULT : MONAD_ERROR = struct
  type ('a, 'e) t =
    | Result of ('a, 'e) result
    | Thunk of (unit -> ('a, 'e) t)

  let return x = Result (Ok x)
  let fail err = Result (Error err)
  let thunk t = Thunk t

  let rec ( >>= ) mx f =
    match mx with
    | Result (Ok x) -> f x
    | Result (Error e) -> Result (Error e)
    | Thunk thunk -> thunk () >>= f
  ;;

  module Syntax = struct
    let ( let* ) = ( >>= )
  end
end

module type Env = sig
  type environment
  type value
  type err
end

module Environment (M : MONAD_ERROR) : Env = struct
  include LAZY_RESULT

  type environment = (string, (value, err) t) Hashtbl.t

  and value =
    | ValInt of int
    | ValBool of bool
    | ValString of string
    | ValFun of string * expr * environment

  and err =
    | NotInScopeError of string
    | ValueTypeError of value
    | TypeError of string
    | RuntimeError of string
    | DivisionByZeroError
    | EvalError of value * value

  let rec pp_value fmt = function
    | ValInt n -> Format.fprintf fmt "ValInt %d" n
    | ValBool b -> Format.fprintf fmt "ValBool %b" b
    | ValFun (param, _body, _env) ->
      (* Just printing the parameter and indicating a function *)
      Format.fprintf fmt "Function(%s)" param
  ;;

  let pp_environment fmt env =
    let bindings = ref [] in
    Hashtbl.iter (fun key value -> bindings := (key, value) :: !bindings) env;
    let pp_binding fmt (key, value) = Format.fprintf fmt "%s -> %a" key pp_value value in
    Format.fprintf fmt "{ ";
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
      pp_binding
      fmt
      !bindings;
    Format.fprintf fmt " }"
  ;;

  type success_result_item = string * value
  type success_result = success_result_item list

  let pp_success_result_item fmt item =
    let item_name, item_value = item in
    fprintf fmt "%S: %a" item_name pp_value item_value
  ;;

  let rec pp_success_result fmt = function
    | [] -> ()
    | h :: tl -> fprintf fmt "%a\n%a" pp_success_result_item h pp_success_result tl
  ;;

  let pp_err fmt = function
    | NotInScopeError str -> fprintf fmt "Not in scope: %S" str
    | ValueTypeError err_val -> fprintf fmt "ValueTypeError: %a" pp_value err_val
    | TypeError err_expr -> fprintf fmt "TypeError: %S" err_expr
    | RuntimeError str -> fprintf fmt "RuntimeError: %S" str
    | DivisionByZeroError -> fprintf fmt "DivisionByZeroError"
    | EvalError (val1, val2) ->
      fprintf fmt "EvalError: %a # %a" pp_value val1 pp_value val2
  ;;
end

module Eval (E : Env) : sig
  val eval_decl : decl -> Environment.environment
end = struct
  open LAZY_RESULT
  open LAZY_RESULT.Syntax
  open E

  let rec eval env expr =
    match expr with
    | ExprBinOp (op, e1, e2) ->
      let* e1' = eval env e1 in
      let* e2' = eval env e2 in
      force_binop e1' e2' op
    | ExprVar x ->
      (match Hashtbl.find_opt env x with
       | Some v -> v
       | None -> fail @@ NotInScopeError x)
    | ExprLit lit ->
      (match lit with
       | LitInt n -> thunk (fun () -> return (ValInt n))
       | LitBool b -> thunk (fun () -> return (ValBool b))
       | LitString s -> thunk (fun () -> return (ValString s)))
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
        let arg = thunk (fun () -> eval env arg) in
        match f_val with
        | ValFun (param, body, f_env) ->
          let local_env = Hashtbl.copy f_env in
          Hashtbl.add local_env param arg;
          eval local_env body
        | _ -> fail @@ TypeError "Application to a non-function")
  (* | ExprVar x -> 
      (match Hashtbl.find_opt env x with
        | Some v -> return v
        | None -> fail @@ NotInScopeError x)
    | ExprLit LitInt _ as n -> return n
    | ExprLit LitBool _ as b -> return b 
    | ExprFunc (PatVar param, body) as f -> return f
    | ExprApp (f, arg) ->
      let* f_val = force_lazy env f in
      let* arg_val = force_lazy env arg in
      (match f_val with
        | ValFun (param, body, f_env) ->
          let local_env = Hashtbl.copy f_env in
          Hashtbl.add local_env param arg_val;
          force local_env body
        | _ -> fail @@ TypeError "Application to a non-function")
    | ExprIf (cond, then_expr, else_expr) ->
      let* cond_val = force env cond in
      (match cond_val with
        | ValBool true -> force env then_expr
        | ValBool false -> force env else_expr
        | _ -> fail @@ TypeError "Condition in if-expression is not a boolean") *)

  and eval_decl env d =
    match d with
    | DeclLet (pat, e) ->
      let val_e = eval env e in
      return @@ match_pattern env pat val_e

  and match_pattern env pat value =
    match pat with
    | PatVar x ->
      Hashtbl.replace env x value;
      return env
  (* | PInt n ->
     (match value with
     | ValInt m when m = n -> return ()
     | _ -> fail "Pattern match failure")
     | PBool b ->
     (match value with
     | ValBool m when m = b -> return ()
     | _ -> fail "Pattern match failure") *)

  (* todo: move thunk to functino call *)
  and force_binop l r op =
    match l, r, op with
    | ValInt x, ValInt y, Add -> thunk (fun () -> return (ValInt (x + y)))
    | ValInt x, ValInt y, Sub -> thunk (fun () -> return (ValInt (x - y)))
    | ValInt x, ValInt y, Mul -> thunk (fun () -> return (ValInt (x * y)))
  ;;
  (* | ValInt x, ValInt y, Div ->
        if y = 0 then fail Division_by_zero else return (ValInt (x / y))
      | ValInt x, ValInt y, Eq -> return (ValBool (x = y))
      | ValInt x, ValInt y, Neq -> return (ValBool (x != y))
      | ValInt x, ValInt y, Leq -> return (ValBool (x <= y))
      | ValInt x, ValInt y, Lt -> return (ValBool (x < y))
      | ValInt x, ValInt y, Geq -> return (ValBool (x >= y))
      | ValInt x, ValInt y, Gt -> return (ValBool (x > y))
      | ValBool x, ValBool y, And -> return (ValBool (x && y))
      | ValBool x, ValBool y, Or -> return (ValBool (x || y))
      | ValBool x, ValBool y, Eq -> return (ValBool (x = y))
      | ValBool x, ValBool y, Neq -> return (ValBool (x != y))
      | ValString x, ValString y, Eq -> return (ValBool (x = y))
      | ValString x, ValString y, Neq -> return (ValBool (x <> y))
      | Undefined, _, _ | _, Undefined, _ -> fail @@ Unbound "name" *)
  (* | _ -> fail (Incorrect_force (EBinOp (op, l, r))) *)
end

module Interpret = Eval (LAZY_RESULT)

let test_expr =
  DeclLet
    ( PatVar "yx"
    , ExprIf
        ( ExprLit (LitBool false)
        , ExprLit (LitInt 45)
        , ExprBinOp (Add, ExprLit (LitInt 70), ExprLit (LitInt 60)) ) )
;;

let pp_infer e =
  match e with
  | Ok value -> Format.printf "%a" pp_environment value
;;

let run () =
  let env = Hashtbl.create 10 in
  match Interpret.force_decl env test_expr () with
  | Ok e -> pp_infer (e ())
  | Error err -> print_endline "error"
;;

let () = run ()

(* type runtime_env = (string * value) list

   let rec lookup (env : runtime_env) (x : string) : value =
   match env with
   | [] -> failwith ("Unbound variable: " ^ x)
   | (name, value) :: _ when name = x -> value
   | _ :: tail -> lookup tail x

   let extend (env : runtime_env) (x : string) (v : value) : runtime_env =
   (x, v) :: env *)

(* let rec force env = function
   | ExprVar x ->
   begin match Hashtbl.find_opt env x with
   | Some thunk -> thunk ()
   | None -> error ("Unbound variable: " ^ x)
   end
   | ExprLit LitInt n -> return (VInt n)
   | ExprLit LitBool b -> return (VBool b)
   | ExprFunc (x, e) -> return (VLambda (x, e, env))
   | ExprApp (e1, e2) ->
   let* v1 = force env e1 in
   let thunk = fun () -> force env e2 in
   begin match v1 with
   | VLambda (x, body, env') ->
   let env'' = Hashtbl.copy env' in
   Hashtbl.add env'' x (fun () -> return (VThunk thunk));
   force env'' body
   | _ -> error "Application to a non-function"
   end
   | ExprLet (x, e1, e2) ->
   let thunk = fun () -> force env e1 in
   Hashtbl.add env x (fun () -> return (VThunk thunk));
   force env e2
   | ExprIf (cond, e_then, e_else) ->
   let* v_cond = force env cond in
   begin match v_cond with
   | VBool b -> force env (if b then e_then else e_else)
   | _ -> error "Non-boolean condition in if expression"
   end
   end *)

(* and force : environment -> expr -> value  = fun env expr ->
      let open Syntax in
      match expr with
      | ExprBinOp (op, e1, e2) ->
        let* e1' = force env e1 in
        let* e2' = force env e2 in
        force_binop e1' e2' op
      | ExprVar x -> 
        (match Hashtbl.find_opt env x with
          | Some v -> return v
          | None -> fail @@ NotInScopeError x)
      | ExprLit lit ->
        (match lit with
        | LitInt n -> return (ValInt n)
        | LitBool b -> return (ValBool b)
        | LitString s -> return (ValString s)
        | LitChar c -> return (ValChar c))
      | ExprFunc (PatVar param, body) -> return (ValFun (param, body, env))
      | ExprApp (f, arg) ->
        let* f_val = force env f in
        let* arg_val = force env arg in
        (match f_val with
          | ValFun (param, body, f_env) ->
            let local_env = Hashtbl.copy f_env in
            Hashtbl.add local_env param arg_val;
            force local_env body
          | _ -> fail @@ TypeError "Application to a non-function")
      | ExprIf (cond, then_expr, else_expr) ->
        let* cond_val = force env cond in
        (match cond_val with
          | ValBool true -> force env then_expr
          | ValBool false -> force env else_expr
          | _ -> fail @@ TypeError "Condition in if-expression is not a boolean") *)
