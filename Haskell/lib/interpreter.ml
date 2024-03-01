(** Copyright 2023-2024, Danil P *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format

module LazyResult : sig
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

  let rec force mx =
    match mx with
    | Result _ -> mx
    | Thunk thunk -> thunk () |> force
  ;;

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

module EnvTypes = struct
  type err =
    | NotInScopeError of string
    | DivisionByZeroError
    | NonExhaustivePatterns of string
    | TypeMismatch

  type res = (value, err) LazyResult.t
  and environment = (string, res, Base.String.comparator_witness) Base.Map.t

  and value =
    | ValInt of int
    | ValBool of bool
    | ValString of string
    | ValChar of char
    | ValNil
    | ValCons of res * res
    | ValTuple of res list
    | ValFun of pat * expr * environment
    | ValEmptyTree
    | ValTree of res * res * res
end

module Env : sig
  include module type of EnvTypes

  val empty : environment
  val find : ('a, 'b, 'c) Base.Map.t -> 'a -> 'b option
  val update : ('a, 'b, 'c) Base.Map.t -> 'a -> 'b -> ('a, 'b, 'c) Base.Map.t
  val pp_err : formatter -> err -> unit
  val pp_value : formatter -> value -> unit
  val pp_environment : formatter -> environment -> unit
  val pp_value_t : formatter -> (value, err) LazyResult.t -> unit
end = struct
  open LazyResult
  include EnvTypes

  let empty = Base.Map.empty (module Base.String)
  let find env key = Base.Map.find env key
  let update env key data = Base.Map.update env key ~f:(fun _ -> data)

  let pp_err fmt = function
    | NotInScopeError str -> fprintf fmt "Not in scope: %S" str
    | DivisionByZeroError -> fprintf fmt "Infinity"
    | NonExhaustivePatterns s -> fprintf fmt "Non-exhausitve patterns in %s" s
    | TypeMismatch ->
      printf "Type mismatch. Please run type checker to get more information."
  ;;

  (* very inefficient, verbose and abhorrent *)
  let rec transform_cons = function
    | Result (Ok (ValCons (hd, nil))) when force nil = Result (Ok ValNil) ->
      force hd :: []
    | Result (Ok (ValCons (hd, tl))) -> force hd :: transform_cons tl
    | Thunk t -> transform_cons (t ())
    | _ -> []
  ;;

  let rec iter f = function
    | [] -> ()
    | [ x ] -> f true x
    | x :: tl ->
      f false x;
      iter f tl
  ;;

  (* credit https://gist.github.com/mjambon/75f54d3c9f1a352b38a8eab81880a735 *)
  let rec pp_tree ?(line_prefix = "") fmt x =
    let get_children = function
      | Result (Ok (ValTree (_, a, b))) ->
        List.filter (( <> ) (Result (Ok ValEmptyTree))) [ force a; force b ]
      | _ -> []
    in
    let pp_node fmt = function
      | Result (Ok (ValTree (v, _, _))) -> fprintf fmt "%a\n" pp_value_t v
      | other -> fprintf fmt "%a\n" pp_value_t other
    in
    let rec print_root fmt indent x =
      let x = force x in
      pp_node fmt x;
      let children = get_children x in
      iter (print_child fmt indent) children
    and print_child fmt indent is_last x =
      let line = if is_last then "└── " else "├── " in
      fprintf fmt "%s%s" indent line;
      let extra_indent = if is_last then "    " else "│   " in
      print_root fmt (indent ^ extra_indent) x
    in
    print_root fmt line_prefix x

  and pp_value fmt = function
    | ValInt n -> fprintf fmt "%d" n
    | ValBool b -> fprintf fmt "%b" b
    | ValString s -> fprintf fmt "\"%s\"" s
    | ValChar c -> fprintf fmt "'%c'" c
    | ValNil -> printf "[]"
    | ValCons (hd, tl) ->
      fprintf fmt "[";
      pp_print_list
        ~pp_sep:(fun fmt () -> fprintf fmt ", ")
        pp_value_t
        fmt
        (force hd :: transform_cons tl);
      fprintf fmt "]"
    | ValTuple es ->
      fprintf fmt "(";
      pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_value_t fmt es;
      fprintf fmt ")"
    | ValFun _ -> fprintf fmt "<fun>"
    | ValEmptyTree -> printf "Leaf"
    | ValTree _ as tree ->
      print_string "\n";
      pp_tree fmt (return tree)

  and pp_value_t fmt = function
    | Result (Ok x) -> pp_value fmt x
    | Result (Error err) -> pp_err fmt err
    | Thunk t -> t () |> pp_value_t fmt
  ;;

  let pp_environment fmt env =
    Base.Map.iteri
      ~f:(fun ~key ~data:value -> fprintf fmt "%s => %a \n" key pp_value_t value)
      env
  ;;
end

module Eval : sig
  val interpret : prog -> unit
  val eval_prog : prog -> (Env.environment, Env.err) LazyResult.t
end = struct
  open LazyResult
  open LazyResult.Syntax
  open Env

  let rec eval env expr =
    match expr with
    | ExprUnOp (op, e) ->
      let* e = eval env e in
      (match op, e with
       | Neg, ValInt i -> return (ValInt (-i))
       | Not, ValBool b -> return (ValBool (not b))
       | _ -> fail TypeMismatch)
    | ExprBinOp (op, e1, e2) ->
      thunk (fun () ->
        let* e1' = eval env e1 in
        let* e2' = eval env e2 in
        eval_binop e1' e2' op)
    | ExprVar x ->
      (match find env x with
       | Some v ->
         (match v with
          | Result (Ok (ValFun (p, e, env))) -> return @@ ValFun (p, e, update env x v)
          | _ -> v)
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
        | _ -> fail TypeMismatch)
    | ExprApp (f, arg) ->
      thunk (fun () ->
        let* f_val = eval env f in
        match f_val with
        | ValFun (pat, body, f_env) ->
          let* local_env =
            match_pattern (return f_env) pat (thunk (fun () -> eval env arg))
          in
          eval local_env body
        | _ -> fail TypeMismatch)
    | ExprNil -> thunk (fun () -> return ValNil)
    | ExprCons (hd, tl) ->
      let hd_t = eval env hd in
      let tl_t = eval env tl in
      thunk (fun () -> return (ValCons (hd_t, tl_t)))
    | ExprFunc (pat, expr) -> return (ValFun (pat, expr, env))
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
        let helper env (pat, expr) =
          let* env' = env in
          let value = eval env' expr in
          match_pattern env pat value
        in
        let* local_env = List.fold_left helper (return env) bindings in
        eval local_env expr)
    | ExprTree Leaf -> thunk (fun () -> return ValEmptyTree)
    | ExprTree (Node (v, n1, n2)) ->
      let v_t = eval env v in
      let n1_t = eval env n1 in
      let n2_t = eval env n2 in
      thunk (fun () -> return (ValTree (v_t, n1_t, n2_t)))

  and eval_case env res = function
    | (pat, expr) :: rest ->
      (match match_pattern env pat res with
       | Result (Ok env) -> eval env expr
       | _ -> eval_case env res rest)
    | [] -> fail TypeMismatch

  and eval_binop l r op =
    match l, r, op with
    | ValInt x, ValInt y, Add -> return (ValInt (x + y))
    | ValInt x, ValInt y, Sub -> return (ValInt (x - y))
    | ValInt x, ValInt y, Mul -> return (ValInt (x * y))
    | ValInt x, ValInt y, Div ->
      if y = 0 then fail DivisionByZeroError else return (ValInt (x / y))
    | ValBool x, ValBool y, And -> return (ValBool (x && y))
    | ValBool x, ValBool y, Or -> return (ValBool (x || y))
    | _, _, Add | _, _, Sub | _, _, Mul | _, _, Div | _, _, And | _, _, Or ->
      fail TypeMismatch
    | l, r, op ->
      let rec compare l r =
        match l, r with
        | ValInt _, ValInt _
        | ValBool _, ValBool _
        | ValString _, ValString _
        | ValChar _, ValChar _ -> return (Base.Poly.compare l r)
        | ValCons (hd1, tl1), ValCons (hd2, tl2) ->
          let* hd1 = hd1 in
          let* hd2 = hd2 in
          let* res = compare hd1 hd2 in
          (match res, l with
           | 0, ValCons _ ->
             let* tl1 = tl1 in
             let* tl2 = tl2 in
             compare tl1 tl2
           | _ -> return res)
        | _ -> fail TypeMismatch
      in
      let* compare_args = compare l r in
      (match op with
       | Eq -> return (ValBool (compare_args = 0))
       | Neq -> return (ValBool (compare_args != 0))
       | Lt -> return (ValBool (compare_args = -1))
       | Leq -> return (ValBool (compare_args < 1))
       | Gt -> return (ValBool (compare_args = 1))
       | _ -> return (ValBool (compare_args > -1)))

  and match_pattern env pat value =
    match pat with
    | PatVar x ->
      let* env' = env in
      let env = update env' x value in
      return env
    | PatCons (hd1, tl1) ->
      (match force value with
       | Result (Ok (ValCons (hd2, tl2))) ->
         let env = match_pattern env hd1 hd2 in
         match_pattern env tl1 tl2
       | _ -> fail @@ NonExhaustivePatterns "list")
    | PatNil ->
      (match force value with
       | Result (Ok ValNil) -> env
       | _ -> fail @@ NonExhaustivePatterns "[]")
    | PatWild -> env
    | PatTuple pats ->
      (match force value with
       | Result (Ok (ValTuple values)) when List.length values = List.length pats ->
         List.fold_left2
           (fun env pat_elem val_elem -> match_pattern env pat_elem val_elem)
           env
           pats
           values
       | _ -> fail @@ NonExhaustivePatterns "tuple")
    | PatLit lit ->
      force value
      >>= fun value ->
      (match lit, value with
       | LitInt n, ValInt m when n = m -> env
       | LitBool b, ValBool bv when b = bv -> env
       | LitChar c, ValChar cv when c = cv -> env
       | LitString s, ValString sv when s = sv -> env
       | _ -> fail @@ NonExhaustivePatterns "literal")
    | PatLeaf ->
      (match force value with
       | Result (Ok ValEmptyTree) -> env
       | _ -> fail @@ NonExhaustivePatterns "Leaf")
    | PatTree (v1, l1, r1) ->
      (match force value with
       | Result (Ok (ValTree (v2, l2, r2))) ->
         let env = match_pattern env v1 v2 in
         let env = match_pattern env l1 l2 in
         match_pattern env r1 r2
       | _ -> fail @@ NonExhaustivePatterns "🌳")

  and eval_decl env (DeclLet (pat, e)) =
    let* env' = env in
    let val_e = eval env' e in
    match_pattern env pat val_e
  ;;

  let eval_prog p = List.fold_left eval_decl (return empty) p

  let pp_environment fmt env =
    Base.Map.iteri
      ~f:(fun ~key ~data:value -> fprintf fmt "%s => %a \n" key pp_value_t value)
      env
  ;;

  let interpret p =
    match eval_prog p with
    | Result (Ok env) -> printf "%a" pp_environment env
    | Result (Error err) -> printf "%a" pp_err err
    | _ -> ()
  ;;
end
