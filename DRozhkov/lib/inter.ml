(** Copyright 2021-2023, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format
open Base

type value =
  | VInt of int
  | VBool of bool
  | VList of value list
  | VFun of pattern * expr * env

and env = (string, value, String.comparator_witness) Map.t

let rec pp_value ppf =
  let open Stdlib.Format in
  function
  | VInt x -> fprintf ppf "%d" x
  | VBool b -> fprintf ppf "%b" b
  | VList vl ->
    fprintf
      ppf
      "[%a]"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "; ") pp_value)
      vl
  | VFun _ -> fprintf ppf "<fun>"
;;

type error =
  | Division_by_zero
  | Pattern_matching_error
  | Unbound_value of string
  | Incorrect_type of value

let pp_error fmt = function
  | Division_by_zero -> fprintf fmt "Division by zero"
  | Unbound_value id -> fprintf fmt "Unbound value %s" id
  | Incorrect_type v -> fprintf fmt "Value %a has incorrect type in expression" pp_value v
  | Pattern_matching_error ->
    fprintf fmt "Value can't be match with any case in this expression"
;;

module type MONADERROR = sig
  type ('a, 'err) t

  val return : 'a -> ('a, 'err) t
  val fail : 'err -> ('a, 'err) t
  val ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  val ( let* ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
end

let vint x = VInt x
let vbool b = VBool b
let vlist l = VList l
let vfun s e env = VFun (s, e, env)

module Env (M : MONADERROR) = struct
  open M

  let empty = Base.Map.empty (module String)

  let find env name =
    match Base.Map.find env name with
    | Some x -> return x
    | None -> fail (Unbound_value name)
  ;;

  let extend env k v = Base.Map.update env k ~f:(fun _ -> v)
end

module Inter (M : MONADERROR) = struct
  open M
  open Env (M)

  let eval_binop (v1, bop, v2) =
    match v1, bop, v2 with
    | VInt x, Mult, VInt y -> return (vint (x * y))
    | VInt _, Split, VInt y when y = 0 -> fail Division_by_zero
    | VInt x, Split, VInt y -> return (vint (x / y))
    | VInt x, Plus, VInt y -> return (vint (x + y))
    | VInt x, Minus, VInt y -> return (vint (x - y))
    | VInt x, Equally, VInt y -> return (vbool (x = y))
    | VInt x, NEqually, VInt y -> return (vbool (x <> y))
    | VBool x, And, VBool y -> return (vbool (x && y))
    | VBool x, Or, VBool y -> return (vbool (x || y))
    | VInt x, Less, VInt y -> return (vbool (x < y))
    | VInt x, LeEq, VInt y -> return (vbool (x <= y))
    | VInt x, More, VInt y -> return (vbool (x > y))
    | VInt x, MoEq, VInt y -> return (vbool (x >= y))
    | _ -> fail (Incorrect_type v1)
  ;;

  let rec match_pattern env = function
    | PDash, _ -> Some env
    | PConst (Int i1), VInt i2 when i1 = i2 -> Some env
    | PConst (Bool b1), VBool b2 when Bool.equal b1 b2 -> Some env
    | PVar id, v -> Some (Base.Map.set env ~key:id ~data:v)
    | PCons (p1, p2), VList (h :: tl) ->
      (match match_pattern env (p1, h) with
       | Some env -> match_pattern env (p2, VList tl)
       | None -> None)
    | _ -> None
  ;;

  let eval_expr =
    let rec helper env = function
      | EConst c ->
        (match c with
         | Int i -> return (vint i)
         | Bool b -> return (vbool b))
      | Var id ->
        let* v = find env id in
        let v =
          match v with
          | VFun (p, e, env) -> VFun (p, e, extend env id v)
          | _ -> v
        in
        return v
      | EBinop (e1, op, e2) ->
        let* v1 = helper env e1 in
        let* v2 = helper env e2 in
        eval_binop (v1, op, v2)
      | Nil -> return (VList [])
      | EIfThenElse (c, t, f) ->
        let* cv = helper env c in
        (match cv with
         | VBool true -> helper env t
         | VBool false -> helper env f
         | _ -> fail (Incorrect_type cv))
      | EMatch (e, cases) ->
        let* v = helper env e in
        let rec match_cases env v = function
          | (pat, expr) :: tl ->
            (match match_pattern env (pat, v) with
             | Some env' -> helper env' expr
             | None -> match_cases env v tl)
          | [] -> fail Pattern_matching_error
        in
        match_cases env v cases
      | ELet (_, name, e1, e2) ->
        let* v1 = helper env e1 in
        let env' = Base.Map.set env ~key:name ~data:v1 in
        let* v2 = helper env' e2 in
        return v2
      | EFun (id, e) -> return (VFun (id, e, env))
      | EList (h, tl) ->
        let* h = helper env h in
        let* tl = helper env tl in
        (match tl with
         | VList tl -> return (VList (h :: tl))
         | _ -> fail (Incorrect_type tl))
      | EApp (f, e) ->
        let* fv = helper env f in
        let* ev = helper env e in
        (match fv with
         | VFun (p, e, env) ->
           let* env' =
             match match_pattern env (p, ev) with
             | Some env -> return env
             | None -> fail Pattern_matching_error
           in
           helper env' e
         | _ -> fail (Incorrect_type fv))
    in
    helper
  ;;

  let eval_base env = function
    | Expr e ->
      let* _ = eval_expr env e in
      return env
    | Decl (NoRec, x, e) ->
      let* v = eval_expr env e in
      let env = extend env x v in
      return env
    | Decl (Rec, x, e) ->
      let* v = eval_expr env e in
      let env1 = extend env x v in
      let v =
        match v with
        | VFun (p, e, _) -> VFun (p, e, env1)
        | _ -> v
      in
      let env = extend env x v in
      return env
  ;;

  let eval_exprs (expr : expressions) =
    List.fold_left
      ~f:(fun env item ->
        let* env = env in
        let* env = eval_base env item in
        return env)
      ~init:(return empty)
      expr
  ;;
end

module MONAD_RESULT : MONADERROR with type ('a, 'err) t = ('a, 'err) Result.t = struct
  type ('a, 'err) t = ('a, 'err) Result.t

  let return x = Ok x
  let fail err = Error err

  let ( >>= ) m f =
    match m with
    | Ok x -> f x
    | Error e -> Error e
  ;;

  let fail e = Error e
  let ( let* ) = ( >>= )
end

module Interpret = Inter (MONAD_RESULT)

let run_inter s =
  let open Stdlib.Format in
  match Parser.parse s with
  | Ok parsed ->
    (match Infer.run_infer parsed with
     | Ok env_inf ->
       (match Interpret.eval_exprs parsed with
        | Ok final_env ->
          let pp_env env =
            Base.Map.iteri
              ~f:(fun ~key ~data ->
                match Base.Map.find env_inf key with
                | Some (S (_, ty)) ->
                  printf "val %s : %a = %a\n" key Infer.pp_typ ty pp_value data
                | None -> printf "val %s = %a\n" key pp_value data)
              env
          in
          pp_env final_env
        | Error e -> printf "Interpreter error: %a\n" pp_error e)
     | Error e -> printf "Typecheck error: %a\n" Infer.pp_error e)
  | Error e -> printf "Parsing error: %s\n" e
;;
