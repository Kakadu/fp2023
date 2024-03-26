(** Copyright 2021-2023, Kakadu, RozhkovAleksandr *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format

type value =
  | VInt of int
  | VBool of bool
  | VNothing
  | VList of value list
  | VFun of string * expr * env

and env = (string, value, Base.String.comparator_witness) Base.Map.t

let rec pp_value ppf =
  let open Stdlib.Format in
  function
  | VInt x -> fprintf ppf "%d" x
  | VBool b -> fprintf ppf "%b" b
  | VNothing -> fprintf ppf "()"
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
let vnothing = VNothing
let vlist l = VList l
let vfun s e env = VFun (s, e, env)

module Env (M : MONADERROR) = struct
  let empty = Base.Map.empty (module Base.String)
  let extend env k v = Base.Map.update env k ~f:(fun _ -> v)
end

module Inter (M : MONADERROR) = struct
  open M
  open Env (M)

  let check_match env = function
    | PDash, _ -> Some env
    | PConst (Int i1), VInt i2 when i1 = i2 -> Some env
    | PConst (Bool b1), VBool b2 when Bool.equal b1 b2 -> Some env
    | PVar x, v -> Some (extend env x v)
    | _ -> None
  ;;

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

  let eval_expr =
    let rec helper env = function
      | EConst c ->
        (match c with
         | Int i -> return (vint i)
         | Bool b -> return (vbool b))
      | Nothing -> return vnothing
      | Var id ->
        let* v =
          match Base.Map.find env id with
          | None -> fail (Unbound_value id)
          | Some v -> return v
        in
        return v
      | EBinop (e1, op, e2) ->
        let* v1 = helper env e1 in
        let* v2 = helper env e2 in
        eval_binop (v1, op, v2)
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
            (match pat, v with
             | PDash, _ -> helper env expr
             | PVar id, v' ->
               let env' = Base.Map.set env ~key:id ~data:v' in
               helper env' expr
             | PConst const, cvalue ->
               (match const, cvalue with
                | Bool e1, VBool e2 when Bool.equal e1 e2 -> helper env expr
                | Int e1, VInt e2 when e1 = e2 -> helper env expr
                | _ -> match_cases env v tl))
          | [] -> fail Pattern_matching_error
        in
        match_cases env v cases
      | ELet (_, name, e1, e2) ->
        let* v1 = helper env e1 in
        let env' = Base.Map.set env ~key:name ~data:v1 in
        let* v2 = helper env' e2 in
        return v2
      | EFun (id, e) -> return (VFun (id, e, env))
      | EApp (f, e) ->
        let* fv = helper env f in
        let* ev = helper env e in
        (match fv with
         | VFun (id, e, fenv) ->
           let fenv = Base.Map.set fenv ~key:id ~data:ev in
           let updated_env =
             Base.Map.fold fenv ~init:env ~f:(fun ~key ~data acc_env ->
               Base.Map.set acc_env ~key ~data)
           in
           let* v = helper updated_env e in
           return v
         | _ -> fail (Incorrect_type fv))
      | EList es ->
        let* vs =
          Base.List.fold_right es ~init:(return []) ~f:(fun e acc ->
            let* v = helper env e in
            let* acc = acc in
            return (v :: acc))
        in
        return (VList vs)
    in
    helper
  ;;

  let interpret p =
    let eval_let env = function
      | ELet (_, id, e, Nothing) ->
        let* v = eval_expr env e in
        let env' = Base.Map.update env id ~f:(function _ -> v) in
        return (env', v)
      | expression ->
        let* v = eval_expr env expression in
        return (env, v)
    in
    let* env, rs =
      Base.List.fold_left
        p
        ~init:(return (empty, []))
        ~f:(fun acc e ->
          let* env, rs = acc in
          let* env, res = eval_let env e in
          return (env, res :: rs))
    in
    return (env, List.rev rs)
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

let run_inter input =
  match Parser.parse input with
  | Result.Ok tree ->
    let infer_result = Infer.run_infer tree in
    if Result.is_ok infer_result
    then (
      let _, et = Result.get_ok infer_result in
      let interpret_result = Interpret.interpret (List.map (fun (expr, _) -> expr) et) in
      if Result.is_ok interpret_result
      then (
        let _, vl = Result.get_ok interpret_result in
        List.iter
          (fun value ->
            pp_value Format.std_formatter value;
            Format.pp_print_flush Format.std_formatter ();
            printf "\n")
          vl)
      else printf "Interpretation error\n")
    else printf "Typecheck error\n"
  | _ -> printf "Parsing error\n"
;;
