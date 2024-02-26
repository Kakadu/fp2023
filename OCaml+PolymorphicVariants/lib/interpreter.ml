(** Copyright 2023-2024, Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

type value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VUnit
  | VList of value list
  | VTuple of value list
  | VFun of pattern * rec_flag * expr * environment

and environment = (id, value, String.comparator_witness) Map.t

let rec pp_value ppf =
  let open Stdlib.Format in
  function
  | VInt x -> fprintf ppf "%d" x
  | VBool b -> fprintf ppf "%b" b
  | VString s -> fprintf ppf "%s" s
  | VUnit -> fprintf ppf "()"
  | VList vl ->
    fprintf
      ppf
      "[%a]"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "; ") pp_value)
      vl
  | VTuple vl ->
    fprintf
      ppf
      "(%a)"
      (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ", ") pp_value)
      vl
  | VFun _ -> fprintf ppf "<fun>"
;;

type error =
  [ `Division_by_zero
  | `Unbound_variable of id
  | `Wrong_type of value
  | `Pattern_mathing_failed
  | `Not_implemented (** for polyvariants or let-patterns, caught in infer *)
  ]

let pp_error ppf : error -> _ =
  let open Stdlib.Format in
  function
  | `Division_by_zero -> fprintf ppf "Division by zero"
  | `Unbound_variable id -> fprintf ppf "Unbound variable '%s'" id
  | `Wrong_type v -> fprintf ppf "Incorrect type of expression: %a" pp_value v
  | `Pattern_mathing_failed -> fprintf ppf "Pattern-matching failed"
  | `Not_implemented -> fprintf ppf "Not implemented"
;;

module type MONAD_FAIL = sig
  include Monad.S2

  val fail : error -> ('a, error) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

let vint x = VInt x
let vbool b = VBool b
let vstring s = VString s
let vunit = VUnit
let vlist l = VList l
let vtuple l = VTuple l
let vfun p f e env = VFun (p, f, e, env)

module Env (M : MONAD_FAIL) = struct
  open M

  let empty = Base.Map.empty (module String)

  let find env name =
    match Base.Map.find env name with
    | Some x -> return x
    | None -> fail (`Unbound_variable name)
  ;;

  let extend env k v = Base.Map.update env k ~f:(fun _ -> v)
end

module Eval (M : MONAD_FAIL) : sig
  val eval_structure : structure -> (environment, error) M.t
end = struct
  open M
  open Env (M)

  let rec check_match env = function
    | PAny, _ -> Some env
    | PConst (CInt i1), VInt i2 when i1 = i2 -> Some env
    | PConst (CBool b1), VBool b2 when Bool.equal b1 b2 -> Some env
    | PConst (CString s1), VString s2 when String.equal s1 s2 -> Some env
    | PConst CNil, VList [] -> Some env
    | PVar x, v -> Some (extend env x v)
    | PTuple pl, VTuple vl ->
      let env =
        List.fold2
          pl
          vl
          ~f:(fun env p v ->
            match env with
            | Some e -> check_match e (p, v)
            | None -> None)
          ~init:(Some env)
      in
      (match env with
       | Ok env -> env
       | _ -> None)
    | PCons (p1, p2), VList (v :: vl) ->
      let env = check_match env (p2, VList vl) in
      (match env with
       | Some env -> check_match env (p1, v)
       | None -> None)
    | _ -> None
  ;;

  let eval_binop (bop, v1, v2) =
    match bop, v1, v2 with
    | Mul, VInt x, VInt y -> return (vint (x * y))
    | Div, VInt _, VInt y when y = 0 -> fail `Division_by_zero
    | Div, VInt x, VInt y -> return (vint (x / y))
    | Add, VInt x, VInt y -> return (vint (x + y))
    | Sub, VInt x, VInt y -> return (vint (x - y))
    | Eq, VInt x, VInt y -> return (vbool (x = y))
    | Neq, VInt x, VInt y -> return (vbool (x <> y))
    | Lt, VInt x, VInt y -> return (vbool (x < y))
    | Lte, VInt x, VInt y -> return (vbool (x <= y))
    | Gt, VInt x, VInt y -> return (vbool (x > y))
    | Gte, VInt x, VInt y -> return (vbool (x >= y))
    | And, VBool x, VBool y -> return (vbool (x && y))
    | Or, VBool x, VBool y -> return (vbool (x || y))
    | _ -> fail (`Wrong_type v1)
  ;;

  let eval_expr =
    let rec helper env = function
      | EConst c ->
        (match c with
         | CInt i -> return (vint i)
         | CBool b -> return (vbool b)
         | CString s -> return (vstring s)
         | CNil -> return (vlist [])
         | CUnit -> return vunit)
      | EVar x ->
        let* v = find env x in
        let v =
          match v with
          | VFun (p, Rec, e, env) -> VFun (p, Rec, e, extend env x v)
          | _ -> v
        in
        return v
      | EBin_op (op, e1, e2) ->
        let* v1 = helper env e1 in
        let* v2 = helper env e2 in
        eval_binop (op, v1, v2)
      | EIf (c, t, f) ->
        let* cv = helper env c in
        (match cv with
         | VBool true -> helper env t
         | VBool false -> helper env f
         | _ -> fail (`Wrong_type cv))
      | EMatch (e, cl) ->
        let* v = helper env e in
        let rec match_helper env v = function
          | (p, e) :: tl ->
            let env' = check_match env (p, v) in
            (match env' with
             | Some env -> helper env e
             | None -> match_helper env v tl)
          | [] -> fail `Pattern_mathing_failed
        in
        match_helper env v cl
      | ELet (Rec, (PVar x, e1), e2) ->
        let* v = helper env e1 in
        let env1 = extend env x v in
        let v =
          match v with
          | VFun (p, _, e, _) -> VFun (p, Rec, e, env1)
          | _ -> v
        in
        let env2 = extend env x v in
        helper env2 e2
      | ELet (Nonrec, (PVar x, e1), e2) ->
        let* v = helper env e1 in
        let env = extend env x v in
        helper env e2
      | EFun (p, e) -> return (vfun p Nonrec e env)
      | ETuple el ->
        let* vl =
          List.fold_left
            ~f:(fun acc e ->
              let* acc = acc in
              let* v = helper env e in
              return (v :: acc))
            ~init:(return [])
            el
        in
        return (vtuple (List.rev vl))
      | ECons (h, tl) ->
        let* hv = helper env h in
        let* tlv = helper env tl in
        (match tlv with
         | VList vl -> return (vlist (hv :: vl))
         | _ -> fail (`Wrong_type tlv))
      | EApply (e1, e2) ->
        let* v1 = helper env e1 in
        let* v2 = helper env e2 in
        (match v1 with
         | VFun (p, _, e, env) ->
           let* env' =
             match check_match env (p, v2) with
             | Some env -> return env
             | None -> fail `Pattern_mathing_failed
           in
           helper env' e
         | _ -> fail (`Wrong_type v1))
      | _ -> fail `Not_implemented
    in
    helper
  ;;

  let eval_str_item env = function
    | SEval e ->
      let* _ = eval_expr env e in
      return env
    | SValue (Nonrec, (PVar x, e)) ->
      let* v = eval_expr env e in
      let env = extend env x v in
      return env
    | SValue (Rec, (PVar x, e)) ->
      let* v = eval_expr env e in
      let env1 = extend env x v in
      let v =
        match v with
        | VFun (p, _, e, _) -> VFun (p, Rec, e, env1)
        | _ -> v
      in
      let env = extend env x v in
      return env
    | _ -> fail `Not_implemented
  ;;

  let eval_structure (s : structure) =
    List.fold_left
      ~f:(fun env item ->
        let* env = env in
        let* env = eval_str_item env item in
        return env)
      ~init:(return empty)
      s
  ;;
end

module Inter = Eval (struct
    include Base.Result

    let ( let* ) m f = bind m ~f
  end)

let pp_env env_t env_v =
  let open Stdlib.Format in
  let open Infer in
  Base.Map.iteri
    ~f:(fun ~key ~data ->
      match Base.Map.find env_t key with
      | Some (S (_, ty)) ->
        printf "val %s : %a = %a\n" key Typedtree.pp_typ ty pp_value data
      | None -> printf "val %s = %a\n" key pp_value data)
    env_v
;;

let test_interpret s =
  let open Stdlib.Format in
  match Parser.parse s with
  | Ok parsed ->
    (match Infer.run_infer parsed with
     | Ok env_inf ->
       (match Inter.eval_structure parsed with
        | Ok env_int -> pp_env env_inf env_int
        | Error e -> printf "Interpreter error: %a\n" pp_error e)
     | Error e -> printf "Infer error: %a\n" Infer.pp_error e)
  | Error e -> printf "Parsing error: %s\n" e
;;
