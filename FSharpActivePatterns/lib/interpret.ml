(** Copyright 2023-2024, Vitaliy Dyachkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Errorinter

module type MonadFail = sig
  include Base.Monad.S2

  val run : ('a, 'e) t -> ok:('a -> ('b, 'e) t) -> err:('e -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

let is_constr = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

type environment = (name, value, String.comparator_witness) Map.t

module Environment (M : MonadFail) = struct
  open M

  let empty = Map.empty (module Base.String)

  let find_val map key =
    match Map.find map key with
    | Some value -> return value
    | None when is_constr @@ String.get key 0 -> fail (UnboundConstructor key)
    | _ -> fail (UnboundValue key)
  ;;

  let add_bind map key value = Map.update map key ~f:(fun _ -> value)

  let add_binds map binds =
    List.fold ~f:(fun map (k, v) -> add_bind map k v) ~init:map binds
  ;;
end

module Interpret (M : MonadFail) : sig
  val eval_program : program -> (value, error) M.t
end = struct
  open M
  open Environment (M)

  let rec bind_fun_params ?(env = empty) =
    let bind_pat_list patl argl =
      let binded_list =
        List.fold2
          patl
          argl
          ~f:(fun acc pat arg ->
            let* acc = acc in
            let* binding = bind_fun_params ~env (pat, arg) in
            return (acc @ binding))
          ~init:(return [])
      in
      match binded_list with
      | Ok v -> v
      | _ -> fail MatchFailure
    in
    function
    | _, VCases _ -> fail MatchFailure
    | Wild, _ -> return []
    | Const c, app_arg ->
      (match c, app_arg with
       | CBool b1, VBool b2 when Bool.equal b1 b2 -> return []
       | CInt i1, VInt i2 when i1 = i2 -> return []
       | CString s1, VString s2 when String.equal s1 s2 -> return []
       | CNil, VNone -> return []       
       | _ -> fail MatchFailure)

    | Var var, app_arg -> return [ var, app_arg ]
    | Tuple pt, VTuple vt -> bind_pat_list pt vt
    | List pl, VList vl -> bind_pat_list pl vl
    | Case (acase_id, acase_args), value_to_match ->
      let* apat = find_val env acase_id in
      (match apat with
       | VLetAPat (_, VFun (apat_arg, apat_expr, apat_env)) ->
         let* bind_matching_val = bind_fun_params ~env (apat_arg, value_to_match) in
         let* eval_res_apat =
           eval apat_expr (add_binds (add_binds empty apat_env) bind_matching_val)
         in
         (match eval_res_apat with
          | VCases (_, opt_res) ->
            (match opt_res, acase_args with
             | Some v, res :: [] -> bind_fun_params (res, v)
             | None, _ -> return []
             | _ -> fail MatchFailure)
          | _ -> fail MatchFailure)
       | _ -> fail Unreachable)
    | _ -> fail MatchFailure

  and eval expr env =
    match expr with
    | ConstExpr v -> return @@ inter_const v
    | BinExpr (op, l, r) -> inter_binary op l r env
    | VarExpr id -> find_val env id
    | ListExpr l -> inter_list l env
    | TupleExpr t -> inter_tuple t env
    | IfExpr (cond, e_then, e_else) -> inter_if cond e_then e_else env
    | FunExpr (pat, expr) -> return @@ VFun (pat, expr, Map.to_alist env)
    | AppExpr (func, arg) -> inter_app func arg env
    | LetExpr (bool, name, body) -> inter_let bool name body env
    | MatchExpr (expr_match, cases) -> inter_match expr_match cases env
    | CaseExpr (constr_id, args) -> inter_case constr_id args env

  and inter_const = function
    | CBool b -> VBool b
    | CInt i -> VInt i
    | CString s -> VString s
    | CNil -> VNone

  and inter_list l env =
    let* eval_list = all (List.map l ~f:(fun expr -> eval expr env)) in
    return @@ VList eval_list

  and inter_tuple t env =
    let* eval_list = all (List.map t ~f:(fun expr -> eval expr env)) in
    return @@ VTuple eval_list

  and inter_binary op l r env =
    let* rigth_val = eval r env in
    let* left_val = eval l env in
    match op, left_val, rigth_val with
    | Div, VInt _, VInt 0 -> fail Division_by_zero
    | Mod, VInt _, VInt 0 -> fail Division_by_zero
    | Add, VInt l, VInt r -> return @@ VInt (l + r)
    | Sub, VInt l, VInt r -> return @@ VInt (l - r)
    | Mul, VInt l, VInt r -> return @@ VInt (l * r)
    | Div, VInt l, VInt r -> return @@ VInt (l / r)
    | Mod, VInt l, VInt r -> return @@ VInt (l % r)
    | Less, VInt l, VInt r -> return @@ VBool (l < r)
    | LEq, VInt l, VInt r -> return @@ VBool (l <= r)
    | Gre, VInt l, VInt r -> return @@ VBool (l > r)
    | GEq, VInt l, VInt r -> return @@ VBool (l >= r)
    | Eq, VInt l, VInt r -> return @@ VBool (l = r)
    | NEq, VInt l, VInt r -> return @@ VBool (l <> r)
    | _ -> fail TypeError

  and inter_if cond e_then e_else env =
    let* cond_branch = eval cond env in
    match cond_branch with
    | VBool b ->
      let e = if b then e_then else e_else in
      eval e env
    | _ -> fail TypeError

  and inter_let bool name body env =
    if bool
    then
      if List.length name == 1
      then
        let* func_body = eval body env in
        return @@ VLetWAPat (List.hd_exn name, func_body)
      else fail Unreachable
    else if List.length name == 1
    then eval body env
    else
      let* fun_pat = eval body env in
      return @@ VLetAPat (name, fun_pat)

  and inter_app func arg env =
    let* fun_to_apply = eval func env in
    match fun_to_apply with
    | VFun (pat, expr, fun_env) ->
      let* arg_to_apply = eval arg env in
      let* res = bind_fun_params ~env (pat, arg_to_apply) in
      eval expr (add_binds (add_binds empty fun_env) res)
    | VLetWAPat (name, VFun (pat, expr, fun_env)) ->
      let* arg_to_apply = eval arg env in
      let* res = bind_fun_params ~env (pat, arg_to_apply) in
      eval
        expr
        (add_binds
           (add_bind
              (add_binds empty fun_env)
              name
              (VLetWAPat (name, VFun (pat, expr, fun_env))))
           res)
    | _ -> fail TypeError

  and inter_match expr_match cases env =
    let* val_match = eval expr_match env in
    let rec eval_match = function
      | (pat, expr) :: cases ->
        run
          (bind_fun_params ~env (pat, val_match))
          ~ok:(fun binds -> eval expr (add_binds env binds))
          ~err:(fun _ -> eval_match cases)
      | [] -> fail NotImplemented
    in
    eval_match cases

  and inter_case constr_id args env =
    match constr_id, args with
    | "Some", arg :: [] ->
      let* opt_val = eval arg env in
      return @@ opt_val
    | "None", [] -> return VNone
    | _, [] -> return @@ VCases (constr_id, None)
    | _, arg :: [] ->
      let* arg_val = eval arg env in
      return @@ VCases (constr_id, Some arg_val)
    | _ -> fail TypeError
  ;;

  let eval_program (program : expr list) : (value, error) t =
    let rec helper env = function
      | h :: [] -> eval h env
      | [] -> fail EmptyProgram
      | h :: tl ->
        let* eval_h = eval h env in
        let eval_env =
          let rec eval_env_apat env = function
            | [] -> env
            | h :: tl -> add_bind (eval_env_apat env tl) h eval_h
          in
          match h with
          | LetExpr (_, f, _) when not @@ String.equal (List.hd_exn f) "_" ->
            add_bind env (List.hd_exn f) eval_h
          | LetExpr (_, fl, _) -> eval_env_apat env fl
          | _ -> env
        in
        helper eval_env tl
    in
    helper empty program
  ;;
end

module InterpretResult = Interpret (struct
    include Result

    let run x ~ok ~err =
      match x with
      | Ok v -> ok v
      | Error e -> err e
    ;;

    let ( let* ) monad f = bind monad ~f
  end)

let eval_program = InterpretResult.eval_program

let run input =
  match Parser.main_parse input with
  | Ok ast ->
    (match eval_program ast with
     | Ok res -> pp_value Format.std_formatter res
     | Error e -> Format.printf "(Error while interpreting): %a" pp_error e)
  | Error e -> Format.printf "(Error while parsing): %s" e
;;

(* TESTS INTERPRET *)

(* cram тесты добавлю немного позже *)

(*  1 + 2 + 3 + 4 * 5 + 6 / 3 + (10 - 5) / 5 *)
let test =
  [ BinExpr
      ( Add
      , BinExpr
          ( Add
          , BinExpr
              ( Add
              , BinExpr
                  ( Add
                  , BinExpr (Add, ConstExpr (CInt 1), ConstExpr (CInt 2))
                  , ConstExpr (CInt 3) )
              , BinExpr (Mul, ConstExpr (CInt 4), ConstExpr (CInt 5)) )
          , BinExpr (Div, ConstExpr (CInt 6), ConstExpr (CInt 3)) )
      , BinExpr
          (Div, BinExpr (Sub, ConstExpr (CInt 10), ConstExpr (CInt 5)), ConstExpr (CInt 5))
      )
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 29) -> true
  | Error t ->
    Format.printf "%a" pp_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* 5 = 5 *)

let test = [ BinExpr (Eq, ConstExpr (CInt 5), ConstExpr (CInt 5)) ]

let%test _ =
  match eval_program test with
  | Ok (VBool true) -> true
  | Error t ->
    Format.printf "%a" pp_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(*[1;2;3]*)

let test = [ ListExpr [ ConstExpr (CInt 1); ConstExpr (CInt 2); ConstExpr (CInt 3) ] ]

let%test _ =
  match eval_program test with
  | Ok (VList [ VInt 1; VInt 2; VInt 3 ]) -> true
  | Error t ->
    Format.printf "%a\n" pp_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(*(1, [2;3;4], 5)*)

let test =
  [ TupleExpr
      [ ConstExpr (CInt 1)
      ; ListExpr [ ConstExpr (CInt 2); ConstExpr (CInt 3); ConstExpr (CInt 4) ]
      ; ConstExpr (CInt 5)
      ]
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VTuple [ VInt 1; VList [ VInt 2; VInt 3; VInt 4 ]; VInt 5 ]) -> true
  | Error t ->
    Format.printf "%a\n" pp_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(*
   let f x = x+x

   f 25
*)

let test =
  [ LetExpr (false, [ "f" ], FunExpr (Var "x", BinExpr (Add, VarExpr "x", VarExpr "x")))
  ; AppExpr (VarExpr "f", ConstExpr (CInt 25))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 50) -> true
  | Error t ->
    Format.printf "%a\n" pp_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* Varis *)

let test = [ CaseExpr ("Varis", []) ]

let%test _ =
  match eval_program test with
  | Ok (VCases ("Varis", None)) -> true
  | Error t ->
    Format.printf "%a\n" pp_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(* (fun x -> x*x) 5 *)

let test =
  [ AppExpr
      (FunExpr (Var "x", BinExpr (Mul, VarExpr "x", VarExpr "x")), ConstExpr (CInt 5))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 25) -> true
  | Error t ->
    Format.printf "%a\n" pp_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(*
   let rec fact n = if n = 1 then 1 else n * (fact (n - 1))

   fact 5
*)

let test =
  [ LetExpr
      ( true
      , [ "fact" ]
      , FunExpr
          ( Var "n"
          , IfExpr
              ( BinExpr (Eq, VarExpr "n", ConstExpr (CInt 1))
              , ConstExpr (CInt 1)
              , BinExpr
                  ( Mul
                  , VarExpr "n"
                  , AppExpr
                      (VarExpr "fact", BinExpr (Sub, VarExpr "n", ConstExpr (CInt 1))) )
              ) ) )
  ; AppExpr (VarExpr "fact", ConstExpr (CInt 5))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 120) -> true
  | Error t ->
    Format.printf "%a" pp_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(*
   let x = 5

   (fun y -> y*y) x
*)

let test =
  [ LetExpr (false, [ "x" ], ConstExpr (CInt 5))
  ; AppExpr (FunExpr (Var "y", BinExpr (Mul, VarExpr "y", VarExpr "y")), VarExpr "x")
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 25) -> true
  | Error t ->
    Format.printf "%a" pp_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(*
   let check input = match input with | 2 -> 5 | 5 -> 10

   check 5
*)

let test =
  [ LetExpr
      ( false
      , [ "check" ]
      , FunExpr
          ( Var "input"
          , MatchExpr
              ( VarExpr "input"
              , [ Const (CInt 2), ConstExpr (CInt 5)
                ; Const (CInt 5), ConstExpr (CInt 10)
                ] ) ) )
  ; AppExpr (VarExpr "check", ConstExpr (CInt 5))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 10) -> true
  | Error t ->
    Format.printf "%a" pp_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(*
   let (|Even|Odd|) v = if v % 2 = 0 then Even else Odd

   let myfunc v = match v with | Even -> 1 | Odd -> 5

   myfunc 5
*)

let test =
  [ LetExpr
      ( false
      , [ "Even"; "Odd" ]
      , FunExpr
          ( Var "input"
          , IfExpr
              ( BinExpr
                  ( Eq
                  , BinExpr (Mod, VarExpr "input", ConstExpr (CInt 2))
                  , ConstExpr (CInt 0) )
              , CaseExpr ("Even", [])
              , CaseExpr ("Odd", []) ) ) )
  ; LetExpr
      ( false
      , [ "myfunc" ]
      , FunExpr
          ( Var "v"
          , MatchExpr
              ( VarExpr "v"
              , [ Case ("Even", []), ConstExpr (CInt 1)
                ; Case ("Odd", []), ConstExpr (CInt 5)
                ] ) ) )
  ; AppExpr (VarExpr "myfunc", ConstExpr (CInt 6))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 1) -> true
  | Error t ->
    Format.printf "%a\n" pp_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;

(*
   let (|Even|) v = if v % 2 = 0 then Even else 0

   let good input = match input with | Even -> 5 | _ -> 7

   good 6
*)

let test =
  [ LetExpr
      ( false
      , [ "Even" ]
      , FunExpr
          ( Var "v"
          , IfExpr
              ( BinExpr
                  (Eq, BinExpr (Mod, VarExpr "v", ConstExpr (CInt 2)), ConstExpr (CInt 0))
              , CaseExpr ("Even", [])
              , ConstExpr (CInt 0) ) ) )
  ; LetExpr
      ( false
      , [ "good" ]
      , FunExpr
          ( Var "input"
          , MatchExpr
              ( VarExpr "input"
              , [ Case ("Even", []), ConstExpr (CInt 5); Wild, ConstExpr (CInt 7) ] ) ) )
  ; AppExpr (VarExpr "good", ConstExpr (CInt 103))
  ]
;;

let%test _ =
  match eval_program test with
  | Ok (VInt 7) -> true
  | Error t ->
    Format.printf "%a\n" pp_error t;
    false
  | Ok t ->
    Format.printf "%s" (show_value t);
    false
;;
