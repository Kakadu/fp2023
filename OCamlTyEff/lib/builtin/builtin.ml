(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base

open Misc
open Types
open Helpers
open Eval

open EvalMonad.Let
open EvalMonad.Let_syntax
open EvalMonad.Eval

module TyEnv = Types.Env
module EvalEnv = Eval.Env

let prelude = Prelude.prelude

(* ======= Arithmetics ======= *)
let b_add = make_bin_fun_int ( + )
let b_sub = make_bin_fun_int ( - )
let b_mul = make_bin_fun_int ( * )

let b_div =
  make_2args_fun (fun x y ->
      let* x = extract_int x in
      let* y = extract_int y in
      if y = 0 then fail @@ EvalError.exc "Division_by_zero"
      else return @@ Val.Val_const (Const_integer (x / y)) )

let b_mod =
  make_2args_fun (fun x y ->
      let* x = extract_int x in
      let* y = extract_int y in
      if y = 0 then fail @@ EvalError.exc "Division_by_zero"
      else return @@ Val.Val_const (Const_integer (x mod y)) )

let b_minus x =
  let* x = extract_int x in
  return @@ Val.Val_const (Const_integer (-x))

let b_int_lt = make_bin_fun_bool ( < )
let b_int_le = make_bin_fun_bool ( <= )
let b_int_gt = make_bin_fun_bool ( > )
let b_int_ge = make_bin_fun_bool ( >= )

(* ======= IO ======= *)
let b_print_string value =
  let* str = extract_string value in
  let* printer = get_printer in
  let () = printer str in
  return Val.unit

let b_print_endline value =
  let* str = extract_string value in
  let* printer = get_printer in
  let () = printer @@ Format.sprintf "%s\n" str in
  return Val.unit

let b_print_int value =
  let* num = extract_int value in
  let* printer = get_printer in
  let () = printer @@ Format.sprintf "%i" num in
  return Val.unit

(* ======= Conversions ======= *)
let b_char_of_int value =
  let* int = extract_int value in
  match Char.of_int int with
  | Some ch ->
      return @@ Val.Val_const (Const_char ch)
  | None ->
      fail @@ EvalError.exc "Invalid_argument"

let b_int_to_string value =
  let* int = extract_int value in
  return @@ Val.Val_const (Const_string (Int.to_string int))

let b_string_of_char value =
  let* ch = extract_char value in
  return @@ Val.Val_const (Const_string (String.of_char ch))

let b_int_of_string value =
  let* str = extract_string value in
  try return @@ Val.Val_const (Const_integer (Int.of_string str))
  with Failure _ -> fail @@ EvalError.exc "Invalid_argument"

let b_string_to_list value =
  let list arg1 arg2 =
    Val.Val_con (Ident "::", Some (Val_tuple [arg1; arg2]))
  in
  let empty_list = Val.Val_con (Ident "[]", None) in
  let* str = extract_string value in
  return
  @@ List.fold_right (String.to_list str) ~init:empty_list ~f:(fun ch acc ->
         list (Val_const (Const_char ch)) acc )

(* ======= Misc =======  *)
let b_eq =
  let rec helper (x : Val.t) (y : Val.t) =
    match (x, y) with
    | Val_const x, Val_const y ->
        Ast.equal_constant x y
    | Val_con (id1, Some arg1), Val_con (id2, Some arg2)
      when Ident.equal id1 id2 ->
        helper arg1 arg2
    | Val_con (id1, None), Val_con (id2, None) ->
        Ident.equal id1 id2
    | Val_tuple vals1, Val_tuple vals2 -> (
      match List.for_all2 vals1 vals2 ~f:helper with
      | Ok res ->
          res
      | Unequal_lengths ->
          false )
    | Val_ref link1, Val_ref link2 ->
        Env.Link.equal link1 link2
    | _ ->
        false
  in

  make_2args_fun (fun x y ->
      return @@ if helper x y then Val.bool_true else Val.bool_false )

let b_string_compare =
  make_2args_fun (fun x y ->
      let* x = extract_string x in
      let* y = extract_string y in
      return @@ Val.Val_const (Const_integer (String.compare x y)) )

let b_string_join =
  make_2args_fun (fun x y ->
      let* x = extract_string x in
      let* y = extract_string y in
      return @@ Val.Val_const (Const_string (x ^ y)) )

let b_char_code value =
  let* ch = extract_char value in
  return @@ Val.Val_const (Const_integer (Char.to_int ch))

let b_char_is_alpha value =
  let* ch = extract_char value in
  return @@ if Char.is_alpha ch then Val.bool_true else Val.bool_false

let b_char_is_whitespace value =
  let* ch = extract_char value in
  return @@ if Char.is_whitespace ch then Val.bool_true else Val.bool_false

let b_unreachable _ = fail @@ EvalError.exc "Unreachable"

let b_raise error = fail @@ Exception error

let b_ref value =
  let* env = get_env in
  let new_env, link = Env.fresh_link env value in
  let* () = set_env new_env in
  return @@ Val.Val_ref link

let b_assign =
  let f assignee value =
    match assignee with
    | Val.Val_ref link ->
        let* env = get_env in
        let* () = set_env @@ Env.assign env link value in
        return Val.unit
    | _ ->
        fail TypeError
  in
  make_2args_fun f

let b_deref = function
  | Val.Val_ref link ->
      let* env = get_env in
      Env.deref env link |> Option.value_map ~f:return ~default:(fail TypeError)
  | _ ->
      fail TypeError

(* ======= Environments ======= *)
let sc_arith = sc "int -> int -> int"
let sc_int_unary = sc "int -> int"
let sc_int_cmp = sc "int -> int -> bool"

let div_by_zero = Ty.Ty_con (Ident "_Division_by_zero", [])
let invalid_arg = Ty.Ty_con (Ident "_Invalid_argument", [])

let eff_console = eff @@ Eff.Label.console ()
let eff_ref = eff @@ Eff.Label.ref ()

let env_functions : (Ident.t * Scheme.t * builtin) list =
  [ (id "+", sc_arith, b_add)
  ; (id "-", sc_arith, b_sub)
  ; (id "*", sc_arith, b_mul)
  ; ( id "/"
    , no_vars @@ (Ty.int, Eff_total)
      @> (Ty.int, eff @@ Eff.Label.exn div_by_zero)
      @> Ty.int
    , b_div )
  ; ( id "mod"
    , no_vars @@ (Ty.int, Eff_total)
      @> (Ty.int, eff @@ Eff.Label.exn div_by_zero)
      @> Ty.int
    , b_mod )
  ; (id "~-", sc_int_unary, b_minus)
  ; (id "~+", sc_int_unary, return)
  ; (id "<", sc_int_cmp, b_int_lt)
  ; (id ">", sc_int_cmp, b_int_gt)
  ; (id ">=", sc_int_cmp, b_int_ge)
  ; (id "<=", sc_int_cmp, b_int_le)
  ; (id "=", sc "'a -> 'a -> bool", b_eq)
  ; (id "string_compare", sc "string -> string -> int", b_string_compare)
  ; (id "string_of_char", sc "char -> string", b_string_of_char)
  ; ( id "int_of_string"
    , no_vars @@ (Ty.string, eff @@ Eff.Label.exn invalid_arg) @> Ty.int
    , b_int_of_string )
  ; ( id "char_of_int"
    , no_vars @@ (Ty.int, eff @@ Eff.Label.exn invalid_arg) @> Ty.char
    , b_char_of_int )
  ; (id "string_to_list", sc "string -> char list", b_string_to_list)
  ; (id "int_to_string", sc "int -> string", b_int_to_string)
  ; (id "^", sc "string -> string -> string", b_string_join)
  ; (id "char_code", sc "char -> int", b_char_code)
  ; (id "char_is_alpha", sc "char -> bool", b_char_is_alpha)
  ; (id "char_is_whitespace", sc "char -> bool", b_char_is_whitespace)
  ; ( id "print_string"
    , no_vars @@ (Ty.string, eff_console) @> Ty.unit
    , b_print_string )
  ; ( id "print_endline"
    , no_vars @@ (Ty.string, eff_console) @> Ty.unit
    , b_print_endline )
  ; (id "print_int", no_vars @@ (Ty.int, eff_console) @> Ty.unit, b_print_int)
  ; (id "unreachable", sc "unit -> 'a", b_unreachable)
  ; ( id "raise"
    , Forall
        ( VarSet.of_list [Var_ty (Var "a"); Var_ty (Var "b")]
        , (Ty.exn alpha, eff @@ Eff.Label.exn @@ var "a") @> var "b" )
    , b_raise )
  ; (id "ref", single_var @@ (alpha, eff_ref) @> Ty.ref alpha, b_ref)
  ; (id "!", single_var @@ (Ty.ref alpha, eff_ref) @> alpha, b_deref)
  ; ( id ":="
    , single_var @@ (Ty.ref alpha, Eff_total) @> (alpha, eff_ref) @> Ty.unit
    , b_assign ) ]

let env_constructors : (Ident.t * Scheme.t) list =
  [(id "[]", sc "'a list"); (id "::", sc "('a * 'a list) -> 'a list")]

(** Built-in types / type constructors and their arity *)
let env_types : (Ident.t * int) list =
  [ (Ident "int", 0)
  ; (Ident "char", 0)
  ; (Ident "string", 0)
  ; (Ident "list", 1)
  ; (Ident "exn", 1)
  ; (Ident "ref", 1) ]

(* ======= Create final environments ======= *)
let ty_env =
  List.fold env_constructors ~init:TyEnv.empty ~f:(fun env (id, sc) ->
      TyEnv.set env ~key:id ~data:sc )

let ty_env =
  TyEnv.set_types_arity ty_env
    (List.fold env_types ~init:(TyEnv.get_types_arity ty_env)
       ~f:(fun acc (id, arity) -> Map.set acc ~key:id ~data:arity ) )

let ty_env, eval_env =
  List.fold env_functions ~init:(ty_env, EvalEnv.empty)
    ~f:(fun (ty_env, eval_env) (id, sc, builtin) ->
      let new_ty_env = TyEnv.set ty_env ~key:id ~data:sc in
      let new_eval_bounds =
        Bounds.set
          (EvalEnv.get_bounds eval_env)
          ~key:id ~data:(Val.Val_fun (Fun_builtin builtin))
      in
      (new_ty_env, EvalEnv.set_bounds eval_env new_eval_bounds) )
