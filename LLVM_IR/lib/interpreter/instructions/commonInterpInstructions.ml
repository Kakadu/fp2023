(** Copyright 2023-2024, Efremov Alexey *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ihelp.State

type instr_launch_res =
  | Ret of Ast.const
  | Jmp of Ast.variable
  | NoRes

let to_ptr x = Ast.CPointer (Ast.PointerInt x)

let err_type exp get =
  fail (Printf.sprintf "expected %s, but get %s\n" (Ast.show_tp exp) (Ast.show_tp get))
;;

let err_type_c exp cnst = err_type exp (Ast.const_to_tp cnst)

let get_const_from_value : Ast.value -> (state, Ast.const) t = function
  | Ast.Const x -> return x
  | Ast.FromVariable (var, exp_tp) ->
    let* cnst = read_var var in
    let real_tp = Ast.const_to_tp cnst in
    if Ast.tp_equal real_tp exp_tp
    then return cnst
    else
      fail
        (Printf.sprintf
           "Variable %s is of type %s, but %s was expected"
           (Ast.show_variable var)
           (Ast.show_tp real_tp)
           (Ast.show_tp exp_tp))
;;

let is_block cnst =
  match cnst with
  | Ast.CLabel x -> return x
  | _ -> err_type_c Ast.TLabel cnst
;;

let is_bool cnst =
  match cnst with
  | Ast.CInteger (1, x) -> return (Int64.equal 1L x)
  | _ -> err_type_c (Ast.TInteger 1) cnst
;;

let is_int cnst =
  match cnst with
  | Ast.CInteger (_, x) -> return x
  | _ -> err_type_c (Ast.TInteger 0) cnst
;;

let is_float cnst =
  match cnst with
  | Ast.CFloat x -> return x
  | _ -> err_type_c Ast.TFloat cnst
;;

let rec is_ptr cnst =
  match cnst with
  | Ast.CPointer (Ast.PointerInt x) -> return x
  | Ast.CPointer (Ast.PointerGlob x) -> read_var x >>= is_ptr
  | _ -> err_type_c Ast.TPointer cnst
;;

let is_vector is_elem cnst =
  match cnst with
  | Ast.CVector x -> map_list is_elem x
  | c -> err_type_c (Ast.TVector (0, Ast.const_to_tp c)) cnst
;;

let is_aggregate is_elem cnst =
  match cnst with
  | Ast.CArr x | Ast.CStruct x -> map_list is_elem x
  | _ ->
    fail
      (Printf.sprintf
         "Expected aggregate type, but got %s\n"
         (Ast.show_tp (Ast.const_to_tp cnst)))
;;

let vectorize1 operat is_elem value =
  let* v = get_const_from_value value >>= is_vector is_elem in
  return (Ast.CVector (List.map operat v))
;;

let vectorize2 operat is_elem v1 v2 =
  let* v1 = get_const_from_value v1 >>= is_vector is_elem in
  let* v2 = get_const_from_value v2 >>= is_vector is_elem in
  return (Ast.CVector (List.map2 operat v1 v2))
;;

let launch_binary_body_operation operat is_elem x y =
  let* x = get_const_from_value x >>= is_elem in
  let* y = get_const_from_value y >>= is_elem in
  try return (operat x y) with
  | Division_by_zero -> fail "Runtime error: Division by 0"
;;

let launch_binary_body_operation_vectorizated tp operat is_elem x y =
  match tp with
  | Ast.TVector (_, elem_tp) -> vectorize2 (operat elem_tp) is_elem x y
  | _ -> launch_binary_body_operation (operat tp) is_elem x y
;;

let write_binop_res tp real_add is_int v1 v2 var =
  let* res = launch_binary_body_operation_vectorizated tp real_add is_int v1 v2 in
  write_var var res
;;

let check_to_int operat tp x y =
  match tp with
  | Ast.TInteger sz -> operat sz x y
  | _ ->
    let _ = Printf.printf "Impossible error: get wrong type at binary/biwise operation" in
    Ast.CVoid
;;

let get_bb_var_from_label = function
  | Ast.FromVariable (x, _) -> return x
  | Ast.Const _ -> fail "expected get basic block"
;;
