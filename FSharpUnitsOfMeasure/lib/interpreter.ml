(** Copyright 2021-2023, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base
open Interpret_error

module type FailMonad = sig
  include Monad.S2

  val run : ('a, 'e) t -> ok:('a -> ('b, 'e) t) -> err:('e -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

type value =
  | VInt of int
  | VString of string
  | VBool of bool
  | VNil
  | VUnit
  | VFloat of float 
  | VTuple of value list
  | VList of value list
  | VBinOp of binary_op
  | VFun of pattern * expression * (id * value) list
  | VMeasure of id
  | VMeasureList of (id * id) list
  | VFloatMeasure of value * id  
[@@deriving show { with_path = false }]

type environment = (id, value, String.comparator_witness) Map.t

module Interpret (M : FailMonad) = struct
  open M

  let measure_list = ref []

  let serch lst_ref element =
    let lst = !lst_ref in  (* Получаем обычный список кортежей из mutable списка *)
    
    let rec search_element list =
      match list with
      | [] -> false  (* Элемент не найден *)
      | (m, _) :: tl -> 
          if (m == fst element) then true
          else search_element tl
    in
    search_element lst
  ;;
  

  let link map links = 
    let linking map id value=
      match Map.add map ~key:id ~data:value with
      | `Ok map -> map
      | `Duplicate -> Map.update map id ~f:(fun  _ -> value)
    in 
    let new_map = List.fold links ~init:map ~f:(fun map (id, value) -> linking map id value)
    in return new_map
  ;;
  
  let binop op left right =
    match op, left, right with 
    (* Int to Int operation *)
    | Add, VInt l, VInt r -> return @@ VInt (l + r)
    | Sub, VInt l, VInt r -> return @@ VInt (l - r)
    | Mul, VInt l, VInt r -> return @@ VInt (l * r)
    | Div, VInt _, VInt r when r = 0 -> fail DivideByZeroException
    | Div, VInt l, VInt r -> return @@ VInt (l / r)
    | Mod, VInt _, VInt r when r = 0 -> fail DivideByZeroException
    | Mod, VInt l, VInt r -> return @@ VInt (l % r)
    (* Boolean operation *)
    | And, VBool l, VBool r -> return @@ VBool (l && r)
    | Or, VBool l, VBool r -> return @@ VBool (l || r)
    | Eq, VInt l, VInt r -> return @@ VBool (l = r)
    | Neq, VInt l, VInt r -> return @@ VBool (l <> r)
    | Less, VInt l, VInt r -> return @@ VBool (l < r)
    | Gre, VInt l, VInt r -> return @@ VBool (l > r)
    | Leq, VInt l, VInt r -> return @@ VBool (l <= r)
    | Greq, VInt l, VInt r -> return @@ VBool (l > r)
    | Eq, VFloat l, VFloat r -> return @@ VBool (Float.(=) l r)
    | Neq, VFloat l, VFloat r -> return @@ VBool (Float.(<>) l r)
    | Less, VFloat l, VFloat r -> return @@ VBool (Float.(<) l r)
    | Gre, VFloat l, VFloat r -> return @@ VBool (Float.(<=) l r)
    | Leq, VFloat l, VFloat r -> return @@ VBool (Float.(>=) l r)
    | Greq, VFloat l, VFloat r -> return @@ VBool (Float.(>) l r)
    (* Float to Float operation *)
    | Add, VFloat l, VFloat r -> return @@ VFloat (l +. r)
    | Sub, VFloat l, VFloat r -> return @@ VFloat (l -. r)
    | Mul, VFloat l, VFloat r -> return @@ VFloat (l *. r)
    | Div, VFloat l, VFloat r -> return @@ VFloat (l /. r)
    | Mod, VFloat l, VFloat r -> return @@ VFloat (l %. r)
    (* Measure to Measure operation *)
    | Add, VFloatMeasure(VFloat nl, ml), VFloatMeasure(VFloat nr, mr) when (String.(=) ml mr) -> return @@ VFloatMeasure (( VFloat(nl +. nr)), ml)
    | Sub, VFloatMeasure(VFloat nl, ml), VFloatMeasure(VFloat nr, mr) when (String.(=) ml mr) -> return @@ VFloatMeasure (( VFloat(nl -. nr)), ml) 
    | Mul, VFloatMeasure(VFloat nl, ml), VFloatMeasure(VFloat nr, mr) when (String.(=) ml mr) -> return @@ VFloatMeasure (( VFloat(nl *. nr)), ml)
    | Div, VFloatMeasure(VFloat nl, ml), VFloatMeasure(VFloat nr, mr) when (ml == mr) -> return @@ VFloatMeasure (( VFloat(nl /. nr)), ml)
    | Mod, VFloatMeasure(VFloat nl, ml), VFloatMeasure(VFloat nr, mr) when (ml == mr) -> return @@ VFloatMeasure (( VFloat(nl %. nr)), ml)
    | _ -> fail UnsupportedOperation
  ;;

  let rec pattern =
    function
    | PWild, _ -> return []
    | PVar name, value -> return [ name, value ]
    | PConst const, cvalue ->
      (match const, cvalue with
        | FBool fb, VBool vb when fb == vb -> return []
        | FInt (s, fnum), VInt vnum 
            when (
              ((s == Plus) && (fnum == vnum)) 
              || ((s == Minus) && (fnum * -1 == vnum))
              ) -> return []
        | FString fs, VString vs when fs == vs -> return []
        | FFloat (s, fnum), VFloat vnum
            when (
              ((s == Plus) && (fnum == vnum)) 
              || ((s == Minus) && (fnum *. -1. == vnum))
              ) -> return []
        | Measure_float (_, Measure_single m1), VFloatMeasure (_, m2) when (m1 == m2) -> return []
        | FUnit, VUnit -> return []
        | FNil, VList v ->
          (match v with
            | [] -> return []
            | _ -> fail UnexpectedPattern)
        | _ -> fail UnexpectedPattern)
    | (PCons _ as pl), VList vl ->
      (match pl, vl with
        | PCons (h, t), hd :: tl ->
          let* hd = pattern (h, hd) in
          let* tl = pattern (t, VList tl) in
          return @@ hd @ tl
        | _ -> fail UnexpectedPattern)
    | PTuple ptl, VTuple vtl 
    | PList ptl, VList vtl -> 
      let create_vtl =
        List.fold2 ptl vtl ~init:(return []) ~f:(fun acc p v ->
          let* comp = pattern (p, v) in
          let* acc1 = acc in
          return (comp @ acc1))
      in
      (match create_vtl with
      | Ok res -> res
      | Unequal_lengths -> fail UnexpectedPattern)
    | _ -> fail UnexpectedPattern
  ;;

  let rec comp_expr expr env vmap : (value, error) t =
    let eval_list l =
      let* list =
        List.fold l ~init:(return []) ~f:(fun l e ->
          let* l = l in
          let* comprassion = comp_expr e env [] in
          return @@ (comprassion :: l))
      in
      return (List.rev list)
    in
    match expr with
    | EMeasure m ->
      (match m with
       | Measure_init (Measure_single m1) ->
          if (serch measure_list (m1, m1)) 
            then measure_list := !measure_list 
          else measure_list := (m1, m1) :: !measure_list;  (* Добавляем новую пару в список measure *)
          return @@ VMeasureList !measure_list  (* Возвращаем обновленный список VMeasureList *)
       | _ -> fail Unreachable
      )
    | EConst c ->
      (match c with
        | FInt (s, i) ->
            (match s with
              | Plus -> return @@ VInt i
              | Minus -> return @@ VInt (-1 * i))
        | FBool b -> return @@ VBool b
        | FString s -> return @@ VString s
        | FFloat (s, f) -> return @@
          (match s with
          | Plus -> VFloat f
          | Minus -> VFloat (-1.0 *. f))
        | Measure_float (a, Measure_single b) ->
          if (serch measure_list (b, b)) 
            then 
              let* f = comp_expr (EConst a) env []
              in
              return @@ VFloatMeasure (f, b) 
          else fail DivideByZeroException
        | FNil -> return VNil
        | FUnit -> return VUnit
        | _ -> fail Unreachable)
    | EFun (p, e) -> return @@ VFun (p, e, vmap)
    | EVar var -> 
        (match Map.find env var with
          | None -> fail (NotDefinedValue var)
          | Some value -> return value)
    | EApp (func, arg) ->
      (match func, arg with 
        | EBinaryOp op, EApp (num1, num2) -> 
            let* n1 = comp_expr num1 env [] in
            let* n2 = comp_expr num2 env [] in
            binop op n1 n2
        | _ -> 
          let* evaled_fun = comp_expr func env [] in
          let* evaled_arg = comp_expr arg env [] in
          (match evaled_fun with
            | VFun (p, e, vmap) ->
              let* pat = pattern (p, evaled_arg) in
              let* link_variable = link env vmap in
              let* link_variables = link link_variable pat in
              comp_expr e link_variables (pat @ vmap)
            | _ -> fail Unreachable))
    | EBinaryOp op -> return @@ VBinOp op
    | EIfElse (i, t, e) ->
      let* comp_eifelse = comp_expr i env [] in
      (match comp_eifelse with
        | VBool res -> if res then comp_expr t env [] else comp_expr e env []
        | _ -> fail Unreachable)
    | EMatch (matched, patterns) ->
      let* comp_match = comp_expr matched env [] in
      let rec comp_match_expr = function
        | [] -> fail UnexpectedPattern
        | (p, e) :: tl ->
          let res = pattern (p, comp_match) in
          run
            res
            ~ok:(fun res ->
              let* env = link env res in
              comp_expr e env [])
            ~err:(fun _res -> comp_match_expr tl)
      in
      comp_match_expr patterns
    | ELet (_, _, expr) -> comp_expr expr env []
    | EList abs -> 
      let* vls = eval_list abs  in
      return (VList vls)
    | ETuple els ->
      let* vls = eval_list els in
      return (VTuple vls)
  ;;

  let run_expr env expr : (environment * value, error) t =
    let* value = comp_expr expr env [] in
    match expr with
    | ELet (_, name, _) ->
      let* env = link env [ name, value ] in
      return (env, value)
    | _ -> return (env, value)
  ;;

  let run_interpreter ?(environment = Map.empty (module String)) program =
    let rec helper env = function
      | [] -> fail EmptyInput
      | hd :: [] ->
        let* env, value = run_expr env hd in
        return (env, value)
      | hd :: tl ->
        let* env, _ = run_expr env hd in
        helper env tl
    in helper environment program
  ;;
end

module InterpretResult = Interpret 
(struct
  include Base.Result

  let run x ~ok ~err =
    match x with
    | Ok v -> ok v
    | Error e -> err e
  ;;

  let ( let* ) monad f = bind monad ~f
end)

let run = InterpretResult.run_interpreter