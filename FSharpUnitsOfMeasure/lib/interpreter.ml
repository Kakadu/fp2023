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

type environment = (id, value, String.comparator_witness) Map.t

let measure_list = ref []

module Interpret (M : FailMonad) = struct
  open M

  let rec find_m2_from_measure_list measure_list element =
    match measure_list with
    | [] -> []
    | (m1, m2) :: tl ->
      if String.equal m1 element then
        m2
      else
        find_m2_from_measure_list tl element
  ;;
      
  let process_l1_with_measure_list l1 measure_list =
    List.map ~f:(fun element -> if ((String.(<>) element "/") && (String.(<>) element "*")) then find_m2_from_measure_list measure_list element else [element]) l1
  ;;

  let flatten_string_lists lists =
    List.concat (List.map ~f:(fun sublist -> sublist) lists)
  ;;
  let search lst_ref element =
    let lst = !lst_ref in  (* Получаем обычный список кортежей из mutable списка *)
    let rec search_element list =
      match list with
      | [] -> false  (* Элемент не найден *)
      | (m1, _) :: tl -> 
          if (m1 == element) then true
          else search_element tl
    in
    search_element lst
  ;;

  let search_list measure_list str_list =
    let mlist = !measure_list in 
    let rec search_element mlist slist =
      match slist with 
      | [] -> true  (* Все элементы были найдены в списке *)
      | hd :: tl ->  
        match hd with
        | "/"
        | "*" -> search_element mlist tl
        | _ ->  if List.exists ~f:(fun (m1, _) -> String.(=) m1 hd) mlist then
            search_element mlist tl
          else
            false
    in 
    search_element mlist str_list
  ;;

  let replace_value_in_ref_list lst_ref old_value new_value =
    let lst = !lst_ref in
    let updated_list = List.map ~f:(fun (first, second) -> if first == old_value then (first, new_value) else (first, second)) lst in
    lst_ref := updated_list
  ;;
  
  let rec process_measure_multiple measure_list measure =
    match measure with
    | Measure_single m -> m :: measure_list
    | Measure_multiple (m1, op, m2) ->
      let op_str op =
        match op with
        | Mul -> "*"  
        | Div -> "/"
      in
      let measure_list_with_op =  (op_str op) :: process_measure_multiple measure_list m2 in
      process_measure_multiple measure_list_with_op m1
  ;;

  let rec mbase m1 measure_list =
    let m3 = flatten_string_lists @@ process_l1_with_measure_list m1 !measure_list
    in
    if Poly.(=) m3 m1
      then return @@ m3
    else mbase m3 measure_list
    ;;

  let meq m1 m2 measure_list =
    let m3 = mbase m1 measure_list
    in let m4 = mbase m2 measure_list
    in Poly.(=) m3 m4
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
    | Add, VFloatMeasure(VFloat vl, ml), VFloatMeasure(VFloat vr, mr) when (meq ml mr measure_list) -> return @@ VFloatMeasure (( VFloat(vl +. vr)), ml)
    | Sub, VFloatMeasure(VFloat nl, ml), VFloatMeasure(VFloat nr, mr) when (meq ml mr measure_list) -> return @@ VFloatMeasure (( VFloat(nl -. nr)), ml) 
    | Mul, VFloatMeasure(VFloat nl, ml), VFloatMeasure(VFloat nr, mr) when (meq ml mr measure_list) -> return @@ VFloatMeasure (( VFloat(nl *. nr)), ml)
    | Div, VFloatMeasure(VFloat nl, ml), VFloatMeasure(VFloat nr, mr) when (meq ml mr measure_list) -> return @@ VFloatMeasure (( VFloat(nl /. nr)), ml)
    | Mod, VFloatMeasure(VFloat nl, ml), VFloatMeasure(VFloat nr, mr) when (meq ml mr measure_list)-> return @@ VFloatMeasure (( VFloat(nl %. nr)), ml)
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
        | Measure_float (_, Measure_single m1), VFloatMeasure (_, m2) when ([m1] == m2) -> return []
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

  let rec eval_expr expr env vmap : (value, error) t =
    let eval_list l =
      let* list =
        List.fold l ~init:(return []) ~f:(fun l e ->
          let* l = l in
          let* comprassion = eval_expr e env [] in
          return @@ (comprassion :: l))
      in
      return (List.rev list)
    in
    match expr with
    | EMeasure m ->
      (match m with
        | Measure_init (Measure_single m1) ->
            if (search measure_list m1) 
              then measure_list := !measure_list 
            else measure_list := (m1, [m1]) :: !measure_list;  (* Добавляем новую пару в список measure *)
            return @@ VMeasureList !measure_list  (* Возвращаем обновленный список VMeasureList *)
        | Measure_multiple_init (Measure_single m1, m2) -> 
            let m2  = process_measure_multiple [] m2 
            in
            (match (search measure_list m1, search_list measure_list m2) with
            | (true, true) -> replace_value_in_ref_list measure_list m1 m2; return @@ VMeasureList !measure_list
            | (_, true) -> measure_list := (m1, m2) :: !measure_list; return @@ VMeasureList !measure_list
            | _ -> fail Unreachable)
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
        | Measure_float (f, Measure_single m) ->
          if (search measure_list m) 
            then 
              let* f = eval_expr (EConst f) env []
              in
              return @@ VFloatMeasure (f, [m]) 
          else fail (UndefinedType m)
        | Measure_float (f, m) ->
          let m  = process_measure_multiple [] m in
          if (search_list measure_list m) 
            then 
              let* f = eval_expr (EConst f) env []
              in
              return @@ VFloatMeasure (f, m) 
          else fail Unreachable
        | FNil -> return VNil
        | FUnit -> return VUnit)
    | EFun (p, e) -> return @@ VFun (p, e, vmap)
    | EVar var -> 
        (match Map.find env var with
          | None -> fail (UndefinedValue  var)
          | Some value -> return value)
    | EApp (func, arg) ->
      (match func, arg with 
        | EBinaryOp op, EApp (num1, num2) -> 
            let* n1 = eval_expr num1 env [] in
            let* n2 = eval_expr num2 env [] in
            binop op n1 n2
        | _ -> 
          let* evaled_fun = eval_expr func env [] in
          let* evaled_arg = eval_expr arg env [] in
          (match evaled_fun with
            | VFun (p, e, vmap) ->
              let* pat = pattern (p, evaled_arg) in
              let* link_variable = link env vmap in
              let* link_variables = link link_variable pat in
              eval_expr e link_variables (pat @ vmap)
            | _ -> fail Unreachable))
    | EBinaryOp op -> return @@ VBinOp op
    | EIfElse (i, t, e) ->
      let* comp_eifelse = eval_expr i env [] in
      (match comp_eifelse with
        | VBool res -> if res then eval_expr t env [] else eval_expr e env []
        | _ -> fail Unreachable)
    | EMatch (matched, patterns) ->
      let* comp_match = eval_expr matched env [] in
      let rec comp_match_expr = function
        | [] -> fail UnexpectedPattern
        | (p, e) :: tl ->
          let res = pattern (p, comp_match) in
          run
            res
            ~ok:(fun res ->
              let* env = link env res in
              eval_expr e env [])
            ~err:(fun _res -> comp_match_expr tl)
      in
      comp_match_expr patterns
    | ELet (_, _, expr) -> eval_expr expr env []
    | EList abs -> 
      let* vls = eval_list abs  in
      return (VList vls)
    | ETuple els ->
      let* vls = eval_list els in
      return (VTuple vls)
  ;;

  let run_expr env expr : (environment * value, error) t =
    let* value = eval_expr expr env [] in
    match expr with
    | ELet (_, name, _) ->
      let* env = link env [ name, value ] in
      return (env, value)
    | _ -> return (env, value)
  ;;

  let run_interpreter ?(environment = Map.empty (module String)) program = 
    measure_list := [];
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