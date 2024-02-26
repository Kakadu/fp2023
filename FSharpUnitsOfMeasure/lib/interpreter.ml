(** Copyright 2023-2024, Efim Perevalov *)

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
  | VInt of int (** int *)
  | VString of string (** string*)
  | VBool of bool (** bool *)
  | VNil (** empty list: [] *)
  | VUnit (** () *)
  | VFloat of float (** float *)
  | VTuple of value list (** tuple *)
  | VList of value list (** list *)
  | VBinOp of binary_op (** binary operation *)
  | VFun of pattern * expression * (id * value) list (** fun *)
  | VMeasureList of id list (** measure list*)
  | VFloatMeasure of value * id list (** float + measure*)
[@@deriving show { with_path = false }]

type environment = (id, value, String.comparator_witness) Map.t

module Interpret (M : FailMonad) = struct
  open M

  let list_to_string s = String.concat s

  (* возвращает значение до ^ из элемента measure*)
  let remove_degree str =
    match String.index str '^' with
    | Some i -> String.sub ~pos:0 ~len:i str
    | None -> str
  ;;

  (* Возвращает значение после ^ из элемента measure*)
  let return_degree str =
    match String.index str '^' with
    | Some caret_index ->
      String.sub ~pos:(caret_index + 1) ~len:(String.length str - caret_index - 1) str
    | None -> ""
  ;;

  (* Создаем string list из какого-то measure *)
  let rec measure_to_strlist list = function
    | SMeasure (m, p) ->
      (match p with
       | Pow (FInt 1) -> m :: list
       | Pow (FInt n) -> (m ^ "^" ^ Int.to_string n) :: list
       | _ -> [])
    | MMeasure (m1, op, m2) ->
      let op_str = function
        | Mul -> "*"
        | Div -> "/"
        | _ -> ""
      in
      let measure_list_with_op = op_str op :: measure_to_strlist list m2 in
      measure_to_strlist measure_list_with_op m1
  ;;

  (* Принимает список списков строк и объединяет их в один список строк *)
  let strlistlist_to_strlist slist =
    List.concat (List.map ~f:(fun sublist -> sublist) slist)
  ;;

  (* Декомпозирует тип element *)
  let dec_element element env =
    let add_degree list pow =
      List.map
        ~f:(fun elem ->
          if String.( = ) elem "*" || String.( = ) elem "/"
          then elem
          else elem ^ "^" ^ pow)
        list
    in
    match Map.find env ("<" ^ remove_degree element ^ ">") with
    | None -> []
    | Some (VMeasureList value) ->
      let n = return_degree element in
      if String.( = ) n "" then value else add_degree value n
    | _ -> []
  ;;

  (* Декомпозирует все значения из списка *)
  let dec_list list env =
    List.map
      ~f:(fun element ->
        if String.( <> ) element "/" && String.( <> ) element "*"
        then dec_element element env
        else [ element ])
      list
  ;;

  (* Декомпозирует список до самого основания*)
  let rec mbase m1 env =
    let m3 = strlistlist_to_strlist @@ dec_list m1 env in
    if Poly.( = ) m3 m1 then return m3 else mbase m3 env
  ;;

  (* Сравнивает, равны ли списки декомпозированные до основания*)
  let meq m1 m2 env =
    let m3 = mbase m1 env in
    let m4 = mbase m2 env in
    Poly.( = ) m3 m4
  ;;

  (* Произведение VFloatMeasure *)

  let rec m1_el_with_m2 z1 elem = function
    | [] -> z1 :: [ elem ]
    | _ :: [] -> []
    | z2 :: hd :: tl ->
      let p1 = return_degree elem in
      let p2 = return_degree hd in
      let hd_eq_elem = String.( = ) (remove_degree hd) (remove_degree elem) in
      let z1_eq_z2 = if String.( = ) z1 z2 then z1 else "" in
      let p1_empty = String.( = ) p1 "" in
      let p2_empty = String.( = ) p2 "" in
      if hd_eq_elem
      then (
        match z2, String.( = ) z1 z1_eq_z2, p1_empty, p2_empty with
        | "/", true, true, true | "*", true, true, true ->
          z1 :: [ remove_degree elem ^ "^2" ]
        | "/", false, true, true | "*", false, true, true ->
          z1 :: [ remove_degree elem ^ "^0" ]
        | "/", true, false, true | "*", true, false, true ->
          z1 :: [ remove_degree elem ^ "^" ^ Int.to_string (Int.of_string p1 + 1) ]
        | "/", false, false, true | "*", false, false, true ->
          z1 :: [ remove_degree elem ^ "^" ^ Int.to_string (Int.of_string p1 - 1) ]
        | "/", true, true, false | "*", true, true, false ->
          z1 :: [ remove_degree elem ^ "^" ^ Int.to_string (Int.of_string p2 + 1) ]
        | "/", false, true, false ->
          z1 :: [ remove_degree elem ^ "^" ^ Int.to_string (1 - Int.of_string p2) ]
        | "*", false, true, false ->
          z2 :: [ remove_degree elem ^ "^" ^ Int.to_string (Int.of_string p2 - 1) ]
        | "/", true, false, false | "*", true, false, false ->
          z1
          :: [ remove_degree elem
               ^ "^"
               ^ Int.to_string (Int.of_string p1 + Int.of_string p2)
             ]
        | "/", false, false, false | "*", false, false, false ->
          z1
          :: [ remove_degree elem
               ^ "^"
               ^ Int.to_string (Int.of_string p1 - Int.of_string p2)
             ]
        | _ -> m1_el_with_m2 z1 elem tl)
      else m1_el_with_m2 z1 elem tl
  ;;

  let rec mult_m1_with_m2 m1 m2 =
    match m1 with
    | [] -> []
    | _ :: [] -> []
    | z :: hd :: tl -> m1_el_with_m2 z hd m2 :: mult_m1_with_m2 tl m2
  ;;

  let rec not_uniq_in_m1 elem = function
    | [] -> ""
    | hd :: tl ->
      if String.( = ) (remove_degree hd) (remove_degree elem)
      then elem
      else not_uniq_in_m1 elem tl
  ;;

  let mult_m2_with_m1 m1 = function
    | [] -> m1
    | hd :: tl ->
      let rec merge_uniq_el m1 tl =
        match tl with
        | [] -> m1
        | hd :: [] ->
          if String.( <> ) (not_uniq_in_m1 hd m1) ""
          then merge_uniq_el m1 []
          else hd :: "*" :: m1
        | hd :: bo :: tl ->
          if String.( <> ) (not_uniq_in_m1 bo m1) ""
          then merge_uniq_el m1 tl
          else merge_uniq_el (m1 @ [ hd ] @ [ bo ]) tl
      in
      if String.( <> ) (not_uniq_in_m1 hd m1) ""
      then merge_uniq_el m1 tl
      else merge_uniq_el m1 tl @ [ "*" ] @ [ hd ]
  ;;

  let remove_first_element = function
    | [] -> []
    | _ :: tl -> tl
  ;;

  let rec remove_excess_pow = function
    | [] -> []
    | hd :: [] -> [ hd ]
    | z :: hd :: tl ->
      (match return_degree hd with
       | "0" -> remove_excess_pow tl
       | "1" -> z :: remove_degree hd :: remove_excess_pow tl
       | _ -> z :: hd :: remove_excess_pow tl)
  ;;

  let multiplication_measure m1 m2 =
    remove_first_element
    @@ remove_excess_pow
    @@ mult_m2_with_m1
         (strlistlist_to_strlist (mult_m1_with_m2 ("*" :: m1) ("*" :: m2)))
         m2
  ;;

  let process_list lst =
    List.map
      ~f:(fun hd ->
        match hd with
        | "*" -> "/"
        | "/" -> "*"
        | _ -> hd)
      lst
  ;;

  let division_measure m1 m2 =
    remove_first_element
    @@ remove_excess_pow
    @@ mult_m2_with_m1
         (strlistlist_to_strlist (mult_m1_with_m2 ("*" :: m1) (process_list ("*" :: m2))))
         m2
  ;;

  let eval_list expr env l =
    let* list =
      List.fold l ~init:(return []) ~f:(fun l e ->
        let* l = l in
        let* eval = expr e env [] in
        return (eval :: l))
    in
    return (List.rev list)
  ;;

  let link map links =
    let linking map id value =
      match Map.add map ~key:id ~data:value with
      | `Ok map -> map
      | `Duplicate -> Map.update map id ~f:(fun _ -> value)
    in
    let new_map =
      List.fold links ~init:map ~f:(fun map (id, value) -> linking map id value)
    in
    return new_map
  ;;

  let rec check list env =
    match list with
    | hd :: tl ->
      if String.( = ) hd "*" || String.( = ) hd "/"
      then check tl env
      else (
        match Map.find env ("<" ^ remove_degree hd ^ ">") with
        | None -> false
        | Some _ -> check tl env)
    | [] -> true
  ;;

  let rec undefined list env =
    match list with
    | hd :: tl ->
      if String.( = ) hd "*" || String.( = ) hd "/"
      then undefined tl env
      else (
        match Map.find env ("<" ^ remove_degree hd ^ ">") with
        | None -> hd
        | Some _ -> undefined tl env)
    | [] -> ""
  ;;

  let binop op left right env =
    match op, left, right with
    (* Int to Int operation *)
    | Add, VInt l, VInt r -> return @@ VInt (l + r)
    | Sub, VInt l, VInt r -> return @@ VInt (l - r)
    | Mul, VInt l, VInt r -> return @@ VInt (l * r)
    | Div, VInt _, VInt r when r = 0 -> fail DivideByZeroException
    | Div, VInt l, VInt r -> return @@ VInt (l / r)
    | Mod, VInt _, VInt r when r = 0 -> fail DivideByZeroException
    | Mod, VInt l, VInt r -> return @@ VInt (l % r)
    (* Float to Float operation *)
    | Add, VFloat l, VFloat r -> return @@ VFloat (l +. r)
    | Sub, VFloat l, VFloat r -> return @@ VFloat (l -. r)
    | Mul, VFloat l, VFloat r -> return @@ VFloat (l *. r)
    | Div, VFloat l, VFloat r -> return @@ VFloat (l /. r)
    | Mod, VFloat l, VFloat r -> return @@ VFloat (l %. r)
    (* Measure to Measure operation *)
    | Add, VFloatMeasure (VFloat fl, ml), VFloatMeasure (VFloat fr, mr) ->
      if meq ml mr env
      then return @@ VFloatMeasure (VFloat (fl +. fr), ml)
      else fail @@ DifferentMeasure (list_to_string ml, list_to_string mr)
    | Sub, VFloatMeasure (VFloat fl, ml), VFloatMeasure (VFloat fr, mr) ->
      if meq ml mr env
      then return @@ VFloatMeasure (VFloat (fl -. fr), ml)
      else fail @@ DifferentMeasure (list_to_string ml, list_to_string mr)
    | Mul, VFloatMeasure (VFloat fl, ml), VFloat r ->
      return @@ VFloatMeasure (VFloat (fl *. r), ml)
    | Mul, VFloat l, VFloatMeasure (VFloat fr, mr) ->
      return @@ VFloatMeasure (VFloat (l *. fr), mr)
    | Mul, VFloatMeasure (VFloat fl, ml), VFloatMeasure (VFloat fr, mr) ->
      let multm = multiplication_measure ml mr in
      (match multm with
       | [] -> return @@ VFloat (fl *. fr)
       | _ -> return @@ VFloatMeasure (VFloat (fl *. fr), multm))
    | Div, VFloatMeasure (VFloat fl, ml), VFloat r ->
      return @@ VFloatMeasure (VFloat (fl /. r), ml)
    | Div, VFloat l, VFloatMeasure (VFloat fr, mr) ->
      return @@ VFloatMeasure (VFloat (l /. fr), mr)
    | Div, VFloatMeasure (VFloat fl, ml), VFloatMeasure (VFloat fr, mr) ->
      let divm = division_measure ml mr in
      (match divm with
       | [] -> return @@ VFloat (fl /. fr)
       | _ -> return @@ VFloatMeasure (VFloat (fl /. fr), divm))
    (* Boolean operation *)
    | And, VBool l, VBool r -> return @@ VBool (l && r)
    | Or, VBool l, VBool r -> return @@ VBool (l || r)
    (* Boolean operation to int*)
    | Eq, VInt l, VInt r -> return @@ VBool (l = r)
    | Neq, VInt l, VInt r -> return @@ VBool (l <> r)
    | Less, VInt l, VInt r -> return @@ VBool (l < r)
    | Gre, VInt l, VInt r -> return @@ VBool (l > r)
    | Leq, VInt l, VInt r -> return @@ VBool (l <= r)
    | Greq, VInt l, VInt r -> return @@ VBool (l > r)
    (* Boolean operation to float*)
    | Eq, VFloat l, VFloat r -> return @@ VBool (Float.( = ) l r)
    | Neq, VFloat l, VFloat r -> return @@ VBool (Float.( <> ) l r)
    | Less, VFloat l, VFloat r -> return @@ VBool (Float.( < ) l r)
    | Gre, VFloat l, VFloat r -> return @@ VBool (Float.( > ) l r)
    | Leq, VFloat l, VFloat r -> return @@ VBool (Float.( <= ) l r)
    | Greq, VFloat l, VFloat r -> return @@ VBool (Float.( >= ) l r)
    (* Boolean operation to string*)
    | Eq, VString l, VString r -> return @@ VBool (String.( = ) l r)
    | Neq, VString l, VString r -> return @@ VBool (String.( <> ) l r)
    | Less, VString l, VString r -> return @@ VBool (String.( < ) l r)
    | Gre, VString l, VString r -> return @@ VBool (String.( > ) l r)
    | Leq, VString l, VString r -> return @@ VBool (String.( <= ) l r)
    | Greq, VString l, VString r -> return @@ VBool (String.( >= ) l r)
    (* Boolean operation to tuple*)
    | Eq, VTuple l, VTuple r -> return @@ VBool (Poly.( = ) l r)
    | Neq, VTuple l, VTuple r -> return @@ VBool (Poly.( <> ) l r)
    | Less, VTuple l, VTuple r -> return @@ VBool (Poly.( < ) l r)
    | Gre, VTuple l, VTuple r -> return @@ VBool (Poly.( > ) l r)
    | Leq, VTuple l, VTuple r -> return @@ VBool (Poly.( <= ) l r)
    | Greq, VTuple l, VTuple r -> return @@ VBool (Poly.( >= ) l r)
    (* Boolean operation to list*)
    | Eq, VList l, VList r -> return @@ VBool (Poly.( = ) l r)
    | Neq, VList l, VList r -> return @@ VBool (Poly.( <> ) l r)
    | Less, VList l, VList r -> return @@ VBool (Poly.( < ) l r)
    | Gre, VList l, VList r -> return @@ VBool (Poly.( > ) l r)
    | Leq, VList l, VList r -> return @@ VBool (Poly.( <= ) l r)
    | Greq, VList l, VList r -> return @@ VBool (Poly.( >= ) l r)
    | _ -> fail UnsupportedOperation
  ;;

  let rec pattern = function
    | PWild, _ -> return []
    | PVar name, value -> return [ name, value ]
    | PConst const, cvalue ->
      (match const, cvalue with
       | FBool fb, VBool vb when Bool.( = ) fb vb -> return []
       | FInt fnum, VInt vnum when fnum = vnum -> return []
       | FString fs, VString vs when String.( = ) fs vs -> return []
       | FFloat fnum, VFloat vnum when Float.( = ) fnum vnum -> return []
       | Measure_float (_, SMeasure (m1, p)), VFloatMeasure (_, m2) ->
         (match p with
          | Pow (FInt n) when Poly.( = ) [ m1 ^ "^" ^ Int.to_string n ] m2 -> return []
          | _ -> fail UnexpectedPattern)
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
    | PTuple ptl, VTuple vtl | PList ptl, VList vtl ->
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
    match expr with
    | EMeasure m ->
      (match m with
       | SMeasure_init (SMeasure (m1, p)) ->
         (match p with
          | Pow (FInt 1) -> return @@ VMeasureList [ m1 ]
          | _ -> fail UnexpectedInfix)
       | MMeasure_init (SMeasure (_, p), m2) ->
         (match p with
          | Pow (FInt 1) ->
            let m2 = measure_to_strlist [] m2 in
            if check m2 env
            then return @@ VMeasureList m2
            else fail (UndefinedType (undefined m2 env))
          | _ -> fail UnexpectedInfix)
       | _ -> fail Unreachable)
    | EConst c ->
      (match c with
       | FInt i -> return @@ VInt i
       | FBool b -> return @@ VBool b
       | FString s -> return @@ VString s
       | FFloat f -> return @@ VFloat f
       | FNil -> return VNil
       | FUnit -> return VUnit
       | Measure_float (f, SMeasure (m, Pow (FInt p))) ->
         (match Map.find env ("<" ^ m ^ ">") with
          | None -> fail (UndefinedValue m)
          | Some _ ->
            if p = 1
            then
              let* f = eval_expr (EConst f) env [] in
              return @@ VFloatMeasure (f, [ m ])
            else
              let* f = eval_expr (EConst f) env [] in
              return @@ VFloatMeasure (f, [ m ^ "^" ^ Int.to_string p ]))
       | Measure_float (f, m) ->
         let m = measure_to_strlist [] m in
         if check m env
         then
           let* f = eval_expr (EConst f) env [] in
           return @@ VFloatMeasure (f, m)
         else fail (UndefinedType (list_to_string m)))
    | EFun (p, e) -> return @@ VFun (p, e, vmap)
    | EVar var ->
      (match Map.find env var with
       | None -> fail (UndefinedValue var)
       | Some value -> return value)
    | EApp (func, arg) ->
      (match func, arg with
       | EBinaryOp op, EApp (num1, num2) ->
         let* n1 = eval_expr num1 env [] in
         let* n2 = eval_expr num2 env [] in
         binop op n1 n2 env
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
      let* eval_eifelse = eval_expr i env [] in
      (match eval_eifelse with
       | VBool res -> if res then eval_expr t env [] else eval_expr e env []
       | _ -> fail Unreachable)
    | EMatch (matc, patterns) ->
      let* eval_match = eval_expr matc env [] in
      let rec eval_matched = function
        | [] -> fail UnexpectedPattern
        | (p, e) :: tl ->
          run
            (pattern (p, eval_match))
            ~ok:(fun res ->
              let* env = link env res in
              eval_expr e env [])
            ~err:(fun _ -> eval_matched tl)
      in
      eval_matched patterns
    | ELet (_, _, expr) -> eval_expr expr env []
    | EList l ->
      let* vl = eval_list eval_expr env l in
      return (VList vl)
    | ETuple t ->
      let* vt = eval_list eval_expr env t in
      return (VTuple vt)
  ;;

  let exp env expr : (environment * value, error) t =
    let* value = eval_expr expr env [] in
    match expr with
    | ELet (_, name, _) ->
      let* env = link env [ name, value ] in
      return (env, value)
    | EMeasure (SMeasure_init (SMeasure (m, _))) ->
      let* env = link env [ "<" ^ m ^ ">", value ] in
      return (env, value)
    | EMeasure (MMeasure_init (SMeasure (m1, _), _m2)) ->
      let* env = link env [ "<" ^ m1 ^ ">", value ] in
      return (env, value)
    | _ -> return (env, value)
  ;;

  let interpreter ?(environment = Map.empty (module String)) program =
    let rec helper env = function
      | [] -> fail Unreachable
      | hd :: [] ->
        let* env, value = exp env hd in
        return (env, value)
      | hd :: tl ->
        let* env, _ = exp env hd in
        helper env tl
    in
    helper environment program
  ;;
end

module Interpreter = Interpret (struct
    include Result

    let run x ~ok ~err =
      match x with
      | Ok v -> ok v
      | Error e -> err e
    ;;

    let ( let* ) monad f = bind monad ~f
  end)

let interpreter = Interpreter.interpreter
