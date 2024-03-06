(** Copyright 2023, Arseniy Baytenov *)

(** SPDX-License-Identifier: MIT *)

open Base
open Ast
module NameMap = Stdlib.Map.Make (String)
module NameSet = Stdlib.Set.Make (String)

type entity = int * NameSet.t * constant NameMap.t

module Node = struct
  type t = entity

  let compare (id1, _, _) (id2, _, _) = id1 - id2
  let hash (id, _, _) = id
  let equal (id1, _, _) (id2, _, _) = id1 = id2
end

module Edge = struct
  type t = entity

  let compare (id1, _, _) (id2, _, _) = id1 - id2
  let equal (id1, _, _) (id2, _, _) = id1 = id2
  let default = 0, NameSet.empty, NameMap.empty
end

module G = Graph.Persistent.Digraph.ConcreteLabeled (Node) (Edge)

type graph = G.t

type value =
  | Constant of constant
  | Node of G.vertex
  | Rel of G.edge
  | DeletedNode of int
  | DeletedRel of int

type expr_error =
  | Type_mismatch
  | Division_by_zero
  | Undefined_id of string
  | Deleted_entity of int
[@@deriving show { with_path = false }]

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( <|> ) : 'a t -> 'a t -> 'a t
end

module type MONADERROR = sig
  include MONAD

  type error

  val fail : error -> 'a t
end

module type MONADERROREXPR = MONADERROR with type error = expr_error

module Eval_expr (M : MONADERROREXPR) = struct
  open M

  let eval_expr v_from_env p_from_env =
    let same v1 v2 =
      match v1, v2 with
      | Constant (Float c1), Constant (Float c2) -> Float.( = ) c1 c2
      | Constant (Int64 c1), Constant (Int64 c2) -> Int64.( = ) c1 c2
      | Constant (String c1), Constant (String c2) -> String.( = ) c1 c2
      | Constant (Bool c1), Constant (Bool c2) -> Bool.( = ) c1 c2
      | Node (id1, _, _), Node (id2, _, _) -> id1 = id2
      | Rel (_, (id1, _, _), _), Rel (_, (id2, _, _), _) -> id1 = id2
      | _ -> false
    in
    let string_of_constant = function
      | Float f -> Float.to_string f ^ if Stdlib.Float.is_integer f then "0" else ""
      | Int64 i -> Int64.to_string i
      | String s -> s
      | Bool b -> Bool.to_string b
      | Null -> ""
    in
    let rec helper = function
      | Const c -> return @@ Constant c
      | Var n -> v_from_env n
      | Property (entity, p) -> p_from_env (entity, p)
      | Bin_op (op, e1, e2) ->
        let h f =
          helper e1
          >>= fun e1 ->
          helper e2
          >>= fun e2 ->
          match e1, e2 with
          | Constant c1, Constant c2 -> f c1 c2
          | _ -> fail Type_mismatch
        in
        let ss_comp op e1 e2 =
          match e1, e2 with
          | String s1, String s2 -> return (Constant (Bool (op s1 s2)))
          | Null, _ | _, Null -> return @@ Constant Null
          | _ -> fail Type_mismatch
        in
        let minus_asterisk_caret iop fop e1 e2 =
          match e1, e2 with
          | Int64 i1, Int64 i2 -> return (Constant (Int64 (iop i1 i2)))
          | Int64 i, Float f -> return (Constant (Float (fop (Int64.to_float i) f)))
          | Float f, Int64 i -> return (Constant (Float (fop f (Int64.to_float i))))
          | Float f1, Float f2 -> return (Constant (Float (fop f1 f2)))
          | Null, _ | _, Null -> return (Constant Null)
          | _ -> fail Type_mismatch
        in
        let slash_percent iop fop e1 e2 =
          match e1, e2 with
          | Int64 i1, Int64 i2 ->
            if Int64.( = ) i2 Int64.zero
            then fail Division_by_zero
            else return (Constant (Int64 (iop i1 i2)))
          | Int64 i, Float f ->
            if Float.( = ) (Float.abs f) Float.zero
            then fail Division_by_zero
            else return (Constant (Float (fop (Int64.to_float i) f)))
          | Float f, Int64 i ->
            if Int64.( = ) i Int64.zero
            then fail Division_by_zero
            else return (Constant (Float (fop f (Int64.to_float i))))
          | Float f1, Float f2 ->
            if Float.( = ) (Float.abs f2) Float.zero
            then fail Division_by_zero
            else return (Constant (Float (fop f1 f2)))
          | Null, _ | _, Null -> return (Constant Null)
          | _ -> fail Type_mismatch
        in
        (match op with
         | CONTAINS -> h @@ ss_comp (fun s1 s2 -> String.is_substring s1 ~substring:s2)
         | STARTS_WITH -> h @@ ss_comp (fun s1 s2 -> String.is_prefix s1 ~prefix:s2)
         | ENDS_WITH -> h @@ ss_comp (fun s1 s2 -> String.is_suffix s1 ~suffix:s2)
         | Plus ->
           h (fun c1 c2 ->
             match c1, c2 with
             | Int64 i1, Int64 i2 -> return (Constant (Int64 (Int64.( + ) i1 i2)))
             | Int64 i, Float f | Float f, Int64 i ->
               return (Constant (Float (f +. Int64.to_float i)))
             | Float f1, Float f2 -> return (Constant (Float (f1 +. f2)))
             | Null, _ | _, Null -> return (Constant Null)
             | String s, smth -> return (Constant (String (s ^ string_of_constant smth)))
             | smth, String s -> return (Constant (String (string_of_constant smth ^ s)))
             | _ -> fail Type_mismatch)
         | Minus -> h (minus_asterisk_caret Int64.( - ) Float.( - ))
         | Slash -> h (slash_percent Int64.( / ) Float.( / ))
         | Asterisk -> h (minus_asterisk_caret Int64.( * ) Float.( * ))
         | Percent ->
           h
             (slash_percent
                (fun i1 i2 -> Int64.( - ) i1 (Int64.( * ) i2 (Int64.( / ) i1 i2)))
                (fun f1 f2 -> f1 -. (f2 *. Stdlib.Float.trunc (f1 /. f2))))
         | Caret -> h (minus_asterisk_caret Int64.( ** ) Float.( ** ))
         | AND ->
           h (fun c1 c2 ->
             match c1, c2 with
             | Bool false, _ | _, Bool false -> return (Constant (Bool false))
             | Bool true, Bool true -> return (Constant (Bool true))
             | Null, _ | _, Null -> return (Constant Null)
             | _ -> fail Type_mismatch)
         | OR ->
           h (fun c1 c2 ->
             match c1, c2 with
             | Bool true, _ | _, Bool true -> return (Constant (Bool true))
             | Bool false, Bool false -> return (Constant (Bool false))
             | Null, _ | _, Null -> return (Constant Null)
             | _ -> fail Type_mismatch)
         | XOR ->
           h (fun c1 c2 ->
             match c1, c2 with
             | Bool b1, Bool b2 -> return (Constant (Bool ((b1 || b2) && not (b1 && b2))))
             | Null, _ | _, Null -> return (Constant Null)
             | _ -> fail Type_mismatch))
      | Un_op (op, e) ->
        helper e
        >>= fun v ->
        (match op with
         | Minus ->
           (match v with
            | Constant (Int64 i) -> return (Constant (Int64 (Int64.neg i)))
            | Constant (Float f) -> return (Constant (Float (-.f)))
            | Constant Null -> return (Constant Null)
            | _ -> fail Type_mismatch)
         | IS_NULL ->
           (match v with
            | Constant Null -> return (Constant (Bool true))
            | _ -> return (Constant (Bool false)))
         | IS_NOT_NULL ->
           (match v with
            | Constant Null -> return (Constant (Bool false))
            | _ -> return (Constant (Bool true)))
         | NOT ->
           (match v with
            | Constant (Bool b) -> return (Constant (Bool (not b)))
            | Constant Null -> return (Constant Null)
            | _ -> fail Type_mismatch))
      | List_op (e, les) ->
        let lop op v1 v2 =
          let h op f1 f2 =
            match op with
            | Less -> return (Constant (Bool (Float.( < ) f1 f2)))
            | Greater -> return (Constant (Bool (Float.( > ) f1 f2)))
            | GEq -> return (Constant (Bool (Float.( >= ) f1 f2)))
            | LEq -> return (Constant (Bool (Float.( <= ) f1 f2)))
            | Eq -> return (Constant (Bool (Float.( = ) f1 f2)))
            | NEq -> return (Constant (Bool (Float.( <> ) f1 f2)))
          in
          match op, v1, v2 with
          | Eq, Constant Null, _ | Eq, _, Constant Null -> return (Constant Null)
          | NEq, Constant Null, _ | NEq, _, Constant Null -> return (Constant Null)
          | _, Constant Null, Constant _ | _, Constant _, Constant Null ->
            return (Constant Null)
          | op, Constant (Int64 i1), Constant (Int64 i2) ->
            (match op with
             | Less -> return (Constant (Bool (Int64.( < ) i1 i2)))
             | Greater -> return (Constant (Bool (Int64.( > ) i1 i2)))
             | GEq -> return (Constant (Bool (Int64.( >= ) i1 i2)))
             | LEq -> return (Constant (Bool (Int64.( <= ) i1 i2)))
             | Eq -> return (Constant (Bool (Int64.( = ) i1 i2)))
             | NEq -> return (Constant (Bool (Int64.( <> ) i1 i2))))
          | op, Constant (Int64 i), Constant (Float f) -> h op (Int64.to_float i) f
          | op, Constant (Float f), Constant (Int64 i) -> h op f (Int64.to_float i)
          | op, Constant (Float f1), Constant (Float f2) -> h op f1 f2
          | op, Constant (String s1), Constant (String s2) ->
            (match op with
             | Less -> return (Constant (Bool (String.( < ) s1 s2)))
             | Greater -> return (Constant (Bool (String.( > ) s1 s2)))
             | GEq -> return (Constant (Bool (String.( >= ) s1 s2)))
             | LEq -> return (Constant (Bool (String.( <= ) s1 s2)))
             | Eq -> return (Constant (Bool (String.( = ) s1 s2)))
             | NEq -> return (Constant (Bool (String.( <> ) s1 s2))))
          | op, Constant (String _), Constant (Int64 _)
          | op, Constant (String _), Constant (Float _)
          | op, Constant (Int64 _), Constant (String _)
          | op, Constant (Float _), Constant (String _) ->
            (match op with
             | Less | Greater | GEq | LEq -> return (Constant Null)
             | Eq -> return (Constant (Bool false))
             | NEq -> return (Constant (Bool true)))
          | Eq, v1, v2 -> return (Constant (Bool (same v1 v2)))
          | NEq, v1, v2 -> return (Constant (Bool (not @@ same v1 v2)))
          | _ -> fail Type_mismatch
        in
        let rec h = function
          | _, [] -> return (Constant (Bool true))
          | v1, (op, e) :: tl ->
            helper e
            >>= fun v2 ->
            lop op v1 v2
            >>= fun r1 ->
            h (v2, tl)
            >>= fun r2 ->
            (match r1, r2 with
             | Constant (Bool false), _ | _, Constant (Bool false) ->
               return (Constant (Bool false))
             | Constant Null, _ | _, Constant Null -> return (Constant Null)
             | _ -> return (Constant (Bool true)))
        in
        helper e >>= fun v -> h (v, les)
    in
    helper
  ;;
end

module Expr (M : MONADERROREXPR) : sig
  open M

  val eval_e : value NameMap.t -> expression -> value t
  val eval_e2 : value NameMap.t -> value NameMap.t -> expression -> value t
end = struct
  open M
  module E = Eval_expr (M)

  let eval_expr = E.eval_expr

  let v_from_nm nm n =
    match NameMap.find_opt n nm with
    | Some (DeletedNode id) | Some (DeletedRel id) -> fail @@ Deleted_entity id
    | Some v -> return v
    | None -> fail @@ Undefined_id n
  ;;

  let v_from_nm2 nm1 nm2 n = v_from_nm nm1 n <|> v_from_nm nm2 n

  let p_from_nm nm (e, p) =
    match NameMap.find_opt e nm with
    | Some (Node (_, _, pm)) | Some (Rel (_, (_, _, pm), _)) ->
      (match NameMap.find_opt p pm with
       | None -> return @@ Constant Null
       | Some v -> return @@ Constant v)
    | Some (DeletedNode id) | Some (DeletedRel id) -> fail @@ Deleted_entity id
    | _ -> fail @@ Undefined_id e
  ;;

  let p_from_nm2 nm1 nm2 ep = p_from_nm nm1 ep <|> p_from_nm nm2 ep
  let eval_e nm = eval_expr (v_from_nm nm) (p_from_nm nm)
  let eval_e2 nm1 nm2 = eval_expr (v_from_nm2 nm1 nm2) (p_from_nm2 nm1 nm2)
end

module ExprResult : MONADERROREXPR with type 'a t = ('a, expr_error) Result.t = struct
  type 'a t = ('a, expr_error) Result.t
  type error = expr_error

  let return a = Ok a
  let fail e = Error e

  let ( >>= ) r f =
    match r with
    | Ok v -> f v
    | Error e -> Error e
  ;;

  let ( <|> ) l r =
    match l with
    | Ok v -> return v
    | Error _ -> r
  ;;
end
