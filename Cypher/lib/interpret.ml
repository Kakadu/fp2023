(** Copyright 2023-2024, Arseniy Baytenov *)

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

type outvalue =
  | OutConstant of constant
  | OutNode of int * name list * (name * constant) list
  | OutRel of int * (int * name list * (name * constant) list) * int
[@@deriving show { with_path = false }]

type output = (int * int) * (int * int) * (name * outvalue) list list
[@@deriving show { with_path = false }]

type expr_error =
  | Type_mismatch
  | Division_by_zero
  | Undefined_id of string
  | Deleted_entity of int
[@@deriving show { with_path = false }]

type interpreter_error =
  | Type_mismatch
  | Multiple_def of string
  | Undefined_id of string
  | Deleted_entity of int
  | Expr_err of expr_error
  | Unable_node_deletion of int
  | ID_overflow
  | Undirected_rel
  | Multiple_using_rel_var of string
[@@deriving show { with_path = false }]

type req_error =
  | Parser_err of string
  | Interpreter_err of interpreter_error
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
module type MONADERRORINTERPRETER = MONADERROR with type error = interpreter_error
module type MONADERRORREQ = MONADERROR with type error = req_error

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

module Interpreter (M : MONADERRORINTERPRETER) : sig
  open M

  val interpret_request : graph -> clause -> (graph * output) t
end = struct
  open M
  module E = Expr (ExprResult)

  let expression_to_name e =
    let bin_op_to_name = function
      | CONTAINS -> " CONTAINS "
      | STARTS_WITH -> " STARTS_WITH "
      | ENDS_WITH -> " ENDS_WITH "
      | Plus -> " + "
      | Minus -> " - "
      | Slash -> " / "
      | Asterisk -> " * "
      | Percent -> " % "
      | Caret -> " ^ "
      | AND -> " AND "
      | OR -> " OR "
      | XOR -> " XOR "
    in
    let list_op_to_name = function
      | Eq -> " = "
      | NEq -> " <> "
      | Less -> " < "
      | Greater -> " > "
      | LEq -> " <= "
      | GEq -> " >= "
    in
    let rec helper e =
      match e with
      | Const (Float f) -> Float.to_string f
      | Const (Int64 i) -> Int64.to_string i
      | Const (String s) -> {|"|} ^ s ^ {|"|}
      | Const (Bool b) -> Bool.to_string b
      | Const Null -> "<null>"
      | Var n -> n
      | Property (en, pr) -> en ^ "." ^ pr
      | Bin_op (bop, e1, e2) -> helper e1 ^ bin_op_to_name bop ^ helper e2
      | Un_op (Minus, e) -> "- " ^ helper e
      | Un_op (NOT, e) -> "NOT " ^ helper e
      | Un_op (IS_NULL, e) -> helper e ^ " IS NULL"
      | Un_op (IS_NOT_NULL, e) -> helper e ^ " IS NOT NULL"
      | List_op (e, les) ->
        helper e
        ^
          (match les with
          | [] -> ""
          | (lop, e) :: les -> list_op_to_name lop ^ helper (List_op (e, les)))
    in
    helper e
  ;;

  let eval_e nm e =
    match E.eval_e nm e with
    | Ok v -> return v
    | Error e -> fail @@ Expr_err e
  ;;

  let eval_e2 nm1 nm2 e =
    match E.eval_e2 nm1 nm2 e with
    | Ok v -> return v
    | Error e -> fail @@ Expr_err e
  ;;

  let lreverse xs = List.fold_left xs ~init:[] ~f:(fun acc x -> x :: acc)

  let order_by ps ob =
    let rec helper nm1 nm2 ob acc =
      match ob with
      | [] -> return (lreverse acc)
      | ([], _) :: ob -> helper nm1 nm2 ob acc
      | (e :: es, c) :: ob ->
        eval_e2 nm1 nm2 e >>= fun v -> helper nm1 nm2 ((es, c) :: ob) ((v, c) :: acc)
    in
    let rec ts_from_ps ps ts =
      match ps with
      | [] -> return (lreverse ts)
      | (nm1, nm2) :: ps ->
        helper nm1 nm2 ob [] >>= fun vs -> ts_from_ps ps ((nm1, nm2, vs) :: ts)
    in
    let cmp v1 v2 =
      match v1, v2 with
      | Constant c1, Constant c2 ->
        (match c1, c2 with
         | String s1, String s2 -> String.compare s1 s2
         | String _, _ -> -1
         | _, String _ -> 1
         | Bool b1, Bool b2 -> Bool.compare b1 b2
         | Bool _, _ -> -1
         | _, Bool _ -> 1
         | Float f1, Float f2 -> Float.compare f1 f2
         | Int64 i1, Int64 i2 -> Int64.compare i1 i2
         | Float f, Int64 i -> Float.compare f (Int64.to_float i)
         | Int64 i, Float f -> Float.compare (Int64.to_float i) f
         | Float _, Null | Int64 _, Null -> -1
         | Null, Float _ | Null, Int64 _ -> 1
         | Null, Null -> 0)
      | Node (i1, _, _), Node (i2, _, _) | Rel (_, (i1, _, _), _), Rel (_, (i2, _, _), _)
        -> i2 - i1
      | _, _ -> -1
    in
    let rec vcs_cmp_helper vcs1 vcs2 =
      match vcs1, vcs2 with
      | [], _ | _, [] -> 0
      | (v1, Asc) :: vcs1, (v2, _) :: vcs2 ->
        (match cmp v1 v2 with
         | 0 -> vcs_cmp_helper vcs1 vcs2
         | r -> r)
      | (v1, Desc) :: vcs1, (v2, _) :: vcs2 ->
        (match cmp v2 v1 with
         | 0 -> vcs_cmp_helper vcs1 vcs2
         | r -> r)
    in
    let ts_cmp (_, _, vcs1) (_, _, vcs2) = vcs_cmp_helper vcs1 vcs2 in
    ts_from_ps ps []
    >>= fun ts ->
    return (List.map (List.sort ts ~compare:ts_cmp) ~f:(fun (nm1, nm2, _) -> nm1, nm2))
  ;;

  let where ms eval_e wh =
    let rec helper eval_e es =
      match es with
      | [] -> return true
      | e :: es ->
        eval_e e
        >>= (function
         | Constant Null | Constant (Bool false) -> return false
         | Constant (Bool true) -> helper eval_e es
         | _ -> fail Type_mismatch)
    in
    let rec filter ms acc =
      match ms with
      | [] -> return (lreverse acc)
      | m :: ms ->
        helper (eval_e m) wh
        >>= (function
         | true -> filter ms (m :: acc)
         | false -> filter ms acc)
    in
    filter ms []
  ;;

  let nm_pair_from_nm nms s_opt als =
    let rec path newnm nm als =
      match als with
      | [] -> return (newnm, nm)
      | (e, n) :: als ->
        (match NameMap.mem n newnm with
         | true -> fail (Multiple_def n)
         | false -> eval_e nm e >>= fun v -> path (NameMap.add n v newnm) nm als)
    in
    let rec paths nms ps s_opt als =
      match nms with
      | [] -> return (lreverse ps)
      | nm :: nms ->
        path
          (match s_opt with
           | Some All -> nm
           | None -> NameMap.empty)
          nm
          als
        >>= fun pair -> paths nms (pair :: ps) s_opt als
    in
    paths nms [] s_opt als
  ;;

  let cwith (g, nms, out) s_opt als ob wh c =
    nm_pair_from_nm nms s_opt als
    >>= fun ps ->
    order_by ps ob
    >>= (fun ps -> where ps (fun (nm1, nm2) -> eval_e2 nm1 nm2) wh)
    >>= fun ps -> return ((g, List.map ps ~f:(fun (newnm, _) -> newnm), out), Some c)
  ;;

  let creturn (g, nms, (cr, del, _)) s_opt al_opts ob =
    let v_to_ov = function
      | Constant c -> return @@ OutConstant c
      | Node (id, lbl, pr) ->
        return
        @@ OutNode
             (id, NameSet.elements lbl, NameMap.fold (fun n c acc -> (n, c) :: acc) pr [])
      | Rel ((nid1, _, _), (id, lbl, pr), (nid2, _, _)) ->
        return
        @@ OutRel
             ( nid1
             , ( id
               , NameSet.elements lbl
               , NameMap.fold (fun n c acc -> (n, c) :: acc) pr [] )
             , nid2 )
      | DeletedNode id | DeletedRel id -> fail @@ Deleted_entity id
    in
    let rec path newnm nm al_opts novs =
      match al_opts with
      | [] -> return (lreverse novs)
      | (e, Some n) :: al_opts ->
        (match NameMap.find_opt n newnm with
         | Some v -> v_to_ov v
         | None -> eval_e nm e >>= fun v -> v_to_ov v)
        >>= fun ov -> path newnm nm al_opts ((n, ov) :: novs)
      | (e, None) :: al_opts ->
        eval_e nm e
        >>= (fun v -> v_to_ov v)
        >>= fun ov -> path newnm nm al_opts ((expression_to_name e, ov) :: novs)
    in
    let rec paths nmps al_opts novss =
      match nmps with
      | [] -> return (lreverse novss)
      | (newnm, nm) :: nmps ->
        (match s_opt with
         | Some All ->
           path
             newnm
             nm
             (NameMap.fold (fun n _ acc -> (Var n, Some n) :: acc) newnm [])
             []
         | None -> return [])
        >>= (fun novs -> path newnm nm al_opts novs)
        >>= fun novs -> paths nmps al_opts (novs :: novss)
    in
    nm_pair_from_nm
      nms
      s_opt
      (List.filter_map al_opts ~f:(function
        | e, Some n -> Some (e, n)
        | _, None -> None))
    >>= (fun ps -> order_by ps ob)
    >>= (fun ps -> paths ps al_opts [])
    >>= fun novss -> return ((g, nms, (cr, del, novss)), None)
  ;;

  let cdelete (g, nms, (cr, (del_nodes, del_rels), tbl)) del_attr ns c_opt =
    let delete_from_nm nm n =
      match NameMap.find_opt n nm with
      | Some v -> return v
      | None -> fail @@ Undefined_id n
    in
    let rec path_to_delete ns nm nodes rels =
      match ns with
      | [] -> return (nm, nodes, rels)
      | n :: ns ->
        delete_from_nm nm n
        >>= (function
         | Node (id, lbl, pr) ->
           path_to_delete
             ns
             (NameMap.add n (DeletedNode id) nm)
             ((id, lbl, pr) :: nodes)
             rels
         | Rel (n1, (id, lbl, pr), n2) ->
           path_to_delete
             ns
             (NameMap.add n (DeletedRel id) nm)
             nodes
             ((n1, (id, lbl, pr), n2) :: rels)
         | DeletedNode _ -> path_to_delete ns nm nodes rels
         | DeletedRel _ -> path_to_delete ns nm nodes rels
         | _ -> fail Type_mismatch)
    in
    let rec paths_to_delete nms newnms nodes rels =
      match nms with
      | [] -> return (lreverse newnms, nodes, rels)
      | nm :: nms ->
        path_to_delete ns nm nodes rels
        >>= fun (newnm, nodes, rels) -> paths_to_delete nms (newnm :: newnms) nodes rels
    in
    let rec detach_delete_nodes g = function
      | [] -> return g
      | node :: nodes -> detach_delete_nodes (G.remove_vertex g node) nodes
    in
    let rec nodetach_delete_nodes g = function
      | [] -> return g
      | (id, lbl, pr) :: nodes ->
        (match
           G.fold_edges
             (fun (id1, _, _) (id2, _, _) acc -> acc || id = id1 || id = id2)
             g
             false
         with
         | false -> nodetach_delete_nodes (G.remove_vertex g (id, lbl, pr)) nodes
         | true -> fail @@ Unable_node_deletion id)
    in
    paths_to_delete nms [] [] []
    >>= fun (newnms, nodes, rels) ->
    (match del_attr with
     | Detach -> detach_delete_nodes
     | Nodetach -> nodetach_delete_nodes)
      (List.fold_left rels ~init:g ~f:(fun g rel -> G.remove_edge_e g rel))
      nodes
    >>= fun g ->
    return
      ( ( g
        , newnms
        , (cr, (del_nodes + List.length nodes, del_rels + List.length rels), tbl) )
      , c_opt )
  ;;

  let ccreate (g, nms, ((cr_nodes, cr_rels), del, tbl)) ps c_opt =
    let next_node_id g =
      let max_id = G.fold_vertex (fun (id, _, _) acc -> Int.max id acc) g 0 in
      match max_id = max_int with
      | true -> fail ID_overflow
      | false -> return (max_id + 1)
    in
    let next_rel_id g =
      let max_id = G.fold_edges_e (fun (_, (id, _, _), _) acc -> Int.max id acc) g 0 in
      match max_id = max_int with
      | true -> fail ID_overflow
      | false -> return (max_id + 1)
    in
    let rec nes_to_pr nm pr = function
      | [] -> return pr
      | (n, e) :: nes ->
        eval_e nm e
        >>= (function
         | Constant c -> nes_to_pr nm (NameMap.add n c pr) nes
         | _ -> fail Type_mismatch)
    in
    let create_rel g nm lbl nes node1 node2 dir =
      nes_to_pr nm NameMap.empty nes
      >>= fun pr ->
      next_rel_id g
      >>= fun id ->
      return (id, NameSet.of_list lbl, pr)
      >>= fun en ->
      match dir with
      | Left -> return (node2, en, node1)
      | Right -> return (node1, en, node2)
      | No -> fail Undirected_rel
    in
    let create_node g nm lbl nes =
      nes_to_pr nm NameMap.empty nes
      >>= fun pr -> next_node_id g >>= fun id -> return (id, NameSet.of_list lbl, pr)
    in
    let add_node g nm = function
      | Some n, (lbl, nes) ->
        (match NameMap.find_opt n nm, lbl, nes with
         | Some (Node node2), [], [] -> return (g, nm, node2, 0)
         | Some _, _, _ -> fail @@ Multiple_def n
         | None, lbl, nes ->
           create_node g nm lbl nes
           >>= fun node2 ->
           return (G.add_vertex g node2, NameMap.add n (Node node2) nm, node2, 1))
      | None, (lbl, nes) ->
        create_node g nm lbl nes
        >>= fun node2 -> return (G.add_vertex g node2, nm, node2, 1)
    in
    let rec path node g nm (cr_nodes, cr_rels) = function
      | [] -> return (g, nm, (cr_nodes, cr_rels))
      | (rel, node2) :: rns ->
        add_node g nm node2
        >>= fun (g, nm, node2, pn) ->
        (match rel with
         | Some n, (lbl, nes), dir ->
           (match NameMap.mem n nm with
            | true -> fail @@ Multiple_def n
            | false ->
              create_rel g nm lbl nes node node2 dir
              >>= fun rel ->
              path
                node2
                (G.add_edge_e g rel)
                (NameMap.add n (Rel rel) nm)
                (cr_nodes + pn, cr_rels + 1)
                rns)
         | None, (lbl, nes), dir ->
           create_rel g nm lbl nes node node2 dir
           >>= fun rel ->
           path node2 (G.add_edge_e g rel) nm (cr_nodes + pn, cr_rels + 1) rns)
    in
    let path g nm (node, rns) =
      add_node g nm node >>= fun (g, nm, node, pn) -> path node g nm (pn, 0) rns
    in
    let rec paths g nm cr_nodes cr_rels = function
      | [] -> return (g, nm, (cr_nodes, cr_rels))
      | p :: ps ->
        path g nm p
        >>= fun (g, nm, (crns, crrs)) -> paths g nm (cr_nodes + crns) (cr_rels + crrs) ps
    in
    let rec helper g newnms cr_nodes cr_rels = function
      | [] -> return (g, lreverse newnms, (cr_nodes, cr_rels))
      | nm :: nms ->
        paths g nm 0 0 ps
        >>= fun (g, newnm, (crns, crrs)) ->
        helper g (newnm :: newnms) (cr_nodes + crns) (cr_rels + crrs) nms
    in
    helper g [] 0 0 nms
    >>= fun (g, newnms, (crns, crrs)) ->
    return ((g, newnms, ((cr_nodes + crns, cr_rels + crrs), del, tbl)), c_opt)
  ;;

  let cmatch (g, nms, out) ps wh c =
    let rec nes_to_ncs nm ncs = function
      | [] -> return ncs
      | (n, e) :: nes ->
        eval_e nm e
        >>= (function
         | Constant c -> nes_to_ncs nm ((n, c) :: ncs) nes
         | _ -> fail Type_mismatch)
    in
    let pt_to_mpt nm (ns, nes) =
      nes_to_ncs nm [] nes >>= fun ncs -> return (NameSet.of_list ns, ncs)
    in
    let rec rns_to_mrns nm mrns = function
      | [] -> return @@ lreverse mrns
      | ((rn_opt, rpt, dir), (nn_opt, npt)) :: rns ->
        pt_to_mpt nm rpt
        >>= fun rpt ->
        pt_to_mpt nm npt
        >>= fun npt -> rns_to_mrns nm (((rn_opt, rpt, dir), (nn_opt, npt)) :: mrns) rns
    in
    let p_to_mp nm ((n_opt, pt), rns) =
      pt_to_mpt nm pt
      >>= fun pt -> rns_to_mrns nm [] rns >>= fun mrns -> return ((n_opt, pt), mrns)
    in
    let rec ps_to_mps nm mps = function
      | [] -> return @@ lreverse mps
      | p :: ps -> p_to_mp nm p >>= fun mp -> ps_to_mps nm (mp :: mps) ps
    in
    let cs_are_equal c1 c2 =
      match c1, c2 with
      | String s1, String s2 -> String.equal s1 s2
      | Bool b1, Bool b2 -> Bool.equal b1 b2
      | Float f1, Float f2 -> Float.equal f1 f2
      | Int64 i1, Int64 i2 -> Int64.equal i1 i2
      | Float f, Int64 i | Int64 i, Float f -> Float.equal (Int64.to_float i) f
      | Null, Null -> true
      | _, _ -> false
    in
    let rec ver_pr pr = function
      | [] -> true
      | (n, c) :: ncs ->
        (match
           cs_are_equal
             (match NameMap.find_opt n pr with
              | Some c -> c
              | None -> Null)
             c
         with
         | true -> ver_pr pr ncs
         | false -> false)
    in
    let ver_entity (lbl2, ncs) (_, lbl1, pr) =
      NameSet.subset lbl2 lbl1 && ver_pr pr ncs
    in
    let ver_by_id (id1, _, _) (id2, _, _) = id1 = id2 in
    let get_ver_node nm n_opt pt =
      match n_opt with
      | Some n ->
        (match NameMap.find_opt n nm with
         | Some (Node en) ->
           (match ver_entity pt en with
            | true -> return @@ ver_by_id en
            | false -> return (fun _ -> false))
         | Some _ -> fail Type_mismatch
         | None -> return @@ ver_entity pt)
      | None -> return @@ ver_entity pt
    in
    let get_ver_rel nm ln rn_opt rpt dir rnn_opt rnpt =
      let ver_ln = ver_by_id ln in
      let ver_left ver_r ver_rn (srcn, r, dstn) =
        match ver_ln dstn && ver_r r && ver_rn srcn with
        | true -> Some srcn
        | false -> None
      in
      let ver_right ver_r ver_rn (srcn, r, dstn) =
        match ver_ln srcn && ver_r r && ver_rn dstn with
        | true -> Some dstn
        | false -> None
      in
      let ver_no ver_r ver_rn rel =
        match ver_left ver_r ver_rn rel, ver_right ver_r ver_rn rel with
        | None, r -> r
        | l, _ -> l
      in
      (match rn_opt with
       | Some n ->
         (match NameMap.find_opt n nm with
          | Some _ -> fail @@ Multiple_using_rel_var n
          | None ->
            (match rnn_opt with
             | Some rnn ->
               (match String.equal n rnn with
                | true -> fail Type_mismatch
                | false -> return @@ ver_entity rpt)
             | None -> return @@ ver_entity rpt))
       | None -> return @@ ver_entity rpt)
      >>= fun ver_r ->
      get_ver_node nm rnn_opt rnpt
      >>= fun ver_rn ->
      return
      @@ (match dir with
          | Left -> ver_left
          | Right -> ver_right
          | No -> ver_no)
           ver_r
           ver_rn
    in
    let try_add_nvar nm node = function
      | Some n -> NameMap.add n (Node node) nm
      | None -> nm
    in
    let try_add_rvar nm rel = function
      | Some n -> NameMap.add n (Rel rel) nm
      | None -> nm
    in
    let rec match_many men sts en acc =
      match sts with
      | [] -> return acc
      | st :: sts -> men st en acc >>= fun acc -> match_many men sts en acc
    in
    let match_rel (ln, g, nm) ((rn_opt, rpt, dir), (rnn_opt, rnpt)) acc =
      get_ver_rel nm ln rn_opt rpt dir rnn_opt rnpt
      >>= fun ver_rel ->
      return
      @@ G.fold_edges_e
           (fun (n1, r, n2) acc ->
             match ver_rel (n1, r, n2) with
             | Some rn ->
               ( rn
               , G.remove_edge_e g (n1, r, n2)
               , try_add_nvar (try_add_rvar nm (n1, r, n2) rn_opt) rn rnn_opt )
               :: acc
             | None -> acc)
           g
           acc
    in
    let match_node (g, nm) (n_opt, pt) acc =
      get_ver_node nm n_opt pt
      >>= fun ver_n ->
      return
      @@ G.fold_vertex
           (fun n acc ->
             match ver_n n with
             | true -> (n, g, try_add_nvar nm n n_opt) :: acc
             | false -> acc)
           g
           acc
    in
    let rec match_rns ngnms = function
      | [] -> return @@ List.map ngnms ~f:(fun (_, g, nm) -> g, nm)
      | rn :: rns -> match_many match_rel ngnms rn [] >>= fun ngnms -> match_rns ngnms rns
    in
    let match_mp gnms (n, rns) =
      match_many match_node gnms n [] >>= fun ngnms -> match_rns ngnms rns
    in
    let rec match_mps acc gnms = function
      | [] -> return @@ List.fold gnms ~init:acc ~f:(fun nms (_, nm) -> nm :: nms)
      | mp :: mps -> match_mp gnms mp >>= fun gnms -> match_mps acc gnms mps
    in
    let rec match_ps ps newnms = function
      | [] -> return newnms
      | nm :: nms ->
        ps_to_mps nm [] ps
        >>= (fun mps -> match_mps newnms [ g, nm ] mps)
        >>= fun newnms -> match_ps ps newnms nms
    in
    match_ps ps [] nms
    >>= (fun nms -> where nms (fun nm -> eval_e nm) wh)
    >>= fun nms -> return ((g, nms, out), Some c)
  ;;

  let interpret_request g request =
    let rec mwith = function
      | gnmsout, Some (With (s_opt, als, ob, wh, c)) ->
        cwith gnmsout s_opt als ob wh c >>= fun state -> mwith state
      | state -> return state
    in
    let withreturn state =
      mwith state
      >>= function
      | gnmsout, Some (Return (s_opt, al_opts, ob)) ->
        creturn gnmsout s_opt al_opts ob >>= fun ((g, _, out), _) -> return (g, out)
      | (g, _, out), _ -> return (g, out)
    in
    let rec withcrdelret state =
      mwith state
      >>= function
      | gnmsout, Some (Create (ps, c_opt)) ->
        ccreate gnmsout ps c_opt >>= fun state -> withcrdelret state
      | gnmsout, Some (Delete (del_attr, ns, c_opt)) ->
        cdelete gnmsout del_attr ns c_opt >>= fun state -> withcrdelret state
      | state -> withreturn state
    in
    let withmatchcdr state =
      mwith state
      >>= function
      | gmnsout, Some (Match (ps, wh, c)) ->
        cmatch gmnsout ps wh c >>= fun state -> withcrdelret state
      | state -> withcrdelret state
    in
    withmatchcdr ((g, [ NameMap.empty ], ((0, 0), (0, 0), [])), Some request)
  ;;
end

module InterpreterResult :
  MONADERRORINTERPRETER with type 'a t = ('a, interpreter_error) Result.t = struct
  type 'a t = ('a, interpreter_error) Result.t
  type error = interpreter_error

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

open Parser

module ReqResult : MONADERRORREQ with type 'a t = ('a, req_error) Result.t = struct
  type 'a t = ('a, req_error) Result.t
  type error = req_error

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

open ReqResult

let create_empty_graph = G.empty

module I = Interpreter (InterpreterResult)

let parse r =
  match parse_request r with
  | Ok r -> return r
  | Error msg -> fail @@ Parser_err msg
;;

let interpret_request g r =
  match I.interpret_request g r with
  | Ok res -> return res
  | Error err -> fail @@ Interpreter_err err
;;

let parse_and_interpret_request g r = parse r >>= fun r -> interpret_request g r

let piprint g r =
  match parse_and_interpret_request g r with
  | Ok (_, out) -> Stdlib.Format.printf "%a" pp_output out
  | Error err -> Stdlib.Format.printf "%a" pp_req_error err
;;

let g = create_empty_graph

let%expect_test "Expr string comparators test" =
  piprint
    g
    {| RETURN
  "Hola" CONTAINS "ol",
  "Boba" STARTS WITH "Bo",
  "Yoba" ENDS WITH "ba",
  "God" CONTAINS null,
  null CONTAINS "God"
  |};
  [%expect
    {|
    ((0, 0), (0, 0),
     [[("\"Hola\" CONTAINS \"ol\"", (OutConstant (Bool true)));
        ("\"Boba\" STARTS_WITH \"Bo\"", (OutConstant (Bool true)));
        ("\"Yoba\" ENDS_WITH \"ba\"", (OutConstant (Bool true)));
        ("\"God\" CONTAINS <null>", (OutConstant Null));
        ("<null> CONTAINS \"God\"", (OutConstant Null))]
       ]) |}]
;;

let%expect_test "Expr string comparator fail test" =
  piprint g {| RETURN 4 CONTAINS "4" |};
  [%expect {| (Interpreter_err (Expr_err Type_mismatch)) |}]
;;

let%expect_test "Expr plus test" =
  piprint
    g
    {| RETURN
  4 + 5, 4 + 4.4, 4.4 + 4, 3.5 + 4.4,
  null + true, 4 + null,
  "4" + true, 4 + "4" |};
  [%expect
    {|
    ((0, 0), (0, 0),
     [[("4 + 5", (OutConstant (Int64 9L)));
        ("4 + 4.4", (OutConstant (Float 8.4)));
        ("4.4 + 4", (OutConstant (Float 8.4)));
        ("3.5 + 4.4", (OutConstant (Float 7.9)));
        ("<null> + true", (OutConstant Null));
        ("4 + <null>", (OutConstant Null));
        ("\"4\" + true", (OutConstant (String "4true")));
        ("4 + \"4\"", (OutConstant (String "44")))]
       ]) |}]
;;

let%expect_test "Expr plus fail test" =
  piprint g {| RETURN true + 4 |};
  [%expect {| (Interpreter_err (Expr_err Type_mismatch)) |}]
;;

let%expect_test "Expr minus asterisk caret test" =
  piprint g {| RETURN 4 - -5, 6 * 6.6, 6.6 ^ 6, 0.5 ^ 0.5, null - true, "" * null |};
  [%expect
    {|
    ((0, 0), (0, 0),
     [[("4 - -5", (OutConstant (Int64 9L)));
        ("6 * 6.6", (OutConstant (Float 39.6)));
        ("6.6 ^ 6", (OutConstant (Float 82653.950016)));
        ("0.5 ^ 0.5", (OutConstant (Float 0.707106781187)));
        ("<null> - true", (OutConstant Null));
        ("\"\" * <null>", (OutConstant Null))]
       ]) |}]
;;

let%expect_test "Expr minus asterisk caret fail test" =
  piprint g {| RETURN 4 - true |};
  [%expect {| (Interpreter_err (Expr_err Type_mismatch)) |}]
;;

let%expect_test "Expr slash percent test" =
  piprint g {| RETURN 4 % 4, 4 / -2.2, 2.2 % 4, 2.2 / 0.1,
  null / 0, 0 % null |};
  [%expect
    {|
    ((0, 0), (0, 0),
     [[("4 % 4", (OutConstant (Int64 0L)));
        ("4 / -2.2", (OutConstant (Float -1.81818181818)));
        ("2.2 % 4", (OutConstant (Float 2.2)));
        ("2.2 / 0.1", (OutConstant (Float 22.)));
        ("<null> / 0", (OutConstant Null)); ("0 % <null>", (OutConstant Null))]
       ]) |}]
;;

let%expect_test "Expr slash percent fail test1" =
  piprint g {| RETURN 4 % 0 |};
  [%expect {| (Interpreter_err (Expr_err Division_by_zero)) |}]
;;

let%expect_test "Expr slash percent fail test2" =
  piprint g {| RETURN 4 / -0.0 |};
  [%expect {| (Interpreter_err (Expr_err Division_by_zero)) |}]
;;

let%expect_test "Expr slash percent fail test3" =
  piprint g {| RETURN 2.2 % 0 |};
  [%expect {| (Interpreter_err (Expr_err Division_by_zero)) |}]
;;

let%expect_test "Expr slash percent fail test4" =
  piprint g {| RETURN 2.2 / 0.000 |};
  [%expect {| (Interpreter_err (Expr_err Division_by_zero)) |}]
;;

let%expect_test "Expr slash percent fail test5" =
  piprint g {| RETURN 2.2 / "4" |};
  [%expect {| (Interpreter_err (Expr_err Type_mismatch)) |}]
;;

let%expect_test "Expr AND OR XOR test" =
  piprint
    g
    {| WITH TRUE AS T, FALSE AS F
  RETURN T AND F, F AND T, T AND T, NULL AND T, T AND NULL,
  T OR F, F OR T, F OR F, NULL OR F, F OR NULL,
  T XOR F, NULL XOR T, F XOR NULL |};
  [%expect
    {|
    ((0, 0), (0, 0),
     [[("T AND F", (OutConstant (Bool false)));
        ("F AND T", (OutConstant (Bool false)));
        ("T AND T", (OutConstant (Bool true)));
        ("<null> AND T", (OutConstant Null));
        ("T AND <null>", (OutConstant Null));
        ("T OR F", (OutConstant (Bool true)));
        ("F OR T", (OutConstant (Bool true)));
        ("F OR F", (OutConstant (Bool false)));
        ("<null> OR F", (OutConstant Null)); ("F OR <null>", (OutConstant Null));
        ("T XOR F", (OutConstant (Bool true)));
        ("<null> XOR T", (OutConstant Null));
        ("F XOR <null>", (OutConstant Null))]
       ]) |}]
;;

let%expect_test "Expr AND fail test" =
  piprint g {| RETURN 4 AND TRUE |};
  [%expect {| (Interpreter_err (Expr_err Type_mismatch)) |}]
;;

let%expect_test "Expr OR fail test" =
  piprint g {| RETURN 4 OR FALSE |};
  [%expect {| (Interpreter_err (Expr_err Type_mismatch)) |}]
;;

let%expect_test "Expr XOR fail test" =
  piprint g {| RETURN 4 XOR TRUE |};
  [%expect {| (Interpreter_err (Expr_err Type_mismatch)) |}]
;;

let%expect_test "Expr neg (is null) (in not null) neg test" =
  piprint
    g
    {| RETURN - (4), - (5.5), - null,
  4 is null, null is null, 4 is not null, null is not null,
  NOT TRUE, NOT NULL |};
  [%expect
    {|
    ((0, 0), (0, 0),
     [[("- 4", (OutConstant (Int64 -4L))); ("- 5.5", (OutConstant (Float -5.5)));
        ("- <null>", (OutConstant Null));
        ("4 IS NULL", (OutConstant (Bool false)));
        ("<null> IS NULL", (OutConstant (Bool true)));
        ("4 IS NOT NULL", (OutConstant (Bool true)));
        ("<null> IS NOT NULL", (OutConstant (Bool false)));
        ("NOT true", (OutConstant (Bool false)));
        ("NOT <null>", (OutConstant Null))]
       ]) |}]
;;

let%expect_test "Expr neg fail test" =
  piprint g {| RETURN - "" |};
  [%expect {| (Interpreter_err (Expr_err Type_mismatch)) |}]
;;

let%expect_test "Expr NOT fail test" =
  piprint g {| RETURN NOT "" |};
  [%expect {| (Interpreter_err (Expr_err Type_mismatch)) |}]
;;

let%expect_test "Expr list operators test" =
  piprint
    g
    {| RETURN 4 < 5 > 3.4 >= 2 <= 5 = 5.0 <> 7.7,
  null < 5,
  4 = 4.0 < -5.0 = -4 |};
  [%expect
    {|
    ((0, 0), (0, 0),
     [[("4 < 5 > 3.4 >= 2 <= 5 = 5. <> 7.7", (OutConstant (Bool true)));
        ("<null> < 5", (OutConstant Null));
        ("4 = 4. < -5. = -4", (OutConstant (Bool false)))]
       ]) |}]
;;

let%expect_test "WITH RETURN test" =
  piprint g {| WITH 4 as four, 5 as five
  RETURN *, 4, 5 |};
  [%expect
    {|
    ((0, 0), (0, 0),
     [[("five", (OutConstant (Int64 5L))); ("four", (OutConstant (Int64 4L)));
        ("4", (OutConstant (Int64 4L))); ("5", (OutConstant (Int64 5L)))]
       ]) |}]
;;

let%expect_test "WITH RETURN test" =
  piprint g {| WITH 4 as four, 5 as five
  RETURN 4, 5, four, five |};
  [%expect
    {|
    ((0, 0), (0, 0),
     [[("4", (OutConstant (Int64 4L))); ("5", (OutConstant (Int64 5L)));
        ("four", (OutConstant (Int64 4L))); ("five", (OutConstant (Int64 5L)))]
       ]) |}]
;;

let%expect_test "WITH RETURN fail test1" =
  piprint g {| WITH r as five RETURN * |};
  [%expect {|
    (Interpreter_err (Expr_err (Undefined_id "r"))) |}]
;;

let%expect_test "WITH RETURN fail test2" =
  piprint g {| WITH 4 as four, 5 as four RETURN * |};
  [%expect {|
    (Interpreter_err (Multiple_def "four")) |}]
;;

let%expect_test "CREATE test1" =
  piprint g {| CREATE (n:Label{name: "Name"})-[r:Label{p:5}]->()<--(), ()-->() RETURN * |};
  [%expect
    {|
    ((5, 3), (0, 0),
     [[("n", (OutNode (1, ["Label"], [("name", (String "Name"))])));
        ("r", (OutRel (1, (1, ["Label"], [("p", (Int64 5L))]), 2)))]
       ]) |}]
;;

let%expect_test "CREATE test2" =
  piprint g {| CREATE (n:Label{c:"c"})-[r]->(n)<-[]-(), (t) RETURN * |};
  [%expect
    {|
    ((3, 2), (0, 0),
     [[("n", (OutNode (1, ["Label"], [("c", (String "c"))])));
        ("r", (OutRel (1, (1, [], []), 1))); ("t", (OutNode (3, [], [])))]
       ]) |}]
;;

let%expect_test "CREATE fail test1" =
  piprint g {| CREATE ()-[r]->()<-[r]-() RETURN * |};
  [%expect {|
    (Interpreter_err (Multiple_def "r")) |}]
;;

let%expect_test "CREATE fail test2" =
  piprint g {| CREATE ()-[r]-() RETURN * |};
  [%expect {|
    (Interpreter_err Undirected_rel) |}]
;;

let%expect_test "CREATE DELETE test1" =
  piprint
    g
    {| CREATE (n1)-[r1]->(n2)<-[r2]-(n3)
  NODETACH DELETE r1, n2, n3, r2
  RETURN n1 |};
  [%expect {|
    ((3, 2), (2, 2), [[("n1", (OutNode (1, [], [])))]]) |}]
;;

let%expect_test "CREATE DELETE test2" =
  piprint g {| CREATE (n1)-[r]->(n2)<--(n3)
  DETACH DELETE n2
  RETURN n1 |};
  [%expect {|
    ((3, 2), (1, 0), [[("n1", (OutNode (1, [], [])))]]) |}]
;;

let%expect_test "CREATE DELETE fail test1" =
  piprint g {| CREATE (n1)-[r]->(n2)<--(n3)
  DELETE n2
  RETURN n1 |};
  [%expect {|
    (Interpreter_err (Unable_node_deletion 2)) |}]
;;

let%expect_test "CREATE DELETE fail test2" =
  piprint g {| CREATE (n1)-[r]->(n2)<--(n3)
  DELETE n4
  RETURN n1 |};
  [%expect {|
    (Interpreter_err (Undefined_id "n4")) |}]
;;

let%expect_test "WITH DELETE fail test" =
  piprint g {| WITH "n4" AS n4
  DELETE n4
  RETURN n1 |};
  [%expect {|
    (Interpreter_err Type_mismatch) |}]
;;

let%expect_test "CREATE DELETE RETURN fail test1" =
  piprint g {| CREATE (n)
  DELETE n
  RETURN n |};
  [%expect {|
    (Interpreter_err (Expr_err (Deleted_entity 1))) |}]
;;

let%expect_test "CREATE DELETE RETURN fail test2" =
  piprint g {| CREATE (n)
  DELETE n
  RETURN * |};
  [%expect {|
    (Interpreter_err (Deleted_entity 1)) |}]
;;

let%expect_test "CREATE DELETE WITH RETURN fail test" =
  piprint g {| CREATE (n)
  DELETE n
  WITH n as b
  RETURN 4 |};
  [%expect {|
    (Interpreter_err (Expr_err (Deleted_entity 1))) |}]
;;

let%expect_test "many WITH CREATE DELETE test" =
  piprint
    g
    {| 
  WITH *, 5 AS R
  CREATE (n1)
  CREATE (n2)
  WITH *, n1 AS n
  DELETE n, (n2)
  WITH 4 AS R
  CREATE (n3)
  WITH *
  WITH *, n3 AS n
  DETACH DELETE n
  WITH *
  WITH *
  CREATE (n4)
  DETACH DELETE (n3)
  CREATE (n5{R:R})
  WITH n5 as N
  RETURN N |};
  [%expect {|
    ((5, 0), (4, 0), [[("N", (OutNode (1, [], [("R", (Int64 4L))])))]]) |}]
;;

let init_res =
  parse_and_interpret_request
    g
    {|
  CREATE
  (charlie:Person {name: 'Charlie Sheen'}),
  (martin:Person {name: 'Martin Sheen'}),
  (michael:Person {name: 'Michael Douglas'}),
  (oliver:Person {name: 'Oliver Stone'}),
  (rob:Person {name: 'Rob Reiner'}),
  (wallStreet:Movie:Wallstreet {title: 'Wall Street', year: 2011}),
  (charlie)-[:ACTED_IN {role: 'Bud Fox'}]->(wallStreet),
  (martin)-[:ACTED_IN {role: 'Carl Fox'}]->(wallStreet),
  (michael)-[:ACTED_IN {role: 'Gordon Gekko'}]->(wallStreet),
  (oliver)-[:DIRECTED]->(wallStreet),
  (thePresident:Movie {title: 'The American President'}),
  (martin)-[:ACTED_IN {role: 'A.J. MacInerney'}]->(thePresident),
  (michael)-[:ACTED_IN {role: 'President Andrew Shepherd'}]->(thePresident),
  (rob)-[:DIRECTED]->(thePresident),
  (martin)-[:FATHER_OF]->(charlie)
    |}
;;

let%expect_test "MATCH RETURN (ORDER BY) test1" =
  (match init_res with
   | Ok (g, _) -> piprint g {|
  MATCH (n)
  RETURN n ORDER BY n.name|}
   | Error err -> Stdlib.Format.printf "Init error: %a" pp_req_error err);
  [%expect
    {|
    ((0, 0), (0, 0),
     [[("n", (OutNode (1, ["Person"], [("name", (String "Charlie Sheen"))])))];
       [("n", (OutNode (2, ["Person"], [("name", (String "Martin Sheen"))])))];
       [("n", (OutNode (3, ["Person"], [("name", (String "Michael Douglas"))])))];
       [("n", (OutNode (4, ["Person"], [("name", (String "Oliver Stone"))])))];
       [("n", (OutNode (5, ["Person"], [("name", (String "Rob Reiner"))])))];
       [("n",
         (OutNode (6, ["Movie"; "Wallstreet"],
            [("year", (Int64 2011L)); ("title", (String "Wall Street"))])))
         ];
       [("n",
         (OutNode (7, ["Movie"], [("title", (String "The American President"))])))
         ]
       ]) |}]
;;

let%expect_test "MATCH RETURN (ORDER BY) test2" =
  (match init_res with
   | Ok (g, _) -> piprint g {|
  MATCH ()-[r:ACTED_IN]-()
  RETURN r ORDER BY r.role|}
   | Error err -> Stdlib.Format.printf "Init error: %a" pp_req_error err);
  [%expect
    {|
    ((0, 0), (0, 0),
     [[("r",
        (OutRel (2, (5, ["ACTED_IN"], [("role", (String "A.J. MacInerney"))]), 7
           )))
        ];
       [("r",
         (OutRel (2, (5, ["ACTED_IN"], [("role", (String "A.J. MacInerney"))]), 7
            )))
         ];
       [("r", (OutRel (1, (1, ["ACTED_IN"], [("role", (String "Bud Fox"))]), 6)))
         ];
       [("r", (OutRel (1, (1, ["ACTED_IN"], [("role", (String "Bud Fox"))]), 6)))
         ];
       [("r", (OutRel (2, (2, ["ACTED_IN"], [("role", (String "Carl Fox"))]), 6)))
         ];
       [("r", (OutRel (2, (2, ["ACTED_IN"], [("role", (String "Carl Fox"))]), 6)))
         ];
       [("r",
         (OutRel (3, (3, ["ACTED_IN"], [("role", (String "Gordon Gekko"))]), 6)))
         ];
       [("r",
         (OutRel (3, (3, ["ACTED_IN"], [("role", (String "Gordon Gekko"))]), 6)))
         ];
       [("r",
         (OutRel (3,
            (6, ["ACTED_IN"], [("role", (String "President Andrew Shepherd"))]),
            7)))
         ];
       [("r",
         (OutRel (3,
            (6, ["ACTED_IN"], [("role", (String "President Andrew Shepherd"))]),
            7)))
         ]
       ]) |}]
;;

let%expect_test "MATCH RETURN (ORDER BY) test3" =
  (match init_res with
   | Ok (g, _) ->
     piprint
       g
       {|
  MATCH (p)-[r:ACTED_IN]->()
  RETURN p, r ORDER BY p.name ASC, r.role DESC|}
   | Error err -> Stdlib.Format.printf "Init error: %a" pp_req_error err);
  [%expect
    {|
    ((0, 0), (0, 0),
     [[("p", (OutNode (1, ["Person"], [("name", (String "Charlie Sheen"))])));
        ("r", (OutRel (1, (1, ["ACTED_IN"], [("role", (String "Bud Fox"))]), 6)))
        ];
       [("p", (OutNode (2, ["Person"], [("name", (String "Martin Sheen"))])));
         ("r",
          (OutRel (2, (2, ["ACTED_IN"], [("role", (String "Carl Fox"))]), 6)))
         ];
       [("p", (OutNode (2, ["Person"], [("name", (String "Martin Sheen"))])));
         ("r",
          (OutRel (2, (5, ["ACTED_IN"], [("role", (String "A.J. MacInerney"))]),
             7)))
         ];
       [("p", (OutNode (3, ["Person"], [("name", (String "Michael Douglas"))])));
         ("r",
          (OutRel (3,
             (6, ["ACTED_IN"], [("role", (String "President Andrew Shepherd"))]),
             7)))
         ];
       [("p", (OutNode (3, ["Person"], [("name", (String "Michael Douglas"))])));
         ("r",
          (OutRel (3, (3, ["ACTED_IN"], [("role", (String "Gordon Gekko"))]), 6)))
         ]
       ]) |}]
;;

let%expect_test "MATCH RETURN (ORDER BY) test4" =
  (match init_res with
   | Ok (g, _) -> piprint g {|
  MATCH ()<-[r:ACTED_IN]-()
  RETURN r ORDER BY r.role|}
   | Error err -> Stdlib.Format.printf "Init error: %a" pp_req_error err);
  [%expect
    {|
    ((0, 0), (0, 0),
     [[("r",
        (OutRel (2, (5, ["ACTED_IN"], [("role", (String "A.J. MacInerney"))]), 7
           )))
        ];
       [("r", (OutRel (1, (1, ["ACTED_IN"], [("role", (String "Bud Fox"))]), 6)))
         ];
       [("r", (OutRel (2, (2, ["ACTED_IN"], [("role", (String "Carl Fox"))]), 6)))
         ];
       [("r",
         (OutRel (3, (3, ["ACTED_IN"], [("role", (String "Gordon Gekko"))]), 6)))
         ];
       [("r",
         (OutRel (3,
            (6, ["ACTED_IN"], [("role", (String "President Andrew Shepherd"))]),
            7)))
         ]
       ]) |}]
;;

let%expect_test "MATCH (WHERE) RETURN (ORDER BY) test1" =
  (match init_res with
   | Ok (g, _) ->
     piprint g {|
  MATCH (n) WHERE n.name ENDS WITH "Sheen"
  RETURN n ORDER BY n.name |}
   | Error err -> Stdlib.Format.printf "Init error: %a" pp_req_error err);
  [%expect
    {|
    ((0, 0), (0, 0),
     [[("n", (OutNode (1, ["Person"], [("name", (String "Charlie Sheen"))])))];
       [("n", (OutNode (2, ["Person"], [("name", (String "Martin Sheen"))])))]]) |}]
;;

let%expect_test "MATCH RETURN test1" =
  (match init_res with
   | Ok (g, _) ->
     piprint
       g
       {|
  MATCH (n:Movie{title:"Wall Street"}), (n:Wallstreet{year:2011})
  RETURN * |}
   | Error err -> Stdlib.Format.printf "Init error: %a" pp_req_error err);
  [%expect
    {|
    ((0, 0), (0, 0),
     [[("n",
        (OutNode (6, ["Movie"; "Wallstreet"],
           [("year", (Int64 2011L)); ("title", (String "Wall Street"))])))
        ]
       ]) |}]
;;

let%expect_test "MATCH RETURN test2" =
  (match init_res with
   | Ok (g, _) ->
     piprint g {|
  MATCH (:Person{name:"Martin Sheen"})-[:FATHER_OF]->(c)
  RETURN * |}
   | Error err -> Stdlib.Format.printf "Init error: %a" pp_req_error err);
  [%expect
    {|
    ((0, 0), (0, 0),
     [[("c", (OutNode (1, ["Person"], [("name", (String "Charlie Sheen"))])))]]) |}]
;;

let%expect_test "MATCH RETURN test3" =
  (match init_res with
   | Ok (g, _) ->
     piprint
       g
       {|
  MATCH (:Person{name:"Martin Sheen"})-[:FATHER_OF]->(n)-[r:ACTED_IN]->(m1)
  RETURN n, m1, r.role|}
   | Error err -> Stdlib.Format.printf "Init error: %a" pp_req_error err);
  [%expect
    {|
    ((0, 0), (0, 0),
     [[("n", (OutNode (1, ["Person"], [("name", (String "Charlie Sheen"))])));
        ("m1",
         (OutNode (6, ["Movie"; "Wallstreet"],
            [("year", (Int64 2011L)); ("title", (String "Wall Street"))])));
        ("r.role", (OutConstant (String "Bud Fox")))]
       ]) |}]
;;

let%expect_test "MATCH (WHERE) RETURN (ORDER_BY) test2" =
  (match init_res with
   | Ok (g, _) ->
     piprint
       g
       {|
  MATCH (:Person{name:"Martin Sheen"})-[:FATHER_OF]->()-[:ACTED_IN]->(m1),
  (m2)<-[:ACTED_IN]-(p:Person{name:"Michael Douglas"})
  WHERE m1 <> m2
  RETURN p, m2.title as movietitle ORDER BY movietitle |}
   | Error err -> Stdlib.Format.printf "Init error: %a" pp_req_error err);
  [%expect
    {|
    ((0, 0), (0, 0),
     [[("p", (OutNode (3, ["Person"], [("name", (String "Michael Douglas"))])));
        ("movietitle", (OutConstant (String "The American President")))]
       ]) |}]
;;

let%expect_test "MATCH fail test" =
  (match init_res with
   | Ok (g, _) -> piprint g {|
  MATCH ()-[r]-(), ()-[r]-()
  RETURN * |}
   | Error err -> Stdlib.Format.printf "Init error: %a" pp_req_error err);
  [%expect {|
    (Interpreter_err (Multiple_using_rel_var "r")) |}]
;;
