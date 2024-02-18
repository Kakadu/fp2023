(** Copyright 2023-2024, Zaytsev Dmitriy *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Types
open Utils

type int_column =
  { column_index : int
  ; meta : column (* meta from Types.Column *)
  }
[@@deriving show { with_path = false }]

(** Expression type for executing *)
type expr =
  | Const of item (* value types I define in Types.ml *)
  | Col of int_column
  | Plus of expr * expr
  | Minus of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr (* % *)
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Equal of expr * expr (* = *)
  | NEqual of expr * expr (* != *)
  | GThan of expr * expr (* > *)
  | LThan of expr * expr (* < *)
  | GThanEq of expr * expr (* >= *)
  | LThanEq of expr * expr (* <= *)
[@@deriving show { with_path = false }]

type join_type =
  | Inner of expr (** ( INNER JOIN ) *)
  | Left of expr (** ( LEFT JOIN ) *)
  | Right of expr (** ( RIGHT JOIN ) *)
  | Full of expr (** ( FULL JOIN ) *)
[@@deriving show { with_path = false }]

type qot_node =
  | Load of Table.t (* load tables from database *)
  | Join of qot_node * qot_node * join_type (* JOIN, combines several nodes *)
  | Restrict of qot_node * expr (* WHERE ( node + condition ) *)
  | Project of qot_node * expr list (* SELECT ( node + columns ) *)
[@@deriving show { with_path = false }]

module Eval (M : Utils.MONAD_FAIL) = struct
  open M

  (** --- Transform AST into new types --- *)

  (** filter tables by column name *)
  let find_table_by_column (base : Database.t) cname =
    let index_of table cname =
      match Table.find_column_i table cname with
      | [ x ] -> Some x
      | _ -> None
    in
    List.filter (fun x -> Option.is_some (index_of x cname)) (Database.tables base)
  ;;

  (** Find table by column name in database *)
  let find_table_id (base : Database.t) name =
    let index_of table =
      match Database.find_table_i base table with
      | None -> fail (UnknownTable table)
      | Some id -> return id
    in
    match find_table_by_column base name with
    | [ x ] -> index_of (Table.name x)
    | _ -> fail (UnknownColumn name)
  ;;

  (** Transform column name to int_column *)
  let transform_column (base : Database.t) name =
    let get_table_i = find_table_id base name in
    let get_table i = return (Database.get_table base ~index:i) in
    let get_column_i t =
      match Table.find_column_i t name with
      | [ x ] -> return x
      | _ -> fail (UnknownColumn name)
    in
    let column colid t = return (Table.get_column t ~index:colid) in
    get_table_i
    >>= fun ti ->
    get_table ti
    >>= fun t ->
    get_column_i t
    >>= fun colid ->
    column colid t
    >>= fun col -> return { column_index = colid; meta = { col with column_name = name } }
  ;;

  (** tranform Ast.value into Interpreter.value *)
  let transform_value (base : Database.t) = function
    (* column *)
    | Ast.Name x -> transform_column base x >>= fun r -> return (Col r) (* TODO *)
    (* values *)
    | Ast.String x -> return (Const (String x))
    | Ast.Digit x -> return (Const (Numeric x))
    | Ast.Float_Digit x -> return (Const (Real x))
    | Ast.Bool x -> return (Const (Bool x))
  ;;

  (** tranform Ast.bin_op into Interpreter.expr *)
  let transform_binary_operation e1 e2 = function
    | Ast.Add -> Plus (e1, e2)
    | Ast.Substract -> Minus (e1, e2)
    | Ast.Multiply -> Mul (e1, e2)
    | Ast.Divide -> Div (e1, e2)
    | Ast.Modulo -> Mod (e1, e2)
    | Ast.And -> And (e1, e2)
    | Ast.Or -> Or (e1, e2)
    | Ast.Equal -> Equal (e1, e2)
    | Ast.Not_Equal -> NEqual (e1, e2)
    | Ast.Greater_Than -> GThan (e1, e2)
    | Ast.Less_Than -> LThan (e1, e2)
    | Ast.Less_Than_Or_Equal -> LThanEq (e1, e2)
    | Ast.Greater_Than_Or_Equal -> GThanEq (e1, e2)
  ;;

  (** tranform Ast.un_op into Interpreter.expr *)
  let transform_unary_operation e = function
    | Ast.Not -> Not e
  ;;

  (** tranform Ast.expr into Interpreter.expr *)
  let rec transform_ast_expression (base : Database.t) = function
    | Ast.Const v -> transform_value base v
    | Ast.Binary_operation (op, e1, e2) ->
      transform_ast_expression base e1
      >>= fun r1 ->
      transform_ast_expression base e2
      >>= fun r2 -> return (transform_binary_operation r1 r2 op)
    | Ast.Unary_operation (op, e) ->
      transform_ast_expression base e >>= fun r -> return (transform_unary_operation r op)
  ;;

  let transform_select_statement (base : Database.t) = function
    | Ast.Asterisk ->
      fail (NotImplementedYet "using an asterisk instead of a column name")
    | Ast.Expression e -> transform_ast_expression base e
  ;;

  let transform_join_type (e : expr) = function
    | Ast.Inner -> return (Inner e)
    | Ast.Left -> return (Left e)
    | Ast.Right -> return (Right e)
    | Ast.Full -> return (Full e)
  ;;

  (* ----- SQL Query Operator Tree ----- *)

  let load_table (base : Database.t) tname =
    let get_id t =
      match Database.find_table_i base t with
      | Some x -> return x
      | None -> fail (UnknownTable tname)
    in
    get_id tname >>= fun id -> return (Database.get_table base ~index:id)
  ;;

  (* Join & Load *)
  (** Transform from Ast.from_statement to qot_node *)
  let rec transform_from_statement (base : Database.t) = function
    | Ast.Table name -> load_table base name >>= fun res -> return (Load res)
    | Ast.Join st ->
      transform_from_statement base st.left
      >>= fun left_node ->
      load_table base st.table
      >>= fun table ->
      transform_ast_expression base st.on
      >>= fun ex ->
      transform_join_type ex st.jtype
      >>= fun t -> return (Join (left_node, Load table, t))
  ;;

  (** Transform request to qot_node (now we can execute request) *)
  let transform_request (base : Database.t) (req : Ast.request) =
    let transform_where (where : Ast.expr option) =
      match where with
      | Some x -> transform_ast_expression base x
      | None -> return (Const (Bool true))
      (* just default value (in case we haven't WHERE) *)
    in
    transform_from_statement base req.from
    >>= fun join_node ->
    transform_where req.where
    >>= fun where_expr ->
    return (Restrict (join_node, where_expr))
    >>= fun restrict_node ->
    return (List.map (transform_select_statement base) req.select)
    >>= fun select_exprs ->
    M.all select_exprs >>= fun exprs -> return (Project (restrict_node, exprs))
  ;;

  (** --- Expressions computation --- *)

  open Typecheck.Exec (M)

  (* Execute expr for cur row *)
  let rec row_expr_exec (sheet : Sheet.t) (i : int) expr =
    let run expr = row_expr_exec sheet i expr in
    match expr with
    | Const x -> return x
    | Col col -> return (Row.get_item (Sheet.get_row sheet i) col.column_index)
    | Plus (x, y) -> run x >>= fun x -> run y >>= fun y -> x #+ y
    | Minus (x, y) -> run x >>= fun x -> run y >>= fun y -> x #- y
    | Mul (x, y) -> run x >>= fun x -> run y >>= fun y -> x #* y
    | Div (x, y) -> run x >>= fun x -> run y >>= fun y -> x #/ y
    | Mod (x, y) -> run x >>= fun x -> run y >>= fun y -> x #% y
    | Equal (x, y) ->
      run x >>= fun x -> run y >>= fun y -> x #= y >>= fun res -> return (Bool res)
    | NEqual (x, y) ->
      run x >>= fun x -> run y >>= fun y -> x #!= y >>= fun res -> return (Bool res)
    | GThan (x, y) ->
      run x >>= fun x -> run y >>= fun y -> x #> y >>= fun res -> return (Bool res)
    | GThanEq (x, y) ->
      run x >>= fun x -> run y >>= fun y -> x #>= y >>= fun res -> return (Bool res)
    | LThan (x, y) ->
      run x >>= fun x -> run y >>= fun y -> x #< y >>= fun res -> return (Bool res)
    | LThanEq (x, y) ->
      run x >>= fun x -> run y >>= fun y -> x #<= y >>= fun res -> return (Bool res)
    | And (x, y) ->
      run x
      >>= fun x ->
      run y
      >>= fun y ->
      bool_of_item x >>= fun x -> bool_of_item y >>= fun y -> return (Bool (x && y))
    | Or (x, y) ->
      run x
      >>= fun x ->
      run y
      >>= fun y ->
      bool_of_item x >>= fun x -> bool_of_item y >>= fun y -> return (Bool (x || y))
    | Not x -> run x >>= fun x -> bool_of_item x >>= fun x -> return (Bool (not x))
  ;;

  (* run expr for all rows and return new sheet with true exprs *)
  let exec_expr (sheet : Sheet.t) (ex : expr) =
    let rec rows_runner (acc : Row.t list) (max_i : int) (i : int) =
      match i with
      | i when i > max_i -> return acc (* end of column *)
      | i ->
        row_expr_exec sheet i ex
        >>= fun res ->
        bool_of_item res
        >>= fun res ->
        if res
        then rows_runner (List.append acc [ Sheet.get_row sheet i ]) max_i (i + 1)
        else rows_runner acc max_i (i + 1)
    in
    rows_runner [] (Sheet.column_length sheet - 1) 0
  ;;

  (* indexes may be incorrect *)
  let rec update_column_indexes (table : Table.t) (expr : expr) =
    let run expr = update_column_indexes table expr in
    let get_id col =
      match Table.find_column_i table col.meta.column_name with
      | [ x ] -> return x
      | _ -> fail (UnknownColumn col.meta.column_name)
    in
    match expr with
    | Const x -> return (Const x)
    | Col col -> get_id col >>= fun id -> return (Col { col with column_index = id })
    | Plus (e1, e2) -> run e1 >>= fun r1 -> run e2 >>= fun r2 -> return (Plus (r1, r2))
    | Minus (e1, e2) -> run e1 >>= fun r1 -> run e2 >>= fun r2 -> return (Minus (r1, r2))
    | Mul (e1, e2) -> run e1 >>= fun r1 -> run e2 >>= fun r2 -> return (Mul (r1, r2))
    | Div (e1, e2) -> run e1 >>= fun r1 -> run e2 >>= fun r2 -> return (Div (r1, r2))
    | Mod (e1, e2) -> run e1 >>= fun r1 -> run e2 >>= fun r2 -> return (Mod (r1, r2))
    | And (e1, e2) -> run e1 >>= fun r1 -> run e2 >>= fun r2 -> return (And (r1, r2))
    | Or (e1, e2) -> run e1 >>= fun r1 -> run e2 >>= fun r2 -> return (Or (r1, r2))
    | Not e1 -> run e1 >>= fun r1 -> return (Not r1)
    | Equal (e1, e2) -> run e1 >>= fun r1 -> run e2 >>= fun r2 -> return (Equal (r1, r2))
    | NEqual (e1, e2) ->
      run e1 >>= fun r1 -> run e2 >>= fun r2 -> return (NEqual (r1, r2))
    | GThan (e1, e2) -> run e1 >>= fun r1 -> run e2 >>= fun r2 -> return (GThan (r1, r2))
    | LThan (e1, e2) -> run e1 >>= fun r1 -> run e2 >>= fun r2 -> return (LThan (r1, r2))
    | GThanEq (e1, e2) ->
      run e1 >>= fun r1 -> run e2 >>= fun r2 -> return (GThanEq (r1, r2))
    | LThanEq (e1, e2) ->
      run e1 >>= fun r1 -> run e2 >>= fun r2 -> return (LThanEq (r1, r2))
  ;;

  let exec_join (table1 : Table.t) (table2 : Table.t) (jtype : join_type) =
    let filter (sheet : Sheet.t) (ex : expr) =
      exec_expr sheet ex >>= fun list -> return (Array.of_list list)
    in
    let replace_sheet (sheet : Sheet.t) (table : Table.t) =
      { Table.data = sheet; Table.meta = table.meta }
    in
    let join t1 t2 = return (Table.join t1 t2) in
    let update new_table expr = update_column_indexes new_table expr in
    let run t1 t2 ex =
      join t1 t2
      >>= fun new_table ->
      update new_table ex
      >>= fun expr ->
      filter new_table.data expr >>= fun res -> return (replace_sheet res new_table)
    in
    match jtype with
    | Full _ -> join table1 table2 >>= fun new_table -> return new_table
    | Inner expr -> run table1 table2 expr >>= fun res -> return res
    | Left expr ->
      run table1 table2 expr
      >>= fun gen_table ->
      return
        (Table.join
           table1
           (Table.sub_width
              gen_table
              (Table.row_length table1 - 1)
              (Table.row_length gen_table - Table.row_length table1 - 1)))
    | Right expr ->
      run table1 table2 expr
      >>= fun gen_table ->
      return
        (Table.join (Table.sub_width gen_table 0 (Table.row_length table1 - 1)) table2)
  ;;

  let exec_restrict (table : Table.t) (expr : expr) =
    update_column_indexes table expr
    >>= fun expr ->
    exec_expr table.data expr
    >>= fun res -> return { Table.data = Array.of_list res; Table.meta = table.meta }
  ;;

  let exec_project (table : Table.t) (cols : expr list) =
    let expr_to_col = function
      | Col x -> return x
      | x -> fail (UnknownColumn (show_expr x))
    in
    let to_cols_list table cols =
      M.all
        (List.map
           (fun el -> update_column_indexes table el >>= fun expr -> expr_to_col expr)
           cols)
    in
    let rec helper (acc_table : Table.t) cols table =
      match cols with
      | [] -> return acc_table
      | hd :: tl -> helper (Table.join acc_table (Table.sub_width table hd 1)) tl table
    in
    to_cols_list table cols
    >>= fun cols ->
    helper
      (Table.empty (Table.column_length table))
      (List.map (fun el -> el.column_index) cols)
      table
    >>= fun res_table -> return (Table.change_table_name res_table "result table")
  ;;

  let rec execute node =
    match node with
    | Load table -> return table
    | Join (left, right, jtype) ->
      execute left
      >>= fun left -> execute right >>= fun right -> exec_join left right jtype
    | Restrict (node, expr) -> execute node >>= fun table -> exec_restrict table expr
    | Project (node, exprs) -> execute node >>= fun table -> exec_project table exprs
  ;;

  open Environment.Env (M)

  let eval (base_folder : string) (request : Ast.request) : (Table.t, error) t =
    let get_node base request = transform_request base request in
    load_database base_folder
    >>= fun base -> get_node base request >>= fun node -> execute node
  ;;
end
