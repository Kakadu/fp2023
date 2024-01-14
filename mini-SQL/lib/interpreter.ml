open Types
(* --- TYPES FOR TYPECHECK & INTERPRETATION --- *)

(** extended version of column type *)
type int_column =
  { table_index : int (* we need to store indexes *)
  ; column_index : int
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

(* --- TYPECHECK --- *)

module Eval (M : Utils.MONAD_FAIL) = struct
  open M

  (** parse table name in column string name *)
  let parse_table_name cname =
    if String.contains cname '.'
    then Some (List.nth (String.split_on_char '.' cname) 0)
    else None
  ;;

  (** filter tables by column name *)
  let find_table_by_column (base : Database.t) cname =
    List.filter
      (fun x -> Option.is_some (Table.find_column_i x cname))
      (Database.tables base)
  ;;

  (** Find table by column name in database *)
  let find_table_id (base : Database.t) name =
    let index_of table =
      match Database.find_table_i base table with
      | None -> fail ("Can't find table with column '" ^ name ^ "'")
      | Some id -> return id
    in
    match parse_table_name name with
    | Some table -> index_of table (* if we can parse table name from string *)
    | None ->
      (match find_table_by_column base name with
       | [ x ] -> index_of (Table.name x)
       | _ -> fail ("Can't determine which table a column '" ^ name ^ "' belongs to"))
  ;;

  (** Transform column name to int_column *)
  let transform_column (base : Database.t) name =
    let get_table_i = find_table_id base name in
    let get_table i = return (Database.get_table base ~index:i) in
    let get_column_i t =
      match Table.find_column_i t name with
      | Some x -> return x
      | None -> fail ("Can't find table with column " ^ name ^ "'")
    in
    let column colid t = return (Table.get_column t ~index:colid) in
    let meta col colid ti = { table_index = ti; column_index = colid; meta = col } in
    get_table_i
    >>= fun ti ->
    get_table ti
    >>= fun t ->
    get_column_i t
    >>= fun colid -> column colid t >>= fun col -> return (meta col colid ti)
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
    | Ast.Asterisk -> fail "I'm too lazy to support asterisk (TODO)"
    | Ast.Expression e -> transform_ast_expression base e
  ;;

  let transform_join_type (e : expr) = function
    | Ast.Inner -> return (Inner e)
    | Ast.Left -> return (Left e)
    | Ast.Right -> return (Right e)
    | Ast.Full -> return (Full e)
  ;;

  (* ----- SQL Query Operator Tree ----- *)

  type qot_node =
    | Load of Table.t (* load tables from database *)
    | Join of qot_node * qot_node * join_type (* JOIN, combines several nodes *)
    | Restrict of qot_node * expr (* WHERE ( node + condition ) *)
    | Project of qot_node * expr list (* SELECT ( node + columns ) *)

  let load_table (base : Database.t) tname =
    let get_id t =
      match Database.find_table_i base t with
      | Some x -> return x
      | None -> fail ("Can't load table '" ^ tname ^ "' from database '" ^ base.name ^ "'")
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
    >>= fun select_exprs -> M.all select_exprs 
    >>= fun exprs ->
      return (Project(restrict_node, exprs))
  ;;

  let execute_join (base : Database.t) left right jtype : (Table.t, string) t =
    

  (** Execute SQL Query Operator Tree *)
  let execute (base : Database.t) (node: qot_node) : (Table.t, string) t = match node with
  | Load t -> return t
  | Join (left, right, jtype) -> execute_join left right jtype
  (* | Restrict (node, expr) -> ...
  | Project (node, expr_list) -> *)
  

  (* let eval (base_folder : string) (request: Ast.request) : (Table.t, string) t *)
end
