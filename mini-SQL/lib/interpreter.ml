open Types
(* --- TYPES FOR TYPECHECK & INTERPRETATION --- *)

(* extended version of column type from Types module *)
(* we need to store indexes (perfomance) *)
type int_column =
  { table_index : int
  ; column_index : int
  ; meta : column
  }
[@@deriving show { with_path = false }]

type value =
  | VInt of int
  | VFloat of float
  | VString of string
  | VBool of bool
[@@deriving show { with_path = false }]

type expr =
  | Const of value
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

(* --- TYPECHECK --- *)

(* interactions with the environment *)

module Eval (M : Utils.MONAD_FAIL) = struct
  open M

  let load_database = Environment.load_database

  (** parse table name in column string name *)
  let parse_table_name name =
    if String.contains name '.'
    then Some (List.nth (String.split_on_char '.' name) 0)
    else None
  ;;

  (** filter tables by column name *)
  let find_table_by_column (base : Database.t) (name : string) =
    List.filter
      (fun x -> Option.is_some (Table.find_column_i x name))
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

  let transform_value (base : Database.t) = function
    (* column *)
    | Ast.Name x -> transform_column base x >>= fun r -> return (Col r) (* TODO *)
    (* values *)
    | Ast.String x -> return (Const (VString x))
    | Ast.Digit x -> return (Const (VInt x))
    | Ast.Float_Digit x -> return (Const (VFloat x))
    | Ast.Bool x -> return (Const (VBool x))
  ;;

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
end
