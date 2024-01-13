(* --- Types with meta-information --- *)

type column_type =
  | String_Column (* STRING *)
  | Numeric_Column (* INT *)
  | Real_Column (* FLOAT *)
  | Boolean_Column (* BOOL *)
[@@deriving show { with_path = false }]

type column =
  { column_name : string
  ; column_type : column_type
  }
[@@deriving show { with_path = false }]

type header = { column_list : column list } [@@deriving show { with_path = false }]

type table =
  { table_name : string
  ; table_header : header
  }
[@@deriving show { with_path = false }]

(* --- Types with tables data --- *)

exception Incorrect_type of string

module Row = struct
  (* array basic element *)
  type item =
    | Numeric of int
    | Real of float
    | String of string
    | Bool of bool
  [@@deriving show { with_path = false }]

  (* row *)
  type t = item array [@@deriving show { with_path = false }]

  let init ts es =
    let item_of e = function
      | String_Column -> String e
      | Numeric_Column ->
        (try Numeric (int_of_string e) with
         | _ -> raise (Incorrect_type ("Value '" ^ e ^ "'\"' should be Numeric")))
      | Real_Column ->
        (try Real (float_of_string e) with
         | _ -> raise (Incorrect_type ("Value '" ^ e ^ "' should be Real")))
      | Boolean_Column ->
        (try Bool (bool_of_string e) with
         | _ -> raise (Incorrect_type ("Value '" ^ e ^ "' should be Boolean")))
    in
    if List.length ts = List.length es && List.length ts > 1
    then Array.init (List.length es) (fun i -> item_of (List.nth es i) (List.nth ts i))
    else
      raise
        (Invalid_argument
           "Types list and strings list have different size or have zero size")
  ;;

  let get_item = Array.get

  let show_row row =
    let s_arr = Array.map show_item row in
    String.concat "|" (Array.to_list s_arr)
  ;;
end

module Sheet = struct
  (* array of rows || 2D elements array *)
  type t = Row.t array

  let init ts es = Array.init (List.length es) (fun i -> Row.init ts (List.nth es i))
  let get_row (rs : t) i = Array.get rs i

  let show_sheet sheet =
    let s_arr = Array.map Row.show_row sheet in
    String.concat "\n" (Array.to_list s_arr)
  ;;
end

module Table = struct
  type t =
    { data : Sheet.t
    ; meta : table
    }

  let name table = table.meta.table_name
  let columns table = table.meta.table_header.column_list

  let find_column_i (table : t) name =
    let rec helper acc name cs =
      match cs with
      | [] -> None
      | hd :: tl -> if hd.column_name = name then Some acc else helper (acc + 1) name tl
    in
    helper 0 name (columns table)
  ;;

  (* faster than with search *)
  let get_column (table : t) ~index = List.nth (columns table) index

  let show_table (table : t) =
    let sep = ref ("+" ^ String.make (String.length (name table)) '-' ^ "+") in
    Format.sprintf
      "%s\n%s\n%s\ncolumns: %s\n\n%s"
      sep.contents
      (" " ^ name table ^ " ")
      sep.contents
      (String.concat ", " (List.map show_column (columns table)))
      (Sheet.show_sheet table.data)
  ;;
end

module Database = struct
  type t =
    { tables : Table.t list
    ; name : string
    }

  let name database = database.name
  let tables database = database.tables

  let find_table_i (base : t) name =
    let rec helper acc name (ts : Table.t list) =
      match ts with
      | [] -> None
      | hd :: tl ->
        if hd.meta.table_name = name then Some acc else helper (acc + 1) name tl
    in
    helper 0 name (tables base)
  ;;

  let get_table database ~index = List.nth database.tables index

  let show_database (base : t) =
    Format.sprintf
      "~ %s ~\n%s"
      base.name
      (String.concat "\n" (List.map Table.show_table base.tables))
  ;;

  (* нужен поиск таблицы по имени и поиск всех колонок *)
end
