type column_type =
  | String_Column (* STRING *)
  | Numeric_Column (* INT *)
  | Real_Column (* FLOAT *)
  | Boolean_Column (* BOOL *)
[@@deriving show { with_path = false }]

(* --- Types with meta-information --- *)

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
  let get_column rs i = Array.get rs i

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

  let find_index =
    let rec helper acc name cs =
      match cs with
      | [] -> raise (Failure ("Can't find column '" ^ name ^ "'"))
      | hd :: tl -> if hd.column_name = name then acc else helper (acc + 1) name tl
    in
    helper 0
  ;;

  (* faster than with search *)
  let get_column table ~index = Sheet.get_column table.data index
end

module Database = struct
  type t =
    { tables : Table.t list
    ; name : string
    }

  let name database = database.name
  let tables database = database.tables
end
