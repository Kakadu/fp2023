(* --- Types with meta-information (name + type) --- *)

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

type header = column array [@@deriving show { with_path = false }]

type table =
  { table_name : string
  ; table_header : header
  }
[@@deriving show { with_path = false }]

(* --- Types with tables data --- *)

exception Incorrect_type of string

(** Represents basic element in row *)
type item =
  | Numeric of int
  | Real of float
  | String of string
  | Bool of bool
[@@deriving show { with_path = false }]

(** Row in a table *)
module Row = struct
  type t = item array [@@deriving show { with_path = false }]

  (** Create row from types and elements *)
  let init ts es =
    let item_of e = function
      | String_Column -> String e
      | Numeric_Column ->
        (try Numeric (int_of_string e) with
         | _ -> raise (Incorrect_type ("Value '" ^ e ^ "' should be Numeric")))
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
  let join (a : t) (b : t) = Array.append a b
  let sub = Array.sub
  let empty : t = [||]

  let show_row row =
    let s_arr = Array.map show_item row in
    String.concat "|" (Array.to_list s_arr)
  ;;
end

(** Array of rows, contain data from csv files *)
module Sheet = struct
  type t = Row.t array [@@deriving show { with_path = false }]

  let init ts es = Array.init (List.length es) (fun i -> Row.init ts (List.nth es i))
  let get_row (rs : t) i = Array.get rs i
  let column_length (sheet : t) = Array.length sheet
  let row_length (sheet : t) = Array.length (Array.get sheet 0)
  let join (a : t) (b : t) = Array.mapi (fun i el -> Row.join el (Array.get b i)) a
  let empty length : t = Array.init length ( fun _ -> Row.empty)

  (* обрезать каждый ряд *)
  let sub_width (s : t) (from_id : int) (len : int) =
    Array.map (fun row -> Row.sub row from_id len) s
  ;;

  let show_sheet sheet =
    let s_arr = Array.map Row.show_row sheet in
    String.concat "\n" (Array.to_list s_arr)
  ;;
end

(** Contain meta-information and sheet with data *)
module Table = struct
  type t =
    { data : Sheet.t
    ; meta : table
    }
  [@@deriving show { with_path = false }]

  let name table = table.meta.table_name
  let columns table = table.meta.table_header

  let empty len =
    { data = Sheet.empty len; meta = { table_name = "new table"; table_header = [||] } }
  ;;

  let join (a : t) (b : t) =
    { data = Sheet.join a.data b.data
    ; meta =
        { table_name = "new table"
        ; table_header = Array.append a.meta.table_header b.meta.table_header
        }
    }
  ;;

  let column_length (t : t) = Sheet.column_length t.data
  let row_length (t : t) = Sheet.row_length t.data

  let sub_width (s : t) (from_id : int) (len : int) =
    { data = Sheet.sub_width s.data from_id len
    ; meta =
        { table_name = s.meta.table_name
        ; table_header = Array.sub s.meta.table_header from_id len
        }
    }
  ;;

  (** Find column index *)
  let find_column_i (table : t) name =
    let rec helper acc name cs =
      match cs with
      | [] -> None
      | hd :: tl -> if hd.column_name = name then Some acc else helper (acc + 1) name tl
    in
    helper 0 name (Array.to_list (columns table))
  ;;

  (** get column by index *)
  let get_column (table : t) ~index = Array.get (columns table) index

  let show_table (table : t) =
    let sep = ref ("+" ^ String.make (String.length (name table)) '-' ^ "+") in
    Format.sprintf
      "%s\n%s\n%s\ncolumns: %s\n\n%s"
      sep.contents
      (" " ^ name table ^ " ")
      sep.contents
      (String.concat ", " (Array.to_list (Array.map show_column (columns table))))
      (Sheet.show_sheet table.data)
  ;;
end

(** Contain meta-information and some tables *)
module Database = struct
  type t =
    { tables : Table.t list
    ; name : string
    }
  [@@deriving show { with_path = false }]

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
end
