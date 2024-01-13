open Types

(*
   File.csv structure:
   +--------------------------------+
   NUMERIC, REAL, (types ...)
   column1, column2, (columns ...)
   value, (value ...)
   +--------------------------------+
*)

exception Unknown_type of string
exception Incorrect_resource of string

(* Just check & open *)
let open_file file =
  if Filename.extension file = ".csv" && Sys.file_exists file
  then open_in file
  else raise (Incorrect_resource ("Incorrect file '" ^ file ^ "'"))
;;

let get_csv_channel channel = Csv.of_channel ?separator:(Some ',') channel

let close_channel channel csv_channel =
  Csv.close_in csv_channel;
  Base.close_in channel
;;

let next_row channel =
  let header =
    try Csv.next channel with
    | _ -> []
  in
  header
;;

let column_type_of = function
  | "string" -> String_Column
  | "numeric" -> Numeric_Column
  | "real" -> Real_Column
  | "bool" -> Boolean_Column
  | x -> raise (Unknown_type ("Can't recognize type '" ^ x ^ "'"))
;;

let to_column_types_list strs = List.map column_type_of strs

let to_columns_list ns ts =
  let init_column n t = { column_name = n; column_type = t } in
  List.map2 init_column ns ts
;;

let to_header list = { column_list = list }
let to_table ~filename ~columns = { table_name = filename; table_header = columns }

(* loads table from file.csv *)
let load_table file =
  let file_channel = ref (open_file file) in
  let csv_channel = ref (get_csv_channel file_channel.contents) in
  let next = next_row csv_channel.contents in
  let column_types = ref (to_column_types_list next) in
  let column_names = ref next in
  let columns = ref (to_columns_list column_names.contents column_types.contents) in
  let csv_rows =
    ref (List.filteri (fun i _ -> i > 0) (Csv.input_all csv_channel.contents))
  in
  (* !!! Csv.input_all reads second row, may cause lags *)
  let sheet = Sheet.init column_types.contents csv_rows.contents in
  let table =
    to_table
      ~filename:(Filename.basename (Filename.remove_extension file))
      ~columns:(to_header columns.contents)
  in
  close_channel file_channel.contents csv_channel.contents;
  { Table.data = sheet; Table.meta = table }
;;

let load_database folder =
  if Sys.is_directory folder
  then (
    let files = ref (Sys.readdir folder) in
    if Array.length files.contents > 0
    then (
      let rec helper acc list =
        match list with
        | [] -> acc
        | hd :: tl -> helper (acc @ [ load_table (folder ^ "/" ^ hd) ]) tl
      in
      { Database.tables = helper [] (Array.to_list files.contents)
      ; name = Filename.basename folder
      })
    else raise (Incorrect_resource ("Can't find files in '" ^ folder ^ "'")))
  else raise (Incorrect_resource ("Can't find folder '" ^ folder ^ "'"))
;;
