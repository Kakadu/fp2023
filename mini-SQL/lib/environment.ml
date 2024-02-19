(** Copyright 2023-2024, Zaytsev Dmitriy *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Types
open Utils

(*
   File.csv structure:
   +--------------------------------+
   numeric, real, (types ...)
   column1, column2, (column names ...)
   value, (values ...)
   +--------------------------------+
*)

module Env (M : Utils.MONAD_FAIL) = struct
  open M
  open M.Syntax

  (* Just check & open *)
  let open_file file =
    if Filename.extension file = ".csv" && Sys.file_exists file
    then return (open_in file)
    else fail (IncorrectData file)
  ;;

  let get_csv_channel channel = return (Csv.of_channel ?separator:(Some ',') channel)
  let row (rows : Csv.t) i = return (List.nth rows i)
  let rows_after (rows : Csv.t) i = return (List.filteri (fun id _ -> id > i) rows)

  let column_type_of = function
    | "string" -> return String_Column
    | "numeric" -> return Numeric_Column
    | "real" -> return Real_Column
    | "bool" -> return Boolean_Column
    | x -> fail (UnknownType x)
  ;;

  let to_column_types_list strs = M.all (List.map column_type_of strs)

  let to_columns_list ns ts =
    let init_column n t = { column_name = n; column_type = t } in
    return (List.map2 init_column ns ts)
  ;;

  let to_table ~filename ~columns = { table_name = filename; table_header = columns }

  (* loads table from file.csv *)
  let load_table file =
    let* file_channel = open_file file in
    let* csv_channel = get_csv_channel file_channel in
    let* all_data = return (Csv.input_all csv_channel) in
    let* row1 = row all_data 0 in
    let* row2 = row all_data 1 in
    let* data_rows = rows_after all_data 2 in
    let* ctypes = to_column_types_list row1 in
    let* cols = to_columns_list row2 ctypes in
    return
      { Table.data = Sheet.init ctypes data_rows
      ; Table.meta =
          to_table
            ~filename:(Filename.basename (Filename.remove_extension file))
            ~columns:(Array.of_list cols)
      }
  ;;

  let load_database folder =
    if Sys.is_directory folder
    then
      let* files = return (Sys.readdir folder) in
      if Array.length files > 0
      then (
        let helper =
          List.fold_left (fun acc hd -> acc @ [ load_table (folder ^ "/" ^ hd) ]) []
        in
        let* tables = M.all (helper (Array.to_list files)) in
        return { Database.tables; name = Filename.basename folder })
      else fail (IncorrectData folder)
    else fail (IncorrectData folder)
  ;;
end
