(** Copyright 2023-2024, Zaytsev Dmitriy *)

(** SPDX-License-Identifier: CC0-1.0 *)

type column_type =
  | String_Column
  | Numeric_Column
  | Real_Column
  | Boolean_Column

type column =
  { column_name : string
  ; column_type : column_type
  }

type header = column array

type table =
  { table_name : string
  ; table_header : header
  }

(** Represents basic element in row *)
type item =
  | Numeric of int
  | Real of float
  | String of string
  | Bool of bool

(** Row in a table *)
module Row : sig
  type t = item array

  val init : column_type list -> string list -> item array
  val get_item : 'a array -> int -> 'a
  val join : t -> t -> item array
  val sub : 'a array -> int -> int -> 'a array
  val empty : t
  val show_row : item array -> string
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

(** Array of rows, contain data from csv files *)
module Sheet : sig
  type t = Row.t array

  val init : column_type list -> string list list -> item array array
  val get_row : t -> int -> Row.t
  val column_length : t -> int
  val row_length : t -> int
  val join : t -> t -> item array array
  val empty : int -> t
  val sub_width : t -> int -> int -> item array array
  val show_sheet : item array array -> string
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

(** Contain meta-information and sheet with data *)
module Table : sig
  type t =
    { data : Sheet.t
    ; meta : table
    }

  val name : t -> string
  val columns : t -> header
  val empty : int -> t
  val join : t -> t -> t
  val column_length : t -> int
  val row_length : t -> int
  val sub_width : t -> int -> int -> t
  val change_table_name : t -> string -> t
  val find_column_i : t -> string -> int list
  val get_column : t -> index:int -> column
  val show_table : t -> string
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

(** Contain meta-information and some tables *)
module Database : sig
  type t =
    { tables : Table.t list
    ; name : string
    }

  val name : t -> string
  val tables : t -> Table.t list
  val find_table_i : t -> string -> int option
  val get_table : t -> index:int -> Table.t
  val show_database : t -> string
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

val pp_column_type : Format.formatter -> column_type -> unit
val show_column_type : column_type -> string
val pp_column : Format.formatter -> column -> unit
val show_column : column -> string
val pp_header : Format.formatter -> header -> unit
val show_header : header -> string
val pp_table : Format.formatter -> table -> unit
val show_table : table -> string
val pp_item : Format.formatter -> item -> unit
val show_item : item -> string
