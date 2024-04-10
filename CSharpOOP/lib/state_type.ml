(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module Name = struct
  type t = name

  let compare = compare
end

module MapName = Map.Make (Name)

type adr = Adr of int [@@deriving show { with_path = false }]

module Adr = struct
  type t = adr

  let compare = compare
end

module MapAdr = Map.Make (Adr)

type obj_content =
  | VType of var_type
  | Method of methods
  | Field of field
  | Constructor of constructor
[@@deriving show { with_path = false }, eq]

type context =
  | TC_Class of obj_class
  | TC_Interface of interface * obj_content MapName.t

module TypeCheck = struct
  type global_env = context MapName.t
  type local_env = obj_content MapName.t
  type cur_class_name = name
  type class_with_main_method = name

  type st_type_check =
    global_env
    * local_env
    * cur_class_name option
    * meth_type option
    * class_with_main_method option
end

module St_Interpreter = struct
  type context =
    | Int_Class of obj_class
    | Int_Interface of interface
  [@@deriving show { with_path = false }]

  type idx = Idx of int [@@deriving show { with_path = false }]

  type code =
    | Constructor of constructor * statement
    | Method of methods * statement
  [@@deriving show { with_path = false }]

  type el =
    | IClass of adr
    | IValue of value
  [@@deriving show { with_path = false }]

  type vl =
    | Init of el
    | Not_init
  [@@deriving show { with_path = false }]

  type local_el =
    | Code of code
    | Value of vl * idx option

  type local_env = idx (* new idx *) * local_el MapName.t

  type obj =
    { mems : (field * vl) MapName.t
    ; cl_name : name
    ; p_adr : adr option
    ; inh_adr : adr option
    }

  type memory = adr * obj MapAdr.t
  type local_adr = adr
  type global_env = context MapName.t
  type st_interpreter = global_env * local_env * local_adr * memory
end
