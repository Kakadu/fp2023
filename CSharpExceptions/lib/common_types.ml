(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type code_ident = Code_ident of ident [@@deriving show { with_path = false }, eq]

module Code_id = struct
  type t = code_ident

  let compare = compare
end

module Code_Map = Stdlib.Map.Make (Code_id)

module Ident = struct
  type t = ident

  let compare = compare
end

module Ident_Map = Stdlib.Map.Make (Ident)

type address = Link of int [@@deriving show { with_path = false }, eq]

let incr_ (Link ad) = Link (ad + 1)
let ln x = Link x

module Mem_Address = struct
  type t = address

  let compare = compare
end

module Mem_Map = Stdlib.Map.Make (Mem_Address)

type internal_address = ILink of int [@@deriving show { with_path = false }, eq]

let i_incr_ (ILink ad) = ILink (ad + 1)
let i_ln x = ILink x

module Intern_Mem_Address = struct
  type t = internal_address

  let compare = compare
end

module Intern_Mem = Stdlib.Map.Make (Intern_Mem_Address)

type base_lib_id = Fl_descriptors

module Sys_id = struct
  type t = base_lib_id

  let compare = compare
end

module Sys_Map = Stdlib.Map.Make (Sys_id)

type file_st = W_File of out_channel
type sys_mem_el = Files of (internal_address * file_st Intern_Mem.t)
type sys_memory = sys_mem_el Sys_Map.t

type code_ctx =
  | Exception_ctx of class_decl
  | Class_ctx of class_decl

let get_class_decl = function
  | Exception_ctx exc -> exc
  | Class_ctx cl -> cl
;;

type text = code_ctx Code_Map.t
