(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Utils

type vt_error =
  | NotSupported
  | AstError of string
  | ReferenceError of string
  | RangeError of string
  | InternalError of string
  | TypeError of string
  | SyntaxError of string

type value =
  | VNumber of float
  | VString of string
  | VBool of bool
  | VUndefined
  | VNull
  | VFunction of fun_ctx
  | VObject of obj_ctx

and fun_ctx =
  { fun_id : string
  ; args : string list
  ; body : statement
  }

and obj_ctx =
  { obj_id : string
  ; proto : obj_ctx
  ; fields : (string * value) list
  }

type var_ctx =
  { var_id : string
  ; is_const : bool
  ; value : value
  }

type scope =
  | Global of fun_preset list * obj_preset list
  | Function
  | ArrowFunction
  | Object
  | Block

and fun_preset =
  { fun_id : string
  ; body : ctx -> value list -> ctx t
  }

and obj_preset =
  { obj_id : string
  ; fields : fun_preset list
  }

and ctx =
  { parent : ctx option
  ; funs : fun_ctx list
  ; vars : var_ctx list
  ; objs : obj_ctx list
  ; v_return : value
  ; stdout : string
  ; scope : scope
  }
