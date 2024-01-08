(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Utils

type vt_error =
  | NotSupported of string
  | AstError of string
  | ReferenceError of string
  | RangeError of string
  | InternalError of string
  | TypeError of string
  | SyntaxError of string

type fun_ctx =
  { args : string list
  ; body : statement
  }

type scope =
  | Global
  | Function
  | ArrowFunction
  | Block

type obj_type =
  | TFunPreset of (ctx -> value list -> ctx t)
  | TFunction of fun_ctx
  | TArrowFun of fun_ctx
  | TObject

and obj_ctx =
  { proto : obj_ctx
  ; fields : (string * value) list
  ; obj_type : obj_type
  }

and value =
  | VNumber of float
  | VString of string
  | VBool of bool
  | VUndefined
  | VNull
  | VObject of obj_ctx

and let_ctx =
  { var_id : string
  ; is_const : bool
  ; value : value
  }

and ctx =
  { parent : ctx option
  ; vars : let_ctx list
  ; v_return : value
  ; stdout : string
  ; scope : scope
  }
