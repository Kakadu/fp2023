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

type value =
  | VNumber of float
  | VString of string
  | VBool of bool
  | VUndefined
  | VNull
  | VObject of int

type fun_ctx =
  { parent_lex_env : int
  ; args : string list
  ; body : statement
  }

type scope =
  | Function
  | ArrowFunction
  | Block

type let_ctx =
  { var_id : string
  ; is_const : bool
  ; value : value
  }

type proto_objs =
  { proto_obj : int
  ; proto_fun : int
  ; proto_array : int
  }

type lexical_env =
  { parent : int option (*lex env where fun init*)
  ; creater : int option (*lex env where fun call, in block creater = parent*)
  ; vars : let_ctx list
  ; scope : scope
  }

and obj_type =
  | TFunPreset of (ctx -> value list -> int option -> (ctx * value option) t)
  | TFunction of fun_ctx
  | TArrowFunction of fun_ctx
  | TArray of value option list
  | TObject

and obj_ctx =
  { proto : value
  ; fields : let_ctx list
  ; obj_type : obj_type
  }

and ctx =
  { lex_env_count : int
  ; cur_lex_env : int
  ; lex_envs : lexical_env IntMap.t
  ; obj_count : int
  ; objs : obj_ctx IntMap.t
  ; proto_objs : proto_objs
  ; vreturn : value option
  ; stdout : string
  }
