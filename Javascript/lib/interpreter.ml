(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Utils
open Ast
open Parser
open VTypes

let error err =
  uerror
    (match err with
     | NotSupported -> "Some feature is not supported. Interpreter has failed"
     | AstError str -> "Invalid ast was given: " ^ str
     | ReferenceError str -> "ReferenceError: " ^ str
     | RangeError str -> "RangeError: " ^ str
     | InternalError str -> "InternalError: " ^ str
     | TypeError str -> "TypeError: " ^ str
     | SyntaxError str -> "SyntaxError: " ^ str)
;;

let nsup () = error NotSupported

let to_vstring = function
  | VNumber x -> return @@ VString (Float.to_string x)
  | _ -> nsup ()
;;

let print ctx = function
  | [ VNumber x ] -> return { ctx with stdout = ctx.stdout ^ Float.to_string x }
  | _ -> nsup ()
;;

let print_preset = { fun_id = "alert"; body = print }

let const_to_val = function
  | Number x -> VNumber x
;;

let eval_exp ctx = function
  | Const x -> return (ctx, const_to_val x)
  | _ -> nsup ()
;;

let context_init =
  { parent = None
  ; funs = []
  ; vars = []
  ; objs = []
  ; v_return = VUndefined
  ; stdout = ""
  ; scope = Global ([], [])
  }
;;

let create_local_ctx ctx scope =
  { parent = Some ctx
  ; funs = []
  ; vars = []
  ; objs = []
  ; v_return = VUndefined
  ; stdout = ctx.stdout
  ; scope
  }
;;

let parse_stm ctx = function
  | Return x -> eval_exp ctx x >>= fun (ctx, ret) -> return { ctx with v_return = ret }
  | _ -> nsup ()
;;

let parse_stms ctx ast =
  fold_left_s parse_stm (fun ctx -> ctx.v_return != VUndefined) ctx ast
;;

(* let parse_block ctx scope ast = create_local_ctx ctx scope >>= parse_stms ast *)

let interpret_ast ast : (value, string) Result.t =
  match ast with
  | Programm x -> parse_stms context_init x >>| fun ctx -> ctx.v_return
  | _ ->
    error @@ AstError "expect Programm in start of programm, but something else was given"
;;

let interpret input : (value, string) Result.t =
  match parse input with
  | Result.Ok ast -> interpret_ast ast
  | Result.Error (`ParsingError str) -> error (SyntaxError str)
;;
