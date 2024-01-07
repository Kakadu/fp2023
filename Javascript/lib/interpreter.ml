(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Utils
open Ast
open Parser
open VTypes

let is_func x =
  match x.obj_type with
  | TFunPreset _ | TFunction _ | TArrowFun _ -> true
  | _ -> false
;;

let strip_dot str =
  match String.split_on_char '.' str with
  | n :: [ "" ] -> n
  | _ -> str
;;

(*JS use diffrent conversion to string in .toString and in print.
  It's the reason why vvalues_to_str and to_vstring is diffrent functions*)
let vvalues_to_str = function
  | VNumber x -> strip_dot @@ Float.to_string x
  | VBool true -> "true"
  | VBool false -> "false"
  | VNull -> "null"
  | VUndefined -> "undefined"
  | VString x -> x
  | _ -> "Cannot convert to string"
;;

let print_val = function
  | VNumber _ -> "number"
  | VString _ -> "string"
  | VBool _ -> "boolean"
  | VUndefined -> "undefined"
  | VNull -> "null"
  | VObject x when is_func x -> "function"
  | VObject _ -> "object"
;;

let error err =
  uerror
    (match err with
     | NotSupported str ->
       if str = ""
       then "Some feature is not supported. Interpreter has failed"
       else "Feature " ^ str ^ " is not supported"
     | AstError str -> "Invalid ast was given: " ^ str
     | ReferenceError str -> "ReferenceError: " ^ str
     | RangeError str -> "RangeError: " ^ str
     | InternalError str -> "InternalError: " ^ str
     | TypeError (str1, str2) ->
       "TypeError: expect " ^ str1 ^ ", but " ^ str2 ^ " was given"
     | SyntaxError str -> "SyntaxError: " ^ str)
;;

let nsup str = error (NotSupported str)

let to_vstring =
  let ret_vstr x = return (VString x) in
  function
  | VString x -> ret_vstr x
  | VNumber x -> ret_vstr (strip_dot @@ Float.to_string x)
  | VBool true -> ret_vstr "true"
  | VBool false -> ret_vstr "false"
  | VNull -> ret_vstr "null"
  | VUndefined -> ret_vstr "undefined"
  | VObject x when is_func x -> nsup "conversion func to str"
  | VObject _ -> ret_vstr "[object Object]"
  | _ -> nsup "" <?> "error in string conversion"
;;

let to_vbool ast =
  return
  @@ VBool
       (match ast with
        | VNull | VUndefined -> false
        | VBool x -> x
        | VNumber x -> x != nan && x != 0.
        | VString x when String.trim x = "" -> false
        | _ -> true)
;;

let to_vnumber ast =
  return
  @@ VNumber
       (match ast with
        | VNumber x -> x
        | VString x ->
          (match String.trim x with
           | "" -> 0.
           | _ as x ->
             (match float_of_string_opt x with
              | Some x -> x
              | _ -> nan))
        | VBool x -> Bool.to_float x
        | VNull -> 0.
        | _ -> nan)
;;

let print ctx = function
  | [ VNumber x ] -> return { ctx with stdout = ctx.stdout ^ Float.to_string x }
  | _ -> nsup ""
;;

let const_to_val = function
  | Number x -> VNumber x
  | String x -> VString x
  | Bool x -> VBool x
  | Undefined -> VUndefined
  | Null -> VNull
;;

let op_with_num op a b =
  to_vnumber a >>= fun (VNumber x) -> to_vnumber b >>| fun (VNumber y) -> VNumber (op x y)
;;

let op_with_string op a b =
  to_vstring a >>= fun (VString x) -> to_vstring b >>| fun (VString y) -> VString (op x y)
;;

let add a b =
  let is_to_string = function
    | VString _ | VObject _ -> true
    | _ -> false
  in
  if is_to_string a || is_to_string b
  then op_with_string ( ^ ) a b
  else op_with_num ( +. ) a b
;;

let eval_bin_op ctx op a b =
  let add_ctx op = op >>| fun op -> ctx, op in
  match op with
  | Add -> add_ctx @@ add a b <?> "error in add operator"
  | _ -> nsup ""
;;

let rec eval_exp ctx = function
  | Const x -> return (ctx, const_to_val x)
  | BinOp (op, a, b) ->
    eval_exp ctx a
    >>= fun (ctx, x) -> eval_exp ctx b >>= fun (ctx, y) -> eval_bin_op ctx op x y
  | _ -> nsup ""
;;

let context_init =
  { parent = None; vars = []; v_return = VUndefined; stdout = ""; scope = Global }
;;

let create_local_ctx ctx scope =
  { parent = Some ctx; vars = []; v_return = VUndefined; stdout = ctx.stdout; scope }
;;

let parse_stm ctx = function
  | Return x ->
    eval_exp ctx x
    <?> "error in return expression"
    >>= fun (ctx, ret) -> return { ctx with v_return = ret }
  | _ -> nsup ""
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
