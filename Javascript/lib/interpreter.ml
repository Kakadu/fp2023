(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Utils
open Ast
open Parser
open VTypes

let is_some = Option.is_some

let is_func x =
  match x.obj_type with
  | TFunPreset _ | TFunction _ | TArrowFun _ -> true
  | _ -> false
;;

let num_to_string n =
  if Float.is_integer n
  then Int.to_string (Float.to_int n)
  else if Float.is_nan n
  then "NaN"
  else if Float.is_infinite n
  then if n > 0. then "Infinity" else "-Infinity"
  else Float.to_string n
;;

(*JS use diffrent conversion to string in .toString and in print.
  It's the reason why vvalues_to_str and to_vstring is diffrent functions*)
let vvalues_to_str = function
  | VNumber x -> num_to_string x
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
     | TypeError str -> "TypeError: " ^ str
     | SyntaxError str -> "SyntaxError: " ^ str)
;;

let ensup str = error (NotSupported str)
let etyp str = error (TypeError str)

let print ctx = function
  | [ VNumber x ] -> return { ctx with stdout = ctx.stdout ^ Float.to_string x }
  | _ -> ensup ""
;;

let get_vreturn ctx =
  match ctx.vreturn with
  | Some x -> x
  | None -> VUndefined
;;

let get_parent ctx =
  match ctx.parent with
  | Some x -> return x
  | _ -> error @@ InternalError "cannot get parent"
;;

(**---------------Expression interpreter---------------*)

let to_vstring =
  let ret_vstr x = return (VString x) in
  function
  | VString x -> ret_vstr x
  | VNumber x -> ret_vstr (num_to_string x)
  | VBool true -> ret_vstr "true"
  | VBool false -> ret_vstr "false"
  | VNull -> ret_vstr "null"
  | VUndefined -> ret_vstr "undefined"
  | VObject x when is_func x -> ensup "conversion func to str"
  | VObject _ -> ret_vstr "[object Object]"
  | _ as t -> etyp ("cannot cast " ^ print_val t ^ " to string")
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

let const_to_val = function
  | Number x -> VNumber x
  | String x -> VString x
  | Bool x -> VBool x
  | Undefined -> VUndefined
  | Null -> VNull
;;

let get_vnum = function
  | VNumber x -> return x
  | _ as t -> etyp @@ "expect number, but " ^ print_val t ^ " was given"
;;

let get_vstring = function
  | VString x -> return x
  | _ as t -> etyp @@ "expect string, but " ^ print_val t ^ " was given"
;;

let bop_with_num op a b =
  to_vnumber a
  >>= get_vnum
  >>= fun x -> to_vnumber b >>= get_vnum >>| fun y -> VNumber (op x y)
;;

let bop_with_string op a b =
  to_vstring a
  >>= get_vstring
  >>= fun x -> to_vstring b >>= get_vstring >>| fun y -> VString (op x y)
;;

let add a b =
  let is_to_string = function
    | VString _ | VObject _ -> true
    | _ -> false
  in
  if is_to_string a || is_to_string b
  then bop_with_string ( ^ ) a b
  else bop_with_num ( +. ) a b
;;

let add_ctx ctx op = op >>| fun op -> ctx, op

let eval_bin_op ctx op a b =
  let add_ctx = add_ctx ctx in
  match op with
  | Add -> add_ctx @@ add a b <?> "error in add operator"
  | _ -> ensup "operator not supported yet"
;;

let eval_un_op ctx op a =
  let add_ctx = add_ctx ctx in
  match op with
  | Plus -> add_ctx @@ to_vnumber a <?> "error in plus operator"
  | Minus ->
    add_ctx @@ (to_vnumber a >>= get_vnum >>| fun n -> VNumber ~-.n)
    <?> "error in plus operator"
  | _ -> ensup "operator not supported yet"
;;

let rec find_in_vars id = function
  | a :: b -> if a.var_id = id then Some a else find_in_vars id b
  | _ -> None
;;

let rec ctx_get_var ctx id =
  match find_in_vars id ctx.vars with
  | Some a -> return a
  | None ->
    (match ctx.parent with
     | Some parent -> ctx_get_var parent id
     | None -> error (ReferenceError ("Cannot access '" ^ id ^ "' before initialization")))
;;

let rec eval_exp ctx = function
  | Const x -> return (ctx, const_to_val x)
  | BinOp (op, a, b) ->
    eval_exp ctx a
    >>= fun (ctx, x) -> eval_exp ctx b >>= fun (ctx, y) -> eval_bin_op ctx op x y
  | UnOp (op, a) -> eval_exp ctx a >>= fun (ctx, a) -> eval_un_op ctx op a
  | Var id -> ctx_get_var ctx id >>| fun a -> ctx, a.value
  | _ -> ensup ""
;;

(**---------------Statment interpreter---------------*)

let context_init =
  { parent = None; vars = []; vreturn = None; stdout = ""; scope = Block }
;;

let create_local_ctx ctx scope =
  { parent = Some ctx; vars = []; vreturn = None; stdout = ctx.stdout; scope }
;;

let ctx_add ctx var =
  match find_in_vars var.var_id ctx.vars with
  | Some _ ->
    error
    @@ SyntaxError ("Identifier \'" ^ var.var_id ^ "\' has already been declared\n  ")
  | _ -> return { ctx with vars = var :: ctx.vars }
;;

let rec in_func ctx =
  match ctx.scope with
  | Function | ArrowFunction -> true
  | Block ->
    (match ctx.parent with
     | Some x -> in_func x
     | None -> false)
;;

let rec parse_stm ctx = function
  | Return x ->
    eval_exp ctx x
    <?> "error in return expression"
    >>= fun (ctx, ret) -> return { ctx with vreturn = Some ret }
  | VarDeck x ->
    eval_exp ctx x.value
    <?> "error in var declaration expression"
    >>= fun (ctx, v) ->
    ctx_add ctx { var_id = x.var_identifier; is_const = x.is_const; value = v }
  | Block x ->
    parse_block ctx Block x
    <?> "error while interpret block"
    >>= fun ctx ->
    get_parent ctx
    >>| fun parent ->
    (match ctx.vreturn with
     | Some a -> { parent with vreturn = Some a }
     | _ -> parent)
  | _ -> ensup ""

and parse_stms ctx ast = fold_left_s parse_stm (fun ctx -> is_some ctx.vreturn) ctx ast
and parse_block ctx scope ast = parse_stms (create_local_ctx ctx scope) ast

let interpret_ast ast : (value, string) Result.t =
  match ast with
  | Programm x -> parse_stms context_init x >>| get_vreturn
  | _ ->
    error @@ AstError "expect Programm in start of programm, but something else was given"
;;

let interpret input : (value, string) Result.t =
  match parse input with
  | Result.Ok ast -> interpret_ast ast <?> "Interpreter error"
  | Result.Error (`ParsingError str) -> error (SyntaxError str) <?> "Parser error"
;;
