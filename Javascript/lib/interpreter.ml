(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Utils
open Ast
open Parser
open VTypes

let is_some = Option.is_some
let asprintf = Format.asprintf

let is_func x =
  match x.obj_type with
  | TFunPreset _ | TFunction _ | TArrowFunction _ -> true
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

let print_val = function
  | VNumber _ -> "number"
  | VString _ -> "string"
  | VBool _ -> "boolean"
  | VUndefined -> "undefined"
  | VNull -> "null"
  | VObject x when is_func x -> "function"
  | VObject _ -> "object"
;;

let rec get_field id = function
  | field :: tl -> if field.var_id = id then field.value else get_field id tl
  | _ -> VUndefined
;;

(*JS use diffrent conversion to string in .toString and in print.
  It's the reason why vvalues_to_str and to_vstring is diffrent functions*)
let rec vvalues_to_str ?(str_quote = false) = function
  | VNumber x -> num_to_string x
  | VBool true -> "true"
  | VBool false -> "false"
  | VNull -> "null"
  | VUndefined -> "undefined"
  | VString x -> if str_quote then "'" ^ x ^ "'" else x
  | VObject x when is_func x ->
    asprintf "[Function: %s]" (vvalues_to_str @@ get_field "name" x.fields)
  | VObject x ->
    asprintf
      "{ %s }"
      (String.concat
         ", "
         (List.map
            (fun x -> x.var_id ^ ": " ^ vvalues_to_str x.value ~str_quote:true)
            x.fields))
  | _ as t -> asprintf "Cannot convert '%s' to string" @@ print_val t
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

let context_init =
  { parent = None; vars = []; vreturn = None; stdout = ""; scope = Block }
;;

let create_local_ctx ctx scope =
  { parent = Some ctx; vars = []; vreturn = None; stdout = ctx.stdout; scope }
;;

let rec find_in_vars id = function
  | a :: b -> if a.var_id = id then Some a else find_in_vars id b
  | _ -> None
;;

let ctx_add ctx var =
  match find_in_vars var.var_id ctx.vars with
  | Some _ ->
    error
    @@ SyntaxError (asprintf "Identifier \'%s\' has already been declared" var.var_id)
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

let tfunction x = TFunction x
let tarrowfun x = TArrowFunction x

let create_func name args body obj_type =
  let length = Float.of_int @@ List.length args in
  let fun_ctx = { args; body } in
  VObject
    { fields =
        [ { var_id = "name"; is_const = true; value = VString name }
        ; { var_id = "length"; is_const = true; value = VNumber length }
        ]
    ; obj_type = obj_type fun_ctx
    }
;;

let prefind_funcs ctx ast =
  let ctx_add_if_func ctx = function
    | FunInit x ->
      ctx_add
        ctx
        { var_id = x.fun_identifier
        ; is_const = false
        ; value = create_func x.fun_identifier x.arguments x.body tfunction
        }
    | _ -> return ctx
  in
  fold_left ctx_add_if_func ctx ast
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
        | VNumber x when Float.is_nan x || x = 0. -> false
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
  | _ as t -> etyp @@ asprintf "expect number, but %s was given" @@ print_val t
;;

let get_vstring = function
  | VString x -> return x
  | _ as t -> etyp @@ asprintf "expect string, but %s was given" @@ print_val t
;;

let get_vbool = function
  | VBool x -> return x
  | _ as t -> etyp @@ asprintf "expect boolean, but %s was given" @@ print_val t
;;

let bop_with_num op a b =
  both to_vnumber a b
  >>= fun (a, b) -> both get_vnum a b >>| fun (x, y) -> VNumber (op x y)
;;

let bop_logical_with_num op a b =
  both to_vnumber a b >>= fun (a, b) -> both get_vnum a b >>| fun (x, y) -> VBool (op x y)
;;

let bop_with_string op a b =
  both to_vstring a b
  >>= fun (a, b) -> both get_vstring a b >>| fun (x, y) -> VString (op x y)
;;

let bop_logical_with_string op a b =
  both to_vstring a b
  >>= fun (a, b) -> both get_vstring a b >>| fun (x, y) -> VBool (op x y)
;;

let bop_bitwise_shift op a b =
  let get_int = function
    | VNumber x -> return (Int32.of_float x)
    | _ as t -> etyp @@ asprintf "expect number, but %s was given" @@ print_val t
  in
  both to_vnumber a b
  >>= fun (a, b) ->
  both get_int a b >>| fun (x, y) -> VNumber (Int32.to_float (op x (Int32.to_int y)))
;;

let bop_with_int op a b =
  let get_int = function
    | VNumber x -> return (int_of_float x)
    | _ as t -> etyp @@ asprintf "expect number, but %s was given" @@ print_val t
  in
  both to_vnumber a b
  >>= fun (a, b) -> both get_int a b >>| fun (x, y) -> VNumber (float_of_int (op x y))
;;

let is_to_string = function
  | VString _ | VObject _ -> true
  | _ -> false
;;

(* Binary operators *)
let add a b =
  if is_to_string a || is_to_string b
  then bop_with_string ( ^ ) a b
  else bop_with_num ( +. ) a b
;;

let negotiate op a b =
  op a b
  >>= fun x ->
  return
    (VBool
       (match x with
        | VBool true -> false
        | _ -> true))
;;

let strict_equal a b = return (VBool (a = b))

let equal a b =
  let is_undefined_nan = function
    | VNumber a when Float.is_nan a -> true
    | VUndefined -> true
    | _ -> false
  in
  let is_null = function
    | VNull -> true
    | _ -> false
  in
  let is_num_bool = function
    | VNumber _ | VBool _ -> true
    | _ -> false
  in
  let is_obj = function
    | VObject _ -> true
    | _ -> false
  in
  if is_null a || is_null b
  then
    return
      (VBool ((is_null a || is_null b) && (is_undefined_nan a || is_undefined_nan b)))
  else if is_obj a || is_obj b
  then bop_logical_with_string ( = ) a b
  else if is_num_bool a || is_num_bool b
  then bop_logical_with_num ( = ) a b
  else strict_equal a b
;;

let rem a b = bop_with_num mod_float a b
let exp a b = bop_with_num ( ** ) a b

let less_than a b =
  if is_to_string a && is_to_string b
  then bop_logical_with_string ( < ) a b
  else bop_logical_with_num ( < ) a b
;;

let less_eq a b =
  let is_undefined = function
    | VUndefined -> true
    | _ -> false
  in
  let is_null = function
    | VNull -> true
    | _ -> false
  in
  if (a = VNull && to_vnumber b = return (VNumber 0.))
     || (to_vnumber a = return (VNumber 0.) && b = VNull)
  then return (VBool true)
  else if (is_undefined a || is_undefined b) && (is_null a || is_null b)
  then return (VBool false)
  else if to_vnumber a = return (VNumber nan) && to_vnumber b = return (VNumber nan)
  then return (VBool false)
  else
    return
      (VBool (less_than a b = return (VBool true) || equal a b = return (VBool true)))
;;

let shift op a b =
  let get_int = function
    | VNumber x -> Some (int_of_float x)
    | _ -> None
  in
  match b with
  | VNumber x when x >= 0. -> bop_bitwise_shift op a b
  | _ -> bop_bitwise_shift op a (VNumber (float_of_int (32 + Option.get (get_int b))))
;;

let logical_and a b =
  let a_preserved = a in
  let b_preserved = b in
  both to_vbool a b
  >>= fun (a, b) ->
  both get_vbool a b
  >>= fun (x, y) ->
  if x then if y then return b_preserved else return b_preserved else return a_preserved
;;

let logical_or a b =
  let a_preserved = a in
  let b_preserved = b in
  both to_vbool a b
  >>= fun (a, b) ->
  both get_vbool a b
  >>= fun (x, _) -> if x then return a_preserved else return b_preserved
;;

let add_ctx ctx op = op >>| fun op -> ctx, op

let eval_bin_op ctx op a b =
  let add_ctx = add_ctx ctx in
  match op with
  | Add -> add_ctx @@ add a b <?> "error in add operator"
  | Sub -> add_ctx @@ bop_with_num ( -. ) a b <?> "error in sub operator"
  | Mul -> add_ctx @@ bop_with_num ( *. ) a b <?> "error in mul operator"
  | Div -> add_ctx @@ bop_with_num ( /. ) a b <?> "error in div operator"
  | Equal -> add_ctx @@ equal a b <?> "error in equal operator"
  | NotEqual -> add_ctx @@ negotiate equal a b <?> "error in not_equal operator"
  | StrictEqual -> add_ctx @@ strict_equal a b <?> "error in strict equal operator"
  | StrictNotEqual ->
    add_ctx @@ negotiate strict_equal a b <?> "error in strict not_equal operator"
  | Rem -> add_ctx @@ rem a b <?> "error in rem operator"
  | LogicalShiftLeft ->
    add_ctx @@ shift Int32.shift_left a b <?> "error in logical_shift_left operator"
  | LogicalShiftRight ->
    add_ctx @@ shift Int32.shift_right a b <?> "error in logical_shift_right operator"
  | UnsignedShiftRight ->
    add_ctx @@ shift Int32.shift_right_logical a b
    <?> "error in unsigned_shift_right operator"
  | GreaterEqual -> add_ctx @@ less_eq b a <?> "error in greater_equal operator"
  | LessEqual -> add_ctx @@ less_eq a b <?> "error in less_equal operator"
  | GreaterThan -> add_ctx @@ less_than b a <?> "error in greater_than operator"
  | LessThan -> add_ctx @@ less_than a b <?> "error in less_than operator"
  | BitwiseAnd -> add_ctx @@ bop_with_int ( land ) a b
  | BitwiseOr -> add_ctx @@ bop_with_int ( lor ) a b
  | LogicalAnd -> add_ctx @@ logical_and a b <?> "error in logical_and operator"
  | LogicalOr -> add_ctx @@ logical_or a b <?> "error in logical_and operator"
  | Xor -> add_ctx @@ bop_with_int ( lxor ) a b
  | Exp -> add_ctx @@ exp a b <?> "error in exp operator"
  | PropAccs ->
    add_ctx
      (match a with
       | VObject x -> to_vstring b >>= get_vstring >>| fun str -> get_field str x.fields
       | _ -> return VUndefined)
  | _ as a -> ensup @@ asprintf "operator %a not supported yet" pp_bin_op a
;;

let eval_un_op ctx op a =
  let add_ctx = add_ctx ctx in
  match op with
  | Plus -> add_ctx @@ to_vnumber a <?> "error in plus operator"
  | Minus ->
    add_ctx @@ (to_vnumber a >>= get_vnum >>| fun n -> VNumber ~-.n)
    <?> "error in plus operator"
  | _ as a -> ensup @@ asprintf "operator %a not supported yet" pp_un_op a
;;

let rec ctx_get_var ctx id =
  match find_in_vars id ctx.vars with
  | Some a -> return a
  | None ->
    (match ctx.parent with
     | Some parent -> ctx_get_var parent id
     | None ->
       error (ReferenceError (asprintf "Cannot access '%s' before initialization" id)))
;;

let rec eval_fun ctx f args =
  let get_fun =
    match f with
    | VObject obj when is_func obj -> return (ctx, obj.obj_type)
    | _ -> etyp @@ asprintf "'%s' is not a function" (vvalues_to_str f)
  in
  let rec val_to_args ctx = function
    | a1 :: atl, v1 :: vtl ->
      ctx_add ctx { var_id = a1; is_const = false; value = v1 }
      >>= fun ctx -> val_to_args ctx (atl, vtl)
    | a1 :: atl, _ ->
      ctx_add ctx { var_id = a1; is_const = false; value = VUndefined }
      >>= fun ctx -> val_to_args ctx (atl, [])
    | _, _ -> return ctx
  in
  let valid_and_run ctx scope f =
    match f.body with
    | Block x ->
      val_to_args (create_local_ctx ctx scope) (f.args, args)
      >>= fun ctx -> parse_stms ctx x >>| fun ctx -> ctx, get_vreturn ctx
    | _ ->
      error (AstError "in top of fun body expected ast, but something else was given")
  in
  get_fun
  >>= fun (ctx, obj_t) ->
  match obj_t with
  | TFunPreset f -> f ctx args >>| fun ctx -> ctx, get_vreturn ctx
  | TFunction f -> valid_and_run ctx Function f
  | TArrowFunction f -> valid_and_run ctx ArrowFunction f
  | _ ->
    error @@ InternalError "get unexpected object type, expect function, but get TObject"

(*main expression interpreter*)
and eval_exp ctx = function
  | Const x -> return (ctx, const_to_val x)
  | BinOp (op, a, b) ->
    both_ext eval_exp ctx a b >>= fun (ctx, (x, y)) -> eval_bin_op ctx op x y
  | UnOp (op, a) -> eval_exp ctx a >>= fun (ctx, a) -> eval_un_op ctx op a
  | Var id -> ctx_get_var ctx id >>| fun a -> ctx, a.value
  | FunctionCall (var, args) ->
    eval_exp ctx var
    <?> "error while try get function"
    >>= fun (ctx, f) ->
    fold_left_map eval_exp ctx args
    <?> "error in function arguments"
    >>= fun (ctx, args) -> eval_fun ctx f args
  | AnonFunction (args, vals) -> return (ctx, create_func "" args vals tfunction)
  | ArrowFunction (args, vals) -> return (ctx, create_func "" args vals tarrowfun)
  | ObjectDef x ->
    fold_left_map
      (fun ctx (key, value) ->
        both_ext eval_exp ctx key value
        >>= fun (ctx, (key, value)) ->
        to_vstring key
        >>= get_vstring
        >>| fun id -> ctx, { var_id = id; is_const = false; value })
      ctx
      x
    >>= fun (ctx, fields) -> return (ctx, VObject { fields; obj_type = TObject })
  | _ -> ensup ""

(**---------------Statement interpreter---------------*)

and parse_stm ctx = function
  | Return x ->
    eval_exp ctx x
    <?> "error in return expression"
    >>= fun (ctx, ret) -> return { ctx with vreturn = Some ret }
  | VarInit x ->
    let eval_for_top_val =
      let create = create_func x.var_identifier in
      function
      | AnonFunction (args, vals) -> return (ctx, create args vals tfunction)
      | ArrowFunction (args, vals) -> return (ctx, create args vals tarrowfun)
      | _ as ast -> eval_exp ctx ast
    in
    eval_for_top_val x.value
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
  | FunInit _ -> return ctx
  | _ -> ensup ""

and parse_stms ctx ast =
  prefind_funcs ctx ast
  >>= fun ctx -> fold_left_s parse_stm (fun ctx -> is_some ctx.vreturn) ctx ast

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
