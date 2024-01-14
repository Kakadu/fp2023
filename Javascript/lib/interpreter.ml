(** Copyright 2023, Kuarni, AlexShmak *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Utils
open Ast
open Parser
open VTypes

let is_some = Option.is_some
let asprintf = Format.asprintf

type program_return =
  { stdout : string
  ; return : string
  }

let print_string (ctx : ctx) str = { ctx with stdout = ctx.stdout ^ str ^ "\n" }

let print_opt f = function
  | Some x -> "Some: " ^ f x
  | None -> "None"
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

let num_to_string n =
  if Float.is_integer n
  then Int.to_string (Float.to_int n)
  else if Float.is_nan n
  then "NaN"
  else if Float.is_infinite n
  then if n > 0. then "Infinity" else "-Infinity"
  else Float.to_string n
;;

let get_obj_ctx_opt ctx id = IntMap.find_opt id ctx.objs

let get_obj_ctx ctx id =
  match get_obj_ctx_opt ctx id with
  | Some x -> return x
  | _ -> error @@ ReferenceError (asprintf "cannot find object with id = %i" id)
;;

let is_func x =
  match x.obj_type with
  | TFunPreset _ | TFunction _ | TArrowFunction _ -> true
  | _ -> false
;;

let is_func_id ctx id =
  match get_obj_ctx_opt ctx id with
  | Some x -> is_func x
  | _ -> false
;;

let print_type ctx = function
  | VNumber _ -> "number"
  | VString _ -> "string"
  | VBool _ -> "boolean"
  | VUndefined -> "undefined"
  | VObject x when is_func_id ctx x -> "function"
  | VNull (*because that's how it is in JS*) | VObject _ -> "object"
;;

let proto_obj_fields = []

let find_proto fields =
  List.partition (fun field -> field.var_id = "__proto__") fields
  |> fun (protos, others) ->
  match protos with
  | a :: [] -> return (a.value, others)
  | _ :: _ ->
    error @@ SyntaxError "Duplicate __proto__ fields are not allowed in object literals"
  | _ -> return (VUndefined, others)
;;

let add_obj ctx fields obj_type =
  find_proto fields
  >>| fun (proto, fields) ->
  let obj = { proto; fields; obj_type } in
  let id = ctx.obj_count in
  { ctx with obj_count = id + 1; objs = IntMap.add id obj ctx.objs }, VObject id
;;

let rec get_field_opt id = function
  | field :: tl -> if field.var_id = id then Some field.value else get_field_opt id tl
  | _ -> None
;;

let get_field id fields =
  match get_field_opt id fields with
  | Some x -> x
  | None -> VUndefined
;;

(*JS uses diffrent conversion to string in .toString and in print.
  It's the reason why vvalues_to_str and to_vstring are diffrent functions*)
let rec print_vvalues ctx ?(str_quote = false) = function
  | VNumber x -> return @@ num_to_string x
  | VBool true -> return "true"
  | VBool false -> return "false"
  | VNull -> return "null"
  | VUndefined -> return "undefined"
  | VString x -> return @@ if str_quote then "'" ^ x ^ "'" else x
  | VObject x ->
    get_obj_ctx ctx x
    >>= fun obj ->
    if is_func obj
    then print_vvalues ctx @@ get_field "name" obj.fields >>| asprintf "[Function: %s]"
    else
      map
        (fun x -> print_vvalues ctx x.value ~str_quote:true >>| ( ^ ) (x.var_id ^ ": "))
        obj.fields
      >>| fun fields ->
      if List.length fields = 0
      then "{}"
      else asprintf "{ %s }" (String.concat ", " fields)
  | _ as t -> return @@ asprintf "Cannot convert '%s' to string" @@ print_type ctx t
;;

let get_vreturn = function
  | Some x -> x
  | None -> VUndefined
;;

let rec find_in_vars id = function
  | a :: b -> if a.var_id = id then Some a else find_in_vars id b
  | _ -> None
;;

let get_lex_env_opt ctx id = IntMap.find_opt id ctx.lex_envs

let get_lex_env ctx id =
  match get_lex_env_opt ctx id with
  | Some x -> return x
  | _ -> error @@ ReferenceError (asprintf "cannot find lexical env with id = %i" id)
;;

let rec add_or_replace var = function
  | a :: b -> if a.var_id = var.var_id then var :: b else a :: add_or_replace var b
  | _ -> [ var ]
;;

let lex_add ctx ?(replace = false) lex_env_id var =
  get_lex_env ctx lex_env_id
  >>= fun lex_env ->
  match find_in_vars var.var_id lex_env.vars with
  | Some _ when not replace ->
    error
    @@ SyntaxError (asprintf "Identifier \'%s\' has already been declared" var.var_id)
  | Some var when var.is_const -> etyp "Assignment to constant variable."
  | None when replace -> error @@ ReferenceError (asprintf "%s is not defined" var.var_id)
  | _ ->
    return
      { ctx with
        lex_envs =
          IntMap.add
            lex_env_id
            { lex_env with vars = add_or_replace var lex_env.vars }
            ctx.lex_envs
      }
;;

let get_parent ctx =
  get_lex_env ctx ctx.cur_lex_env
  >>= fun lex_env ->
  match lex_env.parent with
  | Some x -> return x
  | _ -> error @@ InternalError "cannot get parent"
;;

let print ctx values =
  fold_left
    (fun ctx value ->
      print_vvalues ctx value >>| fun str -> { ctx with stdout = ctx.stdout ^ str ^ " " })
    ctx
    values
  >>| fun ctx -> { ctx with stdout = ctx.stdout ^ "\n" }, None
;;

let first_lex_env = 0

let context_init =
  let global_obj_id = 0 in
  let global_obj = { proto = VUndefined; fields = []; obj_type = TObject } in
  let vars = [ { var_id = "this"; is_const = true; value = VObject global_obj_id } ] in
  let lex_env = { parent = None; creater = None; vars; scope = Block } in
  let id = first_lex_env in
  let ctx =
    { lex_env_count = id + 1
    ; cur_lex_env = id
    ; lex_envs = IntMap.singleton id lex_env
    ; obj_count = global_obj_id + 1
    ; objs = IntMap.singleton global_obj_id global_obj
    ; put_this = None
    ; vreturn = None
    ; stdout = ""
    }
  in
  let fun_print ctx name =
    add_obj
      ctx
      [ { var_id = "name"; is_const = true; value = VString name }
      ; { var_id = "length"; is_const = true; value = VNumber 0. }
      ]
      (TFunPreset print)
  in
  let console =
    fun_print ctx "log"
    >>= fun (ctx, f) ->
    add_obj ctx [ { var_id = "log"; is_const = false; value = f } ] TObject
    >>= fun (ctx, value) ->
    lex_add ctx first_lex_env { var_id = "console"; is_const = false; value }
  in
  let alert ctx =
    fun_print ctx "alert"
    >>= fun (ctx, f) ->
    lex_add ctx first_lex_env { var_id = "alert"; is_const = false; value = f }
  in
  console >>= alert
;;

let create_local_ctx ctx parent scope =
  let vars =
    match ctx.put_this with
    | Some int -> [ { var_id = "this"; is_const = true; value = VObject int } ]
    | None -> []
  in
  let ctx = { ctx with put_this = None } in
  let new_lex_env =
    { parent = Some parent; creater = Some ctx.cur_lex_env; vars; scope }
  in
  let id = ctx.lex_env_count in
  if id = Int.max_int
  then error @@ InternalError "Max amount of lexical envs was created"
  else
    return
      { ctx with
        lex_env_count = id + 1
      ; cur_lex_env = id
      ; lex_envs = IntMap.add id new_lex_env ctx.lex_envs
      }
;;

let end_of_block ctx =
  let ret = ctx.vreturn in
  get_lex_env ctx ctx.cur_lex_env
  >>= fun lex_env ->
  match lex_env.creater with
  | Some x -> return ({ ctx with cur_lex_env = x; vreturn = None }, ret)
  | _ when ctx.cur_lex_env = first_lex_env -> return (ctx, ret)
  | _ -> error @@ InternalError "Cannot find the lexical environment creater"
;;

let rec in_func ctx lex_env =
  match lex_env.scope with
  | Function | ArrowFunction -> true
  | Block ->
    (match lex_env.creater with
     | Some x ->
       (match get_lex_env_opt ctx x with
        | Some x -> in_func ctx x
        | _ -> false)
     | None -> false)
;;

let tfunction x = TFunction x
let tarrowfun x = TArrowFunction x

let create_func ctx name args body obj_type =
  let length = Float.of_int @@ List.length args in
  let fun_ctx = { parent_lex_env = ctx.cur_lex_env; args; body } in
  add_obj
    ctx
    [ { var_id = "name"; is_const = true; value = VString name }
    ; { var_id = "length"; is_const = true; value = VNumber length }
    ]
    (obj_type fun_ctx)
;;

let prefind_funcs ctx ast =
  let ctx_add_if_func ctx = function
    | FunInit x ->
      create_func ctx x.fun_identifier x.arguments x.body tfunction
      >>= fun f ->
      let ctx, value = f in
      lex_add ctx ctx.cur_lex_env { var_id = x.fun_identifier; is_const = false; value }
    | _ -> return ctx
  in
  fold_left ctx_add_if_func ctx ast
;;

(**---------------Expression interpreter---------------*)

let to_vstring ctx =
  let ret_vstr x = return (VString x) in
  function
  | VString x -> ret_vstr x
  | VNumber x -> ret_vstr (num_to_string x)
  | VBool true -> ret_vstr "true"
  | VBool false -> ret_vstr "false"
  | VNull -> ret_vstr "null"
  | VUndefined -> ret_vstr "undefined"
  | VObject x when is_func_id ctx x -> ensup "conversion func to str"
  | VObject _ -> ret_vstr "[object Object]"
  | _ as t -> etyp ("cannot cast " ^ print_type ctx t ^ " to string")
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

let add_ctx ctx x = x >>| fun x -> ctx, x

let get_vnum ctx = function
  | VNumber x -> return x
  | _ as t -> etyp @@ asprintf "expect number, but %s was given" @@ print_type ctx t
;;

let get_vstring ctx = function
  | VString x -> return x
  | _ as t -> etyp @@ asprintf "expect string, but %s was given" @@ print_type ctx t
;;

let get_vbool ctx = function
  | VBool x -> return x
  | _ as t -> etyp @@ asprintf "expect boolean, but %s was given" @@ print_type ctx t
;;

let get_int ctx bit = function
  | VNumber x -> return (bit x)
  | _ as t -> etyp @@ asprintf "expect number, but %s was given" @@ print_type ctx t
;;

let get_obj_id ctx = function
  | VObject id -> return id
  | _ as t -> etyp @@ asprintf "expect object, but %s was given" @@ print_type ctx t
;;

let rec ctx_not_change_bop ctx op a b =
  let bop_with_num op =
    both to_vnumber a b
    >>= fun (a, b) -> both (get_vnum ctx) a b >>| fun (x, y) -> VNumber (op x y)
  in
  let bop_logical_with_num op =
    both to_vnumber a b
    >>= fun (a, b) -> both (get_vnum ctx) a b >>| fun (x, y) -> VBool (op x y)
  in
  let bop_with_string op =
    both (to_vstring ctx) a b
    >>= fun (a, b) -> both (get_vstring ctx) a b >>| fun (x, y) -> VString (op x y)
  in
  let bop_logical_with_string op =
    both (to_vstring ctx) a b
    >>= fun (a, b) -> both (get_vstring ctx) a b >>| fun (x, y) -> VBool (op x y)
  in
  let bop_bitwise_shift op b =
    both to_vnumber a b
    >>= fun (a, b) ->
    both (get_int ctx Int32.of_float) a b
    >>| fun (x, y) -> VNumber (Int32.to_float (op x (Int32.to_int y)))
  in
  let bop_with_int op =
    both to_vnumber a b
    >>= fun (a, b) ->
    both (get_int ctx int_of_float) a b >>| fun (x, y) -> VNumber (float_of_int (op x y))
  in
  let is_to_string = function
    | VString _ | VObject _ -> true
    | _ -> false
  in
  let negotiate res = res >>= fun x -> get_vbool ctx x >>| fun res -> VBool (not res) in
  let add () =
    if is_to_string a || is_to_string b
    then bop_with_string ( ^ )
    else bop_with_num ( +. )
  in
  let strict_equal () = return (VBool (a = b)) in
  let equal () =
    let is_undefined = function
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
    then return (VBool ((is_null a || is_null b) && (is_undefined a || is_undefined b)))
    else if is_num_bool a || is_num_bool b
    then bop_logical_with_num ( = )
    else
      ctx_not_change_bop ctx StrictEqual a b
      >>= function
      | VBool true -> return (VBool true)
      | _ ->
        if is_obj a || is_obj b
        then bop_logical_with_string ( = )
        else return (VBool false)
  in
  let less_than () =
    if is_to_string a && is_to_string b
    then bop_logical_with_string ( < )
    else bop_logical_with_num ( < )
  in
  let shift op =
    let get_int = function
      | VNumber x -> Some (int_of_float x)
      | _ -> None
    in
    match b with
    | VNumber x when x >= 0. -> bop_bitwise_shift op b
    | _ -> bop_bitwise_shift op (VNumber (float_of_int (32 + Option.get (get_int b))))
  in
  let logical_and () =
    let a_preserved = a in
    let b_preserved = b in
    both to_vbool a b
    >>= fun (a, b) ->
    both (get_vbool ctx) a b
    >>| function
    | true, _ -> b_preserved
    | _ -> a_preserved
  in
  let logical_or () =
    let a_preserved = a in
    let b_preserved = b in
    both to_vbool a b
    >>= fun (a, b) ->
    both (get_vbool ctx) a b
    >>| function
    | true, _ -> a_preserved
    | _ -> b_preserved
  in
  let less_eq () =
    let bop cast a b =
      both cast a b
      >>= fun (x, y) ->
      ctx_not_change_bop ctx LessThan x y
      >>= fun res1 ->
      ctx_not_change_bop ctx Equal x y
      >>= fun res2 ->
      ctx_not_change_bop ctx LogicalOr res1 res2
      >>= fun res3 -> get_vbool ctx res3 >>| fun res -> VBool res
    in
    if is_to_string a && is_to_string b
    then bop (to_vstring ctx) a b
    else bop to_vnumber a b
  in
  let nullish_coal () =
    match a with
    | VNull | VUndefined -> return b
    | _ -> return a
  in
  match op with
  | Add -> add () <?> "error in add operator"
  | Sub -> bop_with_num ( -. ) <?> "error in sub operator"
  | Mul -> bop_with_num ( *. ) <?> "error in mul operator"
  | Div -> bop_with_num ( /. ) <?> "error in div operator"
  | Equal -> equal () <?> "error in equal operator"
  | NotEqual -> negotiate @@ equal () <?> "error in not_equal operator"
  | StrictEqual -> strict_equal () <?> "error in strict equal operator"
  | StrictNotEqual ->
    negotiate @@ strict_equal () <?> "error in strict not_equal operator"
  | Rem -> bop_with_num mod_float <?> "error in rem operator"
  | LogicalShiftLeft -> shift Int32.shift_left <?> "error in logical_shift_left operator"
  | LogicalShiftRight ->
    shift Int32.shift_right <?> "error in logical_shift_right operator"
  | UnsignedShiftRight ->
    shift Int32.shift_right_logical <?> "error in unsigned_shift_right operator"
  | GreaterEqual ->
    ctx_not_change_bop ctx LessEqual b a <?> "error in greater_equal operator"
  | LessEqual -> less_eq () <?> "error in less_equal operator"
  | GreaterThan ->
    ctx_not_change_bop ctx LessThan b a <?> "error in greater_than operator"
  | LessThan -> less_than () <?> "error in less_than operator"
  | BitwiseAnd -> bop_with_int ( land ) <?> "error in bitwise and"
  | BitwiseOr -> bop_with_int ( lor ) <?> "error in bitwise or"
  | LogicalAnd -> logical_and () <?> "error in logical_and operator"
  | LogicalOr -> logical_or () <?> "error in logical_and operator"
  | NullishCoal -> nullish_coal () <?> "error in nullish_coalescing operator"
  | Xor -> bop_with_int ( lxor )
  | Exp -> bop_with_num ( ** ) <?> "error in exp operator"
  | _ as a -> ensup @@ asprintf "operator %a not supported yet" pp_bin_op a
;;

let ctx_not_change_unop ctx op a =
  match op with
  | Plus -> to_vnumber a <?> "error in plus operator"
  | Minus ->
    to_vnumber a
    >>= get_vnum ctx
    >>| (fun n -> VNumber ~-.n)
    <?> "error in minus operator"
  | PreInc ->
    to_vnumber a
    >>= get_vnum ctx
    >>| (fun n -> VNumber (n +. 1.))
    <?> "error in prefix increment operator"
  | PreDec ->
    to_vnumber a
    >>= get_vnum ctx
    >>| (fun n -> VNumber (n -. 1.))
    <?> "error in prefix decrement operator"
  | LogicalNot ->
    to_vbool a
    >>= get_vbool ctx
    >>| (fun b -> VBool (not b))
    <?> "error in logical NOT operator"
  | BitwiseNot ->
    to_vnumber a
    >>= get_vnum ctx
    >>| (fun n -> VNumber (float_of_int (lnot (Int32.to_int (Int32.of_float n)))))
    <?> "error in bitwise NOT operator"
  | TypeOf -> return @@ VString (print_type ctx a)
  | _ as a -> ensup @@ asprintf "operator %a not supported yet" pp_un_op a
;;

let ctx_get_var ctx id =
  let rec go lex_id =
    get_lex_env ctx lex_id
    >>= fun lex_env ->
    match find_in_vars id lex_env.vars with
    | Some a -> return (a, lex_id)
    | None ->
      (*for correct work if function doesn't see this*)
      if id = "this" && lex_env.scope = Function
      then return ({ var_id = id; is_const = true; value = VUndefined }, lex_id)
      else (
        match lex_env.parent with
        | Some parent -> go parent
        | None ->
          error (ReferenceError (asprintf "Cannot access '%s' before initialization" id)))
  in
  go ctx.cur_lex_env
;;

let rec eval_fun ctx f args =
  let get_fun =
    match f with
    | VObject id ->
      get_obj_ctx ctx id
      >>= fun obj ->
      if is_func obj then return (ctx, obj.obj_type) else etyp "object is not a function"
    | _ -> etyp @@ asprintf "'%s' is not a function" (print_type ctx f)
  in
  let valid_and_run ctx scope f =
    let rec val_to_args ctx = function
      | a1 :: atl, v1 :: vtl ->
        lex_add ctx ctx.cur_lex_env { var_id = a1; is_const = false; value = v1 }
        >>= fun ctx -> val_to_args ctx (atl, vtl)
      | a1 :: atl, _ ->
        lex_add ctx ctx.cur_lex_env { var_id = a1; is_const = false; value = VUndefined }
        >>= fun ctx -> val_to_args ctx (atl, [])
      | _, _ -> return ctx
    in
    create_local_ctx ctx f.parent_lex_env scope
    >>= fun ctx ->
    match f.body with
    | Block x ->
      val_to_args ctx (f.args, args)
      >>= fun ctx -> parse_stms ctx x >>| fun (ctx, ret) -> ctx, get_vreturn ret
    | _ ->
      error (AstError "in top of fun body expected Block, but something else was given")
  in
  get_fun
  >>= fun (ctx, obj_t) ->
  match obj_t with
  | TFunPreset f -> f ctx args >>| fun (ctx, ret) -> ctx, get_vreturn ret
  | TFunction f -> valid_and_run ctx Function f
  | TArrowFunction f -> valid_and_run ctx ArrowFunction f
  | _ ->
    error @@ InternalError "get unexpected object type, expect function, but get TObject"

and ctx_change_unop ctx op a =
  let op_new () =
    add_obj ctx [] TObject
    >>= fun (ctx, obj) ->
    get_obj_id ctx obj
    >>= fun id ->
    eval_exp { ctx with put_this = Some id } a
    >>| fun (ctx, _) ->
    let ctx = { ctx with put_this = None } in
    ctx, VObject id
  in
  let add_ctx = add_ctx ctx in
  match op with
  | New -> op_new ()
  | _ -> eval_exp ctx a >>= fun (ctx, a) -> add_ctx @@ ctx_not_change_unop ctx op a

and ctx_change_bop ctx op a b =
  let assign () =
    let obj_assign obj prop =
      let get_obj = function
        | VObject x -> get_obj_ctx ctx x >>| fun obj -> x, obj
        | _ -> error @@ SyntaxError "Invalid or unexpected token"
      in
      fold_left_map eval_exp ctx [ obj; prop; b ]
      >>= function
      | ctx, [ obj_val; prop; b ] ->
        get_obj obj_val
        >>= fun (id, obj) ->
        to_vstring ctx prop
        >>= get_vstring ctx
        >>= fun prop ->
        let get_new_obj =
          match prop with
          | "__proto__" ->
            if obj_val = b
            then etyp "Cyclic __proto__ value"
            else return { obj with proto = b }
          | _ ->
            (match find_in_vars prop obj.fields with
             | Some x when x.is_const ->
               print_vvalues ctx obj_val
               >>= fun str ->
               etyp
               @@ asprintf "Cannot assign to read only property '%s' of %s" x.var_id str
             | _ ->
               let fields =
                 add_or_replace { var_id = prop; is_const = false; value = b } obj.fields
               in
               return { obj with fields })
        in
        get_new_obj >>| fun obj -> { ctx with objs = IntMap.add id obj ctx.objs }, b
      | _ -> error @@ InternalError "Error in assignment to object field"
    in
    match a with
    | Var id ->
      eval_exp ctx b
      >>= fun (ctx, res) ->
      ctx_get_var ctx id
      >>= fun (_, lex_id) ->
      lex_add ctx ~replace:true lex_id { var_id = id; is_const = false; value = res }
      >>| fun ctx -> ctx, res
    | BinOp (PropAccs, obj, prop) -> obj_assign obj prop
    | _ -> error @@ SyntaxError "Invalid left-hand side in assignment"
  in
  let prop_accs () =
    eval_exp ctx a
    >>= fun (ctx, a) ->
    match a with
    | VObject id ->
      (*to pass this to methods*)
      eval_exp { ctx with put_this = Some id } b
      >>= fun (ctx, b) ->
      to_vstring { ctx with put_this = None } b
      >>= get_vstring ctx
      >>= fun str ->
      let rec proto_find = function
        | VObject x ->
          get_obj_ctx ctx x
          >>= fun obj ->
          (match get_field_opt str obj.fields with
           | None -> proto_find obj.proto
           | Some x -> return x)
        | _ -> return @@ get_field str proto_obj_fields
      in
      add_ctx ctx @@ proto_find (VObject id)
    | _ -> ensup "reading from not object is not supported"
  in
  let add_ctx = add_ctx ctx in
  match op with
  | Assign -> assign () <?> "error in assignment"
  | AddAssign -> ctx_change_bop ctx Assign a (BinOp (Add, a, b))
  | SubAssign -> ctx_change_bop ctx Assign a (BinOp (Sub, a, b))
  | MulAssign -> ctx_change_bop ctx Assign a (BinOp (Mul, a, b))
  | DivAssign -> ctx_change_bop ctx Assign a (BinOp (Div, a, b))
  | ExpAssign -> ctx_change_bop ctx Assign a (BinOp (Exp, a, b))
  | RemAssign -> ctx_change_bop ctx Assign a (BinOp (Rem, a, b))
  | LShiftAssign -> ctx_change_bop ctx Assign a (BinOp (LogicalShiftLeft, a, b))
  | RShiftAssign -> ctx_change_bop ctx Assign a (BinOp (LogicalShiftRight, a, b))
  | URShiftAssign -> ctx_change_bop ctx Assign a (BinOp (UnsignedShiftRight, a, b))
  | BitAndAssign -> ctx_change_bop ctx Assign a (BinOp (BitwiseAnd, a, b))
  | BitOrAssign -> ctx_change_bop ctx Assign a (BinOp (BitwiseOr, a, b))
  | BitXorAssign -> ctx_change_bop ctx Assign a (BinOp (Xor, a, b))
  | LogAndAssign -> ctx_change_bop ctx Assign a (BinOp (LogicalAnd, a, b))
  | LogOrAssign -> ctx_change_bop ctx Assign a (BinOp (LogicalOr, a, b))
  | NullAssign -> ctx_change_bop ctx Assign a (BinOp (NullishCoal, a, b))
  | PropAccs -> prop_accs () <?> "error in property access"
  | _ ->
    both_ext eval_exp ctx a b
    >>= fun (ctx, (x, y)) -> add_ctx @@ ctx_not_change_bop ctx op x y

and eval_for_top_val ctx name =
  let create = create_func ctx name in
  function
  | AnonFunction (args, vals) -> create args vals tfunction
  | ArrowFunction (args, vals) -> create args vals tarrowfun
  | _ as ast -> eval_exp ctx ast

(**main expression interpreter*)
and eval_exp ctx =
  let add_ctx = add_ctx ctx in
  function
  | Const x -> return (ctx, const_to_val x)
  | BinOp (op, a, b) -> ctx_change_bop ctx op a b
  | UnOp (op, a) -> ctx_change_unop ctx op a
  | Var id -> ctx_get_var ctx id >>| fun (var, _) -> ctx, var.value
  | FunctionCall (var, args) ->
    eval_exp ctx var
    <?> "error while try get function"
    >>= fun (ctx, f) ->
    fold_left_map eval_exp ctx args
    <?> "error in function arguments"
    >>= fun (ctx, args) -> eval_fun ctx f args
  | AnonFunction (args, vals) -> create_func ctx "" args vals tfunction
  | ArrowFunction (args, vals) -> create_func ctx "" args vals tarrowfun
  | ObjectDef x ->
    fold_left_map
      (fun ctx (key, value) ->
        eval_exp ctx key
        >>= fun (ctx, key) ->
        to_vstring ctx key
        >>= get_vstring ctx
        >>= fun id ->
        eval_for_top_val ctx id value
        >>| fun (ctx, value) -> ctx, { var_id = id; is_const = false; value })
      ctx
      x
    >>= fun (ctx, fields) -> add_obj ctx fields TObject
  | _ -> ensup ""

(**---------------Statement interpreter---------------*)

and parse_stm ctx = function
  | Return x ->
    eval_exp ctx x
    <?> "error in return expression"
    >>| fun (ctx, ret) -> { ctx with vreturn = Some ret }
  | VarInit x ->
    eval_for_top_val ctx x.var_identifier x.value
    <?> "error in var declaration expression"
    >>= fun (ctx, v) ->
    lex_add
      ctx
      ctx.cur_lex_env
      { var_id = x.var_identifier; is_const = x.is_const; value = v }
  | If (condition, then_stm, else_stm) ->
    eval_exp ctx condition
    >>= fun (ctx, res) ->
    to_vbool res
    >>= get_vbool ctx
    >>= (function
     | true -> parse_stm ctx then_stm
     | false -> parse_stm ctx else_stm)
  | Block ast ->
    create_local_ctx ctx ctx.cur_lex_env Block
    >>= fun ctx ->
    parse_stms ctx ast
    <?> "error while interpret block"
    >>| fun (ctx, ret) ->
    (match ret with
     | Some _ as x -> { ctx with vreturn = x }
     | _ -> ctx)
  | FunInit _ -> return ctx
  | Expression e -> eval_exp ctx e >>| fst <?> "error in expression statement"
  | _ -> ensup ""

and parse_stms ctx ast =
  prefind_funcs ctx ast
  >>= fun ctx ->
  fold_left_s parse_stm (fun ctx -> is_some ctx.vreturn) ctx ast >>= end_of_block
;;

let interpret_ast ast : (program_return, string) Result.t =
  match ast with
  | Programm x ->
    context_init
    >>= fun ctx ->
    parse_stms ctx x
    >>= fun (ctx, ret) ->
    print_vvalues ctx @@ get_vreturn ret
    >>| fun ret -> { stdout = ctx.stdout; return = ret }
  | _ ->
    error @@ AstError "expect Programm in start of programm, but something else was given"
;;

let interpret input : (program_return, string) Result.t =
  match parse input with
  | Result.Ok ast -> interpret_ast ast <?> "Interpreter error"
  | Result.Error (`ParsingError str) -> error (SyntaxError str) <?> "Parser error"
;;
