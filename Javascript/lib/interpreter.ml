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

let first_lex_env = 0
let print_string (ctx : ctx) str = { ctx with stdout = ctx.stdout ^ str ^ "\n" }
let vbool x = VBool x
let vnumber x = VNumber x
let vstring x = VString x
let vobject x = VObject x

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

let is_array x =
  match x.obj_type with
  | TArray _ -> true
  | _ -> false
;;

let get_array_list obj =
  match obj.obj_type with
  | TArray array -> return array
  | _ -> error @@ TypeError "Not array was given"
;;

let print_type ctx = function
  | VNumber _ -> "number"
  | VString _ -> "string"
  | VBool _ -> "boolean"
  | VUndefined -> "undefined"
  | VObject x when is_func_id ctx x -> "function"
  | VNull (*because that's how it is in JS*) | VObject _ -> "object"
;;

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
  { ctx with obj_count = id + 1; objs = IntMap.add id obj ctx.objs }, id
;;

let rec find_in_vars_opt id = function
  | a :: b -> if a.var_id = id then Some a else find_in_vars_opt id b
  | _ -> None
;;

let get_field id fields =
  match find_in_vars_opt id fields with
  | Some x -> x.value
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
    else if is_array obj
    then
      get_array_list obj
      >>= map (fun x -> print_vvalues ctx x ~str_quote:true)
      >>| fun array ->
      if List.length array = 0 then "[]" else asprintf "[ %s ]" (String.concat ", " array)
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
  match find_in_vars_opt var.var_id lex_env.vars with
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

let print ctx values _ =
  fold_left
    (fun ctx value ->
      print_vvalues ctx value >>| fun str -> { ctx with stdout = ctx.stdout ^ str ^ " " })
    ctx
    values
  >>| fun ctx -> { ctx with stdout = ctx.stdout ^ "\n" }, None
;;

let tfunction x = TFunction x
let tarrowfun x = TArrowFunction x
let tfunpreset x = TFunPreset x

let create_func ctx name args obj_type =
  let length = Float.of_int @@ List.length args in
  add_obj
    ctx
    [ { var_id = "name"; is_const = true; value = VString name }
    ; { var_id = "length"; is_const = true; value = VNumber length }
    ]
    obj_type
  >>| fun (ctx, id) -> ctx, vobject id
;;

let create_arrowfunc ctx name args body =
  create_func ctx name args (tarrowfun { parent_lex_env = ctx.cur_lex_env; args; body })
;;

let create_function ctx name args body =
  create_func ctx name args (tfunction { parent_lex_env = ctx.cur_lex_env; args; body })
;;

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
    ; proto_objs = { proto_obj = 0; proto_fun = 0; proto_array = 0 }
    ; vreturn = None
    ; stdout = ""
    }
  in
  let fun_pres ctx name f =
    create_func ctx name [] (TFunPreset f)
    >>| fun (ctx, value) -> ctx, { var_id = name; is_const = true; value }
  in
  let get_this_val = function
    | Some x -> return x
    | None -> error @@ InternalError "Cannot find this"
  in
  let add_proto_objs new_val ctx vals =
    add_obj ctx vals TObject
    >>| fun (ctx, value) -> { ctx with proto_objs = new_val value }
  in
  let proto_obj ctx =
    let valueOf ctx =
      fun_pres ctx "valueOf" (fun ctx _ this ->
        get_this_val this >>| fun x -> ctx, Some (vobject x))
    in
    valueOf ctx
    >>= fun (ctx, v) ->
    add_proto_objs (fun v -> { ctx.proto_objs with proto_obj = v }) ctx [ v ]
  in
  let proto_fun ctx =
    add_proto_objs (fun v -> { ctx.proto_objs with proto_fun = v }) ctx []
  in
  let proto_array ctx =
    let shift ctx =
      fun_pres ctx "shift" (fun ctx _ this ->
        get_this_val this
        >>= fun id ->
        get_obj_ctx ctx id
        >>= fun obj ->
        get_array_list obj
        >>| (function
               | a :: tl -> { obj with obj_type = TArray tl }, a
               | _ -> obj, VUndefined)
        >>| fun (obj, ret) -> { ctx with objs = IntMap.add id obj ctx.objs }, Some ret)
    in
    let unshift ctx =
      fun_pres ctx "unshift" (fun ctx args this ->
        get_this_val this
        >>= fun id ->
        get_obj_ctx ctx id
        >>= fun obj ->
        get_array_list obj
        >>| fun tl ->
        let new_arr = List.append args tl in
        let obj = { obj with obj_type = TArray new_arr } in
        let ret = vnumber (float_of_int @@ List.length new_arr) in
        { ctx with objs = IntMap.add id obj ctx.objs }, Some ret)
    in
    fold_left_map (fun ctx f -> f ctx) ctx [ shift; unshift ]
    >>= fun (ctx, v) ->
    add_proto_objs (fun v -> { ctx.proto_objs with proto_array = v }) ctx v
  in
  let console ctx =
    fun_pres ctx "log" print
    >>= fun (ctx, f) ->
    add_obj ctx [ f ] TObject
    >>= fun (ctx, value) ->
    lex_add
      ctx
      first_lex_env
      { var_id = "console"; is_const = false; value = vobject value }
  in
  let alert ctx =
    fun_pres ctx "alert" print >>= fun (ctx, f) -> lex_add ctx first_lex_env f
  in
  fold_left (fun ctx f -> f ctx) ctx [ proto_obj; proto_fun; proto_array; console; alert ]
;;

let create_local_ctx ctx parent scope this =
  get_lex_env ctx ctx.cur_lex_env
  >>= fun cur_lex_env ->
  let vars =
    match this with
    | Some int -> [ { var_id = "this"; is_const = true; value = VObject int } ]
    | None ->
      (match scope, find_in_vars_opt "this" cur_lex_env.vars with
       | Function, _ | _, None -> []
       | _, Some x -> [ x ])
  in
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

let prefind_funcs ctx ast =
  let ctx_add_if_func ctx = function
    | FunInit x ->
      create_function ctx x.fun_identifier x.arguments x.body
      >>= fun f ->
      let ctx, value = f in
      lex_add ctx ctx.cur_lex_env { var_id = x.fun_identifier; is_const = false; value }
    | _ -> return ctx
  in
  fold_left ctx_add_if_func ctx ast
;;

let obj_get_property ctx obj key =
  match obj with
  | VObject id as obj_val ->
    let obj_type = get_obj_ctx ctx id >>| fun obj -> obj.obj_type in
    let rec go = function
      | VObject x ->
        get_obj_ctx ctx x
        >>= fun obj ->
        (match find_in_vars_opt key obj.fields with
         | None -> go obj.proto
         | Some x -> return @@ Some x.value)
      | _ -> return None
    in
    let if_none f a =
      a
      >>= function
      | Some x -> return x
      | None -> f
    in
    let spec_objs = function
      | TFunPreset _ | TFunction _ | TArrowFunction _ ->
        go (vobject ctx.proto_objs.proto_fun)
      | TArray _ -> go (vobject ctx.proto_objs.proto_array)
      | _ -> return None
    in
    let proto_obj () =
      if_none (return VUndefined) (go (vobject ctx.proto_objs.proto_obj))
    in
    let protos () = obj_type >>= fun typ -> if_none (proto_obj ()) (spec_objs typ) in
    if_none (protos ()) (go obj_val)
  | _ -> ensup "reading from not object"
;;

(**---------------Expression interpreter---------------*)

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

let ctx_get_var ctx id =
  let rec go lex_id =
    get_lex_env ctx lex_id
    >>= fun lex_env ->
    match find_in_vars_opt id lex_env.vars with
    | Some a -> return (a, lex_id)
    | None ->
      (*function mustn't see 'this' in parent lex envs*)
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

(**In this interpretator all context change what valueOf make will be discard*)
let rec get_primitive ctx = function
  | VObject id as x ->
    obj_get_property ctx x "valueOf"
    >>= fun f -> eval_fun ctx f [] (Some id) >>| fun (_, value) -> value
  | _ as t -> return t

and to_string ctx v =
  get_primitive ctx v
  >>= function
  | VString x -> return x
  | VNumber x -> return (num_to_string x)
  | VBool true -> return "true"
  | VBool false -> return "false"
  | VNull -> return "null"
  | VUndefined -> return "undefined"
  | VObject x when is_func_id ctx x -> ensup "conversion func to str"
  | VObject _ -> return "[object Object]"
  | _ as t -> etyp ("cannot cast " ^ print_type ctx t ^ " to string")

and to_bool ctx v =
  get_primitive ctx v
  >>| function
  | VNull | VUndefined -> false
  | VBool x -> x
  | VNumber x when Float.is_nan x || x = 0. -> false
  | VString x when String.trim x = "" -> false
  | _ -> true

and to_number ctx v =
  get_primitive ctx v
  >>| function
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
  | _ -> nan

and ctx_not_change_bop ctx op a b =
  let bop_with_num op = both (to_number ctx) a b >>| fun (a, b) -> vnumber @@ op a b in
  let bop_logical_with_num op =
    both (to_number ctx) a b >>| fun (a, b) -> vbool @@ op a b
  in
  let bop_with_string op = both (to_string ctx) a b >>| fun (a, b) -> vstring @@ op a b in
  let bop_logical_with_string op =
    both (to_string ctx) a b >>| fun (a, b) -> vbool @@ op a b
  in
  let bop_bitwise_shift op b =
    both (to_number ctx) a b
    >>| fun (a, b) -> vnumber @@ Int32.to_float (op (Int32.of_float a) (Int.of_float b))
  in
  let bop_with_int op =
    both (to_number ctx) a b
    >>| fun (a, b) -> vnumber (float_of_int (op (int_of_float a) (int_of_float b)))
  in
  let is_to_string v =
    get_primitive ctx v
    >>| function
    | VString _ | VObject _ -> true
    | _ -> false
  in
  let negotiate res = res >>= fun x -> get_vbool ctx x >>| fun res -> VBool (not res) in
  let add () =
    both is_to_string a b
    >>= fun (l, r) -> if l || r then bop_with_string ( ^ ) else bop_with_num ( +. )
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
    both is_to_string a b
    >>= fun (l, r) ->
    if l && r then bop_logical_with_string ( < ) else bop_logical_with_num ( < )
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
    both (to_bool ctx) a b
    >>| function
    | true, _ -> b_preserved
    | _ -> a_preserved
  in
  let logical_or () =
    let a_preserved = a in
    let b_preserved = b in
    both (to_bool ctx) a b
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
      >>= fun res2 -> ctx_not_change_bop ctx LogicalOr res1 res2
    in
    both is_to_string a b
    >>= fun (l, r) ->
    if l && r
    then bop (fun x -> to_string ctx x >>| vstring) a b
    else bop (fun x -> to_number ctx x >>| vnumber) a b
  in
  let prop_accs () = to_string ctx b >>= obj_get_property ctx a in
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
  | PropAccs -> prop_accs () <?> "error in property accession"
  | _ as a -> ensup @@ asprintf "operator %a not supported yet" pp_bin_op a

and ctx_not_change_unop ctx op a =
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

and eval_fun ctx f args this =
  fold_left_map eval_exp ctx args
  <?> "error in function arguments"
  >>= fun (ctx, args) ->
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
    create_local_ctx ctx f.parent_lex_env scope this
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
  | TFunction f -> valid_and_run ctx Function f
  | TArrowFunction f -> valid_and_run ctx ArrowFunction f
  | TFunPreset f -> f ctx args this >>| fun (ctx, ret) -> ctx, get_vreturn ret
  | _ ->
    error @@ InternalError "get unexpected object type, expect function, but get TObject"

and ctx_change_unop ctx op a =
  let op_new () =
    match a with
    | FunctionCall (f, args) ->
      add_obj ctx [] TObject
      >>= fun (ctx, id) ->
      eval_exp ctx f
      >>= fun (ctx, f) ->
      eval_fun ctx f args (Some id) >>| fun (ctx, _) -> ctx, vobject id
    | _ as v ->
      eval_exp ctx v
      >>= fun (ctx, v) ->
      print_vvalues ctx v >>= fun v -> etyp (v ^ " is not a constructor")
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
        to_string ctx prop
        >>= fun prop ->
        let get_new_obj =
          match prop with
          | "__proto__" ->
            if obj_val = b
            then etyp "Cyclic __proto__ value"
            else return { obj with proto = b }
          | _ ->
            (match find_in_vars_opt prop obj.fields with
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

and eval_for_top_val ctx name = function
  | AnonFunction (args, vals) -> create_function ctx name args vals
  | ArrowFunction (args, vals) -> create_arrowfunc ctx name args vals
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
    let find_this =
      match var with
      | BinOp (PropAccs, x, y) ->
        eval_exp ctx x
        >>= fun (ctx, obj) ->
        let this =
          match obj with
          | VObject id -> Some id
          | _ -> None
        in
        eval_exp ctx y
        >>= fun (ctx, y) ->
        ctx_not_change_bop ctx PropAccs obj y >>| fun res -> (ctx, res), this
      | _ -> eval_exp ctx var >>| fun res -> res, None
    in
    find_this
    <?> "error while try get function"
    >>= fun ((ctx, f), this) -> eval_fun ctx f args this
  | AnonFunction (args, vals) -> create_function ctx "" args vals
  | ArrowFunction (args, vals) -> create_arrowfunc ctx "" args vals
  | ObjectDef x ->
    fold_left_map
      (fun ctx (key, value) ->
        eval_exp ctx key
        >>= fun (ctx, key) ->
        to_string ctx key
        >>= fun id ->
        eval_for_top_val ctx id value
        >>| fun (ctx, value) -> ctx, { var_id = id; is_const = false; value })
      ctx
      x
    >>= fun (ctx, fields) ->
    add_obj ctx fields TObject >>| fun (ctx, id) -> ctx, vobject id
  | ArrayList array ->
    fold_left_map (fun ctx elem -> eval_exp ctx elem) ctx array
    >>= fun (ctx, vals) ->
    add_obj ctx [] (TArray vals) >>| fun (ctx, id) -> ctx, vobject id
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
    to_bool ctx res
    >>= (function
     | true -> parse_stm ctx then_stm
     | false -> parse_stm ctx else_stm)
  | Block ast ->
    create_local_ctx ctx ctx.cur_lex_env Block None
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
