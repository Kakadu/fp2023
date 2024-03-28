(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open State_type
open State_type.St_Interpreter
open Monad.Monad_Interpreter

let is_val = function
  | Value (x, _) -> return x
  | _ -> fail (Interpret_error (Other "It's not a value"))
;;

let is_init x =
  is_val x
  >>= function
  | Init x -> return x
  | _ -> fail (Interpret_error (Other "Value is not initialized"))
;;

let is_init_val x =
  is_val x
  >>= function
  | Init (IValue x) -> return x
  | _ -> fail (Interpret_error (Other "Value is not initialized"))
;;

let is_code = function
  | Code c -> return c
  | _ -> fail (Interpret_error (Other "It's not a method/constructor"))
;;

let is_int x =
  is_init_val x
  >>= function
  | VInt x -> return x
  | _ -> fail (Interpret_error Mismatch)
;;

let is_bool x =
  is_init_val x
  >>= function
  | VBool x -> return x
  | _ -> fail (Interpret_error Mismatch)
;;

let v_int x = VInt x
let v_bool x = VBool x
let v_bool_not x = VBool (not x)
let v_int_minus x = VInt (-x)
let i_const c = return (Value (Init (IValue c), None))
let i_name n = find_local_el n

let get_meth_from_class cl name =
  let f acc = function
    | CMethod (m, b) when equal_name m.m_name name -> return (Some (m, b))
    | _ -> return acc
  in
  fold_left f None cl.cl_body
;;

let i_access_by_point e1 e2 =
  let rec get_access_point_val adr e =
    read_memory_obj adr
    >>= fun obj ->
    match e with
    | Exp_Name n ->
      MapName.find_opt n obj.mems
      |> pipe_name_with_fail n
      >>= (fun (_, vl) -> return (Value (vl, None), adr, n))
      <|> (read_global_el obj.cl_name
           >>= function
           | Int_Class cl ->
             get_meth_from_class cl n
             >>= (function
              | Some (m, b) -> return (Code (Method (m, b)), adr, m.m_name)
              | None ->
                fail (Interpret_error (Impossible_result "Check during typecheck")))
           | _ -> fail (Interpret_error (Impossible_result "Object creation checking")))
    | Access_By_Point (e1, e2) ->
      (match e1 with
       | Exp_Name n ->
         MapName.find_opt n obj.mems
         |> pipe_name_with_fail n
         >>= (function
          | _, Init (IClass cl_adr) -> get_access_point_val cl_adr e2
          | _ -> fail (Interpret_error (Impossible_result "Check during typecheck")))
       | _ -> fail (Interpret_error (Impossible_result "Check during typecheck")))
    | _ -> fail (Interpret_error (Impossible_result "Check during typecheck"))
  in
  match e1 with
  | Exp_Name n ->
    find_local_el n
    >>= (function
     | Value (Init (IClass cl_adr), _) -> get_access_point_val cl_adr e2
     | _ -> fail (Interpret_error (Other "Reference must be to an object")))
  | _ -> fail (Interpret_error (Impossible_result "Check during typecheck"))
;;

let i_method_invoke args i_expr code i_statement =
  let get_args = map i_expr args in
  is_code code
  >>= function
  | Method (m, body) ->
    read_local_adr
    >>= fun adr ->
    get_args >>= fun args -> run_method args m.m_params adr m.m_type (i_statement body)
  | Constructor _ ->
    fail (Interpret_error (Other "Constructor can be used only with 'new' keyword"))
;;

let i_assign e1 e2 i_expr =
  let is_val_with_idx = function
    | Value (_, i) -> return i
    | _ ->
      fail
        (Interpret_error
           (Other "The assignment operator must assign a value to the variable"))
  in
  i_expr e2
  >>= is_init
  >>= fun v ->
  match e1 with
  | Exp_Name n ->
    (read_local
     >>= (fun (idx, l) ->
           read_local_el n
           >>= is_val_with_idx
           >>= fun i -> write_local (idx, MapName.add n (Value (Init v, i)) l))
     <|> (read_local_adr
          >>= fun adr ->
          read_memory_obj adr
          >>= fun obj ->
          MapName.find_opt n obj.mems
          |> function
          | Some (f, _) ->
            write_memory_obj adr { obj with mems = MapName.add n (f, Init v) obj.mems }
          | None -> fail (Interpret_error Mismatch)))
    *> find_local_el n
  | Access_By_Point (e1, e2) ->
    i_access_by_point e1 e2
    >>= (function
     | _, adr, name ->
       read_memory_obj adr
       >>= fun obj ->
       MapName.find_opt name obj.mems
       |> pipe_name_with_fail name
       >>= fun (f, _) ->
       write_memory_obj adr { obj with mems = MapName.add name (f, Init v) obj.mems }
       *> return (Value (Init v, None)))
  | _ ->
    fail
      (Interpret_error
         (Other "The assignment operator must assign a value to the variable"))
;;

let i_constructor_invoke e a i_expr i_statement =
  let get_args = map i_expr a in
  i_expr e
  >>= is_code
  >>= function
  | Constructor (c, _) ->
    get_args >>= fun args -> allocate_object c args i_expr i_statement
  | Method _ -> fail (Interpret_error (Other "'new' can be used only with constructor"))
;;

let i_bin_op bin_op e1 e2 i_expr =
  let r_val op v f =
    lift2 (fun e1 e2 -> e1, e2) (i_expr e1 >>= f) (i_expr e2 >>= f)
    >>= fun (c1, c2) -> return (Value (Init (IValue (v (op c1 c2))), None))
  in
  let int_r_int op = r_val op v_int is_int in
  let int_r_bool op = r_val op v_bool is_int in
  let bool_r_bool op = r_val op v_bool is_bool in
  let not_equal_value c1 c2 =
    equal_value c1 c2
    |> function
    | true -> false
    | false -> true
  in
  let eq op = r_val op v_bool is_init_val in
  match bin_op with
  | Plus -> int_r_int ( + )
  | Asterisk -> int_r_int ( * )
  | Dash -> int_r_int ( - )
  | Slash -> int_r_int ( / )
  | Mod -> int_r_int ( mod )
  | Equal -> eq equal_value
  | NotEqual -> eq not_equal_value
  | Less -> int_r_bool ( < )
  | LessOrEqual -> int_r_bool ( <= )
  | More -> int_r_bool ( > )
  | MoreOrEqual -> int_r_bool ( >= )
  | And -> bool_r_bool ( && )
  | Or -> bool_r_bool ( || )
  | Assign -> i_assign e1 e2 i_expr
;;

let i_un_op un_op e i_expr i_statement =
  let res f v = i_expr e >>= f >>= fun x -> i_const (v x) in
  match un_op with
  | Not -> res is_bool v_bool_not
  | Minus -> res is_int v_int_minus
  | New ->
    (match e with
     | Method_invoke (e, Args args) ->
       i_constructor_invoke e args i_expr i_statement
       >>= fun adr -> return (Value (Init (IClass adr), None))
     | _ -> fail (Interpret_error (Other "'new' can be used only with constructor")))
;;

let i_expr i_statement =
  let check_return = function
    | Some x -> return (Value (x, None))
    | None -> fail (Interpret_error (Other "Void cannot be used with expr"))
  in
  let rec i_expr_ = function
    | Exp_Const c -> i_const c
    | Exp_Name n -> i_name n
    | Bin_op (bin_op, e1, e2) -> i_bin_op bin_op e1 e2 i_expr_
    | Un_op (un_op, e) -> i_un_op un_op e i_expr_ i_statement
    | Method_invoke (e, Args args) ->
      (match e with
       | Exp_Name n ->
         i_name n
         >>= fun el -> i_method_invoke args i_expr_ el i_statement >>= check_return
       | Access_By_Point (e1, e2) ->
         i_access_by_point e1 e2
         >>= fun (vl, adr, _) ->
         write_cur_adr adr *> i_method_invoke args i_expr_ vl i_statement >>= check_return
       | _ -> fail (Interpret_error (Impossible_result "Check during typecheck")))
    | Access_By_Point (e1, e2) -> i_access_by_point e1 e2 >>= fun (vl, _, _) -> return vl
  in
  i_expr_
;;

let i_sexpr expr i_expr i_statement =
  match expr with
  | Method_invoke (e, Args args) ->
    i_expr e
    >>= fun code ->
    i_method_invoke args i_expr code i_statement
    >>= (function
     | None -> return ()
     | Some _ ->
       fail (Interpret_error (Other "The statement can only have a method of void type")))
  | Bin_op (Assign, _, _) -> i_expr expr *> return ()
  | _ -> fail (Interpret_error Mismatch)
;;

let local f =
  let helper idx k v acc =
    match v with
    | Value (v, Some (Idx cur_idx)) ->
      (match cur_idx <= idx with
       | true -> MapName.add k (Value (v, Some (Idx cur_idx))) acc
       | false -> acc)
    | Code c -> MapName.add k (Code c) acc
    | _ -> acc
  in
  read_local
  >>= fun (Idx i, _) ->
  f *> read_local
  >>= fun (_, l) -> write_local (Idx i, MapName.fold (helper i) l MapName.empty)
;;

let bool_expr i_statement e = i_expr i_statement e >>= is_bool

let i_if_state i_statement e b s_opt =
  bool_expr i_statement e
  >>= function
  | true -> i_statement b
  | false ->
    (match s_opt with
     | Some b -> i_statement b
     | None -> return ())
;;

let rec iteration f1 f2 =
  f1
  >>= function
  | true -> f2 *> iteration f1 f2
  | false -> return ()
;;

let i_while_state i_statement e s = iteration (bool_expr i_statement e) (i_statement s)

let i_for_state i_statement init cond iter b =
  let get_init =
    match init with
    | Some init -> i_statement init
    | None -> return ()
  in
  let get_cond =
    match cond, iter with
    | Some c, Some i -> i_expr i_statement i *> bool_expr i_statement c
    | Some c, None -> bool_expr i_statement c
    | None, Some i -> i_expr i_statement i *> return true
    | None, None -> return true
  in
  get_init *> iteration get_cond (i_statement b)
;;

let i_statement =
  let rec i_statement_ = function
    | Expr e -> i_sexpr e (i_expr i_statement_) i_statement_
    | Decl (Var_Declaration (TVar _, n), e) ->
      get_new_idx
      >>= fun new_idx ->
      (match e with
       | Some e ->
         i_expr i_statement_ e
         >>= (function
          | Value (v, _) -> write_new_local_el n (Value (v, Some new_idx))
          | _ -> fail (Interpret_error (Impossible_result "Check during typecheck")))
       | None -> write_new_local_el n (Value (Not_init, Some new_idx)))
    | Return e ->
      (match e with
       | Some e -> i_expr i_statement_ e >>= is_val >>= fun r -> fun_return (Some r)
       | None -> fun_return None)
    | While (e, s) -> local (i_while_state i_statement_ e s)
    | For (init, cond, iter, b) -> local (i_for_state i_statement_ init cond iter b)
    | If (e, b, s_opt) -> local (i_if_state i_statement_ e b s_opt)
    | Body st_l -> local (iter i_statement_ st_l)
  in
  i_statement_
;;

let run_interpreter cl_with_main g_env =
  let get_g_env =
    let f = function
      | Class cl -> write_global_el cl.cl_name (Int_Class cl)
      | Interface intf -> write_global_el intf.i_name (Int_Interface intf)
    in
    let g = function
      | Class cl ->
        (match cl.cl_parent with
         | Some p_n ->
           read_global_el p_n
           >>= (function
            | Int_Interface _ ->
              write_global_el cl.cl_name (Int_Class { cl with cl_parent = None })
            | _ -> return ())
         | None -> return ())
      | Interface _ -> return ()
    in
    iter f g_env *> iter g g_env
  in
  let get_l_env =
    let save_constr cl =
      let f = function
        | CConstructor (c, b) -> write_new_local_el c.c_name (Code (Constructor (c, b)))
        | _ -> return ()
      in
      iter f cl.cl_body
      *> (write_new_local_el
            cl.cl_name
            (Code
               (Constructor
                  ( { c_modifier = Some Public
                    ; c_name = cl.cl_name
                    ; c_params = Params []
                    ; c_base = None
                    }
                  , Body [] )))
          <|> return ())
    in
    let f = function
      | Class cl -> save_constr cl
      | Interface _ -> return ()
    in
    iter f g_env
  in
  get_g_env *> get_l_env *> read_global_el cl_with_main
  >>= function
  | Int_Class cl ->
    find_local_el cl.cl_name
    >>= (function
     | Code (Constructor (c, _)) ->
       allocate_object c [] (i_expr i_statement) i_statement
       >>= fun adr ->
       get_meth_from_class cl (Name "Main")
       >>= (function
        | Some (m, b) -> run_method [] (Params []) adr m.m_type (i_statement b)
        | None -> fail (Interpret_error (Impossible_result "Check during typecheck")))
     | _ -> fail (Interpret_error Mismatch))
  | _ -> fail (Interpret_error (Other "The main method must be in a class"))
;;

let interpreter str =
  match Parser.parse Parser.p_ast str with
  | Result.Ok (Ast ast) ->
    (match Type_checker.type_checker_with_main ast with
     | Some cl_with_main, Result.Ok _ ->
       run (run_interpreter cl_with_main ast)
       |> (function
        | _, Signal (Pipe x) -> Result.Ok x
        | _, IError er -> Result.Error er
        | _, _ ->
          Result.Error
            (Interpret_error (Impossible_result "Run_method returns return or error")))
     | None, Result.Ok _ -> Result.Error (Interpret_error (Other "Main method not found"))
     | _, Result.Error er -> Result.Error er)
  | Result.Error er -> Result.Error (Typecheck_error (Other er))
;;
