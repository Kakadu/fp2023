(** Copyright 2023-2024, Julia Kononova *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open State_type
open Monad.Monad_TypeCheck

let val_to_type = function
  | VInt _ -> TInt
  | VChar _ -> TChar
  | VBool _ -> TBool
  | VString _ -> TString
;;

let vart_to_type = function
  | TVar t -> t
;;

let vardec_to_type = function
  | Var_Declaration (t, _) -> return (vart_to_type t)
;;

let find_mem_type = function
  | VType (TVar n) | Field { f_type = TVar n } | Method { m_type = TReturn n } -> return n
  | Constructor c -> return (TObj c.c_name)
  | _ -> fail (Typecheck_error Mismatch)
;;

let name_to_obj_ctx n = read_local_el n

let eq e el1 el2 =
  match e el1 el2 with
  | true -> return el1
  | false -> fail (Typecheck_error Mismatch)
;;

let eq_type t1 t2 = eq equal_typ t1 t2
let eq_name n1 n2 = eq equal_name n1 n2

let eq_name_return_ctx n1 n2 m m_t =
  match equal_name n1 n2 with
  | true -> Some (m_t m)
  | false -> None
;;

let get_class_mem name = function
  | CField (f, _) -> eq_name_return_ctx f.f_name name f (fun f -> Field f)
  | CMethod (m, _) -> eq_name_return_ctx m.m_name name m (fun m -> Method m)
  | CConstructor (c, _) -> eq_name_return_ctx c.c_name name c (fun c -> Constructor c)
;;

let get_interface_mem name = function
  | IMethod m -> eq_name_return_ctx m.m_name name m (fun m -> Method m)
;;

let find_member_from_obj obj_name name =
  let find_mem b name f = List.find_map (f name) b in
  let find_cl_mem b name = find_mem b name get_class_mem in
  let find_itf_mem b name = find_mem b name get_interface_mem in
  read_global_el obj_name
  >>= function
  | TC_Class { cl_body } -> find_cl_mem cl_body name |> return
  | TC_Interface ({ i_body }, _) -> find_itf_mem i_body name |> return
;;

let is_public obj_name ctx = function
  | Some Public -> return ctx
  | Some Protected -> fail (Typecheck_error Access_error)
  | Some Private -> fail (Typecheck_error Access_error)
  | None ->
    read_global_el obj_name
    >>= (function
     | TC_Class _ -> fail (Typecheck_error Access_error)
     | TC_Interface _ -> return ctx)
;;

let find_obj_mem_with_fail n_obj n_mem =
  find_member_from_obj n_obj n_mem
  >>= function
  | Some mem ->
    (match mem with
     | Field f -> is_public n_obj (Field f) f.f_modifier
     | Method m -> is_public n_obj (Method m) m.m_acc_modifier
     | Constructor c -> is_public n_obj (Constructor c) c.c_modifier
     | _ ->
       fail
         (Typecheck_error
            (Impossible_result "Object can only have fields, constructors and methods")))
  | None -> fail (Typecheck_error (Other "Member not found"))
;;

let tc_access_by_p e1 e2 =
  let get_first_mem_class_name = function
    | VType (TVar (TObj n)) | Field { f_type = TVar (TObj n) } -> return n
    | _ -> fail (Typecheck_error (Other "Point access check error"))
  in
  let get_next_mem_class_name = function
    | Field { f_type = TVar (TObj n) } -> return n
    | _ -> fail (Typecheck_error (Other "Point access check error"))
  in
  let next_step mem_ctx expr f =
    get_next_mem_class_name mem_ctx >>= fun p_name -> f p_name expr
  in
  let rec acc_by_p n_obj = function
    | Exp_Name n_mem -> find_obj_mem_with_fail n_obj n_mem
    | Access_By_Point (e1, e2) ->
      (match e1 with
       | Exp_Name n_mem ->
         find_obj_mem_with_fail n_obj n_mem >>= fun mem -> next_step mem e2 acc_by_p
       | _ -> fail (Typecheck_error (Other "Point access check error")))
    | _ -> fail (Typecheck_error (Other "Point access check error"))
  in
  let acc_by_p_ = function
    | Exp_Name n_mem -> read_local_el n_mem
    | Access_By_Point (e1, e2) ->
      (match e1 with
       | Exp_Name n_mem -> read_local_el n_mem >>= fun mem -> next_step mem e2 acc_by_p
       | _ -> fail (Typecheck_error (Other "Point access check error")))
    | _ -> fail (Typecheck_error (Other "Point access check error"))
  in
  match e1 with
  | Exp_Name n_mem ->
    let get_el = read_local_el n_mem in
    let get_inst_tp = get_el >>= get_first_mem_class_name in
    let eq_name_ cl_n cur_cl_n =
      equal_name cl_n cur_cl_n
      |> function
      | true -> acc_by_p_ e2
      | false -> acc_by_p cl_n e2
    in
    get_inst_tp >>= fun cl_name -> get_cur_class_name >>= eq_name_ cl_name
  | _ -> fail (Typecheck_error (Other "Point access check error"))
;;

let tc_method_args (Params params) args expr_tc =
  let params_to_list_of_type p =
    List.map
      (function
        | Var_Declaration (t, _) -> vart_to_type t)
      p
  in
  let args_to_list_of_type a = map (fun x -> expr_tc x >>= fun x -> find_mem_type x) a in
  let compare_two_lists l1 l2 eq rez =
    match List.compare_lengths l1 l2 with
    | 0 ->
      (match List.equal eq l1 l2 with
       | true -> return rez
       | false -> fail (Typecheck_error (Other "Method invocation check error")))
    | _ -> fail (Typecheck_error (Other "Method invocation check error"))
  in
  args_to_list_of_type args
  >>= fun args -> compare_two_lists (params_to_list_of_type params) args equal_typ params
;;

let tc_method_invoke e args expr_tc =
  expr_tc e
  >>= function
  | Method m ->
    tc_method_args m.m_params args expr_tc
    *>
      (match m.m_type with
      | TReturn t -> return (VType (TVar t))
      | TVoid -> fail (Typecheck_error (Other "Method invocation check error")))
  | Constructor c ->
    tc_method_args c.c_params args expr_tc *> return (VType (TVar (TObj c.c_name)))
  | _ -> fail (Typecheck_error (Other "Method invocation check error"))
;;

let find_expr_typ e expr_tc = expr_tc e >>= fun e -> find_mem_type e

let tc_bin_op b e1 e2 expr_tc =
  let compare_two_expr_typ e1 e2 =
    find_expr_typ e1 expr_tc
    >>= fun e1 -> find_expr_typ e2 expr_tc >>= fun e2 -> eq_type e1 e2
  in
  let compare_three_expr_typ e1 e2 t =
    compare_two_expr_typ e1 e2 >>= fun e -> eq_type e t
  in
  let return_rez rez = return (VType (TVar rez)) in
  match b with
  | Plus | Asterisk | Dash | Slash | Mod ->
    compare_three_expr_typ e1 e2 TInt *> return_rez TInt
  | Less | LessOrEqual | More | MoreOrEqual ->
    compare_three_expr_typ e1 e2 TInt *> return_rez TBool
  | Equal | NotEqual -> compare_two_expr_typ e1 e2 *> return_rez TBool
  | And | Or -> compare_three_expr_typ e1 e2 TBool *> return_rez TBool
  | Assign ->
    find_expr_typ e1 expr_tc >>= fun e -> compare_two_expr_typ e1 e2 *> return_rez e
;;

let tc_un_op u e expr_tc =
  let un_op u e =
    find_expr_typ e expr_tc
    >>= fun t ->
    match u with
    | Not -> eq_type t TBool
    | Minus -> eq_type t TInt
    | New ->
      (match t with
       | TObj _ -> return t
       | _ -> fail (Typecheck_error Mismatch))
  in
  un_op u e >>= fun t -> return (VType (TVar t))
;;

let tc_expr =
  let rec tc_expr_ = function
    | Exp_Const t -> return (VType (TVar (val_to_type t)))
    | Exp_Name n -> name_to_obj_ctx n
    | Method_invoke (e, Args args) -> tc_method_invoke e args tc_expr_
    | Access_By_Point (e1, e2) -> tc_access_by_p e1 e2
    | Bin_op (b, e1, e2) -> tc_bin_op b e1 e2 tc_expr_
    | Un_op (u, e) -> tc_un_op u e tc_expr_
  in
  tc_expr_
;;

let tc_expr_with_typ e = tc_expr e >>= fun x -> find_mem_type x

(* STATEMENTS TYPECHECK*)

let eq_type_with_expr t e = tc_expr_with_typ e >>= fun e_t -> eq_type e_t t

let save_decl n ctx =
  read_local_el_opt n
  >>= function
  | None -> write_local_el n ctx
  | Some _ -> fail (Typecheck_error (Other "This variable is already declared"))
;;

let local f = read_local >>= fun old_l -> f *> write_local old_l

let rec tc_statement =
  let is_expr_bool e = tc_expr_with_typ e >>= fun t -> eq_type t TBool in
  let tc_sexpr expr =
    match expr with
    | Method_invoke (e, Args args) ->
      tc_expr e
      >>= (function
       | Method { m_params; m_type = TVoid } ->
         tc_method_args m_params args tc_expr *> return ()
       | _ -> fail (Typecheck_error Mismatch))
    | Bin_op (Assign, _, _) -> tc_expr expr *> return ()
    | _ -> fail (Typecheck_error Mismatch)
  in
  let tc_decl t n = function
    | Some e -> eq_type_with_expr t e *> save_decl n (VType (TVar t)) *> return ()
    | None -> save_decl n (VType (TVar t)) *> return ()
  in
  let tc_return e_opt =
    read_meth_type
    >>= fun m_t ->
    match m_t, e_opt with
    | Some TVoid, None -> return ()
    | Some (TReturn t), Some e ->
      (eq_type_with_expr t e
       <|> fail (Typecheck_error (Other "Returned type does not match the function type"))
      )
      *> return ()
    | _ -> fail (Typecheck_error Mismatch)
  in
  let opt_unpack f = function
    | None -> return ()
    | Some s -> f s *> return ()
  in
  let tc_for_state init cond iter =
    let tc_init = function
      | None -> return ()
      | Some (Decl (Var_Declaration (TVar t, n), e)) -> tc_decl t n e
      | _ -> fail (Typecheck_error Mismatch)
    in
    let tc_cond = opt_unpack is_expr_bool cond in
    let tc_iter = opt_unpack tc_sexpr iter in
    lift3 (fun _ _ _ -> ()) (tc_init init) tc_cond tc_iter
  in
  let tc_if_state cond b s_opt tc_st =
    let tc_cond = is_expr_bool cond in
    let tc_state = function
      | Some st -> tc_st st
      | None -> return ()
    in
    lift3 (fun _ _ _ -> ()) tc_cond (tc_st b) (tc_state s_opt)
  in
  function
  | Expr expr -> tc_sexpr expr
  | Decl (Var_Declaration (TVar t, n), e) -> tc_decl t n e
  | Return e -> tc_return e
  | While (e, s) -> local (is_expr_bool e *> tc_statement s)
  | For (init, cond, iter, b) -> local (tc_for_state init cond iter *> tc_statement b)
  | If (e, b, s_opt) -> local (tc_if_state e b s_opt tc_statement)
  | Body st_l -> local (iter tc_statement st_l)
;;

(* MEMBERS TYPECHECK *)

let tc_member =
  let tc_class_field f_type = function
    | Some e -> eq_type_with_expr (vart_to_type f_type) e *> return ()
    | None -> return ()
  in
  let save_params_to_l (Params params) =
    let f = function
      | Var_Declaration (t, n) -> write_local_el n (VType t)
    in
    iter f params
  in
  let tc_base name base =
    read_global_el name
    >>= function
    | TC_Class cl ->
      (match cl.cl_parent with
       | Some n ->
         (match base with
          | Some (Args args) ->
            read_local_el n
            >>= (function
             | Constructor c -> tc_method_args c.c_params args tc_expr *> return ()
             | _ -> fail (Typecheck_error Mismatch))
          | _ ->
            fail
              (Typecheck_error (Other "Class with a parent must have the base parameter")))
       | None ->
         (match base with
          | None -> return ()
          | _ ->
            fail
              (Typecheck_error
                 (Other "Class without a parent should not have a base parameter"))))
    | TC_Interface _ -> return ()
  in
  let tc_constr typ params body base name =
    local
      (write_meth_type typ
       *> save_params_to_l params
       *> tc_base name base
       *> tc_statement body)
  in
  let tc_meth typ params body =
    local (write_meth_type typ *> save_params_to_l params *> tc_statement body)
  in
  let tc_class_method b = function
    | { m_name; m_poly_modifier; m_type; m_params } ->
      (match equal_name m_name (Name "Main") with
       | true ->
         (match m_poly_modifier, m_params, m_type with
          | Some Static, Params [], TReturn TInt | Some Static, Params [], TVoid ->
            tc_meth m_type (Params []) b *> read_main_class
            >>= (function
             | None -> get_cur_class_name >>= fun n -> write_main_class (Some n)
             | Some _ -> fail (Typecheck_error (Other "Main method already exists")))
          | _, _, _ ->
            fail
              (Typecheck_error
                 (Other
                    "Main must be a static method, have no params and return only int \
                     and void")))
       | false -> tc_meth m_type m_params b)
  in
  function
  | CField ({ f_type }, e_opt) -> tc_class_field f_type e_opt
  | CMethod (m, b) -> tc_class_method b m
  | CConstructor ({ c_params; c_base; c_name }, b) ->
    tc_constr TVoid c_params b c_base c_name
;;

(* OBJECTS TYPECHECK *)

let save_global n ctx =
  read_global_el_opt n
  >>= function
  | None -> write_global_el n ctx
  | Some _ -> fail (Typecheck_error (Other "This variable is already declared"))
;;

let tc_obj =
  let write_mems b =
    let f = function
      | CField (f, _) -> save_decl f.f_name (Field f)
      | CMethod (m, _) -> save_decl m.m_name (Method m)
      | CConstructor _ -> return ()
    in
    iter f b
  in
  let tc_mems b = iter tc_member b in
  let tc_and_write_i_mems b =
    let f = function
      | IMethod m -> save_decl m.m_name (Method m)
    in
    iter f b
  in
  let cl_inher_from_cl body p_body =
    let acc_mod = function
      | Some Public | Some Protected -> true
      | Some Private | None -> false
    in
    let tc_poly_mod p_m body =
      List.exists
        (function
          | CMethod (cl_m, _) ->
            p_m.m_poly_modifier <> None
            && (cl_m = { p_m with m_poly_modifier = Some Override }
                || cl_m
                   = { p_m with
                       m_poly_modifier = Some MNew
                     ; m_acc_modifier = Some Public
                     }
                || cl_m
                   = { p_m with
                       m_poly_modifier = Some MNew
                     ; m_acc_modifier = Some Protected
                     })
          | _ -> true)
        body
    in
    let tc_cl_method_inher p_m p_b body acc =
      acc_mod p_m.m_acc_modifier
      |> function
      | true ->
        List.for_all
          (function
            | CMethod (cl_m, _) -> not (equal_name cl_m.m_name p_m.m_name)
            | _ -> true)
          body
        |> (function
         | false ->
           tc_poly_mod p_m body
           |> (function
            | true -> return acc
            | false -> fail (Typecheck_error (Other "Problem with method modifiers")))
         | true ->
           write_local_el p_m.m_name (Method p_m) *> return (CMethod (p_m, p_b) :: acc))
      | false -> return acc
    in
    let f body acc = function
      | CField (f, f_b) ->
        (match acc_mod f.f_modifier with
         | true ->
           List.exists
             (function
               | CField (cl_f, _) -> cl_f.f_name = f.f_name
               | _ -> false)
             body
           |> (function
            | true -> return acc
            | false -> write_local_el f.f_name (Field f) *> return (CField (f, f_b) :: acc))
         | false -> return acc)
      | CMethod (m, m_b) -> tc_cl_method_inher m m_b body acc
      | CConstructor _ -> return acc
    in
    fold_left (f body) body p_body
  in
  let cl_inher_from_intrfc body _ = function
    | Field f ->
      List.exists
        (function
          | CField (cl_f, _) -> cl_f = { f with f_modifier = Some Public }
          | _ -> false)
        body
    | Method m ->
      List.exists
        (function
          | CMethod (cl_m, _) -> cl_m = { m with m_acc_modifier = Some Public }
          | _ -> false)
        body
    | _ -> false
  in
  let tc_inheritance body = function
    | Some parent ->
      read_global_el parent
      >>= (function
       | TC_Class p_cl -> cl_inher_from_cl body p_cl.cl_body
       | TC_Interface (_, p_map) ->
         (match MapName.for_all (cl_inher_from_intrfc body) p_map with
          | true -> return body
          | false ->
            fail (Typecheck_error (Other "Class incorrectly inherited from an interface"))))
    | None -> return body
  in
  let tc_interface_inheritance = function
    | Some parent ->
      read_global_el parent
      >>= (function
       | TC_Class _ ->
         fail (Typecheck_error (Other "Interface can only inherit from an interface "))
       | TC_Interface (_, p_map) -> return p_map)
    | None -> return MapName.empty
  in
  let save_class cl =
    tc_inheritance cl.cl_body cl.cl_parent
    >>= fun body -> save_global cl.cl_name (TC_Class { cl with cl_body = body })
  in
  let save_i_body_to_map m i =
    let f map = function
      | IMethod m ->
        (match MapName.find_opt m.m_name map with
         | Some _ -> fail (Typecheck_error (Other "Inheritance error "))
         | None -> return (MapName.add m.m_name (Method m) map))
    in
    fold_left f m i
  in
  let save_interface i =
    tc_interface_inheritance i.i_parent
    >>= fun p_map ->
    save_i_body_to_map p_map i.i_body
    >>= fun m -> save_global i.i_name (TC_Interface (i, m))
  in
  let save_constructor cl =
    let f n = function
      | CConstructor (c, _) -> eq_name c.c_name n *> save_decl c.c_name (Constructor c)
      | _ -> return ()
    in
    iter (f cl.cl_name) cl.cl_body
    *> (save_decl
          cl.cl_name
          (Constructor
             { c_modifier = Some Public
             ; c_name = cl.cl_name
             ; c_params = Params []
             ; c_base = None
             })
        <|> return ())
  in
  function
  | Class cl ->
    let { cl_name; cl_body } = cl in
    write_cur_class_name cl_name
    *> local (write_mems cl_body *> save_class cl *> tc_mems cl_body)
    *> save_constructor cl
    *> return ()
  | Interface i ->
    let { i_name; i_body } = i in
    write_cur_class_name i_name
    *> local (tc_and_write_i_mems i_body *> save_interface i)
    *> return ()
;;

let type_checker ast =
  run (iter tc_obj ast) (MapName.empty, MapName.empty, None, None, None)
;;

let type_checker_with_main ast =
  type_checker ast |> fun ((_, _, _, _, main), res) -> main, res
;;
