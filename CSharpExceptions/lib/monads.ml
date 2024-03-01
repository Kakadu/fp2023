(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Errors
open Common_types

module BaseSE_Monad = struct
  type ('st, 'a, 'err) t = 'st -> 'st * ('a, 'err) Result.t

  let return : 'a -> ('st, 'a, 'err) t = fun x st -> st, Result.ok x
  let fail : 'err -> ('st, 'a, 'err) t = fun er st -> st, Result.error er

  let ( >>= ) : ('st, 'a, 'err) t -> ('a -> ('st, 'b, 'err) t) -> ('st, 'b, 'err) t =
    fun x f st ->
    let st1, x1 = x st in
    match x1 with
    | Result.Ok x -> f x st1
    | Result.Error s -> st1, Result.error s
  ;;

  let save : 'st -> ('st, unit, 'err) t = fun new_ctx _ -> new_ctx, Result.ok ()
  let read : ('st, 'a, 'err) t = fun st -> (return st) st

  let continue : ('st, 'a, 'err) t -> 'st -> 'st * ('a, 'err) Result.t =
    fun f c_env -> f c_env
  ;;

  let ( <|> ) : ('st, 'a, 'err) t -> ('st, 'a, 'err) t -> ('st, 'a, 'err) t =
    fun a1 a2 st ->
    let st, x = a1 st in
    match x with
    | Result.Ok x -> return x st
    | Result.Error _ -> a2 st
  ;;

  let lift2
    : ('a -> 'b -> 'c) -> ('st, 'a, 'err) t -> ('st, 'b, 'err) t -> ('st, 'c, 'err) t
    =
    fun f a b -> a >>= fun a1 -> b >>= fun b1 -> return (f a1 b1)
  ;;

  let lift3
    :  ('a -> 'b -> 'c -> 'd) -> ('st, 'a, 'err) t -> ('st, 'b, 'err) t
    -> ('st, 'c, 'err) t -> ('st, 'd, 'err) t
    =
    fun f a b c -> a >>= fun a1 -> b >>= fun b1 -> c >>= fun c1 -> return (f a1 b1 c1)
  ;;

  let lift4
    :  ('a -> 'b -> 'c -> 'd -> 'e) -> ('st, 'a, 'err) t -> ('st, 'b, 'err) t
    -> ('st, 'c, 'err) t -> ('st, 'd, 'err) t -> ('st, 'e, 'err) t
    =
    fun f a b c d ->
    lift2
      (fun (a1, b1) (c1, d1) -> f a1 b1 c1 d1)
      (lift2 (fun a1 b1 -> a1, b1) a b)
      (lift2 (fun c1 d1 -> c1, d1) c d)
  ;;

  let ( *> ) : ('st, 'a, 'err) t -> ('st, 'b, 'err) t -> ('st, 'b, 'err) t =
    fun a b -> lift2 (fun _ x -> x) a b
  ;;

  let ( <* ) : ('st, 'a, 'err) t -> ('st, 'b, 'err) t -> ('st, 'a, 'err) t =
    fun a b -> lift2 (fun x _ -> x) a b
  ;;

  let ( >>| ) : ('st, 'a, 'err) t -> ('a -> 'b) -> ('st, 'b, 'err) t =
    fun x f -> x >>= fun x_res -> return (f x_res)
  ;;

  let map_left : ('a -> ('st, 'b, 'err) t) -> 'a list -> ('st, 'b list, 'err) t =
    fun custom_f mlst ->
    let f acc cur = acc >>= fun tl -> custom_f cur >>= fun x -> return (x :: tl) in
    List.fold_left f (return []) mlst >>| List.rev
  ;;

  let iter_left : ('a -> ('st, unit, 'err) t) -> 'a list -> ('st, unit, 'err) t =
    fun custom_f mlst ->
    let f acc cur = acc *> custom_f cur *> return () in
    List.fold_left f (return ()) mlst
  ;;
end

module Type_check_Monad = struct
  open Env_types.Type_check_env
  include BaseSE_Monad

  type ctx_env = type_check_ctx
  type 'a t = ctx_env -> ctx_env * ('a, error) Result.t

  let choice : 'a t list -> 'a t = function
    | [] -> fail (Type_check_error (Other "Empty choice"))
    | h :: tl -> List.fold_left ( <|> ) h tl
  ;;

  let run : 'a t -> ctx_env * ('a, error) Result.t =
    fun f -> f (Code_Map.empty, Ident_Map.empty, None, None)
  ;;

  let save_local : t_loc_env -> unit t =
    fun l_env c_env ->
    let code, _, tp, main = c_env in
    (code, l_env, tp, main), Result.ok ()
  ;;

  let save_local_el : ident -> t_env_value -> unit t =
    fun id t_val c_env ->
    let _, local_env, _, _ = c_env in
    let new_l = Ident_Map.add id t_val local_env in
    save_local new_l c_env
  ;;

  let save_global : text -> unit t =
    fun g_env c_env ->
    let _, local_env, tp, main = c_env in
    (g_env, local_env, tp, main), Result.ok ()
  ;;

  let save_global_el : code_ident -> code_ctx -> unit t =
    fun id ctx c_env ->
    let code, _, _, _ = c_env in
    let new_l = Code_Map.add id ctx code in
    save_global new_l c_env
  ;;

  let save_scope_tp : meth_type option -> unit t =
    fun new_scope_tp st ->
    let code, local_env, _, main = st in
    (code, local_env, new_scope_tp, main), Result.ok ()
  ;;

  let save_main_ctx : code_ident option -> unit t =
    fun class_with_main st ->
    let code, local_env, scope_tp, _ = st in
    (code, local_env, scope_tp, class_with_main), Result.ok ()
  ;;

  let read : ctx_env t = fun st -> (return st) st

  let read_local_el : ident -> t_env_value t =
    fun id c_env ->
    let _, local_env, _, _ = c_env in
    match Ident_Map.find_opt id local_env with
    | Some x -> return x c_env
    | None -> fail (Type_check_error (Not_find_ident id)) c_env
  ;;

  let read_local_el_opt : ident -> t_env_value option t =
    fun id c_env -> (read_local_el id >>| (fun x -> Some x) <|> return None) c_env
  ;;

  let read_global_el : code_ident -> code_ctx t =
    fun id c_env ->
    let id_ =
      match id with
      | Code_ident x -> x
    in
    let code, _, _, _ = c_env in
    match Code_Map.find_opt id code with
    | Some x -> return x c_env
    | None -> fail (Type_check_error (Not_find_ident id_)) c_env
  ;;

  let read_global_el_opt : code_ident -> code_ctx option t =
    fun id c_env -> (read_global_el id >>| (fun x -> Some x) <|> return None) c_env
  ;;

  let read_scope_tp : meth_type option t = function
    | t, l, scope_tp, c -> (return scope_tp) (t, l, scope_tp, c)
  ;;

  let read_main_ctx : code_ident option t = function
    | t, l, m, id -> (return id) (t, l, m, id)
  ;;
end

module Eval_Monad = struct
  open Env_types.Eval_env

  type ctx_env = interpret_ctx
  type ('a, 'b) t = ctx_env -> ctx_env * ('a, 'b, error) eval_t

  (* ****************** State-error monad functionality ****************** *)

  let return : ('a, 'b, error) eval_t -> ('a, 'b) t = fun x st -> st, x
  let return_n : 'a -> ('a, 'b) t = fun x st -> st, nsig x
  let return_r : 'b option -> ('a, 'b) t = fun x st -> st, rsig x
  let return_b : 'c -> ('a, 'b) t = fun _ st -> st, bsig
  let return_e : address -> ('a, 'b) t = fun ad st -> st, esig ad
  let fail : error -> ('a, 'b) t = fun er st -> st, error er

  let ( >>= ) : ('a, 'c) t -> ('a -> ('b, 'c) t) -> ('b, 'c) t =
    fun x f st ->
    let st1, x1 = x st in
    match x1 with
    | Eval_res x -> f x st1
    | Signal s ->
      (match s with
       | Return x -> return_r x st1
       | Exn ad -> return_e ad st1
       | Break -> return_b () st1
       | Err err -> fail err st1)
  ;;

  let ( |>>= )
    : 'a 'c 'b 'd.
    ('a, 'c) t -> ('c option -> ('b, 'd) t) * ('a -> ('b, 'd) t) -> ('b, 'd) t
    =
    fun x f_tpl st ->
    let ret_f, err_g = f_tpl in
    let st1, x1 = x st in
    match x1 with
    | Eval_res x -> err_g x st1
    | Signal s ->
      (match s with
       | Return x -> ret_f x st1
       | Exn ad -> return_e ad st1
       | Err err -> fail err st1
       | Break ->
         fail
           (Interpret_error
              (Runtime_error "Impossible using of break statement without loop"))
           st1)
  ;;

  let ( @!|>>= ) : ('a, 'b) t -> (('a, 'b, error) eval_t -> ('d, 'c) t) -> ('d, 'c) t =
    fun x f st ->
    let st1, x1 = x st in
    f x1 st1
  ;;

  let ( <|> ) : ('b, 'a) t -> ('b, 'a) t -> ('b, 'a) t =
    fun a1 a2 st ->
    let st1, x1 = a1 st in
    match x1 with
    | Eval_res x -> return_n x st1
    | Signal s ->
      (match s with
       | Return x -> return_r x st1
       | Exn ad -> return_e ad st1
       | Break -> return_b () st1
       | Err _ -> a2 st)
  ;;

  let init_sys_mem =
    let empt = Sys_Map.empty in
    Sys_Map.add Fl_descriptors (Files (i_ln 0, Intern_Mem.empty)) empt
  ;;

  let run : 'c Code_Map.t -> ('a, 'b) t -> ctx_env * ('a, 'b, error) eval_t =
    fun glenv f ->
    f (glenv, (ln (-1), [ Ident_Map.empty ]), (ln 0, Mem_Map.empty), init_sys_mem)
  ;;

  let save : ctx_env -> (unit, 'b) t = fun new_ctx _ -> new_ctx, nsig ()
  let read : (ctx_env, 'c) t = fun st -> (return_n st) st
end

module Eval_M_Extention = struct
  include Eval_Monad

  let ( >>| ) x f = x >>= fun x_res -> return_n (f x_res)
  let ( *> ) a b = a >>= fun _ -> b >>= fun b1 -> return_n b1
  let lift2 f a b = a >>= fun a1 -> b >>= fun b1 -> return_n (f a1 b1)

  let fold_left custom_f acc mlst =
    let f acc cur = acc >>= fun acc_ -> custom_f acc_ cur >>= return_n in
    List.fold_left f (return_n acc) mlst
  ;;

  let map_left custom_f mlst =
    let f acc cur = acc >>= fun tl -> custom_f cur >>= fun x -> return_n (x :: tl) in
    List.fold_left f (return_n []) mlst >>| List.rev
  ;;

  let map2_left custom_f alst blst =
    let f acc a b = acc >>= fun tl -> custom_f a b >>= fun x -> return_n (x :: tl) in
    List.fold_left2 f (return_n []) alst blst >>| List.rev
  ;;

  let iter_left custom_f mlst =
    let f acc cur = acc *> custom_f cur *> return_n () in
    List.fold_left f (return_n ()) mlst
  ;;

  let iter2_left custom_f alst blst =
    let f acc a b = acc *> custom_f a b *> return_n () in
    List.fold_left2 f (return_n ()) alst blst
  ;;
end

module Eval_M_State_Extention = struct
  include Eval_M_Extention
  open Env_types.Eval_env

  (* ****************** Global handling ****************** *)

  let read_global : code_ident -> (code_ctx, 'c) t =
    fun id st ->
    let code, _, _, _ = st in
    match Code_Map.find_opt id code with
    | Some x -> return_n x st
    | None -> fail (Interpret_error (Runtime_error "Non existent id")) st
  ;;

  let find_cl_decl_meth id decl =
    let f acc el =
      let is_method (sign, body) =
        match sign with
        | { m_id } when equal_ident m_id id -> Some (to_meth sign body)
        | _ -> None
      in
      let is_cons (sign, body) =
        match sign with
        | { con_id } when equal_ident con_id id -> Some (to_cons sign body)
        | _ -> None
      in
      match acc, el with
      | Some x, _ -> Some x
      | None, Method (sign, b) -> is_method (sign, b)
      | None, Constructor (sign, b) -> is_cons (sign, b)
      | _ -> None
    in
    List.fold_left f None decl.cl_mems
  ;;

  let find_cl_meth id cl =
    let decl = get_class_decl cl in
    find_cl_decl_meth id decl
  ;;

  let read_global_method cl_id el_id =
    read_global cl_id >>= fun cl -> find_cl_meth el_id cl |> return_n
  ;;

  (* ****************** Memory handling ****************** *)

  let save_mem : memory -> (unit, 'c) t =
    fun mem (code, l_env, _, smem) -> (code, l_env, mem, smem), nsig ()
  ;;

  let read_mem = read >>| fun (_, _, mem, _) -> mem

  let alloc_instance custom_f decl =
    let { cl_id; cl_mems } = decl in
    let g acc cur =
      let eval = function
        | Some e -> custom_f e
        | None -> return_n (to_const Not_init)
      in
      acc
      >>= fun tl ->
      match cur with
      | Fild (sign, e_opt) ->
        let { f_id } = sign in
        eval e_opt >>| fun v -> Ident_Map.add f_id (v, sign) tl
      | _ -> return_n tl
    in
    List.fold_left g (return_n Ident_Map.empty) cl_mems
    >>= fun filds ->
    read_mem
    >>= fun (ad, mem) ->
    let new_mem = Mem_Map.add ad (Code_ident cl_id, filds) mem in
    let new_ad = incr_ ad in
    save_mem (new_ad, new_mem) *> return_n ad
  ;;

  let read_instance ad =
    read_mem
    >>= fun (_, mem) ->
    match Mem_Map.find_opt ad mem with
    | Some el -> return_n el
    | None -> fail (Interpret_error (System_error "Addressing a non-existent address"))
  ;;

  let save_instance ad mem_el =
    read_mem >>= fun (x, mem) -> save_mem (x, Mem_Map.add ad mem_el mem)
  ;;

  let read_inst_cl ad = read_instance ad >>= fun (cl_id, _) -> return_n cl_id

  let read_inst_el id ad =
    read_instance ad
    >>= fun (_, el) ->
    match Ident_Map.find_opt id el with
    | Some x -> return_n x
    | None -> fail (Interpret_error (Runtime_error "Non existent id"))
  ;;

  let read_inst_el id ad = read_inst_el id ad >>| fun (v, _) -> v

  let read_inst_meth id ad =
    read_instance ad
    >>= fun (cl_id, _) ->
    read_global_method cl_id id
    >>= function
    | Some x -> return_n (to_code x)
    | None -> fail (Interpret_error (Runtime_error "Non existent id"))
  ;;

  let update_instance_el id ad v =
    return_n () *> read_instance ad
    >>= fun (cl_id, el) ->
    match Ident_Map.find_opt id el with
    | Some (_, sign) ->
      let new_ = Ident_Map.add id (v, sign) el in
      save_instance ad (cl_id, new_)
    | None -> fail (Interpret_error (Runtime_error "Non existent id"))
  ;;

  (* ****************** Sys_memory handling ****************** *)

  let save_smem : sys_memory -> (unit, 'c) t =
    fun smem (code, l_env_l, mem, _) -> (code, l_env_l, mem, smem), nsig ()
  ;;

  let read_smem = read >>| fun (_, _, _, smem) -> smem

  (* ****************** Local_env handling ******************* *)

  let save_local : t_loc_env -> (unit, 'c) t =
    fun l_env_l (code, _, mem, smem) -> (code, l_env_l, mem, smem), nsig ()
  ;;

  let read_local = read >>| fun (_, l_env_l, _, _) -> l_env_l

  let find_local_ id l_env_l =
    let f acc x =
      match acc with
      | Some x -> Some x
      | None -> Ident_Map.find_opt id x
    in
    List.fold_left f None l_env_l
    |> function
    | Some x -> return_n x
    | None -> fail (Interpret_error (Runtime_error "Just for skip"))
  ;;

  let find_local_split_ id l_env_l =
    let f acc x =
      match acc with
      | fist_part, None, second_part ->
        (match second_part with
         | [] ->
           Ident_Map.find_opt id x
           |> (function
            | Some v -> fist_part, Some (x, v), []
            | None -> x :: fist_part, None, [])
         | x :: tl ->
           Ident_Map.find_opt id x
           |> (function
            | Some v -> fist_part, Some (x, v), tl
            | None -> x :: fist_part, None, tl))
      | fist_part, Some x, second_part -> fist_part, Some x, second_part
    in
    List.fold_left f ([], None, l_env_l) l_env_l
    |> fun (fst, lenv, tl) ->
    match lenv with
    | Some x -> List.rev fst |> fun fst -> return_n (fst, x, tl)
    | None -> fail (Interpret_error (Runtime_error "Non existent id"))
  ;;

  let read_local_el id =
    read_local
    >>= fun (ad, l_env) ->
    find_local_ id l_env <|> read_inst_el id ad <|> read_inst_meth id ad
  ;;

  let update_local_el id v =
    read_local
    >>= fun (ad, l_env_l) ->
    let s_local =
      let new_l_env l_env = return_n @@ Ident_Map.add id v l_env in
      let is_mutable_ = function
        | ICode _ -> fail (Interpret_error (Syntax_error "Methods cannot be assignable"))
        | x -> return_n x
      in
      let update_l_env l_env v = is_mutable_ v *> new_l_env l_env in
      find_local_split_ id l_env_l
      >>= function
      | [], (x, v), tl -> update_l_env x v >>= fun x -> save_local (ad, x :: tl)
      | fst, (x, v), [] ->
        update_l_env x v >>= fun x -> save_local (ad, List.append fst [ x ])
      | fst, (x, v), tl ->
        update_l_env x v >>= fun x -> save_local (ad, List.append fst (x :: tl))
    in
    let s_self = update_instance_el id ad v in
    s_local <|> s_self
  ;;

  let add_local_el id v =
    read_local
    >>= fun (ad, l_env_l) ->
    match l_env_l with
    | [] -> fail (Interpret_error (System_error "Problems with the local environment"))
    | l_env :: tl ->
      let s_local =
        let new_l_env = Ident_Map.add id v l_env in
        save_local (ad, new_l_env :: tl)
      in
      let cond = read_local_el id *> return_n false <|> return_n true in
      cond
      >>= (function
       | true -> s_local
       | false ->
         let (Id s) = id in
         fail (Interpret_error (Runtime_error ("The id:" ^ s ^ " already exists"))))
  ;;

  let read_self_ad = read_local >>= fun (self_ad, _) -> return_n self_ad
  let save_self_ad ad = read_local >>= fun (_, lenv) -> save_local (ad, lenv)
end

module Eval = struct
  open Env_types.Eval_env
  include Eval_M_State_Extention

  let local f =
    read_local
    >>= fun (ad, l_env_l) ->
    let return_env =
      read_local
      >>= function
      | _, [] ->
        fail (Interpret_error (System_error "Problems with the local environment"))
      | ad, _ :: tl -> save_local (ad, tl)
    in
    let new_l_env_l = save_local (ad, Ident_Map.empty :: l_env_l) in
    (new_l_env_l *> f)
    @!|>>= function
    | Eval_res x -> return_env *> return_n x
    | Signal s ->
      (match s with
       | Exn ad -> return_env *> return_e ad
       | Return x -> return_env *> return_r x
       | Break -> return_env *> return_b ()
       | Err x -> fail x)
  ;;

  let in_isolation f =
    read_local
    >>= fun old_env ->
    let return_env = save_local old_env in
    f
    @!|>>= function
    | Eval_res x -> return_env *> return_n x
    | Signal s ->
      (match s with
       | Exn ad -> return_env *> return_e ad
       | Return x -> return_env *> return_r x
       | Break -> return_env *> return_b ()
       | Err x -> fail x)
  ;;

  let run_in_another_self ad new_lenv f =
    let new_f = save_local (ad, new_lenv) *> f in
    in_isolation new_f
  ;;

  let further = function
    | Eval_res x -> return_n x
    | Signal s ->
      (match s with
       | Exn ad -> return_e ad
       | Break -> return_b ()
       | Return x -> return_r x
       | Err err -> fail err)
  ;;

  let run_tcf tf cf ff =
    let save_reserv_mem_and_lenvl =
      lift2 (fun mem lenv -> mem, lenv) read_local read_mem
    in
    let ff_new = local ff in
    let cf_new ad = local (cf ad) @!|>>= fun sig_ -> ff_new *> further sig_ in
    save_reserv_mem_and_lenvl
    >>= fun (old_l_env_l, old_mem) ->
    local tf
    @!|>>= function
    | Eval_res x -> ff_new *> return_n x
    | Signal s ->
      (match s with
       | Exn ad ->
         let restore_old_mem_and_lenvl =
           let save_old_mem_with_exn el =
             let ad_, mem = old_mem in
             save_mem (incr_ ad_, mem) *> read_mem
             >>= fun (x, mem) -> save_mem (x, Mem_Map.add ad_ el mem) *> return_n ad_
           in
           read_instance ad
           >>= fun mem_el -> save_local old_l_env_l *> save_old_mem_with_exn mem_el
         in
         restore_old_mem_and_lenvl >>= cf_new
       | Break ->
         ff_new *> fail (Interpret_error (Runtime_error "Attempt to use break with try"))
       | Return x -> ff_new *> return_r x
       | Err err -> ff_new *> fail err)
  ;;

  let run_method
    :  meth_type -> ident list -> t_env_value list -> address
    -> t_env_value Ident_Map.t list -> ('a, 'c) t -> ('c option, 'd) t
    =
    fun return_tp params args ad base_lenv f ->
    let run_f =
      let add_args_in_lenv =
        let f id env_val = add_local_el id env_val in
        iter2_left f params args
      in
      let if_no_ret _ =
        match return_tp with
        | Void -> return_n None
        | _ ->
          fail
            (Interpret_error
               (Runtime_error "Without return can be used only methods of 'Void' type"))
      in
      let if_ret x = return_n x in
      add_args_in_lenv *> f |>>= (if_ret, if_no_ret)
    in
    run_in_another_self ad base_lenv run_f
  ;;

  let run_loop f_cond f_body =
    let rec helper _ =
      f_cond
      >>= function
      | true -> f_body *> helper ()
      | false -> return_n ()
    in
    local
    @@ helper ()
    @!|>>= function
    | Eval_res _ -> return_n ()
    | Signal s ->
      (match s with
       | Break -> return_n ()
       | Exn x -> return_e x
       | Return x -> return_r x
       | Err err -> fail err)
  ;;
end
