(** Copyright 2021-2023, Georgy Sichkar *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
(* open Base *)

(*
   Идея:
   (runtime_ctx, error) Result.t <-| это штука, которая является контекстом
   * runtime_ctx.code -- это хэшмапа, где колючи - имена классов / исключения + вся информация о них
   * runtim_ctx.local_env -- сюда добавляются всякие вычисления (создается новая при заходе в функцию + параметры)
   * runtime_ctx.self -- здесь хранится состояние того класса внутри которого сейчас исполняется код

   (instance) <-| описывает экземпляр класса т.е. его состояние (набор полей)
   * Copy -- эмулирует передачуу по ссылке, означает, что определение надо искать в локальном окружении
   * State -- это оригинальный надор полей в локальном окруженииy

   (после этапа )
*)

(*
   TODO:

   1) все конструкторы добавляются в локальное окружение (в любом локальном окружении есть конструкторы)

   2) добавить в глобальное/локальное окружение дополнительные штуки (типо base_lib)
   * Exception - базовое исключение, которое можно создать
   * метод print (local)
   * метод open_file + close_file + write_file(string)

   3) сделать проверку внутри класса на поля конструктора (крч, если у нас поля есть, они неопределены, и нет конструктора, то фэйл)
   * все конструкторы добавляются в класс автоматически
*)

module Ident = struct
  type t = ident

  let compare = compare
end

(* TODO: переписать красиво *)
type code_ident = Code_ident of ident
type env_ident = Env_ident of ident

module Env_id = struct
  type t = env_ident

  let compare = compare
end

module Code_id = struct
  type t = code_ident

  let compare = compare
end

module IdentMap = Stdlib.Map.Make (Ident)
module CodeMap = Stdlib.Map.Make (Code_id)
module EnvMap = Stdlib.Map.Make (Env_id)

type excaption = Exception of ident

type code_ctx =
  | Exception_ctx of class_decl
    (* TODO: переделывать классы с наследованием в этот тип, так как нормального наследования у меня нет *)
  | Class_ctx of class_decl (* Сюда просто перепихнуть инфу *)

type instance =
  (* TODO: Очень осторожно работать с этим... *)
  | Copy of env_ident (* ident - имя instance в локальном окружении *)
  | State of state
(* Генерируется при создании экземпляра *)

and state = code_ident * env_value EnvMap.t

and env_value =
  | VInstance of instance
  | Value of value_

type env = env_value EnvMap.t
type text = code_ctx CodeMap.t

type runtime_ctx =
  { code : text
  ; local_env : env
  ; self : state
  }

type t_env_value =
  | Metod_sig of method_sign
  | Constructor_sig of constructor_sign
  | Value_sig of var_type
  | Fild_sig of fild_sign
[@@deriving show { with_path = false }]

type t_loc_env = t_env_value IdentMap.t
type type_check_ctx = text * t_loc_env * meth_type option

type type_ =
  | Mtype_ of meth_type
  | Vtype_ of var_type

type error =
  | Not_find_ident of ident
  | Type_mismatch
  | Double_definition
  | Double_definition_of of ident
  | Other_error of string
  | Method_not_find
  | User_exception of excaption
  | Access_error of string

module Type_checker = struct
  type ctx_env = type_check_ctx
  type ('st, 'a) t = 'st -> 'st * ('a, error) Result.t
  type 'a tt = (ctx_env, 'a) t

  let return : 'a -> 'a tt = fun x st -> st, Result.ok x
  let fail : error -> 'a tt = fun er st -> st, Result.error er

  let ( >>= ) : 'a tt -> ('a -> 'b tt) -> 'b tt =
    fun x f st ->
    let st1, x1 = x st in
    match x1 with
    | Result.Ok x -> f x st1
    | Result.Error s -> st1, Result.error s
  ;;

  let save : ctx_env -> unit tt = fun new_ctx _ -> new_ctx, Result.ok ()

  let run : 'a tt -> ctx_env * ('a, error) Result.t =
    fun f -> f (CodeMap.empty, IdentMap.empty, None)
  ;;

  let continue : 'a tt -> ctx_env -> ctx_env * ('a, error) Result.t =
    fun f c_env -> f c_env
  ;;

  let lift2 : ('a -> 'b -> 'c) -> 'a tt -> 'b tt -> 'c tt =
    fun f a b st -> (a >>= fun a1 -> b >>= fun b1 -> return (f a1 b1)) st
  ;;

  let lift3 : ('a -> 'b -> 'c -> 'd) -> 'a tt -> 'b tt -> 'c tt -> 'd tt =
    fun f a b c st ->
    (a >>= fun a1 -> b >>= fun b1 -> c >>= fun c1 -> return (f a1 b1 c1)) st
  ;;

  let lift4 : ('a -> 'b -> 'c -> 'd -> 'e) -> 'a tt -> 'b tt -> 'c tt -> 'd tt -> 'e tt =
    fun f a b c d st ->
    lift2
      (fun (a1, b1) (c1, d1) -> f a1 b1 c1 d1)
      (lift2 (fun a1 b1 -> a1, b1) a b)
      (lift2 (fun c1 d1 -> c1, d1) c d)
      st
  ;;

  let ( *> ) : 'a tt -> 'b tt -> 'b tt = fun a b st -> (lift2 (fun _ x -> x) a b) st
  let ( <* ) : 'a tt -> 'b tt -> 'a tt = fun a b st -> (lift2 (fun x _ -> x) a b) st

  let ( <|> ) : 'a tt -> 'a tt -> 'a tt =
    fun a1 a2 st ->
    let st, x = a1 st in
    match x with
    | Result.Ok x -> return x st
    | Result.Error _ -> a2 st
  ;;

  let choice : 'a tt list -> 'a tt =
    fun l ->
    match l with
    | [] -> fail (Other_error "Empty choice\n")
    | h :: tl -> List.fold_left ( <|> ) h tl
  ;;

  let ( >>| ) : 'a tt -> ('a -> 'b) -> 'b tt =
    fun x f -> x >>= fun x_res -> return (f x_res)
  ;;

  let map_left : ('a -> 'b tt) -> 'a list -> 'b list tt =
    fun custom_f mlst ->
    let f acc cur = acc >>= fun tl -> custom_f cur >>= fun x -> return (x :: tl) in
    List.fold_left f (return []) mlst >>| List.rev
  ;;

  let iter_left : ('a -> unit tt) -> 'a list -> unit tt =
    fun custom_f mlst ->
    let f acc cur = acc >>= fun _ -> custom_f cur >>= fun _ -> return () in
    List.fold_left f (return ()) mlst
  ;;

  let save_local : t_loc_env -> unit tt =
    fun l_env c_env ->
    match c_env with
    | code, _, tp -> (code, l_env, tp), Result.ok ()
  ;;

  let save_local_el : ident -> t_env_value -> unit tt =
    fun id t_val c_env ->
    match c_env with
    | _, local_env, _ ->
      let new_l = IdentMap.add id t_val local_env in
      save_local new_l c_env
  ;;

  let save_global : text -> unit tt =
    fun g_env c_env ->
    match c_env with
    | _, local_env, tp -> (g_env, local_env, tp), Result.ok ()
  ;;

  let save_global_el : code_ident -> code_ctx -> unit tt =
    fun id ctx c_env ->
    match c_env with
    | code, _, _ ->
      let new_l = CodeMap.add id ctx code in
      save_global new_l c_env
  ;;

  let save_scope_tp : meth_type option -> unit tt =
    fun new_scope_tp st ->
    match st with
    | code, local_env, _ -> (code, local_env, new_scope_tp), Result.ok ()
  ;;

  let read : ctx_env tt = fun st -> (return st) st

  let read_local : ident -> t_env_value tt =
    fun id c_env ->
    match c_env with
    | _, local_env, _ ->
      (match IdentMap.find_opt id local_env with
       | Some x -> return x c_env
       | None -> fail (Not_find_ident id) c_env)
  ;;

  let read_local_opt : ident -> t_env_value option tt =
    fun id c_env -> (read_local id >>| (fun x -> Some x) <|> return None) c_env
  ;;

  let read_global : code_ident -> code_ctx tt =
    fun id c_env ->
    let id_ =
      match id with
      | Code_ident x -> x
    in
    match c_env with
    | code, _, _ ->
      (match CodeMap.find_opt id code with
       | Some x -> return x c_env
       | None -> fail (Not_find_ident id_) c_env)
  ;;

  let read_global_opt : code_ident -> code_ctx option tt =
    fun id c_env -> (read_global id >>| (fun x -> Some x) <|> return None) c_env
  ;;

  let read_scope_tp : meth_type option tt =
    fun st ->
    match st with
    | _, _, scope_tp -> (return scope_tp) st
  ;;
end

open Type_checker

let to_atype = function
  | Null -> None
  | VString _ -> Some (TNullable TString)
  | VInt _ -> Some (TNot_Nullable TInt)
  | VChar _ -> Some (TNot_Nullable TChar)
  | VBool _ -> Some (TNot_Nullable TBool)
;;

let to_var_type = function
  | Some x -> Some (TVar x)
  | None -> None
;;

let to_type_v = function
  | Some x -> Some (Value_sig x)
  | None -> None
;;

let to_type_m = function
  | Void -> Result.error "Not assignable type\n"
  | TReturn x -> Result.ok (Value_sig (TVar x))
;;

let to_type_m_opt = function
  | Void -> return None
  | TReturn x -> return (Some (Value_sig (TVar x)))
;;

let to_assign_t = function
  | Value_sig (TVar a_tp) -> Result.ok a_tp
  | Metod_sig { m_modif = _; m_type; _ } ->
    (match m_type with
     | Void -> Result.error "Non-assignable type\n"
     | TReturn a_tp -> Result.ok a_tp)
  | Constructor_sig { con_modif = _; con_id; _ } -> Result.ok (TNullable (TClass con_id))
  | Fild_sig { f_modif = _; f_type = TVar tp; _ } -> Result.ok tp
;;

let opt_to_assign_t = function
  | None -> Result.ok None
  | Some a ->
    (match to_assign_t a with
     | Result.Ok a_tp -> Result.Ok (Some a_tp)
     | Result.Error x -> Result.error x)
;;

let compare a b f =
  let a_tp_res = opt_to_assign_t a in
  let b_tp_res = opt_to_assign_t b in
  match a_tp_res, b_tp_res with
  | Result.Error _, Result.Error _ | _, Result.Error _ | Result.Error _, _ ->
    Result.error "Non-comparable types\n"
  | Result.Ok a_tp_opt, Result.Ok b_tp_opt -> f (a_tp_opt, b_tp_opt)
;;

let compare_t_env_opt f a b =
  match f a b with
  | Result.Ok None -> return None
  | Result.Ok (Some x) -> return (Some (Value_sig (TVar x)))
  | Result.Error msg -> fail (Other_error msg)
;;

let ( =!= ) a b =
  let helper (a_tp_opt, b_tp_opt) =
    match a_tp_opt, b_tp_opt with
    | Some a_tp, Some b_tp when equal_assignable_type a_tp b_tp -> Result.ok a_tp_opt
    | Some (TNullable _), None -> Result.ok a_tp_opt
    | None, Some (TNullable _) -> Result.ok b_tp_opt
    | None, None -> Result.ok None
    | _, _ -> Result.error "Types are not equal\n"
  in
  compare a b helper
;;

let eq_t_env_opt a b = compare_t_env_opt ( =!= ) a b

let ( =!> ) a b =
  match a =!= b with
  | Result.Ok a_tp -> Result.Ok a_tp
  | Result.Error _ ->
    let helper (a_tp_opt, b_tp_opt) =
      match a_tp_opt, b_tp_opt with
      | Some (TNullable (TBase a_tp)), Some (TNot_Nullable b_tp)
        when equal_base_type a_tp b_tp -> Result.ok a_tp_opt
      | _, _ -> Result.error "Types are not equal.\n"
    in
    compare a b helper
;;

let greater_t_env_opt a b = compare_t_env_opt ( =!> ) a b
let params_check f prms = map_left f prms

let v_decl_to_type_v = function
  | Var_decl (tp, _) -> Value_sig tp
;;

let is_success_list lst =
  let f acc el =
    match acc, el with
    | true, Result.Ok _ -> true
    | _, _ -> false
  in
  List.fold_left f true lst
;;

let map2_opt f l1 l2 =
  let len1 = List.length l1 in
  let len2 = List.length l2 in
  match Int.equal len1 len2 with
  | true -> Result.ok (List.map2 f l1 l2)
  | false -> Result.error "len(list1) != len(list2)\n"
;;

let check_method_ tp args params =
  let compare_prms = map2_opt ( =!> ) in
  let to_env elem = Some (v_decl_to_type_v elem) in
  let args_env = List.map to_env args in
  let result =
    params
    >>| fun prms_env ->
    match compare_prms args_env prms_env with
    | Result.Ok ans -> is_success_list ans
    | Result.Error _ -> false
  in
  result
  >>= function
  | true -> return (Some tp)
  | false -> fail Type_mismatch
;;

let check_invoke sign env_prms =
  match sign with
  | Some (Metod_sig { m_modif = _; m_type; m_id = _; m_args = Args args }) ->
    let tp = to_type_m m_type in
    (match tp with
     | Result.Error _ -> fail Type_mismatch
     | Result.Ok tp -> check_method_ tp args env_prms)
  | Some (Constructor_sig { con_modif = _; con_id; con_args; base_params }) ->
    let tp = Value_sig (TVar (TNullable (TClass con_id))) in
    (match con_args, base_params with
     | Args args, None -> check_method_ tp args env_prms
     | _ -> fail (Other_error "Inheritance with constructors is not supported\n"))
  | _ -> fail Type_mismatch
;;

let get_class_decl = function
  | Exception_ctx exc -> exc
  | Class_ctx cl -> cl
;;

let get_sign id el =
  let is_fild s id =
    match s with
    | { f_modif = _; f_type = _; f_id } when equal_ident f_id id -> Some (Fild_sig s)
    | _ -> None
  in
  let is_method s id =
    match s with
    | { m_modif = _; m_type = _; m_id; _ } when equal_ident m_id id -> Some (Metod_sig s)
    | _ -> None
  in
  let is_constructor s id =
    match s with
    | { con_modif = _; con_id; _ } when equal_ident con_id id -> Some (Constructor_sig s)
    | _ -> None
  in
  match el with
  | Fild (sign, _) -> is_fild sign id
  | Method (sign, _) -> is_method sign id
  | Constructor (sign, _) -> is_constructor sign id
  | Main _ -> None
;;

let get_class_member_sign cl_d el_id =
  let f id acc el =
    match acc, el with
    | None, el -> get_sign id el
    | ret, _ -> ret
  in
  List.fold_left (f el_id) None cl_d.cl_mems
;;

let find_global id = read_global (Code_ident id) >>| fun x -> get_class_decl x

let is_public sign =
  match sign with
  | Fild_sig { f_modif = Some (FAccess MPublic); _ }
  | Metod_sig { m_modif = Some (MAccess MPublic); _ }
  | Constructor_sig { con_modif = Some MPublic; _ } -> return sign
  | _ -> fail (Access_error "Attempt to get a private class member\n")
;;

let rec find_class_member expr cl_decl =
  let find_mem id =
    match get_class_member_sign cl_decl id with
    | Some x -> return x
    | None -> fail (Other_error "Parsing error in EPoint_access\n")
  in
  let get_next_decl_id mem_id =
    find_mem mem_id
    >>= is_public
    >>= function
    | Fild_sig { f_modif = _; f_type = TVar (TNullable (TClass id)); _ } -> return id
    | Metod_sig { m_modif = _; m_type = TReturn (TNullable (TClass id)); _ } -> return id
    | Constructor_sig { con_modif = _; con_id; _ } -> return con_id
    | _ -> fail (Access_error "Magic case\n")
  in
  match expr with
  | EIdentifier id -> find_mem id >>= is_public
  | EPoint_access (EIdentifier id, expr1) ->
    get_next_decl_id id >>= find_global >>= find_class_member expr1
  | _ -> fail (Other_error "Parsing error in EPoint_access\n")
;;

let check_point_acc e1 e2 =
  let helper =
    match e1 with
    | EIdentifier id ->
      let local_find = read_local id in
      let global_find id_ = read_global (Code_ident id_) >>| get_class_decl in
      let get_class = function
        | Fild_sig { f_modif = _; f_type = TVar (TNullable (TClass id_)); _ } ->
          return id_
        | Value_sig (TVar (TNullable (TClass id_))) -> return id_
        | _ -> fail (Other_error "Error during parsing in Epoint_access\n")
      in
      local_find >>= get_class >>= global_find >>= find_class_member e2 >>= is_public
    | _ -> fail (Other_error "Error during parsing in Epoint_access\n")
  in
  helper >>| fun x -> Some x
;;

let env_bool = Value_sig (TVar (TNot_Nullable TBool))
let env_bool_null = Value_sig (TVar (TNullable (TBase TBool)))
let env_int = Value_sig (TVar (TNot_Nullable TInt))
let env_int_null = Value_sig (TVar (TNullable (TBase TInt)))
let env_char = Value_sig (TVar (TNot_Nullable TChar))
let env_char_null = Value_sig (TVar (TNullable (TBase TChar)))
let env_string = Value_sig (TVar (TNullable TString))

let operands_eq oper1 oper2 =
  match oper1, oper2 with
  | None, None | None, _ | _, None ->
    fail (Other_error "Using keyword Null with math operations\n")
  | _ -> eq_t_env_opt oper1 oper2
;;

let base_type_eq2 tp0 tp1 tp2 = tp0 >>= eq_t_env_opt tp1 <|> (tp0 >>= eq_t_env_opt tp2)

let check_bin_op op (env1, env2) =
  let is_operands_eq = operands_eq env1 env2 in
  let base_type_eq2 = base_type_eq2 is_operands_eq in
  (* _ *)
  let int_op = base_type_eq2 (Some env_int) (Some env_int_null) in
  let int_bool_op = int_op *> return (Some env_bool) in
  let bool_op = base_type_eq2 (Some env_bool) (Some env_bool_null) in
  let char_op = base_type_eq2 (Some env_char) (Some env_char_null) in
  (*  *)
  let string_op = is_operands_eq >>= eq_t_env_opt (Some env_string) in
  let compare_op = choice [ int_op; bool_op; char_op; string_op ] in
  match op with
  | Asterisk | Plus | Minus | Division | Mod -> int_op
  | Less | LessOrEqual | More | MoreOrEqual -> int_bool_op
  | And | Or -> bool_op
  | Equal | NotEqual -> compare_op *> return (Some env_bool)
  | Assign -> greater_t_env_opt env1 env2
;;

let check_un_op op env1 =
  match op with
  | UMinus -> base_type_eq2 env1 (Some env_int) (Some env_int_null)
  | UNot -> base_type_eq2 env1 (Some env_bool) (Some env_bool_null)
  | New ->
    (* TODO: ВСЕ КОНСТРУКТОРЫ ДОЛЖНЫ БЫТЬ В ЛОКАЛЬНОЙ ОБЛАСТИ ВИДИМОСТИ *)
    env1
    >>= (function
     | Some (Constructor_sig { con_modif = _; con_id; _ }) ->
       return (Some (Value_sig (TVar (TNullable (TClass con_id)))))
     | _ -> fail Type_mismatch)
;;

let check_expr exp =
  let rec helper = function
    | EConst x ->
      (* TODO: отсюда берется None, потому что у Null нет типа *)
      return (to_type_v @@ to_var_type @@ to_atype x)
    | EIdentifier x -> read_local x >>| fun x -> Some x
    | EMethod_invoke (e2, Params prms) ->
      helper e2
      >>= fun x ->
      let env_val_prms = params_check helper prms in
      check_invoke x env_val_prms
    | EPoint_access (e1, e2) -> check_point_acc e1 e2
    | EBin_op (op, e1, e2) ->
      lift2 (fun x y -> x, y) (helper e1) (helper e2) >>= check_bin_op op
    | EUn_op (op, e1) -> check_un_op op (helper e1)
  in
  helper exp
;;

let local_scope mnd =
  read >>= fun old_ctx -> mnd >>= fun res -> save old_ctx >>= fun _ -> return res
;;

type tp_checked =
  | TP_Ok
  | TP_Error

let var_exp_check e env_tp =
  let r_tp x = Some x in
  (check_expr e >>| fun e_tp -> r_tp env_tp =!> e_tp) *> return TP_Ok
;;

let return_check e_opt =
  read_scope_tp
  >>= fun tp ->
  match e_opt, tp with
  | None, Some Void -> return TP_Ok
  | Some e, Some (TReturn r) -> var_exp_check e (Value_sig (TVar r))
  | _ -> fail Type_mismatch
;;

let is_exception_ con_id =
  read_global (Code_ident con_id)
  >>= function
  | Exception_ctx _ -> return TP_Ok
  | _ -> fail (Other_error "throw can be used only with exceprions\n")
;;

let throw_check env_obj =
  let get_class_id =
    match env_obj with
    | Some (Constructor_sig { con_modif = _; con_id; _ }) -> return con_id
    | Some (Fild_sig { f_modif = _; f_type = TVar (TNullable (TClass id)) }) -> return id
    | Some (Value_sig (TVar (TNullable (TClass id)))) -> return id
    | _ -> fail (Other_error "throw can be used only with exceprions\n")
  in
  get_class_id >>= is_exception_
;;

let add_local id tp =
  let is_exist = read_local_opt id in
  is_exist
  >>= function
  | None -> save_local_el id tp
  | Some _ -> fail (Double_definition_of id)
;;

let add_global id tp =
  let is_exist = read_global_opt id in
  is_exist
  >>= function
  | None -> save_global_el id tp
  | Some _ ->
    let (Code_ident id) = id in
    fail (Double_definition_of id)
;;

let add_local_decl id tp = add_local id tp *> return TP_Ok
let cond_check_ e = base_type_eq2 (check_expr e) (Some env_bool) (Some env_bool_null)

let if_else_check e st st_opt st_check =
  let f _ _ _ = TP_Ok in
  let exp_res = cond_check_ e in
  let st_res = st_check st in
  let st_opt_res =
    match st_opt with
    | None -> return TP_Ok
    | Some st1 -> st_check st1
  in
  lift3 f exp_res st_res st_opt_res
;;

let un_opt_ mnd x =
  match x with
  | Some x -> mnd x *> return TP_Ok
  | None -> return TP_Ok
;;

let for_check init_opt e1_opt e2_opt s h =
  let f _ _ _ _ = TP_Ok in
  let init_opt_res = un_opt_ h init_opt in
  let e1_opt_res = un_opt_ check_expr e1_opt in
  let e2_opt_res = un_opt_ check_expr e2_opt in
  let s_res = h s in
  lift4 f init_opt_res e1_opt_res e2_opt_res s_res
;;

let catch_s_check_ h = function
  | None -> return TP_Ok
  | Some (edecl_opt, s) ->
    (match edecl_opt with
     | None -> h s
     | Some (Var_decl (tp, id), e_opt) ->
       let tp = Value_sig tp in
       let add = add_local_decl id tp in
       let is_exc_ =
         match tp with
         | Value_sig (TVar (TNullable (TClass exc_id))) -> is_exception_ exc_id
         | _ -> fail (Other_error "Parsing error in catch statement\n")
       in
       (match e_opt with
        | Some e -> is_exc_ *> add *> cond_check_ e *> return TP_Ok
        | None -> is_exc_ *> add *> return TP_Ok))
;;

let try_catch_fin_check try_s catch_s finally_s h =
  let f _ _ _ = TP_Ok in
  let try_s_res = local_scope (h try_s) in
  let catch_s_res = local_scope (catch_s_check_ h catch_s) in
  let fin_s_res = local_scope (un_opt_ h finally_s) in
  match catch_s, finally_s with
  | None, None -> fail (Other_error "After try shold be catch or finally\n")
  | _ -> lift3 f try_s_res catch_s_res fin_s_res
;;

let check_statement stat =
  let rec helper = function
    | SExpr e -> check_expr e *> return TP_Ok
    | Steps stp ->
      let f acc = helper acc *> return () in
      let check_steps = iter_left f stp *> return TP_Ok in
      local_scope check_steps
    | SReturn e_opt -> return_check e_opt
    | SBreak -> return TP_Ok
    | SThrow e -> check_expr e >>= throw_check
    | SDecl (Var_decl (tp, id), e_opt) ->
      let tp = Value_sig tp in
      let add = add_local_decl id tp in
      (match e_opt with
       | Some e -> var_exp_check e tp *> add
       | None -> add)
    | SIf_else (e, s, st1) -> local_scope @@ if_else_check e s st1 helper
    | SWhile (e, s) -> local_scope @@ (cond_check_ e *> helper s)
    | SFor { f_init_p; f_cond_p; f_iter_p; f_body } ->
      local_scope @@ for_check f_init_p f_cond_p f_iter_p f_body helper
    | STry_catch_fin { try_s; catch_s; finally_s } ->
      try_catch_fin_check try_s catch_s finally_s helper
  in
  helper stat
;;

let memb_check = function
  | Fild (sign, e_opt) ->
    (match e_opt with
     | Some e -> var_exp_check e (Fild_sig sign)
     | None -> return TP_Ok)
  | Method ({ m_modif = _; m_type; _ }, s) ->
    local_scope @@ (save_scope_tp (Some m_type) *> check_statement s)
  | Main (m_type, s) -> local_scope @@ (save_scope_tp (Some m_type) *> check_statement s)
  | Constructor ({ con_modif = _; con_id; _ }, s) ->
    let cons_tp = TReturn (TNullable (TClass con_id)) in
    local_scope @@ (save_scope_tp (Some cons_tp) *> check_statement s)
;;

let class_check cl_decl =
  let { cl_modif = _; cl_id; parent; cl_mems } = cl_decl in
  let add_class =
    match parent with
    | Some id when equal_ident id (Id "Exception") ->
      add_global (Code_ident cl_id) (Exception_ctx cl_decl)
    | None -> add_global (Code_ident cl_id) (Class_ctx cl_decl)
    | _ ->
      fail
        (Other_error
           "Only rudimentary class inheritance is supported from Exception class\n")
  in
  let f = function
    | Fild _ | Main _ | Method _ -> return ()
    | Constructor (sign, _) ->
      let { con_modif = _; con_id; _ } = sign in
      (match equal_ident con_id cl_id with
       | true -> add_local con_id (Constructor_sig sign)
       | false -> fail (Other_error "The constructor name must match the class name\n"))
  in
  let add_constructor = iter_left f cl_mems in
  let g = function
    | Main _ | Constructor _ -> return ()
    | Fild (sign, _) ->
      let { f_modif = _; f_type = _; f_id } = sign in
      add_local f_id (Fild_sig sign)
    | Method (sign, _) ->
      let { m_modif = _; m_type = _; m_id; _ } = sign in
      add_local m_id (Metod_sig sign)
  in
  let add_members = iter_left g cl_mems in
  let helper el = memb_check el *> return () in
  add_class
  *> add_constructor
  *> (local_scope @@ (add_members *> iter_left helper cl_mems))
;;

let type_check ast = 
  let (Ast ast) = ast in
  let helper = iter_left class_check ast in
  run helper

(* TODO: Переписать тесты, а то какаду ругаться будет( *)

let show_wrap form = function
  | Result.Ok (Some x) -> Format.printf "%a@\n" form x
  | Result.Ok None -> Format.print_string "There were Null\n"
  | Result.Error _ -> Format.print_string "Type check error\n"
;;

let expr_show_wrap h =
  let _, ans = run (check_expr h) in
  show_wrap pp_t_env_value ans
;;

let expr_show_wrap_cont ctx h =
  let _, ans = continue (check_expr h) ctx in
  show_wrap pp_t_env_value ans
;;

let cl =
  { cl_modif = None
  ; cl_id = Id "Program"
  ; parent = Some (Id "Exception")
  ; cl_mems =
      [ Fild
          ( { f_modif = None; f_type = TVar (TNullable (TBase TInt)); f_id = Id "A1" }
          , Some (EConst (VInt 0)) )
      ; Fild
          ( { f_modif = Some (FAccess MPublic)
            ; f_type = TVar (TNullable (TClass (Id "MyClass")))
            ; f_id = Id "A2"
            }
          , None )
      ; Constructor
          ( { con_modif = Some MPublic
            ; con_id = Id "Program"
            ; con_args = Args [ Var_decl (TVar (TNot_Nullable TInt), Id "num") ]
            ; base_params =
                Some (Params [ EConst (VInt 1); EConst (VInt 2); EConst (VInt 3) ])
            }
          , Steps
              [ SIf_else
                  ( EBin_op (Equal, EIdentifier (Id "num"), EConst (VInt 1))
                  , Steps [ SReturn (Some (EConst (VInt 1))) ]
                  , Some
                      (Steps
                         [ SReturn
                             (Some
                                (EBin_op
                                   ( Asterisk
                                   , EIdentifier (Id "num")
                                   , EMethod_invoke
                                       ( EIdentifier (Id "Fac")
                                       , Params
                                           [ EBin_op
                                               ( Minus
                                               , EIdentifier (Id "num")
                                               , EConst (VInt 1) )
                                           ] ) )))
                         ]) )
              ] )
      ; Method
          ( { m_modif = Some (MAccess MPublic)
            ; m_type = TReturn (TNot_Nullable TInt)
            ; m_id = Id "Fuc"
            ; m_args =
                Args
                  [ Var_decl (TVar (TNot_Nullable TInt), Id "num")
                  ; Var_decl (TVar (TNullable TString), Id "dr")
                  ; Var_decl (TVar (TNullable (TClass (Id "Program"))), Id "rer")
                  ]
            }
          , Steps
              [ SIf_else
                  ( EBin_op (Equal, EIdentifier (Id "num"), EConst (VInt 1))
                  , Steps [ SReturn (Some (EConst (VInt 1))) ]
                  , Some
                      (Steps
                         [ SReturn
                             (Some
                                (EBin_op
                                   ( Asterisk
                                   , EIdentifier (Id "num")
                                   , EMethod_invoke
                                       ( EIdentifier (Id "Fac")
                                       , Params
                                           [ EBin_op
                                               ( Minus
                                               , EIdentifier (Id "num")
                                               , EConst (VInt 1) )
                                           ] ) )))
                         ]) )
              ] )
      ]
  }
;;

let cons =
  { con_modif = Some MPublic
  ; con_id = Id "Program"
  ; con_args =
      Args
        [ Var_decl (TVar (TNot_Nullable TInt), Id "num")
        ; Var_decl (TVar (TNullable TString), Id "dr")
        ; Var_decl (TVar (TNullable (TClass (Id "Program"))), Id "rer")
        ]
  ; base_params = None
  }
;;

let meth =
  { m_modif = Some (MAccess MPublic)
  ; m_type = TReturn (TNot_Nullable TInt)
  ; m_id = Id "Fuc"
  ; m_args =
      Args
        [ Var_decl (TVar (TNot_Nullable TInt), Id "num")
        ; Var_decl (TVar (TNullable TString), Id "dr")
        ; Var_decl (TVar (TNullable (TClass (Id "Program"))), Id "rer")
        ]
  }
;;

let f2 =
  { f_modif = Some (FAccess MPublic)
  ; f_type = TVar (TNullable (TClass (Id "MyClass")))
  ; f_id = Id "A2"
  }
;;

let f1 = { f_modif = None; f_type = TVar (TNullable (TBase TInt)); f_id = Id "A1" }
let tp = TReturn (TNot_Nullable TInt)

let ctx : type_check_ctx =
  let global = CodeMap.empty in
  let global = CodeMap.add (Code_ident (Id "Program")) (Class_ctx cl) global in
  let local = IdentMap.empty in
  let local = IdentMap.add (Id "A1") (Fild_sig f1) local in
  let local = IdentMap.add (Id "A2") (Fild_sig f2) local in
  let local = IdentMap.add (Id "Program") (Constructor_sig cons) local in
  let local = IdentMap.add (Id "Fuc") (Metod_sig meth) local in
  let local =
    IdentMap.add
      (Id "myclass")
      (Value_sig (TVar (TNullable (TClass (Id "Program")))))
      local
  in
  let local = IdentMap.add (Id "str") (Value_sig (TVar (TNullable TString))) local in
  global, local, None
;;

let%expect_test _ =
  expr_show_wrap (EBin_op (Plus, EConst (VInt 3), EConst (VInt 2)));
  [%expect {| (Value_sig (TVar (TNot_Nullable TInt))) |}]
;;

let%expect_test _ =
  expr_show_wrap (EConst (VChar 'a'));
  [%expect {| (Value_sig (TVar (TNot_Nullable TChar))) |}]
;;

let%expect_test _ =
  expr_show_wrap_cont
    ctx
    (EBin_op
       ( Or
       , EBin_op
           ( Equal
           , EBin_op
               ( Plus
               , EConst (VInt 1)
               , EMethod_invoke
                   ( EIdentifier (Id "Fuc")
                   , Params
                       [ EConst (VInt 2)
                       ; EIdentifier (Id "str")
                       ; EIdentifier (Id "myclass")
                       ] ) )
           , EConst (VInt 1) )
       , EConst (VBool true) ));
  [%expect {| (Value_sig (TVar (TNot_Nullable TBool))) |}]
;;

let%expect_test _ =
  expr_show_wrap_cont ctx (EIdentifier (Id "myclass"));
  [%expect {| (Value_sig (TVar (TNullable (TClass (Id "Program"))))) |}]
;;

let%expect_test _ =
  expr_show_wrap_cont ctx (EUn_op (New, EIdentifier (Id "Program")));
  [%expect {| (Value_sig (TVar (TNullable (TClass (Id "Program"))))) |}]
;;

let%expect_test _ =
  expr_show_wrap_cont ctx (EUn_op (UMinus, EConst (VInt 1)));
  [%expect {| (Value_sig (TVar (TNot_Nullable TInt))) |}]
;;

let%expect_test _ =
  expr_show_wrap_cont
    ctx
    (EMethod_invoke
       ( EIdentifier (Id "Fuc")
       , Params [ EConst (VInt 2); EIdentifier (Id "str"); EIdentifier (Id "myclass") ] ));
  [%expect {| (Value_sig (TVar (TNot_Nullable TInt))) |}]
;;

let%expect_test _ =
  expr_show_wrap_cont
    ctx
    (EBin_op
       ( Plus
       , EConst (VInt 1)
       , EMethod_invoke
           ( EIdentifier (Id "Fuc")
           , Params
               [ EConst (VInt 2); EIdentifier (Id "str"); EIdentifier (Id "myclass") ] )
       ));
  [%expect {| (Value_sig (TVar (TNot_Nullable TInt))) |}]
;;

let%expect_test _ =
  expr_show_wrap_cont ctx (EIdentifier (Id "A1"));
  [%expect
    {|
    (Fild_sig
       { f_modif = None; f_type = (TVar (TNullable (TBase TInt)));
         f_id = (Id "A1") }) |}]
;;

let%expect_test _ =
  expr_show_wrap_cont ctx (EBin_op (Assign, EIdentifier (Id "A1"), EConst (VInt 2)));
  [%expect {| (Value_sig (TVar (TNullable (TBase TInt)))) |}]
;;

let%expect_test _ =
  expr_show_wrap_cont
    ctx
    (EBin_op
       ( Equal
       , EBin_op
           ( Plus
           , EConst (VInt 1)
           , EMethod_invoke
               ( EIdentifier (Id "Fuc")
               , Params
                   [ EConst (VInt 2); EIdentifier (Id "str"); EIdentifier (Id "myclass") ]
               ) )
       , EConst (VInt 1) ));
  [%expect {| (Value_sig (TVar (TNot_Nullable TBool))) |}]
;;

let%expect_test _ =
  expr_show_wrap_cont
    ctx
    (EMethod_invoke
       ( EPoint_access (EIdentifier (Id "myclass"), EIdentifier (Id "Fuc"))
       , Params [ EConst (VInt 1); EConst (VString "d"); EIdentifier (Id "myclass") ] ));
  [%expect {|
    (Value_sig (TVar (TNot_Nullable TInt))) |}]
;;
