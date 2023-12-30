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
  | Exception_ctx of excaption
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
  | Metod_sig of meth_type
  | Value_sign of var_type

type t_loc_env = t_env_value IdentMap.t
type type_check_ctx = text * t_loc_env

(* type type_ =
  | Mtype_ of meth_type
  | Vtype_ of var_type *)

type error =
  [ `Not_find_ident of ident
  | `Type_mismatch
  | `Double_definition
  | `Other_error of string
  | `User_exception of excaption
  ]

module Type_checker = struct
  type ctx_env = type_check_ctx
  type ('st, 'a) t = 'st -> 'st * ('a, error) Result.t
  type 'a tt = (ctx_env, 'a) t

  let return : 'a -> 'a tt = fun x st -> st, Result.ok x

  let ( >>= ) : 'a tt -> ('a -> 'b tt) -> 'b tt =
    fun x f st ->
    let st, x = x st in
    match x with
    | Result.Ok x -> f x st
    | Result.Error s -> st, Result.error s
  ;;

  let save_local : t_loc_env -> unit tt =
    fun l_env c_env ->
    match c_env with
    | code, _ -> (code, l_env), Result.ok ()
  ;;

  let save_local_el : ident -> t_env_value -> unit tt =
    fun id t_val c_env ->
    match c_env with
    | _, local_env ->
      let new_l = IdentMap.add id t_val local_env in
      save_local new_l c_env
  ;;

  let save_global : text -> unit tt =
    fun g_env c_env ->
    match c_env with
    | _, local_env -> (g_env, local_env), Result.ok ()
  ;;

  let save_global_el : code_ident -> code_ctx -> unit tt =
    fun id ctx c_env ->
    match c_env with
    | code, _ ->
      let new_l = CodeMap.add id ctx code in
      save_global new_l c_env
  ;;

  let read_local : ident -> t_env_value option tt =
    fun id c_env ->
    match c_env with
    | _, local_env -> return (IdentMap.find_opt id local_env) c_env
  ;;

  let read_global : code_ident -> code_ctx option tt =
    fun id c_env ->
    match c_env with
    | code, _ -> return (CodeMap.find_opt id code) c_env
  ;;

  let run : 'a tt -> ctx_env * ('a, error) Result.t =
    fun f -> f (CodeMap.empty, IdentMap.empty)
  ;;

  let continue : 'a tt -> ctx_env -> ctx_env * ('a, error) Result.t =
    fun f c_env -> f c_env
  ;;

  let lift2: ('a -> 'b -> 'c) -> 'a tt -> 'b tt -> 'c tt = 
  fun f a b st -> (a >>= fun a1 -> b >>= fun b1 -> return(f a1 b1)) st

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

let to_type_v = function
| Some x -> Some (Value_sign x)
| None -> None

let check_expr exp =
  let rec helper e1 =
    match e1 with
    | EConst x -> return (to_type_v @@ to_var_type @@ to_atype x)
    | EIdentifier x -> read_local x
    | EMethod_invoke (e2, prms) -> lift2 (fun tp -> ) (helper e2) ()
  in
  helper exp
;;
(* let checker: tast -> (code_ctx , error) result =
   fun ast ->
   let *)
