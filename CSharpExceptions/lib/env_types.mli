module Type_check_env : sig
  type t_env_value =
    | Method_sig of Ast.method_sign
    | Constructor_sig of Ast.constructor_sign
    | Value_sig of Ast.var_type
    | Fild_sig of Ast.fild_sign

  val pp_t_env_value : Format.formatter -> t_env_value -> unit
  val show_t_env_value : t_env_value -> string

  type t_loc_env = t_env_value Common_types.IdentMap.t

  (** Classes * Local space * Signature of the method in which the check occurs * Information about main *)
  type type_check_ctx =
    Common_types.text * t_loc_env * Ast.meth_type option * Common_types.code_ident option

  type tp_checked = TP_Ok
end

module Eval_env : sig
  type iconst =
    | Null_v
    | String_v of string
    | Int_v of int
    | Char_v of char
    | Bool_v of bool
    | Instance_v of Common_types.address

  val pp_iconst : Format.formatter -> iconst -> unit
  val show_iconst : iconst -> string
  val equal_iconst : iconst -> iconst -> bool
  val val_to_iconst : Ast.value_ -> iconst
  val to_inst : Common_types.address -> iconst

  type instructions =
    | IMethod of Ast.method_sign * Ast.statement
    | IConstructor of Ast.constructor_sign * Ast.statement

  val pp_instructions : Format.formatter -> instructions -> unit
  val show_instructions : instructions -> string
  val to_meth : Ast.method_sign -> Ast.statement -> instructions
  val to_cons : Ast.constructor_sign -> Ast.statement -> instructions

  type t_env_eval_const =
    | Init of iconst
    | Not_init

  val pp_t_env_eval_const : Format.formatter -> t_env_eval_const -> unit
  val show_t_env_eval_const : t_env_eval_const -> string
  val to_init : iconst -> t_env_eval_const
  val to_not_init : 'a -> t_env_eval_const

  type t_env_value =
    | IConst of t_env_eval_const
    | ICode of instructions

  val pp_t_env_value : Format.formatter -> t_env_value -> unit
  val show_t_env_value : t_env_value -> string
  val to_const : t_env_eval_const -> t_env_value
  val to_code : instructions -> t_env_value
  val create_val : iconst -> t_env_value
  val create_inst : Common_types.address -> t_env_value

  type t_loc_env = Common_types.address * t_env_value Common_types.IdentMap.t list
  type t_global_env = Common_types.text

  type mem_els =
    Common_types.code_ident * (t_env_value * Ast.fild_sign) Common_types.IdentMap.t

  type memory = Common_types.address * mem_els Common_types.MemMap.t

    (** * Classes
        * (A reference to an instance of the class within which calculations occur * Local env)
        * (Next available free address * Map with class instances)
        * System information about file descriptors, etc. *)
  type interpret_ctx = t_global_env * t_loc_env * memory * Common_types.sys_memory

  type ('b, 'sys_err) sig_ =
    | Return of 'b option
    | Exn of Common_types.address
    | Break
    | Error of 'sys_err

  type ('a, 'b, 'sys_err) eval_t =
    | Eval_res of 'a
    | Signal of ('b, 'sys_err) sig_

  val to_sig : ('a, 'b) sig_ -> ('c, 'a, 'b) eval_t
  val nsig : 'a -> ('a, 'b, 'c) eval_t
  val rsig : 'a option -> ('b, 'a, 'c) eval_t
  val bsig : ('a, 'b, 'c) eval_t
  val esig : Common_types.address -> ('a, 'b, 'c) eval_t
  val error : 'a -> ('b, 'c, 'a) eval_t
end
