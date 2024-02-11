type const =
  | CInt of int
  | CBool of bool
  | CNil
  | CUnit

val equal_const : const -> const -> bool
val pp_const : Format.formatter -> const -> unit
val show_const : const -> string

type bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Leq
  | Less
  | Geq
  | Gre
  | Eq
  | Neq
  | And
  | Or

val equal_bin_op : bin_op -> bin_op -> bool
val pp_bin_op : Format.formatter -> bin_op -> unit
val show_bin_op : bin_op -> string

type unary_op =
  | Plus
  | Minus
  | Not

val equal_unary_op : unary_op -> unary_op -> bool
val pp_unary_op : Format.formatter -> unary_op -> unit
val show_unary_op : unary_op -> string

type ident = string

val equal_ident : ident -> ident -> bool
val pp_ident : Format.formatter -> ident -> unit
val show_ident : ident -> string

type capitalized_ident = Capitalized_ident of ident

val equal_capitalized_ident : capitalized_ident -> capitalized_ident -> bool
val pp_capitalized_ident : Format.formatter -> capitalized_ident -> unit
val show_capitalized_ident : capitalized_ident -> string

type rec_flag =
  | Recursive
  | Not_recursive

val equal_rec_flag : rec_flag -> rec_flag -> bool
val pp_rec_flag : Format.formatter -> rec_flag -> unit
val show_rec_flag : rec_flag -> string

type pattern =
  | PId of ident
  | PTuple of pattern list
  | PList of pattern * pattern
  | PConst of const

val equal_pattern : pattern -> pattern -> bool
val pp_pattern : Format.formatter -> pattern -> unit
val show_pattern : pattern -> string

type expr =
  | EConst of const
  | EBinop of expr * bin_op * expr
  | EUnop of unary_op * expr
  | EId of ident
  | EFun of pattern * expr
  | EApp of expr * expr
  | EIf of expr * expr * expr
  | EList of expr * expr
  | ETuple of expr list
  | EClsr of decl * expr
  | EMatch of expr * (pattern * expr) list
  | EStd of ident

and decl = DLet of rec_flag * ident * expr

val equal_expr : expr -> expr -> bool
val equal_decl : decl -> decl -> bool
val pp_expr : Format.formatter -> expr -> unit
val show_expr : expr -> string
val pp_decl : Format.formatter -> decl -> unit
val show_decl : decl -> string

type prog = decl list

val equal_prog : prog -> prog -> bool
val pp_prog : Format.formatter -> prog -> unit
val show_prog : prog -> string
val pid : ident -> pattern
val ptuple : pattern list -> pattern
val plist : pattern -> pattern -> pattern
val pconst : const -> pattern
val econst : const -> expr
val ebinop : expr -> bin_op -> expr -> expr
val eunop : unary_op -> expr -> expr
val eid : ident -> expr
val efun : pattern -> expr -> expr
val eapp : expr -> expr -> expr
val eif : expr -> expr -> expr -> expr
val elist : expr -> expr -> expr
val etuple : expr list -> expr
val eclsr : decl -> expr -> expr
val ematch : expr -> (pattern * expr) list -> expr
val dlet : rec_flag -> ident -> expr -> decl
val prog : decl list -> prog
