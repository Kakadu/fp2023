(** Copyright 2023-2024, tepa46 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binop =
  | Add (**  +   *)
  | Sub (**  -   *)
  | Mul (**  * *)
  | Div (**  / *)
  | Eq (** = *)
  | Neq (** <> *)
  | Les (**  < *)
  | Leq (**  <= *)
  | Gre (**  > *)
  | Geq (**  >= *)
  | And (** && *)
  | Or (** || *)
  | Cons (** :: *)
[@@deriving eq, show { with_path = false }]

let bop_add = Add
let bop_sub = Sub
let bop_mul = Mul
let bop_div = Div
let bop_eq = Eq
let bop_neq = Neq
let bop_les = Les
let bop_leq = Leq
let bop_gre = Gre
let bop_geq = Geq
let bop_and = And
let bop_or = Or
let bop_cons = Cons

type decl_name =
  | LName of string (* abc, aBc *)
  | UName of string (* Number *)
[@@deriving eq, show { with_path = false }]

(* decl_type is used to denote the type of an algebraic type constructor
   example: type node = | Red of int | Black of int
   (DType
   ((LName "node"),
   [((UName "Red"), (DType TInt)); ((UName "Black"), (DType TInt))])) *)
type decl_type =
  | DType of decl_type
  | TEmptyType
  | TInt
  | TString
  | TBool
  | TFun of decl_type * decl_type
  | TVar of decl_name
  | TList of decl_type
  | TTuple of decl_type list
[@@deriving eq, show { with_path = false }]

let lname str = LName str
let uname str = UName str
let dtype decl_type = DType decl_type
let temptytype = TEmptyType
let tint = TInt
let tstring = TString
let tbool = TBool
let tfun decl_type1 decl_type2 = TFun (decl_type1, decl_type2)
let tvar string = TVar string
let tlist decl_type = TList decl_type
let ttuple decl_type_list = TTuple decl_type_list

type decl_rec = DRec of bool [@@deriving eq, show { with_path = false }]

let drec bool = DRec bool

type pattern =
  | PNill (* [] *)
  | PWild (* _ *)
  | PString of string
  | PBool of bool
  | PInt of int
  | PVar of decl_name
  | PCons of pattern * pattern
  | PTuple of pattern list
  | PAdt of decl_name * pattern option (* pattern for ADT types: Tree(_, _) *)
[@@deriving eq, show { with_path = false }]

let pnill = PNill
let pwild = PWild
let pstring str = PString str
let pbool bool = PBool bool
let pint int = PInt int
let pvar decl_name = PVar decl_name
let pcons pattern1 pattern2 = PCons (pattern1, pattern2)
let ptuple pattern_lst = PTuple pattern_lst
let padt decl_name pattern = PAdt (decl_name, pattern)

(* type_decl is used for creating custom ADT types
   example: type node = | Red of int | Black of int
   (DType
   ((LName "node"),
   [((UName "Red"), (DType TInt)); ((UName "Black"), (DType TInt))]))
*)
type type_decl = decl_name * (decl_name * decl_type) list
[@@deriving eq, show { with_path = false }]

type let_decl = decl_rec * decl_name * decl_exp
[@@deriving eq, show { with_path = false }]

and decl_exp =
  | EEmptyList
  | EInt of int
  | EString of string
  | EBool of bool
  | EVar of decl_name
  | ETuple of decl_exp list
  | EBinop of binop * decl_exp * decl_exp
  | EFun of pattern * decl_exp (* fun n -> n + 1 *)
  | EConstr of decl_name * decl_exp option
  | ELet of let_decl * decl_exp
  | EApp of decl_exp * decl_exp
  | EMatch of decl_exp * (pattern * decl_exp) list
  | EIf of decl_exp * decl_exp * decl_exp
[@@deriving eq, show { with_path = false }]

type decl =
  | DLet of let_decl
  | DType of type_decl
[@@deriving eq, show { with_path = false }]

let eemptylist = EEmptyList
let eint num = EInt num
let estring str = EString str
let ebool bool = EBool bool
let evar var = EVar var
let etuple decl_exp_list = ETuple decl_exp_list
let ebinop binop decl_exp1 decl_exp2 = EBinop (binop, decl_exp1, decl_exp2)
let efun pattern decl_exp = EFun (pattern, decl_exp)
let econstr decl_name decl_exp = EConstr (decl_name, decl_exp)
let elet let_decl decl_exp = ELet (let_decl, decl_exp)
let eapp decl_exp1 decl_exp2 = EApp (decl_exp1, decl_exp2)
let ematch decl_exp lst = EMatch (decl_exp, lst)
let eif decl_exp1 decl_exp2 decl_exp3 = EIf (decl_exp1, decl_exp2, decl_exp3)
let decllet bool decl_name decl_exp = DLet (bool, decl_name, decl_exp)
let decltype decl_name lst = DType (decl_name, lst)

type program = decl list [@@deriving eq, show { with_path = false }]
