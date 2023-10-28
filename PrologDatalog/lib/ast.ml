type atom =
  | Name of string
  | Oper of string
[@@deriving eq, show { with_path = false }]

type const =
  | Num of int
  | Atom of atom
[@@deriving eq, show { with_path = false }]

type term =
  | Const of const
  | Var of string
  | Relation of
      { atom : atom
      ; terms : term list
      }
[@@deriving eq, show { with_path = false }]

type many_term = Many_term of term list [@@deriving eq, show { with_path = false }]
