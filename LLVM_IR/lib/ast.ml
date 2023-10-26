type tp =
  (* only for function ret*)
  | Void (** void *)
  (* primitive types*)
  | Float (** float *)
  | Integer of int (** i1, i2 ... in *)
  | Pointer (** ptr *)
  (* first class types*)
  | Vector of int * tp (** <int x primitive_type> *)
  | Arr of int * tp (** [int x type] *)
  | Struct of tp list (** {type1, type2...} *)
(*
   | Label
   | Token
   | Metadata
*)
[@@deriving show { with_path = false }]

type instruction = string
[@@deriving show { with_path = false }]

type basic_block =
  { num : int
  ; instructions : instruction list
  }
[@@deriving show { with_path = false }]


type variable =
  { name : string
  ; tp : tp
  }
[@@deriving show { with_path = false }]


type func =
  { name : string
  ; returnType : tp
  ; parameters : variable list
  ; basic_blocks : basic_block list
  }
[@@deriving show { with_path = false }]

