type tp =
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

type ret_type =
  | Void (** void *)
  | Type of tp
[@@deriving show { with_path = false }]

type func =
  { returnType : ret_type
  ; parameters : tp list
  }
