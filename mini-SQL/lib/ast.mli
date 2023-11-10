type name = string

type value =
    Name of name
  | String of name
  | Digit of int
  | Float_Digit of float
  | Bool of bool

  (** Binary operators *)
type bin_op =
    Add                   (** ( + ) *)
  | Substract             (** ( - ) *)
  | Multiply              (** ( * ) *)
  | Divide                (** ( / ) *)
  | Modulo                (** ( % ) *)
  | And                   (** ( AND ) *)
  | Or                    (** ( OR ) *)
  | Equal                 (** ( = ) *)
  | Not_Equal             (** ( <> | != ) *)
  | Greater_Than          (** ( > ) *)
  | Less_Than             (** ( < ) *)
  | Less_Than_Or_Equal    (** ( >= ) *)
  | Greater_Than_Or_Equal (** ( <= ) *)

  (** Unary operators *)
type unary_op = Not (** ( NOT ) *)

  (** Expression *)
type expr =
    Const of value
  | Unary_operation of unary_op * expr
  | Binary_operation of bin_op * expr * expr

  (** SELECT <expr> *)
type select_statement = 
  | Asterisk            (** ( "*" ) *)
  | Expression of expr  (** Expr *)

  (** JOIN type *)
type join_type = 
  | Inner (** ( INNER JOIN ) *)
  | Left  (** ( LEFT JOIN ) *)
  | Right (** ( RIGHT JOIN ) *)
  | Full  (** ( FULL JOIN ) *)

  (** FROM *)
type from_statement =
  | Table of name (** Table name *)
  | Join of       (** JOIN statement *)
  {   jtype : join_type;
      left  : from_statement;
      table : name;
      on    : expr 
  }

  (* Full statement *)
type request = {
  select : select_statement list;
  from : from_statement;
  where : expr option;
}

val pp_name : Format.formatter -> name -> unit
val show_name : name -> string

val pp_value : Format.formatter -> value -> unit
val show_value : value -> string

val pp_request : Format.formatter -> request -> unit
val show_request : request -> string

val pp_bin_op : Format.formatter -> bin_op -> unit
val show_bin_op : bin_op -> string

val pp_unary_op : Format.formatter -> unary_op -> unit
val show_unary_op : unary_op -> string

val pp_expr : Format.formatter -> expr -> unit
val show_expr : expr -> string

val pp_select_statement : Format.formatter -> select_statement -> unit
val show_select_statement : select_statement -> string

val pp_join_type : Format.formatter -> join_type -> unit
val show_join_type : join_type -> string

val pp_from_statement : Format.formatter -> from_statement -> unit
val show_from_statement : from_statement -> string