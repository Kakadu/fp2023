module type MONAD_FAIL = sig
  type ('a, 'e) t

  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t

  module Let_syntax : sig
    val return : 'a -> ('a, 'b) t
    val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t

    module Let_syntax : sig
      val return : 'a -> ('a, 'b) t
      val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
      val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
      val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t

      module Open_on_rhs : sig end
    end
  end

  module Monad_infix : sig
    val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
  end

  val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
  val return : 'a -> ('a, 'b) t
  val map : ('a, 'e) t -> f:('a -> 'b) -> ('b, 'e) t
  val join : (('a, 'e) t, 'e) t -> ('a, 'e) t
  val ignore_m : ('a, 'e) t -> (unit, 'e) t
  val all : ('a, 'e) t list -> ('a list, 'e) t
  val all_unit : (unit, 'e) t list -> (unit, 'e) t
  val run : ('a, 'e) t -> ok:('a -> ('b, 'e) t) -> err:('e -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val ( *> ) : ('a, 'e) t -> ('a, 'e) t -> ('a, 'e) t
end

type environment = (string, value, Base.String.comparator_witness) Base.Map.t

and value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VList of value list
  | VTuple of value list
  | VFun of Ast.pattern list * Ast.expr * Ast.rec_flag * environment

type error =
  | UnboundValue of string
  | ParserAvoidedError
  | UnsupportedOperation
  | Division_by_zero
  | NonExhaustivePatternMatching
  | TypeError
  | UnexpectedState

val pp_value : Format.formatter -> value -> unit
val print_value : value -> unit
val pp_error : Format.formatter -> error -> unit
val print_error : error -> unit

module Environment : functor (M : MONAD_FAIL) -> sig
  val find : (string, 'a, 'b) Base.Map.t -> string -> ('a, error) M.t
  val update : ('a, 'b, 'c) Base.Map.t -> 'a -> 'b -> ('a, 'b, 'c) Base.Map.t
  val empty : (string, 'a, Base.String.comparator_witness) Base.Map.t
end

module Interpret : functor (M : MONAD_FAIL) -> sig
  val run : Ast.prog -> (environment, error) M.t
end

module InterpretResult : sig
  val run : Ast.prog -> (environment, error) result
end

val run_and_pp : string -> unit
