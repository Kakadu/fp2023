type error =
  | ParsingError of string
  | UnknownDatabase of string
  | UnknownTable of string
  | UknownColumn of string
  | TypesMismatch of string * string
  | DivisionByZero
  | NotImplementedYet
[@@deriving show { with_path = false }]

let error_to_string = function
  | ParsingError msg -> Format.sprintf "Error \"%s\" occurred during parsing" msg
  | UnknownDatabase msg -> Format.sprintf "Can't find database \"%s\"" msg
  | UnknownTable msg -> Format.sprintf "Can't find table \"%s\"" msg
  | UknownColumn msg -> Format.sprintf "Can't find column \"%s\"" msg
  | TypesMismatch (t1, t2) ->
    Format.sprintf "Incompatible types encountered: \"%s\", \"%s\"" t1 t2
  | DivisionByZero -> "Division by zero encountered"
  | NotImplementedYet -> "This feature cannot be used because it is not yet implemented"
;;


module type MONAD_FAIL = sig
  include Base.Monad.S2

  val fail : 'e -> ('a, 'e) t
end

(* RESULT MONADE *)
module Result : sig
  type ('a, 'e) t = ('a, 'e) Result.t

  include MONAD_FAIL with type ('a, 'e) t := ('a, 'e) t

  module Syntax : sig
    val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  end
end = struct
  include Base.Result

  type ('a, 'e) t = ('a, 'e) Result.t

  let fail msg = Error msg

  module Syntax = struct
    let ( let* ) x f = bind x ~f
  end
end
