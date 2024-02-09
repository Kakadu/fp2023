(** Copyright 2023-2024, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  | DivideByZeroException
  | NotDefinedValue of string
  | UnsupportedOperation
  | UnexpectedPattern
  | EmptyInput
  | Unreachable

let print_error f : error -> unit =
  let open Format in
  function
  | DivideByZeroException -> fprintf f "Attempted to divide by zero"
  | NotDefinedValue v -> fprintf f "The value or constructor %s is not defined" v
  | UnsupportedOperation -> fprintf f "This type does not support this operator"
  | UnexpectedPattern -> fprintf f "Unexpected in pattern"
  | EmptyInput -> fprintf f "Empty input to interpret"
  | Unreachable -> fprintf f "Unreachable"
;;
