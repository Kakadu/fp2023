(** Copyright 2023-2024, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  [ `DivisionByZero
  | `UnboundValue of string
  | `TypeMismatch
  | `UnsupportedOperation
  | `PatternMismatch
  | `EmptyInput
  | `NotImplemented of string
  | `Unreachable]

let print_error f : error -> unit =
  let open Format in
  function
  | `DivisionByZero -> fprintf f "Runtime error: division by zero"
  | `UnboundValue v -> fprintf f "Runtime error: unbind variable %s" v
  | `TypeMismatch -> fprintf f "Runtime error: "
  | `UnsupportedOperation -> fprintf f "Runtime error: unsupported operation"
  | `PatternMismatch -> fprintf f "Runtime error: pattern mismatch"
  | `EmptyInput -> fprintf f "Runtime error: empty input to interpret"
  | `NotImplemented v -> fprintf f "Runtime error: %s is not implemented" v
  | `Unreachable -> fprintf f "Runtime error: oops... This was never supposed to happen. Report the bug, please."
;;
