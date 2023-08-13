(** Copyright 2021-2022, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Real monadic interpreter goes here *)

open Base
open Utils

module Interpret (M : MONAD_FAIL) : sig
  val run : _ Ast.t -> (int, Utils.error) M.t
end = struct
  let run _ =
    (* implement interpreter here *)
    if true then M.fail (UnknownVariable "var") else failwith "not implemented"
  ;;
end

let parse_and_run str =
  let ans =
    let module I = Interpret (Result) in
    match Parser.parse str with
    | Caml.Result.Ok ast -> I.run ast
    | Caml.Result.Error _ ->
      Caml.Format.eprintf "Parsing error\n%!";
      Caml.exit 1
  in
  ans
;;
