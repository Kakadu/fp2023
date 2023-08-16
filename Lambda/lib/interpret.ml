(** Copyright 2021-2023, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Real monadic interpreter goes here *)

open Utils

type error = [ `UnknownVariable of string (** just for example *) ]

module Interpret (M : MONAD_FAIL) : sig
  val run : _ Ast.t -> (int, [> error ]) M.t
end = struct
  let run _ =
    (* TODO: implement interpreter here *)
    if true then M.fail (`UnknownVariable "var") else failwith "not implemented"
  ;;
end

let parse_and_run str =
  let module I = Interpret (Base.Result) in
  let rez = Base.Result.(Parser.parse str >>= I.run) in
  match rez with
  | Result.Ok n -> Printf.printf "Success: %d\n" n
  | Result.Error #Parser.error ->
    Format.eprintf "Parsing error\n%!";
    exit 1
  | Result.Error #error ->
    Format.eprintf "Interpreter error\n%!";
    exit 1
;;
