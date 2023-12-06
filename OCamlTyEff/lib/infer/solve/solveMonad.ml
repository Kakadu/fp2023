open! Base
open Utils
open Types
include MakeSEMonad (Int) (TyError)

let run m = run m 0 |> Result.map ~f:fst

module Solve = struct
  let fresh_var =
    let* count = State.get in
    let* () = State.put (count + 1) in
    (* "solve" prefix is important to avoid collision
       with vars created in gen monad *)
    return @@ Var.Var ("solve" ^ Int.to_string count)

  let fail = Error.fail
end
