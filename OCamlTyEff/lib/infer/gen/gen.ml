module Assumptions = Containers.Assumptions
module ConArityAssumpt = Containers.ConArityAssumpt

open! Base

let gen str_item =
  GenMonad.run @@ Structure.gen str_item
  |> Result.map ~f:(fun ((asm, ty), constrs, con_assumpt) ->
         (asm, ty, constrs, con_assumpt) )
