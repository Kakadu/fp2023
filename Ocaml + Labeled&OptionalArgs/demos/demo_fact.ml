open LabeledOptionalArgs_lib.Ast
open LabeledOptionalArgs_lib.Parser

let () =
  let test = " let rec fact ( n : int ) : int = if n < 1 then 1 else n * fact (n - 1) " in
  match parse test with
  | Result.Ok res -> Format.printf "%a\n" pp_program res
  | _ -> Format.printf "Git gud"
;;
