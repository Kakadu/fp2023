let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match (Llvm_lib.Parser.parse_programm s) with
  | Result.Ok lst -> Format.printf "%s" lst
  | Error e -> Format.printf "Error: %s" e
;;
