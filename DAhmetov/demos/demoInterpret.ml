open DAhmetov_lib

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.parse s with
  | Ok parse_result ->
    (match Interpret.InterpretResult.exec_program parse_result with
     | Ok actual -> Format.printf "%a" Interpret.pp_env actual
     | Error err -> Format.printf "%a" Interpret.print_failure err)
  | Error err -> Format.printf "Parsing error: %s\n " err
;;
