open OCaml_ExtensibleVariantTypes_lib.Interpreter

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  run_and_pp s
;;
