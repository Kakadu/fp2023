open OCaml_ExtensibleVariantTypes_lib

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.parse_program s with
  | ast -> Format.printf "%a\n%!" Ast.pp_prog ast
;;
