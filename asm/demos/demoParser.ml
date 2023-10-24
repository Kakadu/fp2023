open AsmLib
open Stdio

let () =
  let input = Stdio.In_channel.input_all stdin in
  Parser.parse Parser.ast_p Ast.show_ast input
;;
