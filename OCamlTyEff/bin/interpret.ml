open! Base

let () =
  In_channel.(input_all stdin)
  |> Parser.parse_exn |> Ast.show_structure
  |> Out_channel.(output_string stdout)
