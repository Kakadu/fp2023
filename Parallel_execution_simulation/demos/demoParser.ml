open Parallel_execution_simulationLib
open Stdio

let () =
  let input = Stdio.In_channel.input_all stdin in
  Parser.parse_prog input
;;
