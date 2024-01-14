open Ruby_lib.Interpret

let () =
  let path = String.trim (Stdio.In_channel.input_all Caml.stdin) in
  let ic = open_in path in
  try
    let code = Stdio.In_channel.input_all ic in
    run_ruby code;
    close_in ic
  with
  | e ->
    close_in ic;
    raise e
;;