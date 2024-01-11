open Miniml_lib
open Format

let input =
  {|
    let fact x =  
      let rec helper x k = if x = 1 then k 1 else helper (x - 1) (fun n -> n * k x) 
    in helper x (fun x -> x)

    let result = fact 5
  |}
;;

let () =
  match Parser.parse input with
  | Result.Ok tree ->
    (match Infer.typecheck tree with
     | Result.Ok tyenv ->
       (match Interpret.interpret ~tyenv tree with
        | Result.Ok (_, rs) ->
          printf
            "%a"
            (pp_print_list
               ~pp_sep:(fun _ _ -> printf "\n")
               (fun _ res -> printf "%a" Interpret.pp_interpret_result res))
            rs
        | Result.Error err -> printf "Interpretation error: %a" Interpret.pp_error err)
     | Result.Error err -> printf "Typecheck error: %a" Typing.pp_error err)
  | _ -> printf "Parsing error"
;;
