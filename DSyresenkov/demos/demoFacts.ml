open Miniml_lib
open Format

let input =
  {|
    let rec fact x = if x = 1 then x else x * fact (x - 1)

    let factCPS x =  
      let rec helper x k = if x = 1 then k 1 else helper (x - 1) (fun n -> n * k x) 
    in helper x (fun x -> x)

    let rec fix f x = f (fix f) x

    let factFix = fix (fun f x -> if x < 2 then x else x * f (x - 1))

    let _ = fact 5, factCPS 5, factFix 5 
  |}
;;

let () =
  match Parser.parse input with
  | Result.Ok tree ->
    (match Infer.typecheck tree with
     | Result.Ok (_, program) ->
       (match Interpret.interpret program with
        | Result.Ok (_, rs) ->
          printf
            "%a"
            (pp_print_list
               ~pp_sep:(fun _ _ -> printf "\n")
               (fun _ res -> printf "%a" Interpret.pp_interpret_result res))
            rs
        | Result.Error err -> printf "Interpretation error: %a\n" Interpret.pp_error err)
     | Result.Error err -> printf "Typecheck error: %a\n" Typing.pp_error err)
  | _ -> printf "Parsing error\n"
;;
