open Miniml_lib
open Format

let input =
  {| 
    let rec fix f x = f (fix f) x

    let map f p = 
      match p with 
      | a, b -> (f a, f b)
    
    let fixpoly l = fix (fun self l -> map (fun li x -> li (self l) x) l) l

    let feven p n = match p with 
      | o, e -> if n = 0 then 1 else o (n - 1)

    let fodd p n = match p with 
      | e, o -> if n = 0 then 0 else e (n - 1)
    
    let tie = fixpoly (feven, fodd)

    let _ = match tie with 
      | even, odd -> odd 1
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
