open Miniml_lib
open Format

let input =
  {| 
    let rec equal_lengths l1 l2 =
      match l1, l2 with
      | _ :: _, [] | [], _ :: _ -> false
      | [], [] -> true
      | _ :: tl1, _ :: tl2 -> equal_lengths tl1 tl2
    
    let _ = [equal_lengths [1; 2] [1; 2; 3]; equal_lengths [1; 2] [3; 4]]

    let rec zip_sum_all lst init =
      match lst with
      | (l, r) :: tl -> zip_sum_all tl l + r + init
      | [] -> init 

    let _ = zip_sum_all [1, 2; 3, 4; 5, 6] 0

    let rec even_length xs = match xs with 
      | h :: h :: tl -> even_length tl
      | h :: [] -> false
      | _ -> true
    
    let _ = even_length [1; 2; 3; 4], even_length [1; 2; 3; 4; 5]

    let rec fold_left lst f init = match lst with
      | [] -> init
      | h :: tl -> fold_left tl f (f init h)

    let _ = fold_left [7; 5; 3] (fun acc x -> if x * acc > 100 then acc else x * acc) 1

    let is_123 l =
      match l with
      | 1 :: 2 :: 3 :: []
      | 1 :: 3 :: 2 :: []
      | 2 :: 1 :: 3 :: []
      | 2 :: 3 :: 1 :: []
      | 3 :: 1 :: 2 :: []
      | 3 :: 2 :: 1 :: [] -> true
      | _ -> false
    
    let _ = is_123 [1; 2; 3], is_123 [3; 1; 2], is_123 [3; 2; 1]
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
