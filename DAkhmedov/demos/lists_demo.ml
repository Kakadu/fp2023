open OCaml_ExtensibleVariantTypes_lib.Interpreter

let s =
  {| 
    let rec fold_left f acc lst =
      match lst with
      | [] -> acc
      | hd :: tl -> fold_left f (f acc hd) tl

    let sum_of_squares_fold =
      let rec init n acc = if n <= 10 then init (n + 1) (n :: acc) else acc in
      let numbers = init 1 [] in
      let rec sum_of_squares_fold acc lst =
        match lst with
        | [] -> acc
        | hd :: tl ->
          let square_hd = hd * hd in
          sum_of_squares_fold (fold_left (fun x y -> x + y) acc [ square_hd ]) tl
      in
      sum_of_squares_fold 0 numbers

    let rec map f lst =
      match lst with
      | [] -> []
      | hd :: tl -> f hd :: map f tl

    let square x = x * x
    let rec init n acc = if n <= 10 then init (n + 1) (n :: acc) else acc
    let numbers = init 1 []

    let rec map_square acc lst =
      match lst with
      | [] -> acc
      | hd :: tl ->
        let square_hd = square hd in
        map_square (fold_left (fun x y -> y :: x) acc [ square_hd ]) tl

    let squares = map_square [] numbers
    let sum_of_squares_map = fold_left (fun x y -> x + y) 0 squares
    let next_number n = if n < 10 then [ n, n + 1 ] else []

    let rec unfoldr_next_number acc n =
      if n < 10 then unfoldr_next_number (n :: acc) (n + 1) else acc

    let numbers_anamorph = unfoldr_next_number [] 1
    let sum_of_squares_anamorph = fold_left (fun x y -> x + (y * y)) 0 numbers_anamorph
    let res = sum_of_squares_fold, sum_of_squares_map, sum_of_squares_anamorph
|}
;;

let () = run_and_pp s
