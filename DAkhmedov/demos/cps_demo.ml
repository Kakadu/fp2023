open OCaml_ExtensibleVariantTypes_lib.Interpreter

let s =
  {|
    let rec fac_cps n k = if n = 0 then k 1 else fac_cps (n - 1) (fun x -> k (n * x))

    let rec sumlist_cps lst c =
      match lst with
      | [] -> c 0
      | h :: t -> sumlist_cps t (fun x -> c (h + x))
    ;;

    let rec map_cps f l k =
      match l with
      | [] -> k []
      | x :: xs -> map_cps f xs (fun t -> k (f x :: t))
    ;;

    let id x = x

    let fac n = fac_cps n id
    let sumlist lst = sumlist_cps lst id
    let map lst c = map_cps lst c id

    let sumlist_test = sumlist [ 1; 3; 5; 7; 100 ]
    let fac_test = fac 7
    let map_test = map (fun a -> a + 1) [ 1; 2; 3; 4; 5 ]
    let _ = print_int fac_test
    let _ = print_newline ()
    let _ = if true then print_int 1 else print_int 0
    let _ = print_newline ()
    let print_int = 5
    let a = 1 + print_int
  |}
;;

let () = run_and_pp s
