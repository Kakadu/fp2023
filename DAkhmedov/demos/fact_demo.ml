open OCaml_ExtensibleVariantTypes_lib.Interpreter

let s =
  {|
    let rec factorial n =
      match n with
      | 0 -> 1
      | _ -> n * factorial (n - 1)

    let rec fibonacci n =
      match n with
      | 0 -> 0
      | 1 -> 1
      | _ ->
        let rec fib_iter a b count =
          match count with
          | 0 -> b
          | _ -> fib_iter (a + b) a (count - 1)
        in
        fib_iter 1 0 (n - 1)

    let rec is_prime n =
      let rec is_divisible_by_any x lst =
        match lst with
        | [] -> false
        | hd :: tl -> if x - (x / hd * hd) = 0 then true else is_divisible_by_any x tl
      in
      let rec range start stop =
        if start > stop then [] else start :: range (start + 1) stop
      in
      let divisors = range 2 (n - 1) in
      not (is_divisible_by_any n divisors)

    let number = 10
    let fact = factorial number
    let fib = fibonacci number
    let is_prime_number = is_prime number
    let res = number, fact, fib, is_prime_number 
  |}
;;

let () = run_and_pp s
