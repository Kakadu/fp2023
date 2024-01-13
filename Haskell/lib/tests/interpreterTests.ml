open HaskellLib
open Interpreter

let parse_interpret input =
  match Parser.parse input with
  | Ok e -> Interpret.interpret e
  | Error err -> Format.printf "%s\n" err
;;

let%expect_test _ =
  parse_interpret {|fact n = if n < 2 then 1 else fact (n - 1) * n
  x = fact 7|};
  [%expect {|
    fact => <fun>
    x => int 5040 |}]
;;

let%expect_test _ =
  parse_interpret
    {|  lst = [1, 2, 6]
incr x = x + 1
map f (x:xs) = f x : map f xs
new_lst = map incr lst|};
  [%expect
    {|
    incr => <fun>
    lst => [int 1, int 2, int 6]
    map => <fun>
    new_lst => TypeError: "DSdasda" |}]
;;

let%expect_test _ =
  parse_interpret {|  lst1 = [1, 2, 6]
    lst2 = [1, 2, 5]
    res = lst1 > lst2|};
  [%expect
    {|
    res => bool true
    lst2 => [int 1, int 2, int 5]
    lst1 => [int 1, int 2, int 6] |}]
;;

let%expect_test _ =
  parse_interpret {|x = 'a'|};
  [%expect {|
    x => char a |}]
;;

let%expect_test _ =
  parse_interpret {|(x:xs)= [52]|};
  [%expect {|
    xs => []
    x => int 52 |}]
;;
