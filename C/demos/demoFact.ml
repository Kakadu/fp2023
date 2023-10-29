open C_lib

let () =
  let s =
    "int factorial(int n) {\n\
    \    if (n >= 1) {\n\
    \      return n * factorial(n - 1);\n\
    \    }\n\
    \    else {\n\
    \      return 1;\n\
    \    } \n\
    \  }\n\
    \  int main() {\n\
    \    int n = 5; \n\
    \    return factorial(n);\n\
    \  }\n\
    \  "
  in
  match Parser.parse s with
  | Result.Ok ast -> Format.printf "%a\n" Ast.pp_prog ast
  | Error _ -> Format.printf "Some error"
;;
