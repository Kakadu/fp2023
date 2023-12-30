open C_lib

let () =
  let s =
    {|
    int factorial(int n) {
      if (n >= 1) {
        return n * factorial(n - 1);
      }
      else {
        return 1;
      }
    }
      
    int main() {
      int n = 5; 
      return factorial(n);
    }
    
    |}
  in
  match Parser.parse s with
  | Result.Ok ast ->
      Format.printf "%a\n" Ast.pp_prog ast
  | Error _ ->
      Format.printf "Some error"
