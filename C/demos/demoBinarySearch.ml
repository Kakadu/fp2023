open C_lib

let () =
  let s =
    {|
  int binarySearch(int a, int *array, int n) {
    int low = 0;
    int high = n - 1;
    int middle;
    while (low <= high) {
      middle = (low + high) / 2;
      if (a < array[middle] || a > array[middle]) {
        if (a < array[middle]) {
          high = middle - 1;
        } 
        else {
          low = middle + 1;
        }
      } 
      else {
        return middle;
      } 
    }
    return -1;
  }
  
  int main() {
    int array[5] = {3, 7, 10, 23, 100};
    return binarySearch(7, array, 5);
  }
  |}
  in
  match Parser.parse s with
  | Result.Ok ast ->
      Format.printf "%a\n" Ast.pp_prog ast
  | Error _ ->
      Format.printf "Some error"
