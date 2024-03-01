let fun1 = function (start_val) {
  let counter = start_val;
  return () => { return ++counter }
}

let fun2 = fun1(5)

console.log(fun2(), fun2(), fun2())
