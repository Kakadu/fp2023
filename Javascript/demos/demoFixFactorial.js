let U = (g) => g(g)

let fact = U((g) => (x) => {
    if (x === 0) return 1
    else return x * g(g)(x - 1)
})

console.log(fact(5))
