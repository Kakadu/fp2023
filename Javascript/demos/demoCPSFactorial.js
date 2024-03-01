function CPSfact(n, k) {
    if (n <= 1)
        return k(1)
    else
        return CPSfact(n - 1, (result) => k(n * result))
}

CPSfact(5, (result) => { console.log(result) })