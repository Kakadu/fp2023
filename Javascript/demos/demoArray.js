let array1 = []

for (let a = 0; a <= 3; a++) {
    let counter = a
    array1[a] = () => { return counter++ }
}

console.log(array1[0](), array1[3](), array1[0](), array1[3]())

let array2 = [100, 101]

console.log(array2 + "")

array2[10] = "Hello world!"

console.log(array2 + "")

array2.__proto__ = [1, 2, 3, 4, 5, 6, 7]

console.log(array2 + "")
