let obj1 = {
  prop1: 4, ["prop" + 2]: "Hi!", getProp1() { return this.prop1 }
}
console.log(obj1)
let obj2 = { prop1: 10, __proto__: obj1 }
console.log(obj2.prop2, obj2.getProp1(), obj2.prop1)
