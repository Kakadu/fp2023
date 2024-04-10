let fact = 8
function calculateFact(fact) {
  if (fact !== 0)
    return fact * calculateFact(fact - 1);
  else return 1;
}
console.log(calculateFact(fact))
