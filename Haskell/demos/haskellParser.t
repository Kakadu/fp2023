Here infinite lists are defined as functions with wildcard pattern
to avoid printing it out as an infinite list (otherwise we are stuck in a loop) 

FACTORIAL 
  $ ./haskellParser.exe <<EOF
  > fact n = if (n < 2) then 1 else fact (n - 1) * n
  > x = fact 7
  > EOF
  fact :: Int -> Int => <fun>
  x :: Int => 5040

YET ANOTHER FACTORIAL
  $ ./haskellParser.exe <<EOF
  > fact 0 = 1
  > fact n = n * fact (n - 1)
  > EOF
  fact :: Int -> Int => <fun>

  $ ./haskellParser.exe <<EOF
  > f a b = a + b
  > x = f 5
  > y = x 6
  f :: Int -> Int -> Int => <fun>
  x :: Int -> Int => <fun>
  y :: Int => 11


  $ ./haskellParser.exe <<EOF
  > f x = x 
  > x =  f (f (f (10 + 5) + f 3 ))
  f :: p1 -> p1 => <fun>
  x :: Int => 18

  $ ./haskellParser.exe <<EOF
  > lst = [1, 2, 6]
  > incr x  = x + 1
  > map f lst = case lst of
  > [] -> []
  > (x:xs) -> f x : map f xs  
  > x = map incr lst
  incr :: Int -> Int => <fun>
  lst :: [Int] => [1, 2, 6]
  map :: (p12 -> p15) -> [p12] -> [p15] => <fun>
  x :: [Int] => [2, 3, 7]


YET ANOTHER FACTORIAL
  $ ./haskellParser.exe <<EOF
  > cond p f g x = if p x then f x else g x
  > equal a b = (a == b)
  > mul a b = a * b
  > pred a = a - 1
  > s f g x = f x (g x)
  > k x y   = x
  > b f g x = f (g x)
  > c f g x = f x g
  > y f     = f (y f)
  > fac  = y (b (cond (equal 0) (k 1)) (b (s mul) (c b pred)))
  > res = fac 8
  b :: (p31 -> p32) -> (p30 -> p31) -> p30 -> p32 => <fun>
  c :: (p36 -> p35 -> p38) -> p35 -> p36 -> p38 => <fun>
  cond :: (p4 -> Bool) -> (p4 -> p7) -> (p4 -> p7) -> p4 -> p7 => <fun>
  equal :: p11 -> p11 -> Bool => <fun>
  fac :: Int -> Int => <fun>
  k :: p25 -> p26 -> p25 => <fun>
  mul :: Int -> Int -> Int => <fun>
  pred :: Int -> Int => <fun>
  res :: Int => 40320
  s :: (p20 -> p22 -> p23) -> (p20 -> p22) -> p20 -> p23 => <fun>
  y :: (p42 -> p42) -> p42 => <fun>

SIEVE OF ERATOSTHENES  
  $ ./haskellParser.exe <<EOF
  > mod x y = (x - (y * (x / y)))
  > 
  > numbers_starting_at n = n : numbers_starting_at (n + 1)  
  > 
  > take n lst =
  >   case (n, lst) of
  >      (_, []) -> []
  >      (1, (x:_)) -> [x]
  >      (n, (x:xs)) -> x : take (n - 1) xs
  > 
  > filter predicate xs = 
  >   case xs of
  >     [] -> []
  >     (x:rest) -> (if predicate x
  >                    then x : filter predicate rest
  >                    else filter predicate rest)
  > primes n = 
  >   let sieve asd = 
  >     case asd of
  >       [] -> []
  >       (p:xs) -> p : sieve (filter (\x -> mod x p /= 0) xs)
  >   in
  >   sieve (numbers_starting_at n)
  > 
  > res = take 10 (primes 2)
  filter :: (p31 -> Bool) -> [p31] -> [p31] => <fun>
  mod :: Int -> Int -> Int => <fun>
  numbers_starting_at :: Int -> [Int] => <fun>
  primes :: Int -> [Int] => <fun>
  res :: [Int] => [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
  take :: Int -> [p20] -> [p20] => <fun>

HAMMING
  $ ./haskellParser.exe <<EOF
  > merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else if x == y then x : merge xs ys else y : merge (x:xs) ys
  > 
  > mul x y = x * y
  > 
  > map f lst =
  >   case lst of
  >     [] -> []
  >     (x:xs) -> f x : map f xs
  > 
  > take n lst =
  >   case (n, lst) of
  >      (_, []) -> []
  >      (1, (x:_)) -> [x]
  >      (n, (x:xs)) -> x : take (n - 1) xs
  > 
  > hamming _ = 1 : merge (map (mul 2) (hamming 0)) (merge (map (mul 3) (hamming 0)) (map (mul 5) (hamming 0)))
  > 
  > res = take 20 (hamming 0)
  hamming :: Int -> [Int] => <fun>
  map :: (p26 -> p29) -> [p26] -> [p29] => <fun>
  merge :: [p7] -> [p7] -> [p7] => <fun>
  mul :: Int -> Int -> Int => <fun>
  res :: [Int] => [1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36]
  take :: Int -> [p44] -> [p44] => <fun>

FIBONACCI
  $ ./haskellParser.exe <<EOF
  > zipWith f lst1 lst2 = 
  >   case (lst1, lst2) of
  >     (_, []) -> []
  >     ([], _) -> []
  >     ((x:xs), (y:ys)) -> f x y : zipWith f xs ys
  > 
  > index (x:xs) n = if n == 0 then x else index xs (n - 1)
  > 
  > add x y = x + y
  > 
  > tail (_:xs) = xs  
  > 
  > fibs _ = 0 : 1 : zipWith add (fibs 0) (tail (fibs 0))
  > 
  > fib = index (fibs 0) 10
  add :: Int -> Int -> Int => <fun>
  fib :: Int => 55
  fibs :: Int -> [Int] => <fun>
  index :: [p29] -> Int -> p29 => <fun>
  tail :: [p36] -> [p36] => <fun>
  zipWith :: (p13 -> p16 -> p21) -> [p13] -> [p16] -> [p21] => <fun>

YET ANOTHER FACTORIAL
  $ ./haskellParser.exe <<EOF
  > numbers_starting_at n = n : numbers_starting_at (n + 1) 
  > 
  > scanl f q ls = q : (case ls of
  >     [] -> []
  >     (x:xs) -> scanl f (f q x) xs)
  > 
  > mul x y = x * y
  > 
  > index (x:xs) n = if n == 0 then x else index xs (n - 1)
  > 
  > facs _ = scanl mul 1 (numbers_starting_at 1)
  > 
  > fac = index (facs 0) 5
  fac :: Int => 120
  facs :: p30 -> [Int] => <fun>
  index :: [p28] -> Int -> p28 => <fun>
  mul :: Int -> Int -> Int => <fun>
  numbers_starting_at :: Int -> [Int] => <fun>
  scanl :: (p9 -> p12 -> p9) -> p9 -> [p12] -> [p9] => <fun>
