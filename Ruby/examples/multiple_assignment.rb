def fib(n)
  if n < 2
    return n
  end

  return fib(n-1) + fib(n-2)
end

x, y, fib, duck, anon = 1, 2, fib(7), ":)", lambda{|x| return x * 2}.call(10)
puts x
puts y
puts fib
puts duck
puts anon