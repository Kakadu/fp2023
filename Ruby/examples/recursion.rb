def fib(n)
  if n < 2
    return n
  end

  return fib(n-1) + fib(n-2)
end

def fac(n)
    if n <= 1
      return 1
    end
    return n * fac(n - 1)
end

puts fib(10)
puts fac(10)