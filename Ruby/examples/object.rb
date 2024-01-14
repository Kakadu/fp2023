class Hope
  def fib(n)
    if n < 2
      return n
    end

    return fib(n-1) + fib(n-2)
  end

  def doxx(bool)
    while bool
      return bool
    end
  end
end

x = Hope.new

if x.doxx(true == x.doxx(true))
  puts x.fib(7)
end


multiplier = 666
not_anonymous = lambda{|x, y, z| t = 3 * multiplier; return t * x + y / z}
puts not_anonymous.call(1, 2, 3)