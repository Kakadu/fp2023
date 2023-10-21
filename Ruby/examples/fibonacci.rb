def fib(i)
    if i == 0 then
        0
    else
        if i == 1 then
            1
        else 
            fib(i - 1) + fib(i - 2)
        end
    end
end

puts (fib(5))
