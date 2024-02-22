def closure_show_case(lbd)
  ccc = 3
  return lbd.call(true)
end

ccc = 1
smth3 = lambda{|x| ccc }
ccc = 2
puts closure_show_case(smth3)