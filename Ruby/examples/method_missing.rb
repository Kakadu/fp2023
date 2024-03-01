class Magic
  def method_missing(m, *args)
      if m.to_s() == "fly"
        return args[0] + args[1]
      else
        return "no such method!"
      end
  end
end

x = Magic.new

puts x.fly(31, 11)
puts x.walk()