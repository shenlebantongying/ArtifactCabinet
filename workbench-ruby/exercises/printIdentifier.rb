# retrieve the info of a class itself

module Debug
  def print_identifier
    "#{self.class.name} (#{self.to_s})"
  end
end

class ThisIsSomeClass
  include Debug # <- "mixin"
end

class AnotherClass
  include Debug
end

o1 = ThisIsSomeClass.new
o2 = ThisIsSomeClass.new
o3 = AnotherClass.new

puts o1.print_identifier
puts o2.print_identifier
puts o3.print_identifier

