pipe = Ractor.new do
  loop do
    Ractor.yield Ractor.receive
  end
end

N = 10

N.times.map{ |n|
  Ractor.new pipe,n do |p, i|
    p.send(i)
  end
}

puts N.times.map {
  pipe.take
}.take(N)
