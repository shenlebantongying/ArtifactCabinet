# extend the intrinsic Integer value
#
# execute a snippet √2 times

class Integer
  def square_root_times
    i = 0
    while i * i < self
      #           ^ the int
      i += 1
      yield
    end
  end
end

16.square_root_times { puts '16' } # Ruby blocks
0.square_root_times { puts '0' }
3.square_root_times { puts '3' }

