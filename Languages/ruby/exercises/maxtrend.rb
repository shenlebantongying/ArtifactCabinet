# Calculate the max number of strictly increasing consecutive values
# eg: [8,3,4,5,7,1] => 4 (3 -> 4 -> 5 -> 7)

def max_trend(a)
  x = a[0] # global variable to hold previous value
  lmax = l = 0   # |
  a.each do |v|  # |
    if v > x     # |
      l += 1     # |
      x=v # <------.  it will be updated each iteration
    else
      lmax = [l,lmax].max
      l = 1
      x=v
    end
  end
  [l, lmax].max
end


p max_trend [1,2,3]
p max_trend [8,3,4,5,7,1]
p max_trend [0,0,0,0,0,0]
