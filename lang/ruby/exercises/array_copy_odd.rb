# Get elements on odd position of an array

def array_copy_odd(a)
  i = 1
  result = []
  while i < a.length
    result[i/2]=a[i]
    i += 2
  end

  result
end

def array_copy_odd2(a)
  result=[]
  (0..(a.length-1)).each{|i| result[i/2]=a[i] if i.odd? }

  result
end

def array_copy_odd3(a)
  (0..(a.length-1)).select{|i| i.odd?}.map{|i| a[i]}
end

puts "array_copy_odd"
p array_copy_odd([1,2,3,4,5])
p array_copy_odd2([1,2,3,4,5])
p array_copy_odd3([1,2,3,4,5])

# concat two lists