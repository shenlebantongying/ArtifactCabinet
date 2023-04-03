#!/usr/bin/env ruby

# List processing

# 1

def p1(a)
  a.last
end

p1([1,2,3,4])

# 2

def p2(a)
  a.last(2)
end

# 3

def element_at(a,k)
  a[k - 1]
end
element_at([1,2,3,4],2)

# 7
# Flatten a nested list structure.

# inject & reduce are same
#

def flatten1(a)
  return [a] unless a.is_a? Array
  a.inject([]) do |acc, element| 
    acc + flatten1(element)
  end
end

flatten1([[1,2,3],[4,5,6]])

# 8. Eliminate consecutive duplicates of list elements. (medium)

def compress(a)
  result = []
  a.each do |i|
    result << i if result.empty? or result.last  != i
  end
  result
end

compress [1,1,2,3,4,5,5,5,5,6,6,6,6]

# 9 group consecutive duplicates

def pack(a)
  b = []
  a.each do |i|
    b << []  if b.empty? or b.last.first != i
    b.last << i
  end
  b
end

pack [1,1,2,3,4,5,5,5,5,6,6,6,6]

# 10 encoding

def encode(a)
  pack(a). map do |l|
    [l.size, l.first]
  end
end

encode [1,1,2,3,4,5,5,5,5,6,6,6,6]