#self recursion
def palindrome(a)
  if a.length == 1 || a.length == 0
    true
  else
    if a[0] == a[-1]
      palindrome(a[1..-2])
      #           ^ range
    else
      false
    end
  end
end

if palindrome("abcba".split(""))
  puts "yes"
end

unless palindrome("abcdefg".split(""))
# aka "if not"
  puts "no"
end