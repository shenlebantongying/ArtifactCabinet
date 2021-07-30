#!/bin/tclsh

puts nice

set var1 john;

puts var1;
puts "this is $var1"; # 1 pass sub
puts {this is $var1}; # no sub
set var3 [set var2 "tomx"]; # composing
puts "$var2"

puts "\nMath:"

set res [expr {001.1+1.11111111111111111111111}]
puts $res

puts [expr {1.0==1}]   ;# compare value
puts [expr {1.0 eq 1}] ;# compare string

set x 1
puts [expr $x>0 ? $x+1 : $x-1] 

puts "\n Define functions:"

proc sum {arg1 arg2} {
  # this var is inside the scope of this funciton
  set sum_temp [expr {$arg1 + $arg2}];
  return $sum_temp;
}

puts [sum 1 2]

puts "\n List manipulation"

set splited_list [split "a,b,c,d" ","]

puts $splited_list

foreach i $splited_list {
  puts $i
}

puts "\n Pattern Matching"

puts [string match f* foo] ;# => 1

# get all files under this dir
puts [glob ./*]

set flattened_list [concat a b {c d e} f {g h}]
puts $flattened_list

puts "\n Data Structure"

#dict
set mymap(first) "first"
set mymap(second) "second"

puts $mymap(first);

