#!/bin/wish

proc hello {arg1} {
    puts "hello $arg1"
} 

button .hello -text "hello" -command { hello "world!"}
place .hello -x 10 -y 10

wm title . "Single command hello"
wm geometry . 200x100
#             ^ width x height