#!/usr/bin/tclsh
package require Tcl;
package require cmdline;

# arg with true/false values

set options {
    {s "Source  to read"}
    {c "Command to run"}
}

set usage ": cpt.tcl \[options] filename"

array set params [::cmdline::getoptions argv $options $usage]

puts [array size params]

parray params

puts $params(s)
if { $params(s) } { 
    set sourcefile "true"
} 

puts $sourcefile