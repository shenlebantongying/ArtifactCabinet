#!/usr/bin/env perl
use v5.38;
use Test::More;

# concateing strings
my $name = "a1b";
ok("new$name" eq "newa1b");

say "";

# power up a array
# then test if done succesfully
my @testarray = (1..3);
my @powerarray = (1,4,9);

say "> Print the list";
say join(',',@testarray);

say "";

for(@testarray){
    $_=$_**2;
}

my $array_is_eq = 1;

for(my $i = 0; $i <= $#testarray; $i++){
    if($testarray[$i] != $powerarray[$i]){
        $array_is_eq = 0;
    }
}

ok($array_is_eq);

say "";
# define a sub
sub printarray{
    foreach (@_) {
        print "$_ ";
    }
    print "\n";
}

printarray(@testarray);

say "";
my $n=0;
# subrountine
sub greeting {
    $n += 1; # note that n is global variable
    print "Cur -> $n\n";
}

&greeting;
&greeting;
&greeting;

say " ";

done_testing();

