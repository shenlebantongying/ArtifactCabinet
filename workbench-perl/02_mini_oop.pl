
#!/usr/bin/env perl
#class Point
package Point;
use strict;

#constructor
sub new {
    my ($class) = @_;
    my $self = {
        _x => undef,
        _y  => undef,
    };
    bless $self, $class; #<<<<<< Holy bless!
    return $self;
}

#accessor method for Person first name
sub x {
    my ( $self, $x ) = @_;
    $self->{_x} = $x if defined($x);
    return $self->{_x};
}

#accessor method for Person last name
sub y {
    my ( $self, $y ) = @_;
    $self->{_y} = $lastName if defined($y);
    return $self->{_y};
}

sub print {
    my ($self) = @_;

    #print Person info
    printf( "Name:%s %s\n\n", $self->x, $self->y );
}

1;
