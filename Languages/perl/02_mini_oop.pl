
#!/usr/bin/env perl
use v5.38;
package Point;

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

sub x {
    my ( $self, $x ) = @_;
    $self->{_x} = $x if defined($x);
    return $self->{_x};
}

sub y {
    my ( $self, $y ) = @_;
    $self->{_y} = $y if defined($y);
    return $self->{_y};
}

sub print {
    my ($self) = @_;

    #print Person info
    printf( "Name:%s %s\n\n", $self->x, $self->y );
}

my $pit = Point->new();


$pit->x(10);
$pit->y(20);


$pit->print();


1;
