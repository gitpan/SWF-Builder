package SWF::Builder::Bitmap::Lossless::Custom;

use strict;

our @ISA = ('SWF::Builder::Bitmap::Lossless');

sub new {
    my ($class, $obj) = @_;

    my %self;
    @self{qw/ _width _height _colors _is_alpha _pixsub /} = @$obj;

    bless \%self, $class;
}

