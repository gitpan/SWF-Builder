package SWF::Builder::Character::Bitmap::Lossless::GD;

use strict;

our @ISA = ('SWF::Builder::Character::Bitmap::Lossless');
our $VERSION = '0.02';

sub new {
    my ($class, $image) = @_;

    my ($width, $height) = $image->getBounds;
    my $tp_i = $image->transparent;
    bless {
	_width  => $width,
	_height => $height,
	_colors => $image->colorsTotal||1<<24,
	_is_alpha => ($tp_i >= 0),
	_pixsub => sub {
	    my ($x, $y) = @_;
	    my $index = $image->getPixel($x, $y);
	    if ($index == $tp_i) {
		return (0,0,0,0);
	    } else {
		return ($image->rgb($index), 255);
	    }
	},
    }, $class;
}

1;

