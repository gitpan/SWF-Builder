package SWF::Builder::Bitmap::Lossless::GD;

use strict;

our @ISA = ('SWF::Builder::Bitmap::Lossless');

sub new {
    my ($class, $image) = @_;

    my ($width, $height) = $image->getBounds;
    my $tp_i = $image->transparent;

    bless {
	_width  => $width,
	_height => $height,
	_colors => $image->colorsTotal,
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

