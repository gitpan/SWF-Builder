package SWF::Builder::Bitmap::Lossless::ImageMagick;

use strict;

our @ISA = ('SWF::Builder::Bitmap::Lossless');
our $VERSION = '0.011';

sub new {
    my ($class, $image) = @_;

    bless {
	_width  => $image->Get('width'),
	_height => $image->Get('height'),
	_colors => $image->Get('colors'),
	_is_alpha => $image->Get('matte'),
	_pixsub => sub {
	    my ($x, $y) = @_;
	    my  @rgba = map{$_ & 255} split /,/, $image->Get("pixel[$x,$y]");
	    $rgba[3] = 255-$rgba[3];
	    return @rgba;
	},
    }, $class;
}

