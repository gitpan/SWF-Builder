package SWF::Builder::Shape;

use strict;
use Carp;
use SWF::Element;
use SWF::Builder::ExElement;
use SWF::Builder::Gradient;

our $VERSION="0.02";

{
    package SWF::Builder::Shape;

    sub new {
	my $class = shift;
	
	my $self = bless {
	    _current_line_width => 1,
	    _current_X => 0,
	    _current_Y => 0,
	    _edges => SWF::Element::SHAPE->ShapeRecords->new,
	    _bounds => SWF::Builder::ExElement::BoundaryRect->new,
	}, $class;
    }

    sub _set_bounds {
	my ($self, $x, $y) = @_;
	my $cw = $self->{_current_line_width} * 10;

	$self->{_bounds}->set_boundary($x-$cw, $y-$cw, $x+$cw, $y+$cw);
    }

    sub _get_stylerecord {
	my $self = shift;
	my $edges = $self->{_edges};
	my $r;
	if (ref($edges->[-1])=~/STYLECHANGERECORD$/) {
	    $r = $edges->[-1];
	} else {
	    $r = $edges->new_element;
	    push @$edges, $r;
	}
	return $r;
    }

    sub r_lineto {
	my $self = shift;

	$self->_r_lineto_twips(map $_*20, @_);
    }

    sub _r_lineto_twips {
	my $self = shift;
	my $edges = $self->{_edges};    

	while (my($dx, $dy) = splice(@_, 0, 2)) {
	    $dx = _round($dx);
	    $dy = _round($dy);
	    push @$edges, $edges->new_element( DeltaX => $dx, DeltaY => $dy );
	    $dx = ($self->{_current_X} += $dx);
	    $dy = ($self->{_current_Y} += $dy);
	    $self->_set_bounds($dx, $dy);
	}
	$self;
    }

    sub lineto {
	my $self = shift;

	$self->_lineto_twips(map $_*20, @_);
    }

    sub _lineto_twips {
	my $self = shift;
	my $edges = $self->{_edges};    
	
	while (my($x, $y) = splice(@_, 0, 2)) {
	    $x = _round($x);
	    $y = _round($y);
	    push @$edges, $edges->new_element( DeltaX => $x-$self->{_current_X}, DeltaY => $y-$self->{_current_Y} );
	    $self->{_current_X} = $x;
	    $self->{_current_Y} = $y;
	    $self->_set_bounds($x, $y);
	}
	$self;
    }

    sub r_curveto {
	my $self = shift;

	$self->_r_curveto_twips(map $_*20, @_);
    }

    sub _r_curveto_twips {
	my $self = shift;
	my $edges = $self->{_edges};    

	while(my($cdx, $cdy, $adx, $ady) = splice(@_, 0, 4)) {
	    my $curx = $self->{_current_X};
	    my $cury = $self->{_current_Y};
	    $cdx = _round($cdx);
	    $cdy = _round($cdy);
	    $adx = _round($adx);
	    $ady = _round($ady);
	    push @$edges, $edges->new_element
		(
		 ControlDeltaX => $cdx,
		 ControlDeltaY => $cdy,
		 AnchorDeltaX  => $adx,
		 AnchorDeltaY  => $ady,
		 );
	    $adx = ($self->{_current_X} += $cdx+$adx);
	    $ady = ($self->{_current_Y} += $cdy+$ady);
	    $self->_set_bounds($adx, $ady);
	    $self->_set_bounds($curx+$cdx, $cury+$cdy);
	}
	$self;
    }

    sub curveto {
	my $self = shift;

	$self->_curveto_twips(map $_*20, @_);
    }

    sub _curveto_twips {
	my $self = shift;
	my $edges = $self->{_edges};    

	while(my ($cx, $cy, $ax, $ay) = splice(@_, 0, 4)) {

	    my $curx = $self->{_current_X};
	    my $cury = $self->{_current_Y};
	    $cx = _round($cx);
	    $cy = _round($cy);
	    $ax = _round($ax);
	    $ay = _round($ay);

	    push @$edges, $edges->new_element
		(
		 ControlDeltaX => $cx-$curx,
		 ControlDeltaY => $cy-$cury,
		 AnchorDeltaX  => $ax-$cx,
		 AnchorDeltaY  => $ay-$cy,
		 );
	    $self->{_current_X} = $ax;
	    $self->{_current_Y} = $ay;
	    $self->_set_bounds($ax, $ay);
	    $self->_set_bounds($cx, $cy);
	}
	$self;
    }

    sub moveto {
	my ($self, $x, $y)=@_;
	$self->_moveto_twips($x*20, $y*20);
    }

    sub _moveto_twips {
	my ($self, $x, $y)=@_;
	my $r = $self->_get_stylerecord;

	$x = _round($x);
	$y = _round($y);
	$r->MoveDeltaX($x);
	$r->MoveDeltaY($y);
	$self->{_current_X} = $x;
	$self->{_current_Y} = $y;
	$self->_set_bounds($x, $y);
	$self;
    }

    sub r_moveto {
	my ($self, $dx, $dy)=@_;
	$self->_r_moveto_twips($dx*20, $dy*20);
    }

    sub _r_moveto_twips {
	my ($self, $dx, $dy)=@_;
	my $r = $self->_get_stylerecord;
	$dx = _round($dx);
	$dy = _round($dy);
	
	$dx = ($self->{_current_X} += $dx);
	$dy = ($self->{_current_Y} += $dy);
	$r->MoveDeltaX($dx);
	$r->MoveDeltaY($dy);
	$self->_set_bounds($dx, $dy);
	$self;
    }

    sub box {
	my ($self, $x1, $y1, $x2, $y2) = @_;

	$self->moveto($x1,$y1)
	    ->lineto($x2,$y1)
		->lineto($x2,$y2)
		    ->lineto($x1,$y2)
			->lineto($x1,$y1);
    }

    my %style = ('none' => 0, 'fill' => 1, 'draw' => 1);
    sub fillstyle {
	my ($self, $f) = @_;
	my $r = $self->_get_stylerecord;
	my $index;
	if (exists $style{$f}) {
	    $index = $style{$f};
	} else {
	    $index = $f;
	}
	$r->FillStyle0($index);
	$self;
    }
    *fillstyle0 = \&fillstyle;

    sub fillstyle1 {
	my ($self, $f) = @_;
	my $r = $self->_get_stylerecord;
	my $index;
	if (exists $style{$f}) {
	    $index = $style{$f};
	} else {
	    $index = $f;
	}
	$r->FillStyle1($index);
	$self;
    }

    sub linestyle {
	my ($self, $f) = @_;
	my $r = $self->_get_stylerecord;
	my $index;
	if (exists $style{$f}) {
	    $index = $style{$f};
	} else {
	    $index = $f;
	}
	$r->LineStyle($index);
	$self;
    }

    sub _round {
	my $a=shift;

	$a||=0;
	return int($a+0.5*($a<=>0));
    }
}
#####
{
    package SWF::Builder::Shape::DefineShape;
    @SWF::Builder::Shape::DefineShape::ISA = qw/ SWF::Builder::Shape SWF::Builder::Character::Displayable SWF::Builder::ExElement::Color::AddColor /;

    sub new {
	my $self = shift->SUPER::new;
	$self->SWF::Builder::Character::Displayable::_init;
	$self->SWF::Builder::ExElement::Color::AddColor::_init;

	$self->{_edges} = SWF::Element::SHAPEWITHSTYLE3->ShapeRecords->new;
	$self->{_current_line_width} = -1;
	$self->{_current_line_color} = undef;
	$self->{_current_fill_style} = '';
	$self->{_line_styles} = $self->{_shape_line_styles} = SWF::Element::SHAPEWITHSTYLE3->LineStyles->new;
	$self->{_line_style_hash} = {};
	$self->{_fill_styles} = $self->{_shape_fill_styles} = SWF::Element::SHAPEWITHSTYLE3->FillStyles->new;
	$self->{_fill_style_hash} = {};
	$self;
    }

    sub _add_gradient {
	my ($self, $gradient) = @_;

	$self->{_is_alpha}->configure($self->{_is_alpha}->value | $gradient->{_is_alpha}->value);
	return bless {
	    _is_alpha => $self->{_is_alpha},
	    _gradient => $gradient,
	}, 'SWF::Builder::Shape::Gradient';
    }

    sub linestyle {
	my $self = shift;
	my ($r, $index, $width, $color);
	$r = $self->_get_stylerecord;

	if ($_[0] eq 'none' or $_[0] eq 0) {
	    $index = 0;
	    $width = -1;
	    $color = undef;
	} else {
	    my %param;
	    if ($_[0] eq 'Width' or $_[0] eq 'Color') {
		%param = @_;
	    } else {
		%param = (Width => $_[0], Color => $_[1]);
	    }
	    $width = $param{Width};
	    $width = $self->{_current_line_width} unless defined $width;
	    if (defined $param{Color}) {
		$color = $self->_add_color($param{Color});
	    } else {
		$color = $self->{_current_line_color};
	    }
	    return $self if ($width == $self->{_current_line_width} and $color eq $self->{_current_line_color});
	    
	    if (exists $self->{_line_style_hash}{"$width:$color"}) {
		$index = $self->{_line_style_hash}{"$width:$color"};
	    } else {
		if (@{$self->{_line_styles}} >= 65534) {
		    $self->{_line_styles} = $r->LineStyles;
		    $self->{_line_style_hash} = {};
		    $self->{_fill_styles} = $r->FillStyles;
		    $self->{_fill_style_hash} = {};
		}
		my $ls = $self->{_line_styles};
		push @$ls, $ls->new_element(Width => $width*20, Color => $color);
		$index = $self->{_line_style_hash}{"$width:$color"} = @$ls;
	    }
	}
	$r->LineStyle($index);
	$self->{_current_line_width} = $width;
	$self->{_current_line_color} = $color;
	$self;
    }

    sub _fillstyle {
	my $self = shift;
	my $setstyle = shift;
	my ($r, $index, $fillkey);
	$r = $self->_get_stylerecord;

	if ($_[0] eq 'none' or $_[0] eq 0) {
	    $index = 0;
	    return unless $self->{_current_fill_style};
	    $fillkey = '';
	} else {
	    my %param;
	    if ($_[0] eq 'Color' or $_[0] eq 'Gradient' or $_[0] eq 'Bitmap') {
		%param = @_;
	    } else {
		for (ref($_[0])) {
		    /Gradient/ and do {
			%param = (Gradient => $_[0], Type => $_[1], Matrix => $_[2]);
			last;
		    };
		    /Bitmap/ and do {
			%param = (Bitmap => $_[0], Type => $_[1], Matrix => $_[2]);
			last;
		    };
		    %param = (Color => $_[0]);
		}
	    }
	    my @param2;

	    $fillkey = join(',', %param);
	    if (exists $param{Gradient}) {
		push @param2, Gradient       => $self->_add_gradient($param{Gradient}),
		              FillStyleType  =>
				 (lc($param{Type}) eq 'radial' ? 0x12 : 0x10), 
			      GradientMatrix => $param{Matrix};
 
	    } elsif (exists $param{Bitmap}) {
		push @param2, BitmapID      => $param{Bitmap}->{ID},
		              FillStyleType =>
				  (lc($param{Type}) =~ /^clip(ped)?$/ ? 0x41 : 0x40),
			      BitmapMatrix  => $param{Matrix};
		$self->{_is_alpha}->configure($self->{_is_alpha} | $param{Bitmap}{_is_alpha});
		$self->_depends($param{Bitmap});
	    } else {
		push @param2, Color => $self->_add_color($param{Color}),
		              FillStyleType => 0x00;
	    }

	    return $self if $self->{_current_fill_style} eq $fillkey;

	    if (exists $self->{_fill_style_hash}{$fillkey}) {
		$index = $self->{_fill_style_hash}{$fillkey};
	    } else {
		if (@{$self->{_fill_styles}} >= 65534) {
		    $self->{_line_styles} = $r->LineStyles;
		    $self->{_line_style_hash} = {};
		    $self->{_fill_styles} = $r->FillStyles;
		    $self->{_fill_style_hash} = {};
		}
		my $fs = $self->{_fill_styles};
		push @$fs, $fs->new_element(@param2);
		$index = $self->{_fill_style_hash}{$fillkey} = @$fs;
	    }
	}
	$r->$setstyle($index);
	$self->{_current_fill_style} = $fillkey;
	$self;
    }

    sub fillstyle {
	my $self = shift;
	_fillstyle($self, 'FillStyle0', @_);
    }

    *fillstyle0 = \&fillstyle;

    sub fillstyle1 {
	my $self = shift;
	_fillstyle($self, 'FillStyle1', @_);
    }

    sub get_bbox {
	return map{$_/20} @{shift->{_bounds}};
    }

    sub pack {
	my ($self, $stream) = @_;

	$self->prepare_to_pack($stream) or return;
	my $tag = ($self->{_is_alpha} ? SWF::Element::Tag::DefineShape3->new : SWF::Element::Tag::DefineShape2->new);
	$tag->ShapeID($self->{ID});
	$tag->ShapeBounds($self->{_bounds});
	$tag->Shapes
	    (
	      FillStyles => $self->{_shape_fill_styles},
	      LineStyles => $self->{_shape_line_styles},
	      ShapeRecords =>$self->{_edges},
	     );
	$tag->pack($stream);
    }
}

#####

{
    package SWF::Builder::Shape::Gradient;

    @SWF::Builder::Shape::Gradient::ISA = ('SWF::Element::Array::GRADIENT3');

    sub pack {
	my ($self, $stream) = @_;

	my $g = $self->{_gradient};
	my $a = $g->{_is_alpha}->value;
	$g->{_is_alpha}->configure($self->{_is_alpha});
	$g->pack($stream);
	$g->{_is_alpha}->configure($a);
    }
}

1;
__END__


=head1 NAME

SWF::Builder::Shape - SWF shape object

=head1 SYNOPSIS

  my $shape = $mc->new_shape
    ->fillstyle('ff0000')
    ->linestyle(1, '000000')
    ->moveto(0,-11)
    ->lineto(10,6)
    ->lineto(-10,6)
    ->lineto(0,-11);
  my @bbox = $shape->get_bbox;

=head1 DESCRIPTION

SWF shape is defined by a list of edges.

=over 4

=item $shape = $mc->new_shape

returns a new shape.

=item $shape->linestyle( [ Width => $width, Color => $color ] )

=item $shape->linestyle( $width, $color )

=item $shape->linestyle( 'none' )

sets line width and color. The color can take a six or eight-figure
hexadecimal string, an array reference of R, G, B, and optional alpha value, 
an array reference of named parameters such as [Red => 255],
and SWF::Element::RGB/RGBA object.
If you set the style 'none', edges are not drawn.

=item $shape->fillstyle( [ Color => $color / Gradient => $gradient, Type => $type, Matrix => $matrix / Bitmap => $bitmap, Type => $type, Matrix => $matrix ] )

=item $shape->fillstyle( $color )

=item $shape->fillstyle( $gradient, $type, $matrix )

=item $shape->fillstyle( $bitmap, $type, $matrix )

=item $shape->fillstyle( 'none' )

sets a fill style.

$color is a solid fill color. 
See $shape->linestyle for the acceptable color value.

$gradient is a gradient object. Give $type 'radial' to fill with 
radial gradient, otherwise linear.
$matrix is a matrix to transform the gradient. 
See L<SWF::Builder::Gradient>.

$bitmap is a bitmap character. Give $type 'clipped' to fill with 
clipped bitmap, otherwise tiled.
$matrix is a matrix to transform the bitmap. 
See L<SWF::Builder::Bitmap>.

=item $shape->fillstyle0( ... )

identical to $shape->fillstyle.

=item $shape->fillstyle1( ... )

sets an additional fillstyle used in self-overlap shape.

=item $shape->moveto( $x, $y )

moves the draw point to ($x, $y).

=item $shape->r_moveto( $dx, $dy )

moves the draw point to ( current X + $dx, current Y + $dy ).

=item $shape->lineto( $x, $y )

draws a line from the current draw point to ($x, $y)

=item $shape->r_lineto( $dx, $dy )

draws a line from the current draw point to ( current X + $dx, current Y + $dy ).

=item $shape->curveto( $cx, $cy, $ax, $ay )

draws a quadratic bezier curve from the current draw point to ($ax, $ay)
using ($cx, $cy) as the control point.

=item $shape->r_curveto( $cdx, $cdy, $adx, $ady )

draws a quadratic bezier curve from the current draw point to 
(current X + $cdx+$adx, current Y + $cdy+$ady)
using (current X + $cdx, current Y + $cdy) as the control point.

=item $shape->box( $x1, $y1, $x2, $y2 )

draws a box. The draw point is moved to ($x1, $y1) after drawing.

=item $shape->get_bbox

returns the bounding box of the shape, a list of coordinates
( top-left X, top-left Y, bottom-right X, bottom-right Y ).

=item $disp_i = $shape->place( ... )

returns the display instance of the shape. See L<SWF::Builder>.

=back

=head1 COPYRIGHT

Copyright 2003 Yasuhiro Sasama (ySas), <ysas@nmt.ne.jp>

This library is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.

=cut
