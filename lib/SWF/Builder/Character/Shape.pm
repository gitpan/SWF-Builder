package SWF::Builder::Character::Shape;

use strict;
use Carp;
use SWF::Element;
use SWF::Builder::Character;
use SWF::Builder::ExElement;
use SWF::Builder::Gradient;

our $VERSION="0.04";

@SWF::Builder::Character::Shape::ISA = qw/ SWF::Builder::Character::UsableAsMask /;
@SWF::Builder::Character::Shape::Imported::ISA = qw/ SWF::Builder::Character::Imported SWF::Builder::Character::Shape /;

####

{
    package SWF::Builder::Shape;

    use SWF::Builder::ExElement;
    use Carp;

    sub new {
	my $class = shift;
	
	my $self = bless {
	    _current_line_width => 1,
	    _current_X => 0,
	    _current_Y => 0,
	    _current_font => undef,
	    _current_size => 12,
	    _edges => SWF::Element::SHAPE->ShapeRecords->new,
	    _bounds => SWF::Builder::ExElement::BoundaryRect->new,
	}, $class;

	$self->_init;
	$self->moveto(0,0);
    }

    sub _init {}

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

    sub _set_style {
	my ($self, %param) = @_;
	my $r = $self->_get_stylerecord;

	for my $p (qw/ MoveDeltaX MoveDeltaY FillStyle0 FillStyle1 LineStyle /) {
	    $r->$p($param{$p}) if exists $param{$p};
	}
	return $r;
    }

#### basic drawing ####
# handling _edges directly

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
	    $self->_set_bounds($curx+$cdx, $cury+$cdy, 1); # 1: off curve
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
	    $self->_set_bounds($cx, $cy, 1);  # 1: off curve
	}
	$self;
    }

    sub moveto {
	my ($self, $x, $y)=@_;
	$self->_moveto_twips($x*20, $y*20);
    }

    sub _moveto_twips {
	my ($self, $x, $y)=@_;

	$x = _round($x);
	$y = _round($y);
	$self->_set_style(MoveDeltaX => $x, MoveDeltaY => $y);
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

	$dx = _round($dx);
	$dy = _round($dy);
	$dx = $self->{_current_X} + $dx;
	$dy = $self->{_current_Y} + $dy;
	$self->_set_style(MoveDeltaX => $dx, MoveDeltaY => $dy);
	$self->{_current_X} = $dx;
	$self->{_current_Y} = $dy;
	$self->_set_bounds($dx, $dy);
	$self;
    }

    my %style = ('none' => 0, 'fill' => 1, 'draw' => 1);
    sub fillstyle {
	my ($self, $f) = @_;
	my $index;
	if (exists $style{$f}) {
	    $index = $style{$f};
	} else {
	    $index = $f;
	}
	$self->_set_style(FillStyle0 => $index);
	$self;
    }
    *fillstyle0 = \&fillstyle;

    sub fillstyle1 {
	my ($self, $f) = @_;
	my $index;
	if (exists $style{$f}) {
	    $index = $style{$f};
	} else {
	    $index = $f;
	}
	$self->_set_style(FillStyle1 => $index);
	$self;
    }

    sub linestyle {
	my ($self, $f) = @_;
	my $index;
	if (exists $style{$f}) {
	    $index = $style{$f};
	} else {
	    $index = $f;
	}
	$self->_set_style(LineStyle => $index);
	$self;
    }

### extension drawing ###
# no-touch _edges directly

    sub box {
	my ($self, $x1, $y1, $x2, $y2) = @_;

	$self->moveto($x1,$y1)
	    ->lineto($x2,$y1)
		->lineto($x2,$y2)
		    ->lineto($x1,$y2)
			->lineto($x1,$y1);
    }

    sub font {
	my ($self, $font) = @_;

	croak "Invalid font" unless UNIVERSAL::isa($font, 'SWF::Builder::Character::Font') and $font->embed;
	$self->{_current_font} = $font;
	$self;
    }

    sub size {
	my $self = shift;
	$self->{_current_size} = shift;
	$self;
    }

    sub text {
	my ($self, $font, $text) = @_;

	unless (defined $text) {
	    $text = $font;
	    $font = $self->{_current_font};
	}
	croak "Invalid font" unless UNIVERSAL::isa($font, 'SWF::Builder::Character::Font') and eval{$font->embed};

	my $hash = $font->{_glyph_hash};
	my $cmap = $font->{_ttf_tables}{_cmap};
	my $glyphs = $font->{_ttf_tables}{_glyphs};
	my $advances = $font->{_ttf_tables}{_advance};
	my $scale = $font->{_scale} * $self->{_current_size} * 20 / 1024;
	my $tag = $font->{_tag};

	for my $c (split //, $text) {
	    my $cx = $self->{_current_X};
	    my $cy = $self->{_current_Y};
	    my $code = ord($c);
	    my $gid = $cmap->{$code};
	    my $adv = $advances->[$gid] * $scale;
	    my $glyph = $glyphs->[$gid];
	    if (defined $glyph) {
		$glyph->read_dat;

		my $i = 0;
		for my $j (@{$glyph->{endPoints}}) {
		    my @x = map {$_ * $scale + $cx} @{$glyph->{x}}[$i..$j];
		    my @y = map {-$_ * $scale + $cy} @{$glyph->{y}}[$i..$j];
		    my @f = @{$glyph->{flags}}[$i..$j];
		    $i=$j+1;
		    my $sx = shift @x;
		    my $sy = shift @y;
		    my $f  = shift @f;
		    unless ($f & 1) {
			push @x, $sx;
			push @y, $sy;
			push @f, $f;
			if ($f[0] & 1) {
			    $sx = shift @x;
			    $sy = shift @y;
			    $f  = shift @f;
			} else {
			    $sx = ($sx+$x[0])/2;
			    $sy = ($sy+$y[0])/2;
			    $f = 1;
			}
		    }
		    push @x, $sx;
		    push @y, $sy;
		    push @f, $f;
		    $self->_moveto_twips($sx, $sy);
		    while(@x) {
			my ($x, $y, $f)=(shift(@x), shift(@y), (shift(@f) & 1));
		    
			if ($f) {
			    $self->_lineto_twips($x, $y);
			} else {
			    my ($ax, $ay);
			    if ($f[0] & 1) {
				$ax=shift @x;
				$ay=shift @y;
				shift @f;
			    } else {
				$ax=($x+$x[0])/2;
				$ay=($y+$y[0])/2;
			    }
			    $self->_curveto_twips($x, $y, $ax, $ay);
			}
		    }
		}
	    }
	    $self->_moveto_twips($cx + $adv, $cy);
	}
	$self;
    }


}


#####


{
    package SWF::Builder::Character::Shape::Def;

    use SWF::Builder::ExElement;

    @SWF::Builder::Character::Shape::Def::ISA = qw/ SWF::Builder::Shape SWF::Builder::Character::Shape SWF::Builder::ExElement::Color::AddColor /;

    sub new {
	my $self = shift->SUPER::new;
    }

    sub _init {
	my $self = shift;
	$self->_init_character;
	$self->_init_is_alpha;

	$self->{_edges} = SWF::Element::SHAPEWITHSTYLE3->ShapeRecords->new;
	$self->{_current_line_width} = -1;
	$self->{_current_line_color} = '';
	$self->{_current_FillStyle0} = '';
	$self->{_current_FillStyle1} = '';
	$self->{_line_styles} = $self->{_shape_line_styles} = SWF::Element::SHAPEWITHSTYLE3->LineStyles->new;
	$self->{_line_style_hash} = {};
	$self->{_fill_styles} = $self->{_shape_fill_styles} = SWF::Element::SHAPEWITHSTYLE3->FillStyles->new;
	$self->{_fill_style_hash} = {};
	$self->{_links} = [];

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
	my ($index, $width, $color);

	if ($_[0] eq 'none' or $_[0] eq 0) {
	    $index = 0;
	    $width = -1;
	    $color = '';
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
		    my $r = $self->_get_stylerecord;
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
	$self->_set_style(LineStyle => $index);
	$self->{_current_line_width} = $width;
	$self->{_current_line_color} = $color;
	$self;
    }

    sub _fillstyle {
	my $self = shift;
	my $setstyle = shift;
	my ($index, $fillkey);

	if ($_[0] eq 'none' or $_[0] eq 0) {
	    $index = 0;
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

	    return $self if $self->{"_current_$setstyle"} eq $fillkey;

	    if (exists $self->{_fill_style_hash}{$fillkey}) {
		$index = $self->{_fill_style_hash}{$fillkey};
	    } else {
		if (@{$self->{_fill_styles}} >= 65534) {
		    my $r = $self->_get_stylerecord;
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
	$self->_set_style($setstyle => $index);
	$self->{"_current_$setstyle"} = $fillkey;
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

    sub anchor {
	my ($self, $anchor) = @_;

	$self->{_anchors}{$anchor} = [$#{$self->{_edges}}, $self->{_current_X}, $self->{_current_Y}, $#{$self->{_links}}];
	$self->{_last_anchor} = $anchor;
	$self;
    }

    sub _set_bounds {
	my ($self, $x, $y, $f) = @_;
	$self->SUPER::_set_bounds($x, $y);
	return if $f;

	if (defined $self->{_links}[-1]) {
#	    my $cw = $self->{_current_line_width} * 10;
	    my $m = $self->{_links}[-1];
#	    $m->[6]->set_boundary($x-$cw, $y-$cw, $x+$cw, $y+$cw);
	    my (undef, $tlx, $tly) = @{$m->[5]};
	    if ($x*$x+$y*$y < $tlx*$tlx+$tly*$tly) {
		$m->[5] = [$#{$self->{_edges}}, $x, $y];
	    }
	}
    }

    sub _set_style {
	my ($self, %param) = @_;

	if (exists $param{MoveDeltaX} and defined $self->{_links}[-1]) {
	    my $m = $self->{_links}[-1];
	    $m->[1] = $#{$self->{_edges}}; 
	    $m->[3] = $self->{_current_X}; 
	    $m->[4] = $self->{_current_Y}; 
	}

	my $r = $self->SUPER::_set_style(%param);

	if (exists $param{MoveDeltaX}) {
	    my ($x, $y) = ($param{MoveDeltaX}, $param{MoveDeltaY});
	    my @linkinfo = 
		($#{$self->{_edges}},        # start edge index
		 undef,                      # last continuous edge index
		 [$#{$self->{_edges}}],      # STYLECHANGERECORD indice
		 undef,                      # last X
		 undef,                      # last Y
		 [$#{$self->{_edges}}, $x, $y],               # top left
#	         SWF::Builder::ExElement::BoundaryRect->new,  # boundary
		 );
	    if (exists $self->{_links}[-1] and $self->{_links}[-1][0] == $linkinfo[0]) {
		$self->{_links}[-1] = \ @linkinfo;
	    } else {
		push @{$self->{_links}}, \ @linkinfo;
	    }
	    if (defined $self->{_last_anchor}) {
		my $last_anchor = $self->{_anchors}{$self->{_last_anchor}};
		if ($last_anchor->[0] == $#{$self->{_edges}} or $last_anchor->[0] == $#{$self->{_edges}}-1) {
		    $last_anchor->[0] = $#{$self->{_edges}};
		    $last_anchor->[1] = $x;
		    $last_anchor->[2] = $y;
		    $last_anchor->[3] = $#{$self->{_links}};
		}
	    }
	    $r->LineStyle($self->{_line_style_hash}{$self->{_current_line_width}.':'.$self->{_current_line_color}}) unless defined $r->LineStyle;
	    $r->FillStyle0($self->{_fill_style_hash}{$self->{_current_FillStyle0}}) unless defined $r->FillStyle0;
	    $r->FillStyle1($self->{_fill_style_hash}{$self->{_current_FillStyle1}}) unless defined $r->FillStyle1;
	} else {
	    push @{$self->{_links}[-1][2]}, $#{$self->{_edges}} if $self->{_links}[-1][2][-1] != $#{$self->{_edges}};
	}
	$r;
    }

    sub get_bbox {
	return map{$_/20} @{shift->{_bounds}};
    }

    sub _pack {
	my ($self, $stream) = @_;

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

SWF::Builder::Character::Shape - SWF shape object

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

=item $shape->font( $font )

applies the font to the following text.
$font is an SWF::Builder::Font object.

=item $shape->size( $size )

sets a font size to $size in pixel.

=item $text->text( $string )

draws the $string.

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
