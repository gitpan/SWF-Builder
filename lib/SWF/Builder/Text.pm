package SWF::Builder::Text;

use strict;
use utf8;

use Carp;
use SWF::Element;
use SWF::Builder::Font;
use SWF::Builder::ExElement;

our $VERSION="0.01";

@SWF::Builder::Text::ISA = qw/ SWF::Builder::Character::Displayable SWF::Builder::ExElement::Color::AddColor/;

sub new {
    my ($class, $font, $text) = @_;
    my $tag;
    my $t1 = SWF::Element::TEXTRECORD2::TYPE1->new;
    my $self = bless {
	_bounds       => SWF::Builder::ExElement::BoundaryRect->new,
	_textrecords  => [$t1],
	_current_font => '',
	_current_size => 12,
	_max_size     => 0,
	_current_X    => 0,
	_current_Y    => 0,
	_leading      => 2,
	_nl           => $t1,
	_nl_X         => 0,
	_nlbounds     => SWF::Builder::ExElement::BoundaryRect->new,
    }, $class;
    $self->SWF::Builder::Character::Displayable::_init;
    $self->SWF::Builder::ExElement::Color::AddColor::_init;
    
    $self->font($font) if defined $font;
    $self->text($text) if defined $text;
    $self;
}

sub _get_type1 {
    my $self = shift;
    my $records = $self->{_textrecords};
    my $r;
    if (UNIVERSAL::isa($records->[-1], 'SWF::Element::TEXTRECORD2::TYPE1')) {
	$r = $records->[-1];
    } else {
	$r = SWF::Element::TEXTRECORD2::TYPE1->new;
	push @$records, $r;
    }
    return $r;
}

sub font {
    my ($self, $font) = @_;
    return if $font eq $self->{_current_font};
    croak "Invalid font" unless UNIVERSAL::isa($font, 'SWF::Builder::Font');
    my $r = $self->_get_type1;
    $r->TextHeight($self->{_current_size}*20);
    $r->FontID($font->{ID});
    $self->{_current_font} = $font;
    $self->_depends($font);
    $self;
}

sub size {
    my ($self, $size) = @_;
    my $r = $self->_get_type1;
    $r->TextHeight($size*20);
    $r->FontID($self->{_current_font}->{ID});
    $self->{_current_size} = $size;
    $self->{_max_size} = $size if $self->{_max_size} < $size;
    $self;
}

sub leading {
    my ($self, $leading) = @_;
    $self->{_leading} = $leading;
}

sub color {
    my ($self, $color) = @_;
    my $r = $self->_get_type1;
    $color = $self->_add_color($color);
    $r->TextColor($color);
    $self;
}

sub _bbox_adjust {
    my $self = $_[0];  # Don't use 'shift'
    my $nl = $self->{_nl};
    my $nlbbox = $self->{_nlbounds};
    return unless defined $nlbbox->[0];

    my $s = $self->{_current_Y} + $self->{_max_size};

    $self->{_bounds}->set_boundary($nlbbox->[0], $nlbbox->[1]+$s, $nlbbox->[2], $nlbbox->[3]+$s);
    $self->{_nlbounds} = SWF::Builder::ExElement::BoundaryRect->new;
}

sub _line_adjust {
    my $self = $_[0];  # Don't use 'shift'
    &_bbox_adjust;
    $self->{_current_Y} += $self->{_max_size};
    $self->{_nl}->YOffset($self->{_current_Y}*20);
    $self->{_max_size} = $self->{_current_size};
    $self->{_nl} = undef;
}

sub position {
    &_line_adjust;
    goto &_position;
}

sub _position {
    my ($self, $x, $y) = @_;
    my $r = $self->_get_type1;
    $r->XOffset($x*20);
    $r->YOffset($y*20);
    $self->{_bounds}->set_boundary($x, $y, $x, $y);
    $self->{_current_X} = $self->{_nl_X} = $x;
    $self->{_current_Y} = $y;
    $self->{_nl} = $r;
    $self;
}

sub _get_textrecord {
    my ($self, $font) = @_;
    my $records = $self->{_textrecords};
    my $r;
    if (UNIVERSAL::isa($records->[-1], 'SWF::Builder::Text::TEXTRECORD')) {
	$r = $records->[-1];
    } else {
	$r = SWF::Builder::Text::TEXTRECORD->new($font);
	push @$records, $r;
    }
    return $r;
}

sub text {
    my ($self, $text) = @_;
    my @text = split /([\x00-\x1f]+)/, $text;
    my $font = $self->{_current_font};
    my $bbox = $self->{_nlbounds};
    my $scale = $self->{_current_size} / 51.2;
    my $glyph_hash = $font->{_glyph_hash};
    
    while (my($text, $ctrl) = splice(@text, 0, 2)) {
	$font->add_glyph($text);
	my @chars = split //, $text;
	if (@chars) {
	    my $trec = $self->_get_textrecord($font);
	    my $c1 = shift @chars;
	    push @chars, undef;
	    my $x = $self->{_current_X};
	    for my $c (@chars) {
		my $ord_c1 = ord($c1);
		my $kern = defined $c ? $font->kern($ord_c1, ord($c)) : 0;
#		my $kern = 0;
		my $adv = ($glyph_hash->{$c1}[0] + $kern) * $scale;
		my $b = $glyph_hash->{$c1}[2];
		if (defined $b->[0]) {
		    $bbox->set_boundary($x+$b->[0]*$scale, $b->[1]*$scale, $x+$b->[2]*$scale, $b->[3]*$scale);
		} else {
		    $bbox->set_boundary($x, 0, $x, 0);
		}
		push @{$trec->[0]}, [$ord_c1, $adv];
		$x += $adv;
		$c1 = $c;
	    }
	    $self->{_current_X} = $x;
	}

	if ($ctrl and (my $n = $ctrl=~tr/\n/\n/)) {
	    $self->_line_adjust;
	    $self->_position($self->{_nl_X}, $self->{_current_Y}+$self->{_max_size}*($n-1) + ($font->{_tag}->FontLeading / 51.2 + $self->{_leading})*$n);
	}
    }
    $self;
}

sub get_bbox {
    my $self = shift;
    $self->_bbox_adjust;
    return @{$self->{_bounds}};
}

sub pack {
    my ($self, $stream) = @_;
    
    $self->_line_adjust if $self->{_nl};
    $self->prepare_to_pack($stream) or return;
    
    my $x = $self->{_current_X} = 0;
    my $y = $self->{_current_Y} = 0;
    
    my $tag;
    if ($self->{_is_alpha}) {
	$tag = SWF::Element::Tag::DefineText2->new;
    } else {
	$tag = SWF::Element::Tag::DefineText->new;
    }
    $tag->configure( CharacterID => $self->{ID},
		     TextBounds  => $self->{_bounds},
		     );

    my $new_tr = $tag->TextRecords;

    for my $tr (@{$self->{_textrecords}}) {
	push @$new_tr,
	($tr->isa('SWF::Builder::Text::TEXTRECORD')) ?
	    $tr->_init_glyphentry :
	    $tr;
    }
    
    $tag->pack($stream);
}


####

{
    package SWF::Builder::Text::TEXTRECORD;
    @SWF::Builder::Text::TEXTRECORD::ISA = ('SWF::Element::TEXTRECORD::TYPE0');

    sub new {
	my $class = shift;
	bless [[], shift], $class;   # [characters, advances], font
    }

    sub _init_glyphentry {
	my $self = shift;
	my $code_hash = $self->[1]{_code_hash};
	my $new_trec = SWF::Element::TEXTRECORD::TYPE0->new;
	my $ga = $new_trec->GlyphEntries;

	for my $c (@{$self->[0]}) {
	    my $index = $code_hash->{$c->[0]};
	    push @$ga, $ga->new_element(GlyphIndex => $index, GlyphAdvance => $c->[1]*20);
	}
	return $new_trec;
    }
}

1;
__END__


=head1 NAME

SWF::Builder::Text - SWF static text object

=head1 SYNOPSIS

  my $text = $mc->new_static_text( $font )
    ->size(10)
    ->color('000000')
    ->text('This is a text.');

  my $text_i = $text->place;

=head1 DESCRIPTION

This module creates static texts, which cannot be changed at playing time.

=over 4

=item $text = $mc->new_static_text( [$font, $text] )

returns a new static text.
$font is an SWF::Builder::Font object.

=item $text->font( $font )

applies the font to the following text.
$font is an SWF::Builder::Font object.

=item $text->size( $size )

sets a font size to $size in pixel.

=item $text->color( $color )

sets color of the following text.
The color can take a six or eight-figure
hexadecimal string, an array reference of R, G, B, and optional alpha value, 
an array reference of named parameters such as [Red => 255],
and SWF::Element::RGB/RGBA object.

=item $text->text( $string )

writes the $string.  The glyph data of the applied font is embedded if needed.
The string can also include a newline code, "\n".

=item $text->position( $x, $y )

sets the position of the following text.
($x, $y) are coordinates in pixel relative to the I<origin> of the text object.

=item $text->leading( $leading )

sets the vertical distance between the lines in pixel.

=item $text->get_bbox

returns the bounding box of the text, a list of coordinates
( top-left X, top-left Y, bottom-right X, bottom-right Y ).

=item $text_i = $text->place( ... )

returns the display instance of the text. See L<SWF::Builder>.

=back

=head1 COPYRIGHT

Copyright 2003 Yasuhiro Sasama (ySas), <ysas@nmt.ne.jp>

This library is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.

=cut
