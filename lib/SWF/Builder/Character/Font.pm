package SWF::Builder::Character::Font;

use strict;
use utf8;

our $VERSION="0.06";

our %indirect;

@indirect{ ('_sans', '_serif', '_typewriter', "_\x{30b4}\x{30b7}\x{30c3}\x{30af}", "_\x{660e}\x{671d}", "_\x{7b49}\x{5e45}") }
        = ('_sans', '_serif', '_typewriter', "_\x{30b4}\x{30b7}\x{30c3}\x{30af}", "_\x{660e}\x{671d}", "_\x{7b49}\x{5e45}");

@SWF::Builder::Character::Font::ISA = qw/ SWF::Builder::Character /;

####

package SWF::Builder::Character::Font::Imported;

@SWF::Builder::Character::Font::Imported::ISA = qw/ SWF::Builder::Character::Imported SWF::Builder::Character::Font /;

sub embed {1}  # ??
sub add_glyph{}

####

package SWF::Builder::Character::Font::Def;

use Carp;
use SWF::Element;
use SWF::Builder;
use SWF::Builder::ExElement;
use SWF::Builder::Character::Shape;
use Font::TTF::Font;
use Font::TTF::Ttc;

@SWF::Builder::Character::Font::Def::ISA = qw/ SWF::Builder::Character::Font /;

sub new {
    my ($class, $fontfile, $fontname) = @_;
    my $tag;
    my $type = 0;
    my $ttft;
    my $self = bless {
	_embed => 1,
	_read_only => 0,
	_code_hash => {},
	_glyph_hash => {},
	_tag => ($tag = SWF::Element::Tag::DefineFont2->new),
	_ttf_tables => ($ttft = bless {}, 'SWF::Builder::Font::TTFTables'),
    }, $class;

    $self->_init_character;
    $tag->FontID($self->{ID});

    my $font = $indirect{$fontfile} ||
               Font::TTF::Font->open($fontfile) ||  
	       Font::TTF::Ttc->open($fontfile) 
		   or croak "Can't open font file '$fontfile'";

    unless (ref($font)) {
	$fontname ||= $font;
	$self->{_embed} = 0;
    } else {
	my ($p_font, $head, $name, $os2, $hhea, $cmap, $loca, $hmtx, $kern);
	$ttft->{_font} = $p_font = $font;

	if (ref($font)=~/:Ttc$/) {   # TrueType collection
	    my @names;
	    $p_font = $font->{directs}[0];   # Primary font needs to access some table. 
	    for my $f (@{$font->{directs}}) { # For each collected font...
		my $names;
		$f->{name}->read;
		for my $pid (@{$f->{name}{strings}[1]}) { # gathers all font names ( latin, unicode...)

		    for my $eid (@$pid) {
			while (my ($lid, $s) = each(%$eid)) {
			    $names .= "$s\n";
			}
		    }
		}
		if (index($names, "$fontname\n") >=0) { # if match $fontname to the gathered,
		    $font = $f;                         # accept the font.
		    last;
		}
	    }
	}

      EMBED:
	{
	    $name = $font->{name}||$p_font->{name} # font name
	        or croak 'Invalid font';
	    if ($os2 = $font->{'OS/2'}||$p_font->{'OS/2'}) {  # get OS/2 table to check the lisence.
		$os2->read;
		my $fstype = $os2->{fsType} && 0;

		if ($fstype & 0x302) {
		    warn "Embedding outlines of the font '$fontfile' is not permitted.\n";
		    $self->{_embed} = 0;
		    last EMBED;
		} elsif ($fstype & 4) {
		    warn "The font '$fontfile' can use only for 'Preview & Print'.\n";
		    $self->{_read_only} = 1;
		}
	    } else {
		warn "The font '$fontfile' doesn't have any lisence information. See the lisence of the font.\n";
	    }
	    $head = $font->{head}||$p_font->{head} # header
	        or croak "Can't find TTF/OTF header of the font $fontname";
	    $hhea = $font->{hhea}||$p_font->{hhea} # horizontal header
	        or croak "Can't find hhea table of the font $fontname";
	    $cmap = $font->{cmap}||$p_font->{cmap} # chr-glyph mapping
	        or croak "Can't find cmap table of the font $fontname";
	    $loca = $font->{loca}||$p_font->{loca} # glyph location index
	        or croak "Can't find glyph index table of the font $fontname";
	    $hmtx = $font->{hmtx}||$p_font->{hmtx} # horizontal metrics
	        or croak "Can't find hmtx table of the font $fontname";
	    $kern = $font->{kern}||$p_font->{kern} # kerning table (optional)
	        and $kern->read;
	    $head->read;
	    $name->read;
	    $hhea->read;
	    $cmap->read;
	    $hmtx->read;
	    $loca->read;
	    my $scale = 1024 / $head->{unitsPerEm};   # 1024(Twips/Em) / S(units/Em) = Scale(twips/unit)
	    $tag->FontAscent($hhea->{Ascender} * $scale);
	    $tag->FontDescent(-$hhea->{Descender} * $scale);
	    $tag->FontLeading($hhea->{LineGap} * $scale);  # ?
	    $self->{_scale}  = $scale/20; # pixels/unit
	    $self->{_average_width} = defined($os2) ? $os2->{xAvgCharWidth}*$scale : 512;
	    $ttft->{_cmap}   = ($cmap->find_ms or croak "Can't find unicode cmap table in the font $fontname")->{val}; # Unicode cmap
	    $ttft->{_advance}= $hmtx->{advance};
	    $ttft->{_loca} = $loca; 
	    eval {
		for my $kt (@{$kern->{tables}}) {
		    if ($kt->{coverage} & 1) {
			$self->{_ttf_tables}{_kern} = $kt->{kern}; # horizontal kerning
			last;
		    }
		}
	    };
	}
	unless ($fontname) {
	    ($fontname) = ($name->find_name(1)=~/(.+)/);  # Cleaning up is needed. But why?
	    ($fontname) = ($fontfile =~ /.*\/([^\\\/.]+)/) unless $fontname;
	}
	$type = $head->{macStyle};
    }
    utf2bin($fontname);
    $tag->FontName($fontname);
    $tag->FontFlagsBold(1) if ($type & 1);
    $tag->FontFlagsItalic(1) if ($type & 2);

    $self;
}

sub embed {
    my ($self, $embed) = @_;

    if (defined $embed) {
	$self->{_embed} = $embed;
    }
    return $self->{_embed};
}

sub is_readonly {
    shift->{_read_only};
}

sub get_fontnames {
    my ($self, $ttc) = @_;

    my $font = Font::TTF::Ttc->open($ttc) 
      or croak "Can't open TTC font file '$ttc'";

    my @names;
    for my $f (@{$font->{directs}}) { # For each collected font...
	$f->{name}->read;
	my @alias_names;
	for my $pid (@{$f->{name}{strings}[1]}) { # gathers all font names ( latin, unicode...)

	    for my $eid (@$pid) {
		while (my ($lid, $s) = each(%$eid)) {
		    push @alias_names, $s;
		}
	    }
	}
	push @names, \@alias_names;
    }
    return \@names;
}

sub get_average_width {
    shift->{_average_width};
}

sub kern {
    my ($self, $code1, $code2) = @_;
    my $kern_t = $self->{_ttf_tables}{_kern} or return 0;
    my $cmap = $self->{_ttf_tables}{_cmap};
    if (exists $kern_t->{$cmap->{$code1}}) {
	if (exists $kern_t->{$cmap->{$code1}}{$cmap->{$code2}}) {
	    return $kern_t->{$cmap->{$code1}}{$cmap->{$code2}}/20;
	}
    }
    return 0;
}

sub glyph_shape {
    my ($self, $char) = @_;

    if (exists $self->{_glyph_hash}{$char} and defined $self->{_glyph_hash}{$char}[1]) {
	return $self->{_glyph_hash}{$char}[1];
    } else {
	my $gshape = SWF::Builder::Character::Font::Glyph->new;
	$self->{_glyph_hash}{$char}[1] = $gshape;
	return $gshape;
    }
}

sub _draw_glyph {
    my ($self, $c, $gshape) = @_;

    return unless $self->{_embed};

    my $scale = $self->{_scale};
    my $gid = $self->{_ttf_tables}{_cmap}{ord($c)};
    my $gtable = $self->{_ttf_tables}{_loca}{glyphs};
    my $glyph1 = $gtable->[$gid];
    if (defined $glyph1) {
	$glyph1->read_dat;
	unless (exists $glyph1->{comps}) {
	    $self->_draw_glyph_component($glyph1, $gshape);
	} else {
	    for my $cg (@{$glyph1->{comps}}) {
		my @m;
		@m = (translate => [$cg->{args}[0] * $scale, -$cg->{args}[1] * $scale]) if exists $cg->{args};
		if (exists $cg->{scale}) {  # Not tested...
		    my $s = $cg->{scale};
		    push @m, (ScaleX => $s->[0], RotateSkew0 => $s->[1], RotateSkew1 => $s->[2], ScaleY => $s->[3]);
		}
		my $ngs = $gshape->transform(\@m);
		my $glyph = $gtable->[$cg->{glyph}];
		$glyph->read_dat;
		$self->_draw_glyph_component($glyph, $ngs);
		$ngs->end_transform;
	    }
	}
    }
    return $self->{_ttf_tables}{_advance}[$gid] * $scale;
}

sub _draw_glyph_component {
    my ($self, $glyph, $gshape) = @_;

    my $scale = $self->{_scale};

    my $i = 0;
    for my $j (@{$glyph->{endPoints}}) {
	my @x = map {$_ * $scale} @{$glyph->{x}}[$i..$j];
	my @y = map {-$_ * $scale} @{$glyph->{y}}[$i..$j];
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
	$gshape->moveto($sx, $sy);
	while(@x) {
	    my ($x, $y, $f)=(shift(@x), shift(@y), (shift(@f) & 1));
	    
	    if ($f) {
		$gshape->lineto($x, $y);
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
		$gshape->curveto($x, $y, $ax, $ay);
	    }
	}
    }
}

sub add_glyph {
    my ($self, $string, $e_char) = @_;
    my @chars;

    return unless $self->{_embed};

    my $hash = $self->{_glyph_hash};

    if (defined $e_char) {
	@chars = map {chr} (ord($string) .. ord($e_char));
    } else {
	@chars = split //, $string;
    }

    for my $c (@chars) {
	next if $hash->{$c};

	my $gshape = $self->glyph_shape($c);
	my $adv = $self->_draw_glyph($c, $gshape);
	$hash->{$c} = [$adv, $gshape];
    }
}

sub LanguageCode {
    my ($self, $code) = @_;

    unless (defined $code) {
	my $l = $self->{_tag}->LanguageCode->value;
	return ('none', 'Latin', 'Japanese', 'Korean', 'Simplified Chinese', 'Traditional Chinese')[$l];
    } elsif ($code!~/\d+/) {
	($code) = 'none:0 Latin:1 Japanese:2 Korean:3 Simplified Chinese:4 Traditional Chinese:5'=~/\b$code.*?:(\d)/i;
    }
    $self->{_tag}->LanguageCode($code);
}

sub AUTOLOAD {
    my $self = shift; 
    our $AUTOLOAD;
    my ($sub) = $AUTOLOAD=~/::([^:]+)$/;
    return if $sub eq 'DESTROY';
    my $tag = $self->{_tag};

    if ($tag->can($sub)) {
	$tag->$sub(@_);
    } elsif ($tag->can(my $fsub="FontFlags$sub")) {
	$tag->$fsub(@_);
    } else {
	croak "Can\'t locate object method \"$sub\" via package \"".ref($self).'"';
    }
}

my $emprect = SWF::Element::RECT->new(Xmin => 0, Ymin => 0, Xmax => 0, Ymax => 0);

sub _pack {
    my ($self, $stream) = @_;

    my $tag = $self->{_tag};
    my $hash = $self->{_glyph_hash};
    my ($code_t, $adv_t, $glyph_t, $bounds_t, $kern_t) = 
	($tag->CodeTable, $tag->FontAdvanceTable, $tag->GlyphShapeTable, $tag->FontBoundsTable, $tag->FontKerningTable);
    my ($kern, $cmap) = ($self->{_ttf_tables}{_kern}, $self->{_ttf_tables}{_cmap});

    for my $c (sort keys %{$self->{_glyph_hash}}) {
	push @$code_t, ord($c);
	push @$adv_t, (defined($hash->{$c}[0]) ? $hash->{$c}[0]*20 : $hash->{$c}[1]{_bounds}->Xmax);
	push @$glyph_t, SWF::Element::SHAPE->new(ShapeRecords => $hash->{$c}[1]{_edges});
#	push @$bounds_t, $hash->{$c}[2];
	push @$bounds_t, $emprect;
    }
    @{$self->{_code_hash}}{@$code_t} = (0..$#$code_t);
#    if ($kern) {
#	for my $c (grep {exists $kern->{$cmap->{$_}}} @$code_t) {
#	    for my $c2 (grep {exists $kern->{$cmap->{$c}}{$cmap->{$_}}} @$code_t) {
#		$kern_t->{"$c-$c2"} = $kern->{$cmap->{$c}}{$cmap->{$c2}};
#	    }
#	}
#    }
    $self->{_tag}->pack($stream);
}

sub _destroy {
    my $self = shift;
    my $f = $self->{_ttf_tables}{_font};
    %{$self->{_ttf_tables}} = ();
    $f->release if $f;
    $self->SUPER::_destroy;
}

####

package SWF::Builder::Character::Font::Glyph;

@SWF::Builder::Character::Font::Glyph::ISA = ('SWF::Builder::Shape');

sub new {
    my $class = shift;

    my $self = $class->SUPER::new;
    $self->fillstyle(1)->linestyle(0);
}


1;
__END__


=head1 NAME

SWF::Builder::Character::Font - SWF font object

=head1 SYNOPSIS

  my $font = $mc->new_font('c:/windows/font/arial.ttf');
  $font->add_glyph('0123456789');

=head1 DESCRIPTION

This module creates SWF fonts from TrueType fonts.

=over 4

=item $font = $mc->new_font( $fontfile [, $fontname] )

returns a new font.
$fontfile is a font file name. It should be a TrueType font file (ttf/ttc).
Optional $fontname is a font name referred by HTMLs in dynamic texts.
The font name is taken from the TrueType file if not defined.

=item $font->embed( [$embed] )

sets/gets a flag to embed the font or not.

=item $font->is_readonly

gets a permission flag to use the font only 'preview & print'.
If the flag is set, the font cannot be used for text field.

=item $font->get_average_width

gets the average character width.

=item $font->add_glyph( $char_string [, $e_char] )

adds glyph data of the characters of the string to the font.
Usually, L<SWF::Builder::character::Text> adds required glyph
data automatically.
It is necessary to do add_glyph if the font is used for a dynamic text 
or a text field which will be changed at playing time. 
if $e_char is present, add_glyph adds glyphs of characters from 
first character of $char_string to first character of $e_char. 
For example, $font->add_glyph('a', 'z') adds glyphs of all lower case alphabet.

=item $font->LanguageCode( $code )

sets the spoken language of texts to which the font is applied.
$code can take 'none', 'Latin', 'Japanese', 'Korean', 'Simplified Chinese', and
'Traditional Chinese'. It can also take a number, 0, 1, 2, 3, 4, and 5,
or an initial, 'n', 'L', 'J', 'K', 'S'(or 'C'), and 'T', respectively.

=back

=head1 COPYRIGHT

Copyright 2003 Yasuhiro Sasama (ySas), <ysas@nmt.ne.jp>

This library is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.

=cut
