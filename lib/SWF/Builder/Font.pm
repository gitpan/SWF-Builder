package SWF::Builder::Font;

use strict;
use utf8;

use Carp;
use SWF::Element;
use SWF::Builder;
use SWF::Builder::Shape;
use Font::TTF::Font;
use Font::TTF::Ttc;

our $VERSION="0.01";

my %indirect;

@indirect{qw/ _sans _serif _typewriter _\u{30b4}\u{30b7}\u{30c3}\u{30af} _\u{660e}\u{671d} _\u{7b49}\u{5e45} /}
        = qw/ _sans _serif _typewriter _\u{30b4}\u{30b7}\u{30c3}\u{30af} _\u{660e}\u{671d} _\u{7b49}\u{5e45} /; 

@SWF::Builder::Font::ISA = qw/ SWF::Builder::Character /;

sub new {
    my ($class, $fontfile, $fontname) = @_;
    my $tag;
    my $type = 0;
    my $ttft;
    my $self = bless {
	_no_glyph => 0,
	_code_hash => {},
	_glyph_hash => {},
	_tag => ($tag = SWF::Element::Tag::DefineFont2->new),
	_ttf_tables => ($ttft = bless {}, 'SWF::Builder::Font::TTFTables'),
    }, $class;

    $self->SWF::Builder::Character::_init;
    $tag->FontID($self->{ID});

    my $font = $indirect{$fontfile} ||
               Font::TTF::Font->open($fontfile) ||  
	       Font::TTF::Ttc->open($fontfile) 
		   or croak "Can't open font file '$fontfile'";

    unless (ref($font)) {
	$fontname ||= $font;
	$self->{_no_glyph} = 1;
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
	    if ($os2 = $font->{'OS/2'}||$p_font->{'OS/2'}) {  # get OS/2 table to check the lisence.
		$os2->read;
		my $fstype = $os2->{fsType};

		if ($fstype & 0x302) {
		    carp "Embedding outlines of the font '$fontfile' is not permitted";
		    $self->{_no_glyph} = 1;
		    last EMBED;
		} elsif ($fstype & 4) {
		    carp "The font '$fontfile' can use only for 'Preview & Print'";
		    $self->{_read_only} = 1;
		}
	    } else {
		carp "The font '$fontfile' doesn't have any lisence information. See the lisence of the font.";
	    }
	    $head = $font->{head}||$p_font->{head} # header
	        or croak 'Invalid font';
	    $name = $font->{name}||$p_font->{name} # font name
	        or croak 'Invalid font';
	    $hhea = $font->{hhea}||$p_font->{hhea} # horizontal header
	        or croak 'Invalid font';
	    $cmap = $font->{cmap}||$p_font->{cmap} # chr-glyph mapping
	        or croak 'Invalid font';
	    $loca = $font->{loca}||$p_font->{loca} # glyph location index
	        or croak 'Invalid font';
	    $hmtx = $font->{hmtx}||$p_font->{hmtx} # horizontal metrics
	        or croak 'Invalid font';
	    $kern = $font->{kern}||$p_font->{kern} # kerning table (optional)
	        and $kern->read;
	    $head->read;
	    $name->read;
	    $hhea->read;
	    $cmap->read;
	    $loca->read;
	    $hmtx->read;
	    my $scale = 51.2 / $head->{unitsPerEm};   # 1024(Twips/Em) / 20(Twips/Pixel) / S(units/Em) = Scale(Pixel/units)
	    $tag->FontAscent($hhea->{Ascender} * $scale*20);
	    $tag->FontDescent(-$hhea->{Descender} * $scale*20);
	    $tag->FontLeading(0);
	    $self->{_scale}  = $scale;
	    $ttft->{_cmap}   = $cmap->{Tables}[1]{val}; # Unicode cmap
	    $ttft->{_advance}= $hmtx->{advance};
	    $ttft->{_glyphs} = $loca->{glyphs};
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
    $tag->FontName($fontname);
    $tag->FontFlagsBold(1) if ($type & 1);
    $tag->FontFlagsItalic(1) if ($type & 2);

    $self;
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

sub add_glyph {
    my ($self, $string) = @_;

    return if $self->{_no_glyph};

    my $hash = $self->{_glyph_hash};
    my $cmap = $self->{_ttf_tables}{_cmap};
    my $glyphs = $self->{_ttf_tables}{_glyphs};
    my $advances = $self->{_ttf_tables}{_advance};
    my $scale = $self->{_scale};
    my $tag = $self->{_tag};

    for my $c (split //, $string) {
	next if $hash->{$c};

	my $code = ord($c);
	my $gid = $cmap->{$code};
	my $adv = $advances->[$gid] * $scale;
	my $gshape = SWF::Builder::Font::Glyph->new;
	my $glyph = $glyphs->[$gid];
	if (defined $glyph) {
	    $glyph->read_dat;

	    my $i = 0;
	    for my $j (@{$glyph->{endPoints}}) {
		my @x = map {int($_ * $scale + .5)} @{$glyph->{x}}[$i..$j];
		my @y = map {- int($_ * $scale + .5)} @{$glyph->{y}}[$i..$j];
		my @f = @{$glyph->{flags}}[$i..$j];
		$i=$j+1;
		my $sx = shift @x;
		my $sy = shift @y;
		my $f  = shift @f;
		unless ($f & 1) {
		    push @x, $sx;
		    push @y, $sy;
		    push @f, 0;
		    if ($f[0] & 1) {
			$sx = shift @x;
			$sy = shift @y;
			$f  = shift @f;
		    } else {
			$sx = int(($sx+$x[0])/2+.5);
			$sy = int(($sy+$y[0])/2+.5);
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
			    $ax=int(($x+$x[0])/2+.5);
			    $ay=int(($y+$y[0])/2+.5);
			}
			$gshape->curveto($x, $y, $ax, $ay);
		    }
		}
	    }
	}
	$hash->{$c} = [$adv, $gshape->{_edges}, $gshape->{_bounds}];
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

our $AUTOLOAD;
sub AUTOLOAD {
    my $self = shift;
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

sub pack {
    my ($self, $stream) = @_;

    $self->prepare_to_pack($stream) or return;

    my $tag = $self->{_tag};
    my $hash = $self->{_glyph_hash};
    my ($code_t, $adv_t, $glyph_t, $bounds_t, $kern_t) = 
	($tag->CodeTable, $tag->FontAdvanceTable, $tag->GlyphShapeTable, $tag->FontBoundsTable, $tag->FontKerningTable);
    my ($kern, $cmap) = ($self->{_ttf_tables}{_kern}, $self->{_ttf_tables}{_cmap});

    for my $c (sort keys %{$self->{_glyph_hash}}) {
	push @$code_t, ord($c);
	push @$adv_t, $hash->{$c}[0]*20;
	push @$glyph_t, SWF::Element::SHAPE->new(ShapeRecords => $hash->{$c}[1]);
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

package SWF::Builder::Font::Glyph;

@SWF::Builder::Font::Glyph::ISA = ('SWF::Builder::Shape');

sub new {
    my $class = shift;

    my $self = $class->SUPER::new;
    $self->fillstyle(1)->linestyle(0);
}


1;
__END__


=head1 NAME

SWF::Builder::Font - SWF font object

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

=item $font->add_glyph( $char_string )

adds glyph data of the characters of the string to the font.
Usually, L<SWF::Builder::Text> adds required glyph data automatically.
It is necessary to do add_glyph if the font is used for a dynamic text 
or a text field which will be changed at playing time. 

=item $font->LanguageCode( $code )

sets the spoken language of texts to which the font is applied.
$code can take 'none', 'Latin', 'Japanese', 'Korean', 'Simplified Chinese', and
'Traditional Chinese'. It can also take a number, 0, 1, 2, 3, 4, and 5,
or an initial, 'n', 'J', 'K', 'S'(or 'C'), and 'T', respectively.

=back

=head1 COPYRIGHT

Copyright 2003 Yasuhiro Sasama (ySas), <ysas@nmt.ne.jp>

This library is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.

=cut
