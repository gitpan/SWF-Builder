package SWF::Builder;

use strict;

use SWF::File;
use SWF::Element;
use SWF::Builder::ExElement;
use Carp;

our $VERSION = '0.01';
my $SFTAG = SWF::Element::Tag::ShowFrame->new;

@SWF::Builder::ISA = ('SWF::Builder::ExElement::Color::AddColor');

sub new {
    my $class = shift;
    my %param = @_;
    my $self = bless {
	_file =>
	  SWF::File->new(undef,
			 Version => 6,
			 FrameRate => $param{FrameRate},
			 FrameSize => [ map {$_*20} @{$param{FrameSize}}],
			 ),
	_backgroundcolor => $param{BackgroundColor},
	_root =>
	  SWF::Builder::Movie::Root->new,
    }, $class;
    $self->SWF::Builder::ExElement::Color::AddColor::_init;
    $self;

}

sub FrameRate {
    my $self = shift;
    $self->{_file}->FrameRate(@_);
}

sub FrameSize {
    my $self = shift;
    $self->{_file}->FrameSize(map {$_*20} @_);
}

sub BackgroundColor {
    my ($self, $color) = @_;
    $self->{_backgroundcolor} = $color if defined $color;
    $self->{_backgroundcolor};
}

sub save {
    my ($self, $file) = @_;
    my $stream = $self->{_file};

    $self->{_is_alpha}->configure(0);
  SWF::Element::Tag::SetBackgroundColor->new(BackgroundColor => $self->_add_color($self->{_backgroundcolor}))->pack($stream);
    $self->{_root}->pack($stream);
    $stream->close($file);
}

sub AUTOLOAD {
    my $self = shift;
    my $sub = $SWF::Builder::AUTOLOAD;

    return if $sub =~/::DESTROY$/;
    $sub =~ s/.+:://;
    croak "Can't locate object method \"$sub\" via package \"".ref($self).'" (perhaps you forgot to load "'.ref($self).'"?)' unless $self->{_root}->can($sub);
    $self->{_root}->$sub(@_);
}


sub DESTROY {
    shift->{_root}->_destroy;
}

####

package SWF::Builder::Depth;
use Carp;

sub new {
    my ($class, $parent, $lower) = @_;
    my $self = bless {
	_parent => $parent,
	_depth => SWF::Element::Depth->new,
    }, $class;
    $lower ||= $self;
    $self->{_lower} = $lower;
    $self->{_upper} = $lower->{_upper}||$lower;
    $lower->{_upper} = $lower->{_upper}{_lower} = $self;
    return $self;
}

sub _destroy {
    my $self = shift;
    while(my $lower = $self->{_lower}) {
	%$self = ();
	$self = $lower;
    }
}

####

package SWF::Builder::Character;
use Carp;

sub _init {
    my $self = shift;

    $self->{ID} = SWF::Element::ID->new;
    $self->{_depends} = {};
    $self->{_parent} = undef;
    $self->{_root} = undef;
}

sub prepare_to_pack {
    my ($self, $stream) = @_;

    return if $self->{ID}->defined;
    for my $dep (values %{$self->{_depends}}) {
	$dep->pack($stream) unless $dep->{ID}->defined;
    }

    if ($self->{_root}) {
	$self->{ID}->configure($self->{_root}->get_ID);
    } else {
	croak "Character ID need to be initialized to pack" unless $self->{ID}->defined;
    }
    1;
}

sub _depends {
    my ($self, $char) = @_;

    $self->{_depends}{$char} = $char;
}

sub _destroy {
    %{+shift} = ();
}

####

package SWF::Builder::Character::Displayable;
@SWF::Builder::Character::Displayable::ISA = qw/SWF::Builder::Character/;

use Carp;

sub _search_sibling {
    my ($parent, $ref) = @_;
    my $p;

    while(exists $ref->{_parent}) {
	$p = $ref->{_parent};
	return $ref if $p eq $parent;
	$ref = $p;
    }
    return undef;
}

sub place {
    my ($self, %param) = @_;

    while(my $k = shift) {
	$param{lc $k} = shift;
    }

    my $parent = $param{MovieClip} || $param{MC} || $self->{_parent} or croak "Can't get the movieclip to place";
    if ($parent eq '_root') {
	$parent = $self->{_root};
    } elsif (ref($parent) eq 'SWF::Builder') {
	$parent = $parent->{_root};
    }
    croak "The item can be placed only on the movie which defines it" if $parent->{_root} != $self->{_root};


    my $depth;

    if (exists $param{below}) {
	my $refitem = _search_sibling($parent, $param{below}) or croak "Can't place the item below what on the different movieclip";
	$depth = SWF::Builder::Depth->new($parent, $refitem->{_depth}{_lower});
    } elsif (exists $param{above}) {
	my $refitem = _search_sibling($parent, $param{below}) or croak "Can't place the item above what on the different movieclip";
	$depth = SWF::Builder::Depth->new($parent, $refitem->{_depth});
    } else {
	$depth = SWF::Builder::Depth->new($parent, $parent->{_depth_list}{_lower});
    }

    my $frame = $param{Frame} || 1;

    $parent->_depends($self, $frame);

    my $disp_i = 
	bless {
	    _parent        => $parent,
	    _root          => $self->{_root},
	    _first_frame   => $frame,
	    _last_frame_offset
		           => 2**64,
	    _current_frame => $frame,
	    _depth         => $depth,
	    _obj           => $self,
	    _tags          => [],
	}, 'SWF::Builder::DisplayInstance';

    push @{$self->{_root}{_to_destroy}}, $disp_i;
    $disp_i->frame($frame);
    $disp_i->{_current_frame} = $frame;
    $disp_i;
}

####

package SWF::Builder::DisplayInstance;

use Carp;

sub frame {
    my ($self, $frame) = @_;
    my $frametag;

    my $frame_offset = $frame - $self->{_first_frame};

    unless (defined($self->{_tags}[$frame_offset])) {
	croak "The frame $frame is out of range" if $frame_offset < 0 or $frame_offset >= $self->{_last_frame_offset};
	$frametag = bless {
	    _parent => $self,
	    _frame_offset => $frame_offset,
	    _tag => 
	      SWF::Element::Tag::PlaceObject2->new
		  ( Depth => $self->{_depth}{_depth} ),
	      }, 'SWF::Builder::DisplayInstance::Frame';
	$self->{_tags}[$frame_offset] = $frametag;
	push @{$self->{_parent}{_frame_list}[$frame-1]}, $frametag;	
	if ($frame_offset == 0) {
	    $frametag->{_tag}->CharacterID($self->{_obj}{ID});
	} else {
	    $frametag->{_tag}->PlaceFlagMove(1);
	}
    } else {
	$frametag = $self->{_tags}[$frame_offset];
    }
    $self->{_current_frame} = $frame+1;
    $frametag;
}

sub name {
    my ($self, $name) = @_;
    my $tag = $self->{_tags}[0]{_tag};
    $tag->Name($name) if @_>1;
    $tag->Name;
}

sub AUTOLOAD {
    my $self = shift;
    my ($name, $class);
    my $sub = $SWF::Builder::DisplayInstance::AUTOLOAD;

    return if $sub =~/::DESTROY$/;
    $sub =~ s/.+:://;
    croak "Can't locate object method \"$sub\" via package \"".ref($self).'" (perhaps you forgot to load "'.ref($self).'"?)' unless SWF::Builder::DisplayInstance::Frame->can($sub);
    $self->frame($self->{_current_frame})->$sub(@_);
}

sub _destroy {
    %{+shift} = ();
}
####

package SWF::Builder::DisplayInstance::Frame;

use Carp;

sub scale {
    my $self = shift;

    $self->matrix->scale(@_);
    $self;
}

sub moveto {
    my $self = shift;
    $self->matrix->moveto($_[0]*20, $_[1]*20);
    $self;
}

sub r_moveto {
    my ($self, $to_rx, $to_ry) = @_;

    my $m = $self->matrix;
    $m->moveto($m->TranslateX + $to_rx*20, $m->TranslateY + $to_ry*20);
    $self;
}

sub rotate {
    my ($self, $r) = @_;

    $self->matrix->rotate($r);
    $self;
}

sub reset {
    my $self = shift;
    my $m = $self->matrix;
    $m->ScaleX(1);
    $m->ScaleY(1);
    $m->RotateSkew0(0);
    $m->RotateSkew1(0);
    $self;
}

sub remove {
    my $self = shift;
    my $parent = $self->{_parent};

    croak "This DisplayInstance has already set to remove " if ($parent->{_last_frame_offset} < 2**64);

    $self->{_tag} = SWF::Element::Tag::RemoveObject2->new( Depth => $parent->{_depth}{_depth} );
    $parent->{_last_frame_offset} = $self->{_frame_offset};
    $self;
}

sub frame_action {
    my $self = shift;

    $self->{_parent}{_parent}->frame_action($self->{_parent}{_first_frame}+$self->{_frame_offset});
}

sub frame_label {
    my $self = shift;

    $self->{_parent}{_parent}->frame_label($self->{_parent}{_first_frame}+$self->{_frame_offset}, @_);
}

sub matrix {
    my $self = shift;
    my $tag = $self->{_tag};

    unless ($tag->Matrix->defined) {
	my $ptags = $self->{_parent}{_tags};
	my $frame_offset = $self->{_frame_offset};
	$frame_offset-- until ($frame_offset == 0 or defined $ptags->[$frame_offset] and $ptags->[$frame_offset]{_tag}->Matrix->defined);
	$tag->Matrix($ptags->[$frame_offset]{_tag}->Matrix->clone);
    }
    $tag->Matrix;
}

sub pack {
    my ($self, $stream) = @_;

    $self->{_tag}->pack($stream);
}

####

package SWF::Builder::_FrameList;

@SWF::Builder::_FrameList::ISA = qw/ SWF::Element::Array::TAGARRAY /;

sub pack {
    my ($self, $stream) = @_;
    
    for my $frame (@$self) {
	for my $tag (@$frame) {
	    $tag->pack($stream);
	}
	$SFTAG->pack($stream);
    }
  SWF::Element::Tag::End->new->pack($stream);
}

####

package SWF::Builder::Movie;
use Carp;

sub new {
    my $class = shift;

    my $self = bless {
	_frame_list => SWF::Builder::_FrameList->new,
    }, $class;
    $self->{_depth_list} = SWF::Builder::Depth->new($self, $self->{_depth_list});

    $self;
}

sub new_shape {
    require SWF::Builder::Shape;

    shift->_new_character(SWF::Builder::Shape::DefineShape->new);
}

sub new_static_text {
    require SWF::Builder::Text;

    shift->_new_character(SWF::Builder::Text->new(@_));
}

sub new_font {
    require SWF::Builder::Font;

    shift->_new_character(SWF::Builder::Font->new(@_));
}

sub new_movie_clip {
    require SWF::Builder::MovieClip;

    shift->_new_character(SWF::Builder::MovieClip->new);
}

*new_mc = \&new_movie_clip;

sub new_gradient {
    require SWF::Builder::Gradient;

    SWF::Builder::Gradient->new;
}

sub new_jpeg {
    require SWF::Builder::Bitmap;

    my $self = shift;

    unshift @_, 'JPEGFile' if @_==1;
    $self->_new_character(SWF::Builder::Bitmap::JPEG->new(@_));
}

sub new_bitmap {
    require SWF::Builder::Bitmap;

    my $self = shift;

    $self->_new_character(SWF::Builder::Bitmap::Lossless->new(@_));
}


sub _new_character {
    my ($parent, $self) = @_;

    push @{$parent->{_root}{_character_IDs}}, $self->{ID};
    push @{$parent->{_root}{_to_destroy}}, $self;
    $self->{_parent} = $parent;
    $self->{_root}   = $parent->{_root};

    return $self;
}

sub frame_action {
    require SWF::Builder::ActionScript;

    my ($self, $frame) = @_;

    my $tag = SWF::Element::Tag::DoAction->new;
    push @{$self->{_frame_list}[$frame-1]}, $tag;
    $tag->Actions(SWF::Builder::ActionScript->new);
}

sub frame_label {
    my ($self, $frame, $label, $anchor) = @_;

    push @{$self->{_frame_list}[$frame-1]}, SWF::Element::Tag::FrameLabel->new(Name => $label, NamedAnchorFlag => $anchor);
}


sub set_depth {
    my $self= shift;
    my $n = 1;
    my $depth = $self->{_depth_list}{_upper};
    while ($depth != $self->{_depth_list}) {
	$depth->{_depth}->configure($n++);
	$depth = $depth->{_upper};
    }
}

sub _destroy {
    my $self = shift;

    $self->{_depth_list}->_destroy;
    %$self = ();
}

####

package SWF::Builder::Movie::Root;
use Carp;

use base qw/ SWF::Builder::Movie /;

sub new {
    my $class = shift;

    my $self = $class->SUPER::new;
    $self->{_root} = $self;
    $self->{_character_IDs} = [];
    $self->{_ID_seed} = 0;
    $self->{_target_path} = '_root';
    $self->{_to_destroy} = [];
    $self;
}

sub get_ID {
    shift->{_ID_seed}++;
}

sub pack {
    my ($self, $stream) = @_;

    $self->{_ID_seed} = 0;
    for my $id (@{$self->{_character_IDs}}) {
	$id->configure(undef);
    }

    $self->set_depth;

    $self->{_frame_list}->pack($stream);

}

sub _depends {
    my ($self, $char, $frame) = @_;

    push @{$self->{_frame_list}[$frame-1]}, $char;
}


sub _destroy {
    my $self = shift;
    undef $self->{_root};
    for (@{$self->{_to_destroy}}) {
	$_->_destroy;
    }
    $self->SUPER::_destroy;
}

# Autoload methods go after =cut, and are processed by the autosplit program.

1;
__END__
# Below is the stub of documentation for your module. You better edit it!

=head1 NAME

SWF::Builder - Create SWF movie.

=head1 SYNOPSIS

  use SWF::Builder;

  my $movie = SWF::Builder->new
    ( FrameRate => 15,
      FrameSize => [0, 0, 400, 400],
      BackgroundColor => 'ffffff'
      );

  my $shape = $movie->new_shape   # red triangle.
    ->fillstyle('ff0000')
    ->linestyle(1, '000000')
    ->moveto(0,-11)
    ->lineto(10,6)
    ->lineto(-10,6)
    ->lineto(0,-11);

  my $instance = $shape->place;

  for (my $x = 0; $x < 400; $x++) {
      $instance->rotate(15)->moveto($x,200);
  }
  $movie->save('triangle.swf');

=head1 DESCRIPTION

I<SWF::Builder> is a wrapper of I<SWF::File>. 
It provides an easy way to create SWF6 movie.

The SWF movie consists a dictionary of character definitions and 
a hierarchical group of movie clips.  You create a movie by following steps:

=over 4

=item 1.

create a '_root' movie by SWF::Builder->new.

=item 2.

define characters such as shapes, fonts, texts, movieclips, and so on, 
by $movie->new_XXX methods.

=item 3.

get an instance of the character by $character->place.

=item 4.

move, scale, and rotate the instance every frame.

=item 5.

repeat 2-4 if you need.

=item 6.

save the whole movie by $movie->save.

=back

=head2 '_root' movie

The '_root' movie is a top of the movie clip hierarchy.
It has properties of the whole SWF movie. It also has character constructors
and other methods for movie. See the next section for details.

=over 4

=item $movie = SWF::Builder->new( [FrameRate => $rate, FrameSize => [$xmin, $ymin, $xmax, $ymax], BackgroundColor => $color] )

creates a new '_root' movie. It can take three optional named parameters.
FrameRate is a frame count per second. FrameSize is a box size of frames,
which is an array reference of the coordinates of top-left and bottom-right
of the box in pixels.
BackgroundColor is a background color of the movie. It can take a six-figure
hexadecimal string, an array reference of R, G, and B value, an array reference
of named parameters such as [Red => 255], and SWF::Element::RGB object.

=item $movie->FrameRate( $rate )

=item $movie->FrameSize( $xmin, $ymin, $xmax, $ymax )

=item $movie->BackgroundColor( $color )

sets the property. See SWF::Builder->new.

=item $movie->save( $filename )

saves the movie.

=back

=head2 Character constructors

All characters must be defined before it uses.

=over 4

=item $mc->new_shape

returns a new shape.
See L<SWF::Builder::shape> for the detail.

=item $mc->new_font( $fontfile [, $fontname] )

returns a new font.
$fontfile is a font file name. It should be a TrueType font file (ttf/ttc).
Optional $fontname is a font name referred by HTMLs in dynamic texts.
It is taken from the TrueType file if not defined.
See L<SWF::Builder::Font> for the detail.

=item $mc->new_static_text( [$font, $text] )

returns a new static text, which is fixed by authoring and cannot
be changed at playing time.
See L<SWF::Builder::Text> for the detail of a text.

=item $mc->new_movie_clip

=item $mc->new_mc

returns a new movie clip. 
See L<SWF::Builder::MovieClip> for the detail.

=item $mc->new_gradient

returns a new gradient object.
See L<SWF::Builder::Gradient> and L<SWF::Builder::Shape> for the detail.

=item $mc->new_jpeg( ... )

returns a new JPEG bitmap. See L<SWF::Builder::Bitmap> for the detail.

=item $mc->new_bitmap( $type => $obj )

returns a new lossless bitmap. See L<SWF::Builder::Bitmap> for the detail.

=back

=head2 Other methods for movies

Here describe other common methods for root movie and movie clips.

=over 4

=item $mc->frame_action( $frame )

returns SWF::Builder::ActionScript object for a frame action.

=item $mc->frame_label( $frame, $label [, $anchorflag] )

gives $label to $frame to which ActionScripts can refer.
If the $anchorflag is set to 1, it is accessible as an
HTML anchor.

=back

=head2 Display instance

It is necessary to get the instance to use the defined character.
Each instance has its own timeline tied to the parent movie clip
and the current frame to move, to rotate, etc.

=over 4

=item $disp_i = $char->place( [ MovieClip => $mc, Frame => $frame, above => $another_i, below => $another_i ] )

returns the display instance of the character.
It can take four optional named parameters.
MovieClip(MC) is a parent movie clip on which the character is placed.
The movie clip must be under the same root with the movie clip in which 
the character is defined. If MC is not set, the character is placed on
the movie clip in which it is defined. 
Frame is a first frame number on which the character is placed. Default is 1.
You can set the relative depth of the new instance by 'above' and 'below'.

=item $disp_i->name( $name )

gives a name to the display instance to which ActionScripts can refer.

=item $fobj = $disp_i->frame( $frame )

gets the specified frame object of the display instance and sets the current
frame of the display instance to $frame.

Moving, rotating, scaling, and any other matrix transforming of 
the display instance are handled in a frame by frame via a frame object.
When a frame object is not specified, the 'current frame object' kept by the
display item is used.  The current frame is counted up after it is used.

=item $fobj/$disp_i->moveto( $x, $y )

moves the display item to ($x, $y) at the (current) frame.

=item $fobj/$disp_i->r_moveto( $dx, $dy )

moves the display item relatively ( to (former X + $x, former Y + $y)).

=item $fobj/$disp_i->scale( $xscale [, $yscale] )

magnifies/reduces the display item at the (current) frame.
The scaling effect is accumulative.

=item $fobj/$disp_i->rotate( $angle )

rotates the display item at the (current) frame.
The rotation angle is accumulative.

=item $fobj/$disp_i->reset

resets the rotation and scaling at the (current) frame.

=item $fobj/$disp_i->remove

removes the display instance from the parent movie clip at the (current) frame.

=item $fobj/$disp_i->matrix

gets the transformation matrix of the display instance at the (current) frame.
The result is an SWF::Element::MATRIX object.

=item $fobj/$disp_i->frame_action

=item $fobj/$disp_i->frame_label( $label [, $anchorflag] )

ssme as those for movie clips, setting the frame number to that of the frame object.

=back

=head1 COPYRIGHT

Copyright 2003 Yasuhiro Sasama (ySas), <ysas@nmt.ne.jp>

This library is free software; you can redistribute it
and/or modify it under the same terms as Perl itself.

=cut
