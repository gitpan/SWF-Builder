package SWF::Builder::ExElement;

use strict;
use Carp;
use SWF::Element;

our $VERSION="0.01";


package SWF::Builder::ExElement::Color;

 SWF::Element::_create_class('_Color', ['RGB','RGBA'],
			     Red   => '$UI8',
			     Green => '$UI8',
			     Blue  => '$UI8',
			     Alpha => '$UI8',
			     is_alpha => 'Scalar');

@SWF::Builder::ExElement::Color::ISA = ('SWF::Element::_Color');
eval{SWF::Element::RGB->new->pack};
eval{SWF::Element::RGBA->new->pack};

use overload
    '""' => sub {
	my $self = shift;
	sprintf('%2.2X%2.2X%2.2X%2.2X', $self->Red, $self->Green, $self->Blue, $self->Alpha);
    },
    fallback =>1;

sub element_names {
    SWF::Element::_Color->element_names;
}

sub element_type {
    SWF::Element::_Color->element_type($_[1]);
}

sub pack {
    my ($self, $stream) = @_;

    if ($self->is_alpha) {
	$self->SWF::Element::RGBA::pack($stream);
    } else {
	$self->SWF::Element::RGB::pack($stream);
    }
}

#####

package SWF::Builder::ExElement::Color::AddColor;

sub _add_color {
    my $self = shift;
    my @param = @_;
    my %param;
    my @color = qw/Red Green Blue Alpha/;

    {
	if (@param == 1) {
	    if ($param[0] =~ /^\#?([0-9a-f][0-9a-f]){3,4}$/i) {
		@param{qw/Red Green Blue Alpha/} = map {oct('0x'.$_)} ($param[0]=~/\#?(..)/g);
	    } elsif (ref($param[0]) eq 'ARRAY') {
		@param = @{$param[0]};
		redo;
	    } elsif (UNIVERSAL::isa($param[0],'SWF::Element::RGB')) {
		%param = $param[0]->configure;
		delete $param{_is_alpha};
	    }
	} elsif ($param[0] =~ /^\d+$/) {
	    @param{qw/Red Green Blue Alpha/} = @param;
	} else {
	    my %param1 = @param;
	    @param{qw/Red Green Blue Alpha/} = @param1{qw/Red Green Blue Alpha/};
	}
    }

    unless (defined $param{Alpha}) {
	$param{Alpha} = 255;
    } elsif ($param{Alpha} < 255) {
	$self->{_is_alpha}->configure(1);
    }
    return SWF::Builder::ExElement::Color->new(%param, is_alpha => $self->{_is_alpha});
}

sub _init {
    shift->{_is_alpha} = SWF::Element::Scalar->new(0);
}

####

package SWF::Builder::ExElement::BoundaryRect;
@SWF::Builder::ExElement::BoundaryRect::ISA = ('SWF::Element::RECT');

sub new {
    my $class =shift;
    bless [@_], $class;
}

sub Xmin {
    my ($self, $v) = @_;
    if (defined $v) {
	$self->[0] = $v;
    }
    $self->[0];
}

sub Ymin {
    my ($self, $v) = @_;
    if (defined $v) {
	$self->[1] = $v;
    }
    $self->[1];
}

sub Xmax {
    my ($self, $v) = @_;
    if (defined $v) {
	$self->[2] = $v;
    }
    $self->[2];
}

sub Ymax {
    my ($self, $v) = @_;
    if (defined $v) {
	$self->[3] = $v;
    }
    $self->[3];
}

sub pack {
    my ($self, $stream) = @_;
    my %rect;
    @rect{qw/ Xmin Ymin Xmax Ymax /} = @$self;
  SWF::Element::RECT->new
      ( Xmin => $self->[0]*20,
	Ymin => $self->[1]*20,
	Xmax => $self->[2]*20,
	Ymax => $self->[3]*20,
	)->pack($stream);
}

sub set_boundary {
    my ($self, $x1, $y1, $x2, $y2)=@_;

    ($x1, $x2) = ($x2, $x1) if $x1 > $x2;
    ($y1, $y2) = ($y2, $y1) if $y1 > $y2;

    unless (defined($self->Xmin)) {
	$self->[0] = $x1;
	$self->[2] = $x2;
	$self->[1] = $y1;
	$self->[3] = $y2;
    } else {
	if ($self->[0]>$x1) {
	    $self->[0] = $x1;
	}
	if ($self->[2]<$x2) {
	    $self->[2] = $x2;
	}
	if ($self->[1]>$y1) {
	    $self->[1] = $y1;
	}
	if ($self->[3]<$y2) {
	    $self->[3] = $y2;
	}
    }
}



1;

