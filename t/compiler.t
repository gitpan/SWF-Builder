use Test;
use strict;

BEGIN { plan tests => 7 }

use SWF::Builder::ActionScript::Compiler;
use SWF::BinStream;
use SWF::Element;

ok(1);

my $INFINITY = 1e+309;
ok($INFINITY=~/INF/);

my $c;
my $actions;

$c = SWF::Builder::ActionScript::Compiler->new('this.test(1)');
actionchk($c->compile);
ok($c->{stat}{code}[0], "Push Number '1' Number '1' String 'this'");
ok($c->{stat}{code}[-2], "CallMethod");

$c = SWF::Builder::ActionScript::Compiler->new('a=1/0');
actionchk($c->compile);
ok($c->{stat}{code}[0], "Push String 'a' Number 'Infinity'");

sub actionchk {
    my $action1 = shift;
    my $w_s = SWF::BinStream::Write->new;
    $action1->pack($w_s);
    my $r_s = SWF::BinStream::Read->new($w_s->flush_stream);
    my $action2 = SWF::Element::Array::ACTIONRECORDARRAY->new;
    $action2->unpack($r_s);

    my ($a1dump, $a2dump);

    $action1->dumper(sub{$a1dump.=shift});
    $action2->dumper(sub{$a2dump.=shift});

    ok($a1dump, $a2dump);
}
