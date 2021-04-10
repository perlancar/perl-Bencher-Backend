package Bencher::Formatter::ScaleRate;

# AUTHORITY
# DATE
# DIST
# VERSION

use 5.010001;
use strict;
use warnings;
#use Log::Any::IfLOG '$log';

use parent qw(Bencher::Formatter);

use Role::Tiny::With;
with 'Bencher::Role::ResultMunger';

sub munge_result {
    my ($self, $envres) = @_;

    $envres->[3]{'table.field_units'} //= [];
    my $fus = $envres->[3]{'table.field_units'};

    my $i = -1;
    for my $f (@{$envres->[3]{'table.fields'}}) {
        $i++;
        if ($f =~ /\A(rate)\z/) {
            $fus->[$i] = "/s";
        }
    }
}

1;
# ABSTRACT: Scale rate to make it convenient

=for Pod::Coverage .*

=head1 DESCRIPTION
