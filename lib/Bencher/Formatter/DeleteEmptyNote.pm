package Bencher::Formatter::DeleteEmptyNote;

# DATE
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

    for my $row (@{$envres->[2]}) {
        return if $row->{notes};
    }

    for my $row (@{$envres->[2]}) {
        delete $row->{notes};
    }
}

1;
# ABSTRACT: Delete note field if there are no notes
