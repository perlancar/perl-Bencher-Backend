package Bencher::Formatter::Sort;

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

    # sort by default from slowest to fastest
    $envres->[2] = [sort {$b->{time} <=> $a->{time}} @{$envres->[2]}];
}

1;
# ABSTRACT: Sort rows

=for Pod::Coverage .*

=head1 DESCRIPTION

By default, sort from slowest (largest C<time> value) to fastest (smallest
C<time> value).
