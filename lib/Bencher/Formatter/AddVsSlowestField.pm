package Bencher::Formatter::AddVsSlowestField;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;
#use Log::Any::IfLOG '$log';

use parent qw(Bencher::Formatter);

use Role::Tiny::With;
with 'Bencher::Role::FieldMunger';
with 'Bencher::Role::ResultMunger';

use Bencher;
use List::MoreUtils qw(firstidx);

sub munge_result {
    my ($self, $envres) = @_;

    my $slowest_time;
    for my $rit (@{ $envres->[2] }) {
        $slowest_time = $rit->{time}
            if !defined($slowest_time) || $slowest_time < $rit->{time};
    }

    return unless defined $slowest_time;

    $self->add_field(
        $envres,
        'vs_slowest',
        {after=>'time'},
        sub {
            for my $rit (@{$envres->[2]}) {
                $rit->{vs_slowest} = $slowest_time / $rit->{time};
            }
        }
    );
}

1;
# ABSTRACT: Add vs_slowest field

=for Pod::Coverage .*

=head1 DESCRIPTION

This formatter adds a C<vs_slowest> field (after C<time>) to give relative
numbers between the items. The slowest item will have a 1 value, the faster
items will have numbers larger than 1. For example, 2 means that the item is
twice faster than the slowest.
