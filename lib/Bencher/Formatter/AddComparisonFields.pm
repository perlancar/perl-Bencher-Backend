package Bencher::Formatter::AddComparisonFields;

# AUTHORITY
# DATE
# DIST
# VERSION

use 5.010001;
use strict;
use warnings;

use parent qw(Bencher::Formatter);

use Role::Tiny::With;
with 'Bencher::Role::FieldMunger';
with 'Bencher::Role::ResultMunger';

sub munge_result {
    my ($self, $envres) = @_;

    my $slowest_time;
    my $fastest_time;
    for my $rit (@{ $envres->[2] }) {
        $slowest_time = $rit->{time}
            if !defined($slowest_time) || $slowest_time < $rit->{time};
        $fastest_time = $rit->{time}
            if !defined($fastest_time) || $fastest_time > $rit->{time};
    }

    return unless defined $slowest_time;

    $self->add_field(
        $envres,
        'pct_faster_vs_slowest',
        {after=>'time', align=>'number', format=>'percent'},
        sub {
            for my $rit (@{$envres->[2]}) {
                $rit->{pct_faster_vs_slowest} =
                    ($slowest_time - $rit->{time}) / $rit->{time};;
            }
        }
    );
    $self->add_field(
        $envres,
        'pct_slower_vs_fastest',
        {after=>'pct_faster_vs_slowest', align=>'number', format=>'percent'},
        sub {
            for my $rit (@{$envres->[2]}) {
                $rit->{pct_slower_vs_fastest} =
                    ($rit->{time} - $fastest_time) / $fastest_time;
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
