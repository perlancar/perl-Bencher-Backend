package Bencher::Formatter::ScaleSize;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;
#use Log::Any::IfLOG '$log';

use parent qw(Bencher::Formatter);

use List::MoreUtils qw(minmax);
use POSIX ();

use Role::Tiny::With;
with 'Bencher::Role::ResultMunger';

sub munge_result {
    my ($self, $envres) = @_;

    return unless @{$envres->[2]};

    my @cols = grep {/^(result_size)\b/} keys %{ $envres->[2][0] };
    return unless @cols;

  COL:
    for my $col (@cols) {
        # pick an appropriate time unit & format the time
        my ($min, $max) = minmax(map {$_->{$col}} @{$envres->[2]});

        # workaround for bug RT#113117 (max sometimes undef)
        $max //= $min;

        my ($unit, $factor);
        if ($max >= 1.1*2**40) {
            $unit = "TB";
            $factor = 2**-40;
        } elsif ($max >= 1.1*2**30) {
            $unit = "GB";
            $factor = 2**-30;
        } elsif ($max >= 1.1*2**20) {
            $unit = "MB";
            $factor = 2**-20;
        } elsif ($max >= 1.1*2**10) {
            $unit = "kB";
            $factor = 2**-10;
        } else {
            $unit = "b";
            $factor = 1;
        }

        if ($unit) {
            for my $rit (@{$envres->[2]}) {
                $rit->{$col} *= $factor;
            }

            $envres->[3]{'table.field_units'} //= [];
            my $fus = $envres->[3]{'table.field_units'};

            my $i = -1;
            for my $f (@{$envres->[3]{'table.fields'}}) {
                $i++;
                if ($f eq $col) {
                    $fus->[$i] = $unit;
                    last;
                }
            }
        } # if unit
    } # for each column
}

1;
# ABSTRACT: Scale size fields to make it convenient

=for Pod::Coverage .*
