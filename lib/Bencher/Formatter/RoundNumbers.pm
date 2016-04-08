package Bencher::Formatter::RoundNumbers;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;
#use Log::Any::IfLOG '$log';

use parent qw(Bencher::Formatter);

use Scalar::Util qw(looks_like_number);

use Role::Tiny::With;
with 'Bencher::Role::ResultMunger';

sub munge_result {
    my ($self, $envres) = @_;

    my $ff = $envres->[3]{'table.fields'};

    for my $rit (@{$envres->[2]}) {
        # 'time' has been scaled by ScaleTime, while 'rate' hasn't. so we use
        # 1/'rate' here
        my $num_significant_digits =
            $rit->{errors} == 0 ? 6 :
            sprintf("%d", log( abs(1/$rit->{rate}) / $rit->{errors})/log(10));
        my $fmt = "%.${num_significant_digits}g";
        $rit->{time} = sprintf($fmt, $rit->{time});
        if (exists $rit->{rate}) {
            $rit->{rate} = sprintf($fmt, $rit->{rate});
        }
        $rit->{errors} = sprintf("%.2g", $rit->{errors});

        # XXX this formatter shouldn't be aware directly of mod_overhead_time
        if (exists $rit->{mod_overhead_time}) {
            $rit->{mod_overhead_time} = sprintf(
                $fmt, $rit->{mod_overhead_time});
        }
        # XXX this formatter shouldn't be aware directly of vs_slowest
        if (exists $rit->{vs_slowest}) {
            $rit->{vs_slowest} = sprintf(
                $fmt, $rit->{vs_slowest});
        }

        # we don't need to round *_size fields to n significant digits because
        # they are not time measurement, but we do want to round it when it has
        # been divided when converting unit to kB, MB, etc.
        for my $col (keys %$rit) {
            if ($col =~ /^(result|proc_\w+|proc)_size$/ && $rit->{$col} != int($rit->{$col})) {
                $rit->{$col} = sprintf($fmt, $rit->{$col});
            }
        }
    }
}

1;
# ABSTRACT: Round numbers (rate, time) to certain significant digits according to errors

=for Pod::Coverage .*
