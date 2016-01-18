package Bencher::Formatter::RoundNumbers;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;
#use Log::Any::IfLOG '$log';

use parent qw(Bencher::Formatter);

#with 'Bencher::Role::ResultMunger';

sub munge_result {
    my ($self, $envres) = @_;

    my $ff = $envres->[3]{'table.fields'};

    for my $rit (@{$envres->[2]}) {
        my $num_significant_digits =
            $rit->{errors} == 0 ? 6 :
                POSIX::round(log($rit->{time} / $rit->{errors})/log(10));
        my $fmt = "%.${num_significant_digits}g";
        $rit->{time} = sprintf($fmt, $rit->{time});
        if (exists $rit->{rate}) {
            $rit->{rate} = sprintf($fmt, $rit->{rate});
        }
        $rit->{errors} = sprintf("%.2g", $rit->{errors});
        if (exists $rit->{mod_overhead_time}) {
            $rit->{mod_overhead_time} = sprintf(
                $fmt, $rit->{mod_overhead_time});
        }
    }
}

1;
# ABSTRACT: Round numbers (rate, time) to certain significant digits according to errors
