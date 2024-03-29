package Bencher::Formatter::RoundNumbers;

use 5.010001;
use strict;
use warnings;
#use Log::Any::IfLOG '$log';

use parent qw(Bencher::Formatter);

use Math::ScientificNotation::Util qw(sci2dec);
use Scalar::Util qw(looks_like_number);

use Role::Tiny::With;
with 'Bencher::Role::ResultMunger';

# AUTHORITY
# DATE
# DIST
# VERSION

sub munge_result {
    my ($self, $envres) = @_;

    my $ff = $envres->[3]{'table.fields'};

    my $code_fmt = sub {
        my ($num_significant_digits, $num, $scientific_notation) = @_;
        $scientific_notation //= $self->{scientific_notation};
        $num = sprintf("%.${num_significant_digits}g", $num);
        $num = sci2dec($num) unless $scientific_notation;
        $num;
    };

    for my $rit (@{$envres->[2]}) {
        my $num_significant_digits = do {
            if (!defined($rit->{errors}) || $rit->{errors} == 0) {
                6;
            } elsif (exists $rit->{time}) {
                sprintf("%d", log( abs($rit->{time}) /
                                       ($envres->[3]{'func.time_factor'} // 1) /
                                       $rit->{errors} )/log(10));
            } else {
                die "BUG: no 'time' defined?";
            }
        };
        $rit->{time} = $code_fmt->($num_significant_digits, $rit->{time});
        if (exists $rit->{rate}) {
            $rit->{rate} = $code_fmt->($num_significant_digits, $rit->{rate});
        }
        $rit->{errors} = $code_fmt->(2, $rit->{errors}, 1)
            if defined $rit->{errors};

        # XXX this formatter shouldn't be aware directly of mod_overhead_time
        if (exists $rit->{mod_overhead_time}) {
            $rit->{mod_overhead_time} = $code_fmt->(
                $num_significant_digits, $rit->{mod_overhead_time});
        }
        # XXX this formatter shouldn't be aware directly of vs_slowest
        if (exists $rit->{vs_slowest}) {
            $rit->{vs_slowest} = $code_fmt->(
                $num_significant_digits, $rit->{vs_slowest});
        }

        # we don't need to round *_size fields to n significant digits because
        # they are not time measurement, but we do want to round it when it has
        # been divided when converting unit to kB, MB, etc.
        for my $col (keys %$rit) {
            if ($col =~ /^(result|proc_\w+|proc|arg_\w+)_size$/ && $rit->{$col} != int($rit->{$col})) {
                $rit->{$col} = $code_fmt->(
                    $num_significant_digits, $rit->{$col});
            }
        }
    }
}

1;
# ABSTRACT: Round numbers (rate, time) to certain significant digits according to errors

=for Pod::Coverage .*
