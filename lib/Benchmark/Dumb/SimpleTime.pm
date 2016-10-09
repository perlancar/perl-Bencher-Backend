package Benchmark::Dumb::SimpleTime;

# DATE
# VERSION

use strict;
use warnings;
use Time::HiRes qw(time);

sub _timethese_guts {
    my ($count, $subs, $silent) = @_;

    my $res = {};
    for my $name (keys %$subs) {
        my $sub = $subs->{$name};

        my $time_start = time();
        $sub->() for 1..$count;
        my $time_end   = time();

        $res->{$name} = bless({
            name => $name,
            result => bless({
                num => ($time_end - $time_start)/$count,
                _dbr_nsamples => $count,
                errors => [undef],
            }, "Dumbbench::result"),
        }, "Benchmark::Dumb");
    }

    # we are always silent for now
    $res;
}

1;
# ABSTRACT: Benchmark::Dumb interface for simple time() based benchmarking

=head1 DESCRIPTION

Used internally by L<Bencher::Backend>.

This benchmarks code using simple C<time()> to measure time interval. No
outliers removal or any statistics methods are applied. Returns result similar
to what L<Benchmark::Dumb>'s C<_timethese_guts()> returns, with C<errors> set to
C<[undef]>. Might be usable if you don't care about any of the stuffs that
L<Dumbbench> cares about, and you want to benchmark code that runs at least one
or a few seconds with few iterations (1 to 5) , where Benchmark::Dumb will
complain that the "number of runs is very small".
