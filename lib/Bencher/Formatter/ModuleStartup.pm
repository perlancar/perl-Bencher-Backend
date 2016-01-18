package Bencher::Formatter::ModuleStartup;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;
#use Log::Any::IfLOG '$log';

use parent qw(Bencher::Formatter);

use Bencher;
use List::MoreUtils qw(firstidx);

#with 'Bencher::Role::ResultMunger';

sub munge_result {
    my ($self, $envres) = @_;

    return unless $envres->[3]{'func.module_startup'};

    my $ff = $envres->[3]{'table.fields'};

    my $rit_baseline = Bencher::_find_record_by_seq($envres->[2], 0);
    for my $rit (@{$envres->[2]}) {
        delete $rit->{rate};
        if ($rit_baseline) {
            $rit->{mod_overhead_time} =
                $rit->{time} - $rit_baseline->{time};
        }
    }
    splice @$ff, (firstidx {$_ eq 'rate'} @$ff), 1;
    splice @$ff, (firstidx {$_ eq 'time'} @$ff)+1, 0, "mod_overhead_time";
}

1;
# ABSTRACT: Munge module_startup results

=head1 DESCRIPTION

Here's what this formatter does:

=over

=item * Remove C<rate> field

=item * Add a field C<mod_overhead_time> after C<time>

=back
