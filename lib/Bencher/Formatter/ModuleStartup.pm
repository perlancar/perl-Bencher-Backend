package Bencher::Formatter::ModuleStartup;

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

    return unless $envres->[3]{'func.module_startup'};

    $self->add_field(
        $envres,
        'mod_overhead_time',
        {after=>'time', unit_of=>'time'},
        sub {
            my $rit_baseline = Bencher::_find_record_by_seq($envres->[2], 0);
            for my $rit (@{$envres->[2]}) {
                if ($rit_baseline) {
                    $rit->{mod_overhead_time} =
                        $rit->{time} - $rit_baseline->{time};
                }
            }
        }
    );

    $self->delete_fields(
        $envres,
        'rate',
    );
}

1;
# ABSTRACT: Munge module_startup results

=head1 DESCRIPTION

Here's what this formatter does:

=over

=item * Remove C<rate> field

=item * Add a field C<mod_overhead_time> after C<time>

=back
