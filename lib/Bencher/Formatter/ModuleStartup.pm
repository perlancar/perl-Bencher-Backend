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

use Bencher::Backend;
use List::Util qw(first);

sub munge_result {
    my ($self, $envres) = @_;

    return unless $envres->[3]{'func.module_startup'};
    return unless @{$envres->[2]};

    $self->add_field(
        $envres,
        'mod_overhead_time',
        {after=>'time', unit_of=>'time', align=>'number'},
        sub {
            for my $rit (@{$envres->[2]}) {
                my $rit_baseline = first {
                    ($_->{participant} // '') eq 'perl -e1 (baseline)' &&
                        ($_->{perl} // '') eq ($rit->{perl} // '')
                    } @{ $envres->[2] };
                next unless $rit_baseline;

                $rit->{mod_overhead_time} =
                    $rit->{time} - $rit_baseline->{time};
            }
        },
    );

    $self->delete_fields(
        $envres,
        'dataset',
        'rate',
    );
}

1;
# ABSTRACT: Munge module_startup results

=for Pod::Coverage .*

=head1 DESCRIPTION

Here's what this formatter does:

=over

=item * Remove C<rate> field

=item * Add a field C<mod_overhead_time> after C<time>

=back
