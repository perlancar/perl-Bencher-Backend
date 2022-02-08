package Bencher::Formatter::Sort;

use 5.010001;
use strict;
use warnings;
#use Log::Any::IfLOG '$log';

use parent qw(Bencher::Formatter);

use Role::Tiny::With;
with 'Bencher::Role::ResultMunger';

# AUTHORITY
# DATE
# DIST
# VERSION

sub munge_result {
    my ($self, $envres) = @_;

    $self->{by} //= ['-time'];

    # sort by default from slowest to fastest
    $envres->[2] = [sort {
        for my $by0 (@{ $self->{by} }) {
            my ($desc, $by) = $by0 =~ /^(-?)(.+)/;
            return 0 unless defined($a->{$by}) && defined($b->{$by});
            my $numeric = $by =~ /time|errors|samples/ ? 1:0; # XXX ad-hoc
            my $res = ($desc ? -1:1) * ($numeric ?
                                            $a->{$by} <=> $b->{$by} :
                                            $a->{$by} cmp $b->{$by});
            return $res if $res;
        }
        0;
    } @{$envres->[2]}];
}

1;
# ABSTRACT: Sort rows

=for Pod::Coverage .*

=head1 DESCRIPTION

By default, sort from slowest (largest C<time> value) to fastest (smallest
C<time> value).


=head1 OPTIONS

=head2 by => array of str (default: ["-time"])

Customize sort orders. One or more field names (field name can be prefixed with
C<-> (dash) to mean descending order).
