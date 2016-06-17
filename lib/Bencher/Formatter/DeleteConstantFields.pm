package Bencher::Formatter::DeleteConstantFields;

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

sub munge_result {
    my ($self, $envres) = @_;

    {
        last unless @{$envres->[2]};
        require TableData::Object::aohos;
        my $td = TableData::Object::aohos->new($envres->[2]);
        last unless $td->row_count >= 2;
        my @const_cols = $td->const_col_names;
        for my $k (@const_cols) {
            next unless $k =~ /^(item_.+|arg_.+|perl|modver|participant|dataset)$/;
            $self->delete_fields($envres, $k);
        }
    }
}

1;
# ABSTRACT: Delete constant item permutation fields to reduce clutter

=for Pod::Coverage .*

=head1 DESCRIPTION

Constant fields are fields that exist in every row and have a single value
across all rows.

Only item permutation fields that are constant are removed. Result fields are
not removed even though they are constant.
