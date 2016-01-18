package Bencher::Formatter::DeleteConstantFields;

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

    {
        last unless @{$envres->[2]};
        require TableData::Object::aohos;
        my $td = TableData::Object::aohos->new($envres->[2]);
        my @const_cols = $td->const_col_names;
        for my $k (@const_cols) {
            next unless $k =~ /^(item_.+|arg_.+|participant|dataset)$/;
            for my $row (@{ $envres->[2] }) {
                delete $row->{$k};
            }
        }
    }
}

1;
# ABSTRACT: Delete constant fields to reduce clutter

=head1 DESCRIPTION

Constant fields are fields that exist in every row and have a single value
across all rows.
