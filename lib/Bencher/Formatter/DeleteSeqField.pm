package Bencher::Formatter::DeleteSeqField;

use 5.010001;
use strict;
use warnings;
#use Log::Any::IfLOG '$log';

use parent qw(Bencher::Formatter);

use Role::Tiny::With;
with 'Bencher::Role::FieldMunger';
with 'Bencher::Role::ResultMunger';

# AUTHORITY
# DATE
# DIST
# VERSION

sub munge_result {
    my ($self, $envres) = @_;

    $self->delete_fields(
        $envres,
        'seq'
    );
}

1;
# ABSTRACT: Delete seq field

=for Pod::Coverage .*
