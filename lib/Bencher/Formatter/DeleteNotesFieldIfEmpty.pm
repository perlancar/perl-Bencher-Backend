package Bencher::Formatter::DeleteNotesFieldIfEmpty;

# AUTHORITY
# DATE
# DIST
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

    for my $row (@{$envres->[2]}) {
        return if $row->{notes};
    }

    $self->delete_fields(
        $envres,
        'notes'
    );
}

1;
# ABSTRACT: Delete notes field if there are no notes

=for Pod::Coverage .*
