package Bencher::Formatter::ShowEnv;

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

    return unless @{$envres->[2]};
    return unless exists $envres->[2][0]{env_hash};

    $self->add_field(
        $envres,
        'env',
        {after=>'env_hash'},
        sub {
            for my $rit (@{$envres->[2]}) {
                my $env_hash = $envres->[3]{'func.scenario_env_hashes'}[ $rit->{env_hash} ];
                $rit->{env} = join(" ", map {"$_=$env_hash->{$_}"} sort keys %$env_hash);
            }
        }
    );
    $self->delete_fields($envres, 'env_hash');
}

1;
# ABSTRACT: Replace 'env_hash' field (numeric) with 'env' (string)

=for Pod::Coverage .*

=head1 DESCRIPTION

The C<env_hash> field only contains the index to the C<env_hashes> array
(scenario property). This formatter replaces the field with C<env> showing the
contents of the environment hash. This makes it clearer to the viewer what
environment variables are being set for an item.
