package Bencher::Role::FieldMunger;

# DATE
# VERSION

use 5.010;
use strict;
use warnings;

use Role::Tiny;

sub add_field {
    my ($self, $envres, $name, $opts, $code) = @_;

    $code->();

    my $ff = $envres->[3]{'table.fields'};
    my $fu = $envres->[3]{'table.field_units'};

    my $pos = 0;
    for my $i (0..$#{$ff}) {
        if ($opts->{after} && $ff->[$i] eq $opts->{after}) {
            $pos = $i+1;
            last;
        }
        if ($opts->{before} && $ff->[$i] eq $opts->{before}) {
            $pos = $i;
            last;
        }
    }

    splice @$ff, $pos, 0, $name;
    if ($fu) {
        my $unit;
        if ($opts->{unit}) {
            $unit = $opts->{unit};
        } elsif ($opts->{unit_of}) {
            for my $i (0..$#{$ff}) {
                if ($ff->[$i] eq $opts->{unit_of}) {
                    $unit = $fu->[$i];
                    last;
                }
            }
        }
        splice @$fu, $pos, 0, $unit;
    }
}

sub delete_fields {
    my ($self, $envres, @names) = @_;

    for my $name (@names) {
        for my $row (@{$envres->[2]}) {
            delete $row->{$name};
        }
    }

    my $ff = $envres->[3]{'table.fields'};
    my $fu = $envres->[3]{'table.field_units'};

    for my $i (reverse 0..$#{$ff}) {
        if (grep {$ff->[$i] eq $_} @names) {
            splice @$ff, $i, 1;
            splice @$fu, $i, 1 if $fu;
        }
    }
}

1;
# ABSTRACT: Field munger role
