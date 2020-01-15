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

    my $fs = $envres->[3]{'table.fields'};
    my $fu = $envres->[3]{'table.field_units'};
    my $fa = $envres->[3]{'table.field_aligns'};
    my $ff = $envres->[3]{'table.field_formats'};

    my $pos = 0;
    for my $i (0..$#{$fs}) {
        if ($opts->{after} && $fs->[$i] eq $opts->{after}) {
            $pos = $i+1;
            last;
        }
        if ($opts->{before} && $fs->[$i] eq $opts->{before}) {
            $pos = $i;
            last;
        }
    }

    splice @$fs, $pos, 0, $name;
    if ($fu) {
        my $unit;
        if ($opts->{unit}) {
            $unit = $opts->{unit};
        } elsif ($opts->{unit_of}) {
            for my $i (0..$#{$ff}) {
                if (($ff->[$i] // '') eq $opts->{unit_of}) {
                    $unit = $fu->[$i];
                    last;
                }
            }
        }
        splice @$fu, $pos, 0, $unit;
    }
    if ($fa) {
        my $align;
        if ($opts->{align}) {
            $align = $opts->{align};
        }
        splice @$fa, $pos, 0, $align;
    }
    if ($ff) {
        my $format;
        if ($opts->{format}) {
            $format = $opts->{format};
        }
        splice @$ff, $pos, 0, $format;
    }
}

sub delete_fields {
    my ($self, $envres, @names) = @_;

    for my $name (@names) {
        for my $row (@{$envres->[2]}) {
            delete $row->{$name};
        }
    }

    my $fs = $envres->[3]{'table.fields'};
    my $fu = $envres->[3]{'table.field_units'};
    my $fa = $envres->[3]{'table.field_aligns'};
    my $ff = $envres->[3]{'table.field_formats'};

    for my $i (reverse 0..$#{$fs}) {
        if (grep {$fs->[$i] eq $_} @names) {
            splice @$fs, $i, 1;
            splice @$fu, $i, 1 if $fu && @$fu > $i;
            splice @$fa, $i, 1 if $fa && @$fa > $i;
            splice @$ff, $i, 1 if $ff && @$ff > $i;
        }
    }
}

1;
# ABSTRACT: Field munger role

=for Pod::Coverage .*
