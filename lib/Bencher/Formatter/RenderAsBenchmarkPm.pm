package Bencher::Formatter::RenderAsBenchmarkPm;

# AUTHORITY
# DATE
# DIST
# VERSION

use 5.010001;
use strict;
use warnings;

use parent qw(Bencher::Formatter);

use Role::Tiny::With;
with 'Bencher::Role::ResultRenderer';

sub render_result {
    require Text::Table::More;

    my ($self, $envres) = @_;

    # because underscored keys were removed; we want _succint_name back.
    my $items = $envres->[2];
    Bencher::Backend::_set_item_names($items);
    my @item_names;
    my %legends; # key = succinct_name
    for my $it (@$items) {
        push @item_names, $it->{_succinct_name};
        $legends{$it->{_succinct_name}} =
            join(" ", map {"$_=$it->{$_}"} grep { !/^_/ && !/^(errors|pct_|rate|samples|time)/ } sort keys %$it);
    }

    my @rows;
    push @rows, [
        # column names
        "", # item name
        "Rate",
        @item_names,
    ];
    for my $i (0..$#{$items}) {
        my $it = $items->[$i];
        push @rows, [
            $it->{_succinct_name},
            "$it->{rate}/s",
        ];
        for my $j (0..$#{$items}) {
            my $pct;
            if ($i != $j) {
                if ($items->[$j]{rate} > $it->{rate}) {
                    # item i is slower than item j by N percent
                    $pct = -(1 - $items->[$i]{rate} / $items->[$j]{rate}) * 100;
                } else {
                    # item i is faster than item j by N percent
                    $pct = ($it->{rate} / $items->[$j]{rate} -1) * 100;
                }
            }
            push @{ $rows[-1] }, $i==$j ? "--" : sprintf("%d%%", $pct);
        }
    }

    my $rres = ''; # render result

    $rres .= Text::Table::More::table(
        rows => \@rows,
        border_style=>'ASCII::None',
        align => 'right',
        col_attrs => [
            [0, {align=>'left'}],
        ],
    );
    $rres .= "\n";
    $rres .= "Legends:\n";
    for (sort keys %legends) {
        $rres .= "  " . $_ . ": " . $legends{$_} . "\n";
    }

    $rres;
}

1;
# ABSTRACT: Scale time to make it convenient

=for Pod::Coverage .*
