package Bencher::Formatter::RenderAsBenchmarkPm;

use 5.010001;
use strict;
use warnings;

use parent qw(Bencher::Formatter);

use Role::Tiny::With;
with 'Bencher::Role::ResultRenderer';

# AUTHORITY
# DATE
# DIST
# VERSION

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
            (defined($it->{rate}) ? "$it->{rate}/s" : sprintf("%.1f/s", 1000/$it->{time})),
        ];
        for my $j (0..$#{$items}) {
            my $pct;
            if ($i == $j) {
                $pct = "--";
            } else {
                if ($items->[$j]{time} < $it->{time}) {
                    # item i is slower than item j by N percent
                    $pct = -(1 - $items->[$j]{time} / $it->{time}) * 100;
                } else {
                    # item i is faster than item j by N percent
                    $pct = ($items->[$j]{time} / $it->{time} -1) * 100;
                }
                $pct = sprintf("%d%%", $pct);
            }
            push @{ $rows[-1] }, $pct;
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
