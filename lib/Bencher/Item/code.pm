package Bench::Item::code;

# DATE
# VERSION

use 5.010;
use strict;
use warnings;

sub schema {
    # XXX must have 'code' (coderef)
}

sub prepare_bench_item {
    my ($self, $item) = @_;
    return {
        code => $item->{code},
    }
}

1;
# ABSTRACT: code item
