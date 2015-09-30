package Bench::Item::function_call;

# DATE
# VERSION

use 5.010;
use strict;
use warnings;

use Data::Dmp;

sub schema {
    # XXX either:
    # - must have 'template' (str)
    # - must have 'module' (str), 'function' (str), 'arg_template' (str)
}

sub prepare_bench_item {
    my ($self, %args) = @_;

    my $item = $args{item};

    my $bitem = {};

    if ($item->{template}) {
        $item->{template} =~ /\A
                              (?P<module>\w+(?:::\w+)*)
                              (?P<function>\w+)
                              \s*
                              (?:
                                  \(
                                  (?P<args>:
                                      \<\w+\>
                                      (?:\s*,\s*\<\w+\>)*
                                  )?
                                  \)
                              )?
                              \s*
                              \z/x
                                  or die "Invalid template '$item->{template}'";
        $bitem->{module} = $+{module};
        $bitem->{function} = $+{function};

    # } elsif ($item->{module} && $item->{function}) { # not yet implemented
    } else {
        die "Item must specify 'template'";
    }
    $bitem;
}

1;
# ABSTRACT: code item
