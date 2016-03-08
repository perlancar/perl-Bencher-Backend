package Sah::Schema::Bencher::Scenario;

# DATE
# VERSION

use strict;
use warnings;

our %SCHEMAS;

my %dh_props = (
    v => {},
    defhash_v => {},
    name => {},
    caption => {},
    summary => {},
    description => {},
    tags => {},
    default_lang => {},
    x => {},
);

$SCHEMAS{bencher_scenario} = [hash => {
    # tmp
    _prop => {
        %dh_props,

        participants => {
            _elem_prop => {
                %dh_props,

                type => {},
                module => {},
                function => {},
                code => {},
                code_template => {},
                fcall_template => {},
                cmdline => {}, # str|array[str]
            },
        },
        datasets => {
            _elem_props => {
                %dh_props,

                args => {}, # hash
                argv => {}, # array
            },
        },
        on_failure => {}, # die*, skip
        module_startup => {},
    },
}];

1;
# ABSTRACT: Sah schemas to validate Bencher specifications

=head1 SYNOPSIS

 # schemas are put in the %SCHEMAS package variable


=head1 DESCRIPTION

This module contains L<Sah> schemas to validate L<Bencher>.


=head1 append:SEE ALSO

L<Bencher>
