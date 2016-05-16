package Sah::Schema::bencher::scenario;

# DATE
# VERSION

use strict;
use warnings;

our %dh_props = (
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

our $schema = [hash => {
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
        extra_modules => {},
    },
}, {}];

1;
# ABSTRACT: Bencher scenario
