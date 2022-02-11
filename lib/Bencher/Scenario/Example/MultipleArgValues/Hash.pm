package Bencher::Scenario::Example::MultipleArgValues::Hash;

use strict;

# AUTHORITY
# DATE
# DIST
# VERSION

our $scenario = {
    participants => [
        {name=>'pow', code_template => '<x>**<y>'},
    ],
    datasets => [
        {name=>'small_base', args=>{'x@'=>{one=>1,two=>2} , 'y@'=>[0,1,2,3]}},
        {name=>'large_base', args=>{'x@'=>{a_hundred=>100}, 'y@'=>[0,1,2,3]}},
    ],
};

1;
# ABSTRACT: An example scenario: demo of multiple argument values (hash)
