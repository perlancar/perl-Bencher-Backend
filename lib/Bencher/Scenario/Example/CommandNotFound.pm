package Bencher::Scenario::Example::CommandNotFound;

# DATE
# VERSION

our $scenario = {
    participants => [
        {name=>'found-array'    , cmdline=>['true']},
        {name=>'found-string'   , cmdline=>'true'},
        {name=>'notfound-array' , cmdline=>['foo1']},
        {name=>'notfound-string', cmdline=>'foo2'},
    ],
};

1;
# ABSTRACT: An example scenario: command not found

=head1 SYNOPSIS

 % bencher -m Example::CommandNotFound [other options]...
