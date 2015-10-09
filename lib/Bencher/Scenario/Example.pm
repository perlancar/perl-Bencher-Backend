package Bencher::Scenario::Example;

# DATE
# VERSION

our $scenario = {
    participants => [
        {fcall_template => q[Text::Wrap::wrap('', '', <text>)]},
    ],
    datasets => [
        { name=>"foobar x100",   args => {text=>"foobar " x 100} },
        { name=>"foobar x1000",  args => {text=>"foobar " x 1000} },
        { name=>"foobar x10000", args => {text=>"foobar " x 10000} },
    ],
};

1;
# ABSTRACT: An example scenario

=head1 SYNOPSIS

 % bencher -m Example [other options]...
