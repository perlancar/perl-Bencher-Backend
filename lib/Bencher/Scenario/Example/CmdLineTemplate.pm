package Bencher::Scenario::Example::CmdLineTemplate;

# DATE
# VERSION

our $scenario = {
    participants => [
        {name => 'template_ary', cmdline_template => ['<prog>', "-e1"]},
        {name => 'template_str', cmdline_template => '<prog> -e1'},
    ],
    datasets => [
        { name=>"perl", args => {prog => $^X} },
    ],
};

1;
# ABSTRACT: Demonstrate cmdline_template

=head1 SYNOPSIS

 % bencher -m Example::CmdLineTemplate [other options]...
