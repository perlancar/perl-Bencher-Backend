package Bencher::Formatter;

use 5.010001;
use strict;
use warnings;

# AUTHORITY
# DATE
# DIST
# VERSION

sub new {
    my ($class, %args) = @_;
    bless \%args, $class;
}

1;
# ABSTRACT: Base class for formatter

=for Pod::Coverage .*
