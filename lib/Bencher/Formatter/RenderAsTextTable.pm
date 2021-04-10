package Bencher::Formatter::RenderAsTextTable;

# AUTHORITY
# DATE
# DIST
# VERSION

use 5.010001;
use strict;
use warnings;
#use Log::Any::IfLOG '$log';

use parent qw(Bencher::Formatter);

use Perinci::Result::Format::Lite;

use Role::Tiny::With;
with 'Bencher::Role::ResultRenderer';

sub render_result {
    my ($self, $envres) = @_;

    my $rres = ""; # render result

    $rres .= $envres->[3]{'table.title'}.":\n" if $envres->[3]{'table.title'};
    $rres .= Perinci::Result::Format::Lite::format($envres, 'text-pretty');

    $rres;
}

1;
# ABSTRACT: Scale time to make it convenient

=for Pod::Coverage .*
