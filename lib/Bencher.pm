package Bencher;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;

use Benchmark::Dumb qw(timethese);

1;
#ABSTRACT: A benchmark framework

=head1 SYNOPSIS


=head1 DESCRIPTION

B<EARLY WORK, LOTS OF UNIMPLEMENTED STUFFS.>

Bencher is a benchmark framework. It helps you:

=over

=item * specify what Perl code (functions/module names or coderefs) or external commands you want to benchmark

along with a set of data (function or command-line arguments).

=item * run the items

You can run all the items, only some of them, with some/all combinations of
arguments, with different module paths/versions, different perl paths, and so
on.

=item * save the result

=item * display the result(s) and graph them

=item * send the result to a server

=back


=head2 SEE ALSO

B<BenchmarkAnything>. There are lot of overlaps of goals between Bencher and
this project. I hope to reuse or interoperate parts of BenchmarkAnything, e.g.
storing Bencher results in a BenchmarkAnything storage backend, sending Bencher
results to a BenchmarkAnything HTTP server, and so on.
