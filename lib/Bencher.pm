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

See L<bencher> CLI.


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


=head1 SCENARIO

The core data structure that you need to prepare is the B<scenario>. It is a
L<DefHash> (i.e. just a regular Perl hash), the two most important keys of this
hash are: B<participants> and B<datasets>.

An example scenario (from C<Bench::Scenario::Example>):

 package Bencher::Scenario::Example;
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

B<participants> (array) lists Perl code (or external command) that we want to
benchmark. Instead of just list of coderefs like what L<Benchmark> expects, you
can use C<fcall_template> instead. It is a string containing a function call
code. From this value, Bencher can extract the name of the module and function
used (and can help you load the modules, benchmark startup overhead of all
involved modules, etc). It can also contain variables enclosed in angle
brackets, like C<< <text> >> which will be replaced with actual data/value
later.

You can also add C<name> key to a participant so you can refer to it more easily
later, e.g.:

 participants => [
     {name=>'pp', fcall_template=>'List::MoreUtils::PP::uniq(@{<array>})'},
     {name=>'xs', fcall_template=>'List::MoreUtils::XS::uniq(@{<array>})'},
 ],

Aside from C<fcall_template>, you can also use C<code_template> (a string
containing arbitrary code) or C<code> (a subroutine reference, just like what
you would provide to the Benchmark module).

Or, if you are benchmarking commands, you specify C<cmdline> (array or strings,
or strings) instead. An array cmdline will not use shell, while the string
version will use shell. See L<Bencher::Scenario::Interpreters>.

B<datasets> (array) lists the function inputs (or command-line arguments). You
can C<name> each dataset too, to be able to refer to it more easily.

You can instruct the B<bencher> CLI to filter wanted/unwanted modules,
participants, or datasets before benchmarking.

Other known scenario properties (keys):

=over

=item * name

From DefHash, scenario name (usually short and one word).

=item * summary

From DefHash, a one-line plaintext summary.

=item * description (str)

From DefHash, longer description in Markdown.

=item * on_failure (str, "skip"|"die")

The default is "die". When set to "skip", will first run the code of each item
before benchmarking and trap command failure/Perl exception and if that happens,
will "skip" the item.

Can be overriden in the CLI with C<--on-failure> option.

=back


=head1 SEE ALSO

L<bencher>

B<BenchmarkAnything>. There are lot of overlaps of goals between Bencher and
this project. I hope to reuse or interoperate parts of BenchmarkAnything, e.g.
storing Bencher results in a BenchmarkAnything storage backend, sending Bencher
results to a BenchmarkAnything HTTP server, and so on.

L<Benchmark>, L<Benchmark::Dumb> (L<Dumbbench>)

C<Bencher::Scenario::*> for examples of scenarios.
