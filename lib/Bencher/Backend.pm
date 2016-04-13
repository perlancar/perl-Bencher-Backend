package Bencher::Backend;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;
use Log::Any::IfLOG '$log';

use Data::Dmp;
use List::MoreUtils qw(all);
use List::Util qw(first);

our %SPEC;

sub _get_tempfile_path {
    my ($filename) = @_;
    state $tempdir = do {
        require File::Temp;
        File::Temp::tempdir(CLEANUP => $log->is_debug ? 0:1);
    };
    "$tempdir/$filename";
}

sub _fill_template {
    no warnings 'uninitialized';
    my ($template, $vars, $escape_method) = @_;

    if ($escape_method eq 'shell') {
        require String::ShellQuote;
    }

    $template =~ s/\<(\w+)(:raw)?\>/
        $2 eq ':raw' ? $vars->{$1} :
        $escape_method eq 'shell' ? String::ShellQuote::shell_quote($vars->{$1}) :
        $escape_method eq 'dmp' ? dmp($vars->{$1}) :
        $vars->{$1}
        /eg;

    $template;
}

sub _get_process_size {
    my ($parsed, $it) = @_;

    my $script_path = _get_tempfile_path("get_process_size-$it->{seq}");

    $log->debugf("Creating script to measure get process size at %s ...", $script_path);
    {
        open my($fh), ">", $script_path or die "Can't open file $script_path: $!";

        print $fh "# load modules\n";
        my $participants = $parsed->{participants};
        my $participant = _find_record_by_seq($participants, $it->{_permute}{participant});
        if ($participant->{module}) {
            print $fh "require $participant->{module};\n";
        } elsif ($participant->{modules}) {
            print $fh "require $_;\n" for @{ $participant->{modules} };
        }
        # XXX should we load extra_modules? i think we should
        print $fh "\n";

        print $fh "# run code\n";
        print $fh 'my $code = ', dmp($it->{_code}), '; $code->(); ', "\n";
        print $fh "\n";

        # we don't want to load extra modules like Linux::Smaps etc because they
        # will increase the process size themselves. instead, we do it very
        # minimally by reading /proc/PID/smaps directly. this means it will only
        # work for linux. support for other OS should be added here.
        print $fh "# get process size\n";
        print $fh 'print "### OUTPUT-TO-PARSE-BY-BENCHER ###\n";', "\n"; # keep sync with reading code
        print $fh 'my %stats;', "\n";
        print $fh 'open my($fh), "<", "/proc/$$/smaps" or die "Cannot open /proc/$$smaps: $!";', "\n";
        print $fh 'while (<$fh>) { /^(\w+):\s*(\d+)/ or next; $stat{lc $1} += $2 }', "\n";
        print $fh 'for (sort keys %stat) { print "$_: $stat{$_}\n" }', "\n";
        print $fh "\n";

        close $fh or die "Can't write to $script_path: $!";
    }

    # run the script
    {
        require Capture::Tiny;
        my @cmd = ($^X, $script_path);
        $log->debugf("Running %s ...", \@cmd);
        my ($stdout, @res) = &Capture::Tiny::capture_stdout(sub {
            system @cmd;
            die "Failed running script '$script_path' to get process size" if $?;
        });
        $stdout =~ /^### OUTPUT-TO-PARSE-BY-BENCHER ###(.+)/ms
            or die "Can't find marker in output of '$script_path' to get process size";
        my $info0 = $1;
        my %info; while ($info0 =~ /^(\w+): (\d+)/gm) { $info{$1} = $2 }
        $it->{"proc_size"} = $info{size}*1024;
        for (qw/rss private_dirty/) {
            $it->{"proc_${_}_size"} = $info{$_}*1024 if defined $info{$_};
        }
    }
}

sub _find_record_by_seq {
    my ($recs, $seq) = @_;

    for my $rec (@$recs) {
        return $rec if $rec->{seq} == $seq;
    }
    undef;
}

sub _filter_records {
    my %args = @_;

    my $recs = $args{records};
    my $entity = $args{entity};
    my $include = $args{include};
    my $exclude = $args{exclude};
    my $include_name = $args{include_name};
    my $exclude_name = $args{exclude_name};
    my $include_seq = $args{include_seq};
    my $exclude_seq = $args{exclude_seq};
    my $include_pattern = $args{include_pattern};
    my $exclude_pattern = $args{exclude_pattern};
    my $include_tags = $args{include_tags};
    my $exclude_tags = $args{exclude_tags};
    my $aibdf = $args{apply_include_by_default_filter} // 1;

    my $frecs = [];

    # check that there is a name that is also a sequence number which could be
    # confusing
    {
        last unless $include || $exclude ||
            $include_pattern || $exclude_pattern;
        my @seq_names;
        for my $rec (@$recs) {
            my $name = $rec->{name} // $rec->{_name};
            next unless $name =~ /\A\d+\z/ && $name < @$recs;
            push @seq_names, $name;
        }
        if (@seq_names) {
            warn "There is at least one $entity which has names that are also ".
                "sequence numbers and this can be confusing when ".
                "including/excluding: " . join(", ", @seq_names) .
                ". Either rename the $entity or use ".
                "--{include,exclude}-$entity-{name,seq}.\n";
        }
    }

    # check that what's mentioned in {in,ex}clude{,_name,_seq} are actually in
    # the records
    {
        for my $incexc (@{$include // []}) {
            my $found;
            for my $rec (@$recs) {
                if ($incexc =~ /\A\d+\z/ && $rec->{seq} == $incexc) {
                    $found++;
                    last;
                } elsif (($rec->{name} // $rec->{_name} // '') eq $incexc) {
                    $found++;
                    last;
                }
            }
            die "Unknown $entity '$incexc' specified in include, try ".
                "one of: " .
                    join(", ",
                         grep { length($_) }
                             map { ($_->{seq},
                                    $_->{name} // $_->{_name} // '') }
                                 @$recs)
                unless $found;
        }
        for my $incexc (@{$exclude // []}) {
            my $found;
            for my $rec (@$recs) {
                if ($incexc =~ /\A\d+\z/ && $rec->{seq} == $incexc) {
                    $found++;
                    last;
                } elsif (($rec->{name} // $rec->{_name} // '') eq $incexc) {
                    $found++;
                    last;
                }
            }
            die "Unknown $entity '$incexc' specified in exclude, try ".
                "one of: " .
                    join(", ",
                         grep { length($_) }
                             map { ($_->{seq},
                                    $_->{name} // $_->{_name} // '') }
                                 @$recs)
                unless $found;
        }
        for my $incexc (@{$include_name // []}) {
            my $found;
            for my $rec (@$recs) {
                if (($rec->{name} // $rec->{_name} // '') eq $incexc) {
                    $found++;
                    last;
                }
            }
            die "Unknown $entity name '$incexc' specified in include_name, try ".
                "one of: " .
                    join(", ",
                         grep { length($_) }
                             map { $_->{name} // $_->{_name} // '' }
                                 @$recs)
                unless $found;
        }
        for my $incexc (@{$exclude_name // []}) {
            my $found;
            for my $rec (@$recs) {
                if (($rec->{name} // $rec->{_name} // '') eq $incexc) {
                    $found++;
                    last;
                }
            }
            die "Unknown $entity name '$incexc' specified in exclude_name, try ".
                "one of: " .
                    join(", ",
                         grep { length($_) }
                             map { $_->{name} // $_->{_name} // '' }
                                 @$recs)
                unless $found;
        }
        for my $incexc (@{$include_seq // []}) {
            my $found;
            for my $rec (@$recs) {
                if ($rec->{seq} == $incexc) {
                    $found++;
                    last;
                }
            }
            die "Unknown $entity sequence '$incexc' specified in include_seq, try ".
                "one of: " . join(", ", map { $_->{seq} } @$recs) unless $found;
        }
        for my $incexc (@{$exclude_seq // []}) {
            my $found;
            for my $rec (@$recs) {
                if ($rec->{seq} == $incexc) {
                    $found++;
                    last;
                }
            }
            die "Unknown $entity sequence '$incexc' specified in exclude_seq, try ".
                "one of: " . join(", ", map { $_->{seq} } @$recs) unless $found;
        }
    }

  REC:
    for my $rec (@$recs) {
        my $explicitly_included;
        if ($include && @$include) {
            my $included;
          INC:
            for my $inc (@$include) {
                if ($inc =~ /\A\d+\z/) {
                    if ($rec->{seq} == $inc) {
                        $included++;
                        last INC;
                    }
                } else {
                    if (($rec->{name} // $rec->{_name} // '') eq $inc) {
                        $included++;
                        last INC;
                    }
                }
            }
            next REC unless $included;
            $explicitly_included++;
        }
        if ($include_name && @$include_name) {
            my $included;
          INC:
            for my $inc (@$include_name) {
                if (($rec->{name} // $rec->{_name} // '') eq $inc) {
                    $included++;
                    last INC;
                }
            }
            next REC unless $included;
            $explicitly_included++;
        }
        if ($include_seq && @$include_seq) {
            my $included;
          INC:
            for my $inc (@$include_seq) {
                if ($rec->{seq} == $inc) {
                    $included++;
                    last INC;
                }
            }
            next REC unless $included;
            $explicitly_included++;
        }
        if ($exclude && @$exclude) {
            for my $exc (@$exclude) {
                if ($exc =~ /\A\d+\z/) {
                    next REC if $rec->{seq} == $exc;
                } else {
                    next REC if (($rec->{name} // $rec->{_name} // '') eq $exc);
                }
            }
        }
        if ($exclude_name && @$exclude_name) {
            for my $exc (@$exclude_name) {
                next REC if (($rec->{name} // $rec->{_name} // '') eq $exc);
            }
        }
        if ($exclude_seq && @$exclude_seq) {
            for my $exc (@$exclude_seq) {
                next REC if $rec->{seq} == $exc;
            }
        }
        if ($include_pattern) {
            next REC unless $rec->{seq} =~ /$include_pattern/i ||
                (($rec->{name} // $rec->{_name} // '') =~ /$include_pattern/i);
            $explicitly_included++;
        }
        if ($exclude_pattern) {
            next REC if $rec->{seq} =~ /$exclude_pattern/i ||
                (($rec->{name} // $rec->{_name} // '') =~ /$exclude_pattern/i);
        }
        if ($include_tags && @$include_tags) {
            my $included;
          INCTAG:
            for my $tag (@$include_tags) {
                if ($tag =~ /&/) {
                    $included = 1;
                    for my $simpletag (split /\s*&\s*/, $tag) {
                        unless (grep {$_ eq $simpletag} @{ $rec->{tags} // [] }) {
                            $included = 0;
                            next REC;
                        }
                    }
                    last INCTAG;
                } else {
                    if (grep {$_ eq $tag} @{ $rec->{tags} // [] }) {
                        $included++;
                        last INCTAG;
                    }
                }
            }
            next REC unless $included;
            $explicitly_included++;
        }
        if ($exclude_tags && @$exclude_tags) {
          EXCTAG:
            for my $tag (@$exclude_tags) {
                if ($tag =~ /&/) {
                    for my $simpletag (split /\s*&\s*/, $tag) {
                        unless (grep {$_ eq $simpletag} @{ $rec->{tags} // [] }) {
                            next EXCTAG;
                        }
                    }
                    next REC;
                } else {
                    next REC if grep {$_ eq $tag} @{ $rec->{tags} // [] };
                }
            }
        }

        unless ($explicitly_included || !$aibdf) {
            next REC if defined($rec->{include_by_default}) &&
                !$rec->{include_by_default};
        }

        push @$frecs, $rec;
    }

    $frecs;
}

sub _get_scenario {
    my %args = @_;

    my $pargs = $args{parent_args};

    my $scenario;
    if (defined $pargs->{scenario_file}) {
        $scenario = do $pargs->{scenario_file};
        die "Can't load scenario file '$pargs->{scenario_file}': $@" if $@;
    } elsif (defined $pargs->{scenario_module}) {
        my $m = "Bencher::Scenario::$pargs->{scenario_module}"; $m =~ s!/!::!g;
        my $mp = $m; $mp =~ s!::!/!g; $mp .= ".pm";
        {
            local @INC = @INC;
            unshift @INC, $_ for @{ $pargs->{include_path} // [] };
            require $mp;
        }
        no strict 'refs';
        $scenario = ${"$m\::scenario"};
    } else {
        $scenario = {
            participants => [],
        };
    }

    if ($pargs->{participants}) {
        for (@{ $pargs->{participants} }) {
            push @{ $scenario->{participants} }, $_;
        }
    }
    if ($pargs->{datasets}) {
        $scenario->{datasets} //= [];
        for (@{ $pargs->{datasets} }) {
            push @{ $scenario->{datasets} }, $_;
        }
    }
    $scenario;
}

sub _parse_scenario {
    use experimental 'smartmatch';

    my %args = @_;

    my $unparsed = $args{scenario};
    my $pargs = $args{parent_args};
    my $apply_filters = $args{apply_filters} // 1;
    my $aibdf = $args{apply_include_by_default_filter} // 1; # skip items that have include_by_default=0

    my $parsed = {%$unparsed}; # shallow copy

    # normalize participants
    {
        $parsed->{participants} = [];
        my $i = -1;
        for my $p0 (@{ $unparsed->{participants} }) {
            $i++;
            my $p = { %$p0, seq=>$i };
            $p->{include_by_default} //= 1;
            $p->{type} //= do {
                if ($p->{cmdline} || $p->{cmdline_template} ||
                        $p->{perl_cmdline} || $p->{perl_cmdline_template}) {
                    'command';
                } else {
                    'perl_code';
                }
            };
            if ($p->{fcall_template}) {
                if ($p->{fcall_template} =~ /\A
                                             (\w+(?:::\w+)*)
                                             (::|->)
                                             (\w+)/x) {
                    $p->{module}   = $1;
                    $p->{function} = $3;
                }
            }

            # try to come up with a nicer name for the participant (not
            # necessarily unique)
            unless (defined($p->{name})) {
                if ($p->{type} eq 'command') {
                    my $c = $p->{cmdline} // $p->{cmdline_template} //
                        $p->{perl_cmdline} // $p->{perl_cmdline_template};
                    if (ref($c) eq 'ARRAY') {
                        $p->{_name} = substr($c->[0], 0, 20);
                    } else {
                        $c =~ /(\S+)/;
                        $p->{_name} = substr($1, 0, 20);
                    }
                } elsif ($p->{type} eq 'perl_code') {
                    if ($p->{function}) {
                        $p->{_name} =
                            ($p->{module} ? "$p->{module}::" : "").
                            $p->{function};
                    } elsif ($p->{module}) {
                        $p->{_name} = $p->{module};
                    } elsif ($p->{modules}) {
                        $p->{_name} = join("+", @{$p->{modules}});
                    }
                }
            }

            push @{ $parsed->{participants} }, $p;
        } # for each participant

        # filter participants by include/exclude module/function
        if ($apply_filters) {
            if ($pargs->{include_modules} && @{ $pargs->{include_modules} }) {
                $parsed->{participants} = [grep {
                    (defined($_->{module}) && $_->{module} ~~ @{ $pargs->{include_modules} }) ||
                        (defined($_->{modules}) && (first { $_ ~~ @{ $pargs->{include_modules} } } @{ $_->{modules} }))
                    } @{ $parsed->{participants} }];
            }
            if ($pargs->{exclude_modules} && @{ $pargs->{exclude_modules} }) {
                $parsed->{participants} = [grep {
                    !(defined($_->{module}) && $_->{module} ~~ @{ $pargs->{exclude_modules} }) &&
                    !(defined($_->{modules}) && (first { $_ ~~ @{ $pargs->{exclude_modules} } } @{ $_->{modules} }))
                } @{ $parsed->{participants} }];
            }
            if ($pargs->{include_module_pattern}) {
                $parsed->{participants} = [grep {
                    (defined($_->{module}) && $_->{module} =~ qr/$pargs->{include_module_pattern}/i) ||
                    (defined($_->{modules}) && (first { /$pargs->{include_module_pattern}/i } @{ $_->{modules} }))
                } @{ $parsed->{participants} }];
            }
            if ($pargs->{exclude_module_pattern}) {
                $parsed->{participants} = [grep {
                    !(defined($_->{module}) && $_->{module} =~ qr/$pargs->{exclude_module_pattern}/i) &&
                    !(defined($_->{modules}) && (first { /$pargs->{exclude_module_pattern}/i } @{ $_->{modules} }))
                } @{ $parsed->{participants} }];
            }

            if ($pargs->{include_functions} && @{ $pargs->{include_functions} }) {
                $parsed->{participants} = [grep {
                    defined($_->{function}) && $_->{function} ~~ @{ $pargs->{include_functions} }
                } @{ $parsed->{participants} }];
            }
            if ($pargs->{exclude_functions} && @{ $pargs->{exclude_functions} }) {
                $parsed->{participants} = [grep {
                    !defined($_->{function}) || !($_->{function} ~~ @{ $pargs->{exclude_functions} })
                } @{ $parsed->{participants} }];
            }
            if ($pargs->{include_function_pattern}) {
                $parsed->{participants} = [grep {
                    defined($_->{function}) && $_->{function} =~ qr/$pargs->{include_function_pattern}/i
                } @{ $parsed->{participants} }];
            }
            if ($pargs->{exclude_function_pattern}) {
                $parsed->{participants} = [grep {
                    !defined($_->{function}) || $_->{function} !~ qr/$pargs->{exclude_function_pattern}/i
                } @{ $parsed->{participants} }];
            }
        }

        $parsed->{participants} = _filter_records(
            entity => 'participant',
            records => $parsed->{participants},
            include => $pargs->{include_participants},
            exclude => $pargs->{exclude_participants},
            include_name => $pargs->{include_participant_names},
            exclude_name => $pargs->{exclude_participant_names},
            include_seq => $pargs->{include_participant_seqs},
            exclude_seq => $pargs->{exclude_participant_seqs},
            include_pattern => $pargs->{include_participant_pattern},
            exclude_pattern => $pargs->{exclude_participant_pattern},
            include_tags => $pargs->{include_participant_tags},
            exclude_tags => $pargs->{exclude_participant_tags},
            apply_include_by_default_filter => $aibdf,
        ) if $apply_filters;
    } # normalize participants

    # normalize datasets
    if ($unparsed->{datasets}) {
        $parsed->{datasets} = [];
        my $i = -1;
        my $dss0 = $unparsed->{datasets};

        my $td_args;
        my @uniq_args;

        for my $ds0 (@$dss0) {
            $i++;
            my $ds = { %$ds0, seq=>$i };
            $ds->{include_by_default} //= 1;

            # try to come up with a nicer name for the dataset (not necessarily
            # unique): extract from argument values
            unless (defined($ds->{name})) {
                unless ($td_args) {
                    if (all {$_->{args}} @$dss0) {
                        require TableData::Object::aohos;
                        $td_args = TableData::Object::aohos->new(
                            [map {$_->{args}} @$dss0]);
                        @uniq_args = $td_args->uniq_col_names;
                    } else {
                        $td_args = -1;
                    }
                }
                if (@uniq_args > 1) {
                    $ds->{name} = dmp(
                        { map {$_ => $ds->{args}{$_}} @uniq_args });
                } elsif (@uniq_args) {
                    $ds->{name} = $ds->{args}{$uniq_args[0]};
                    $ds->{name} = dmp($ds->{name}) if ref($ds->{name});
                }
            }


            push @{ $parsed->{datasets} }, $ds;
        } # for each dataset

        $parsed->{datasets} = _filter_records(
            entity => 'dataset',
            records => $parsed->{datasets},
            include => $pargs->{include_datasets},
            exclude => $pargs->{exclude_datasets},
            include_name => $pargs->{include_dataset_names},
            exclude_name => $pargs->{exclude_dataset_names},
            include_seq => $pargs->{include_dataset_seqs},
            exclude_seq => $pargs->{exclude_dataset_seqs},
            include_pattern => $pargs->{include_dataset_pattern},
            exclude_pattern => $pargs->{exclude_dataset_pattern},
            include_tags => $pargs->{include_dataset_tags},
            exclude_tags => $pargs->{exclude_dataset_tags},
            apply_include_by_default_filter => $aibdf,
        ) if $apply_filters;
    } # normalize datasets

    $parsed;
}

sub _get_participant_modules {
    use experimental 'smartmatch';

    my $parsed = shift;

    my @modules;
    for my $p (@{ $parsed->{participants} }) {
        if (defined $p->{module}) {
            push @modules, $p->{module} unless $p->{module} ~~ @modules;
        } elsif (defined $p->{modules}) {
            for (@{ $p->{modules} }) {
                push @modules, $_ unless $_ ~~ @modules;
            }
        }
    }

    @modules;
}

sub _get_participant_functions {
    use experimental 'smartmatch';

    my $parsed = shift;

    my @functions;
    for my $p (@{ $parsed->{participants} }) {
        next unless defined $p->{function};
        push @functions, $p->{function} unless $p->{function} ~~ @functions;
    }

    @functions;
}

sub _gen_items {
    require Permute::Named::Iter;

    my %args = @_;

    my $parsed = $args{scenario};
    my $pargs  = $args{parent_args};
    my $apply_filters = $args{apply_filters} // 1;

    $parsed->{items} = [];
    my @permute;

    my $participants;
    my $datasets;
    my $module_startup = $pargs->{module_startup} // $parsed->{module_startup};

    my @modules = _get_participant_modules($parsed);

    if ($module_startup) {
        my %mem;
        # push perl as base-line
        push @$participants, {
            seq  => 0,
            name => "perl -e1 (baseline)",
            type => 'command',
            perl_cmdline => ["-e1"],
        };

        my $i = 0;
        for my $p0 (@{ $parsed->{participants} }) {
            my $key;
            if (defined $p0->{module}) {
                $key = $p0->{module};
                next if $mem{$key}++;
                push @$participants, {
                    seq  => ++$i,
                    name => $key,
                    type => 'command',
                    module => $p0->{module},
                    perl_cmdline => ["-M$p0->{module}", "-e1"],
                };
            } elsif (defined $p0->{modules}) {
                $key = join("+", @{ $p0->{modules} });
                next if $mem{$key}++;
                push @$participants, {
                    seq  => ++$i,
                    name => $key,
                    type => 'command',
                    modules => $p0->{modules},
                    perl_cmdline => [(map {"-M$_"} @{ $p0->{modules} }), "-e1"],
                };
            }
        }
        return [412, "There are no modules to benchmark ".
                    "the startup overhead of"] unless %mem;
    } else {
        return [412, "Please load a scenario (-m, -f) or ".
                    "include at least one participant (-p)"]
            unless @{$parsed->{participants}};
        $participants = $parsed->{participants};
        $datasets = $parsed->{datasets} if $parsed->{datasets};
    }

    my %perl_exes; # key=name, val=path
    {
        my @perls;
        if ($pargs->{multiperl}) {
            require App::perlbrew;
            @perls = grep {$_->{has_bencher}} _list_perls();
            return [412, "Can't multiperl because no perl has Bencher installed"]
                unless @perls;

            if ($pargs->{include_perls} && @{ $pargs->{include_perls} }) {
                @perls = grep {
                    my $p = $_;
                    (grep { $p->{name} eq $_ } @{ $pargs->{include_perls} }) ? 1:0;
                } @perls;
            }
            if ($pargs->{exclude_perls} && @{ $pargs->{exclude_perls} }) {
                @perls = grep {
                    my $p = $_;
                    (grep { $p->{name} eq $_ } @{ $pargs->{exclude_perls} }) ? 0:1;
                } @perls;
            }
            die "You have to include at least one perl\n" unless @perls;
            for (@perls) {
                $perl_exes{$_->{name}} = $_->{executable};
            }
            @perls = map {$_->{name}} @perls;
        } else {
            $perl_exes{perl} = $^X;
            @perls = ("perl");
        }
        push @permute, "perl", \@perls;
    }

    my %perl_opts; # key=name, val=[opt, ...]
    if ($pargs->{multimodver}) {
        require ExtUtils::MakeMaker;
        require Module::Path::More;

        return [412, "There are no modules to search multiple versions of"]
            unless @modules;

        return [412, "Module '$pargs->{multimodver}' is not among modules to benchmark"]
            unless grep {$pargs->{multimodver} eq $_} @modules;

        local @INC = @INC;
        if ($pargs->{include_path} && @{ $pargs->{include_path} }) {
            unshift @INC, $_ for reverse @{ $pargs->{include_path} };
        }

        my %versions; # key=module name
        my $paths = Module::Path::More::module_path(module=>$pargs->{multimodver}, all=>1);

        if (@$paths < 1) {
            return [412, "Can't find module '$pargs->{multimodver}', try adding some --include-path"];
        } elsif (@$paths == 1) {
            return [412, "Only found one path for module '$pargs->{multimodver}', try adding some --include-path"];
        }
        for my $path (@$paths) {
            my $v = MM->parse_version($path);
            $v = undef if defined($v) && $v eq 'undef';
            if (!defined($v)) {
                $log->warnf("Can't parse version from %s", $path);
                next;
            }
            $versions{$v}++;
            my $incdir = $path;
            my $mod_pm = $pargs->{multimodver}; $mod_pm =~ s!::!/!g; $mod_pm .= ".pm";
            $incdir =~ s!/\Q$mod_pm\E$!!;
            $perl_opts{$v} //= ["-I$incdir"];
        }
        return [412, "Can't find version number for module '$pargs->{multimodver}'"]
            unless keys(%versions);
        return [412, "Only found one version of module '$pargs->{multimodver}', try adding some --include-path"]
            unless keys(%versions) > 1;
        push @permute, "modver", [keys %versions];
    }

    push @permute, "participant", [map {$_->{seq}} @$participants];

    if ($datasets && @$datasets) {
        push @permute, "dataset", [map {$_->{seq}} @$datasets];
    }

    $log->debugf("permute: %s", \@permute);

    # to store multiple argument values that are hash, e.g.
    # {args=>{sizes=>{"1M"=>1024**2, "1G"=>1024**3, "1T"=>1024**4}}} instead of
    # array: {args=>{sizes=>[1024**2, 1024**3, 1024**4]}}
    my %ds_arg_values; # key=ds seq, val=hash(key=arg name, val=arg values)

    my $iter = Permute::Named::Iter::permute_named_iter(@permute);
    my $item_seq = 0;
    my $items = [];
    my %item_mems; # key=item key, value=1
  ITER:
    while (my $h = $iter->()) {
        $log->tracef("iter returns: %s", $h);

        my $p = _find_record_by_seq($participants, $h->{participant});
        my $ds;

        if (exists $h->{dataset}) {
            $ds = _find_record_by_seq($datasets, $h->{dataset});
            # filter first
            if ($ds->{include_participant_tags}) {
                my $included = 0;
              INCTAG:
                for my $tag (@{ $ds->{include_participant_tags} }) {
                    if ($tag =~ /\&/) {
                        for my $simpletag (split /\s*&\s*/, $tag) {
                            unless (grep {$simpletag eq $_} @{ $p->{tags} // [] }) {
                                next INCTAG;
                            }
                        }
                        $included++;
                        last INCTAG;
                    } else {
                        if (grep {$tag eq $_} @{ $p->{tags} // [] }) {
                            $included++;
                            last INCTAG;
                        }
                    }
                }
                unless ($included) {
                    $log->tracef(
                        "skipped dataset by include_participant_tags ".
                            "(%s vs participant:%s)",
                        $ds->{include_participant_tags}, $p->{tags});
                    next ITER;
                }
            }
            if ($ds->{exclude_participant_tags}) {
                my $excluded = 0;
              EXCTAG:
                for my $tag (@{ $ds->{exclude_participant_tags} }) {
                    if ($tag =~ /\&/) {
                        for my $simpletag (split /\s*&\s*/, $tag) {
                            unless (grep {$simpletag eq $_} @{ $p->{tags} // [] }) {
                                next EXCTAG;
                            }
                        }
                        $excluded++;
                        last EXCTAG;
                    } else {
                        if (grep {$tag eq $_} @{ $p->{tags} // [] }) {
                            $excluded++;
                            last EXCTAG;
                        }
                    }
                }
                if ($excluded) {
                    $log->tracef(
                        "skipped dataset by exclude_participant_tags ".
                            "(%s vs participant:%s)",
                        $ds->{exclude_participant_tags}, $p->{tags});
                    next ITER;
                }
            }
        }

        my $iter_args;
        if ($ds && $ds->{args} &&
                (my @multi_keys = grep {/\@\z/} keys %{$ds->{args}})) {
            # we need to permute arguments also
            my @permute_args;
            for my $mk0 (@multi_keys) {
                my $vals = $ds->{args}{$mk0};
                my $mk = $mk0; $mk =~ s/\@\z//;
                if (ref($vals) eq 'HASH') {
                    push @permute_args, $mk => [sort keys %$vals];
                    $ds_arg_values{$h->{dataset}}{$mk} = $vals;
                } elsif(ref($vals) eq 'ARRAY') {
                    push @permute_args, $mk => $vals;
                } else {
                    return [400, "Error in dataset #$h->{dataset} arg '$mk0': value must be hash or array"];
                }
            }
            $iter_args = Permute::Named::Iter::permute_named_iter(
                @permute_args);
            $log->debugf("permute args: %s", \@permute_args);
        } else {
            # create an iterator that returns just a single item: {}
            # require Array::Iter; $iter_args = Array::Iter::list_iter({});
            $iter_args = do {
                my $ary = [{}];
                my $i = 0;
                sub {
                    if ($i < @$ary) {
                        return $ary->[$i++];
                    } else {
                        return undef;
                    }
                };
            };
        }

        # don't permute module versions if participant doesn't involve said
        # module
        if ($pargs->{multimodver} && defined($h->{modver}) &&
                $pargs->{multimodver} ne ($p->{module} // '')) {
            $h->{modver} = '';
        }

      ITER_ARGS:
        while (my $h_args = $iter_args->()) {
            my $args;
            if ($ds && $ds->{args}) {
                $args = { %{$ds->{args}} };
                delete $args->{$_} for (grep {/\@\z/} keys %$args);
                for my $arg (keys %$h_args) {
                    if ($ds_arg_values{$h->{dataset}}{$arg}) {
                        $args->{$arg} = $ds_arg_values{$h->{dataset}}{$arg}{ $h_args->{$arg} };
                    } else {
                        $args->{$arg} = $h_args->{$arg};
                    }
                }
            }

            my $code;
            if ($p->{type} eq 'command') {
                require String::ShellQuote;
                my @cmd;
                my $shell;
                if (defined $p->{cmdline}) {
                    if (ref($p->{cmdline}) eq 'ARRAY') {
                        @cmd = @{ $p->{cmdline} };
                        $shell = 0;
                    } else {
                        @cmd = ($p->{cmdline});
                        $shell = 1;
                    }
                } elsif (defined $p->{perl_cmdline}) {
                    if (ref($p->{perl_cmdline}) eq 'ARRAY') {
                        @cmd = ($perl_exes{$h->{perl}},
                                ($h->{modver} ? @{$perl_opts{$h->{modver}} // []} : ()),
                                @{ $p->{perl_cmdline} });
                        $shell = 0;
                    } else {
                        @cmd = (
                            join(
                                " ",
                                $perl_exes{$h->{perl}},
                                ($h->{modver} ? map {String::ShellQuote::shell_quote($_)} @{$perl_opts{$h->{modver}} // []} : ()),
                                $p->{perl_cmdline},
                            )
                        );
                        $shell = 1;
                    }
                } elsif (defined $p->{cmdline_template}) {
                    my $template_vars;
                    if ($ds->{args}) {
                        $template_vars = { %$args };
                    } elsif ($ds->{argv}) {
                        $template_vars = { map {$_=>$ds->{argv}[$_]}
                                               0..$#{ $ds->{argv} } };
                    }
                    if (ref($p->{cmdline_template}) eq 'ARRAY') {
                        @cmd = map { _fill_template($_, $template_vars) }
                            @{ $p->{cmdline_template} };
                        $shell = 0;
                    } else {
                        my $cmd = _fill_template(
                            $p->{cmdline_template}, $template_vars, 'shell');
                        @cmd = ($cmd);
                        $shell = 1;
                    }
                } elsif (defined $p->{perl_cmdline_template}) {
                    my $template_vars;
                    if ($ds->{args}) {
                        $template_vars = { %$args };
                    } elsif ($ds->{argv}) {
                        $template_vars = { map {$_=>$ds->{argv}[$_]}
                                               0..$#{ $ds->{argv} } };
                    }
                    if (ref($p->{perl_cmdline_template}) eq 'ARRAY') {
                        @cmd = (
                            $perl_exes{$h->{perl}},
                            ($h->{modver} ? @{$perl_opts{$h->{modver}} // []} : ()),
                            map { _fill_template($_, $template_vars) }
                                @{ $p->{perl_cmdline_template} }
                        );
                        $shell = 0;
                    } else {
                        my $cmd = _fill_template(
                            join(
                                " ",
                                $perl_exes{$h->{perl}},
                                ($h->{modver} ? map {String::ShellQuote::shell_quote($_)} @{$perl_opts{$h->{modver}} // []} : ()),
                                $p->{perl_cmdline_template},
                            ),
                            $template_vars,
                            'shell',
                        );
                        @cmd = ($cmd);
                        $shell = 1;
                    }
                } else {
                    die "BUG: Unknown command type";
                }

                $log->debugf("Item #%d: cmdline=%s", $item_seq, \@cmd);

                {
                    my $code_str = "package main; sub { ";
                    if ($shell) {
                        $code_str .= "system ".dmp($cmd[0])."; ";
                    } else {
                        $code_str .= "system {".dmp($cmd[0])."} \@{".dmp(\@cmd)."}; ";
                    }
                    $code_str .= q[die "Command failed (child error=$?, os error=$!)\\n" if $?];
                    $code_str .= "}";
                    $code = eval $code_str;
                    die "BUG: Can't produce code for cmdline: $@ (code string is: $code_str)" if $@;
                };
            } elsif ($p->{type} eq 'perl_code') {
                if ($p->{code}) {
                    if ($ds) {
                        if ($ds->{argv}) {
                            $code = sub { $p->{code}->(@{$ds->{argv}}) };
                        } elsif ($ds->{args}) {
                            $code = sub { $p->{code}->(%$args) };
                        } else {
                            return [400, "Participant #$p->{seq}, dataset #$h->{dataset}: No argv/args supplied for code"];
                        }
                    } else {
                        $code = $p->{code};
                    }
                } elsif (my $template = $p->{code_template} || $p->{fcall_template}) {
                    my $template_vars;
                    if ($ds->{args}) {
                        $template_vars = { %$args };
                    } elsif ($ds->{argv}) {
                        $template_vars = { map {$_=>$ds->{argv}[$_]}
                                               0..$#{ $ds->{argv} } };
                    } else {
                        #warn "Item #$item_seq: participant specifies code_template/fcall_template but there is no args/argv in the dataset #$h->{dataset}\n";
                    }

                    my $code_str = "package main; sub { ".
                        _fill_template($template, $template_vars, 'dmp') . " }";
                    $log->debugf("Item #%d: code=%s", $item_seq, $code_str);
                    $code = eval $code_str;
                    return [400, "Item #$item_seq: code compile error: $@ (code: $code_str)"] if $@;
                }
            } else {
                return [400, "Unknown participant type '$p->{type}'"];
            }

            my $item = {
                _code => $code,
                _permute => $h,
                ((_permute_args => $h_args) x !!$ds->{args}),
            };
            for my $k (keys %$h) {
                if ($k eq 'perl') {
                    $item->{perl} = $h->{$k};
                    $item->{_perl_exe} = $perl_exes{ $h->{$k} };
                } elsif ($k eq 'modver') {
                    $item->{modver} = $h->{$k};
                    $item->{_perl_opts} = $perl_opts{ $h->{$k} };
                } elsif ($k eq 'dataset') {
                    $item->{"dataset"} = $ds->{name} // "#$ds->{seq}";
                } elsif ($k eq 'participant') {
                    $item->{"participant"} = $p->{name} // $p->{_name} // "#$p->{seq}";
                } else {
                    $item->{"item_$k"} = $h->{$k};
                }
            }
            if ($ds->{args}) {
                for my $k (keys %$h_args) {
                    $item->{"arg_$k"} = $h_args->{$k};
                }
            }

            # skip duplicate items
            my $key = dmp [map { $item->{$_} }
                               grep { !/^_/ }
                               sort keys %$item];
            $log->tracef("item key=%s", $key);
            if ($item_mems{$key}++) {
                $log->tracef("Duplicate key, skipped item, recycling seq number %d", $item_seq);
                next ITER;
            }

            $item->{seq} = $item_seq++;

            push @$items, $item;

            last ITER_ARGS unless $ds->{args};
        } # ITER_ARGS

    } # ITER

    # give each item a convenient name, which is a short combination of its
    # permutation (unnecessarily unique, just as a human-readable name)
    {
        last unless @$items;
        require TableData::Object::aohos;
        my $td = TableData::Object::aohos->new($items);
        my @const_cols = $td->const_col_names;

        my @name_keys;
        for my $k (sort keys %{$items->[0]}) {
            next unless $k =~ /^(participant|dataset|item_.+|arg_.+)$/;
            next if grep {$k eq $_} @const_cols;
            push @name_keys, $k;
        }

        for my $it (@$items) {
            $it->{_name} = join(" ", map {"$_=$it->{$_}"}
                                    @name_keys);
        }
    }

    $items = _filter_records(
        entity => 'item',
        records => $items,
        include => $pargs->{include_items},
        exclude => $pargs->{exclude_items},
        include_name => $pargs->{include_item_names},
        exclude_name => $pargs->{exclude_item_names},
        include_seq => $pargs->{include_item_seqs},
        exclude_seq => $pargs->{exclude_item_seqs},
        include_pattern => $pargs->{include_item_pattern},
        exclude_pattern => $pargs->{exclude_item_pattern},
    ) if $apply_filters;

    [200, "OK", $items];
}

sub _complete_scenario_module {
    my %args = @_;
    my $word    = $args{word} // '';
    my $cmdline = $args{cmdline};
    my $r       = $args{r};

    return undef unless $cmdline;

    # force reading config file
    $r->{read_config} = 1;
    my $res = $cmdline->parse_argv($r);
    my $args = $res->[2];

    require Complete::Module;
    {
        local @INC = @INC;
        unshift @INC, $_ for @{ $args->{include_path} // [] };
        Complete::Module::complete_module(
            word=>$args{word}, ns_prefix=>'Bencher::Scenario');
    }
}

sub _complete_participant_module {
    my %args = @_;
    my $word    = $args{word} // '';
    my $cmdline = $args{cmdline};
    my $r       = $args{r};

    return undef unless $cmdline;

    # force reading config file
    $r->{read_config} = 1;
    my $res = $cmdline->parse_argv($r);

    my $args = $res->[2];
    my $unparsed = _get_scenario(parent_args=>$args);
    my $parsed = _parse_scenario(
        scenario=>$unparsed,
        parent_args=>$args,
        apply_filters => $args{apply_filters},
    );

    my @modules = _get_participant_modules($parsed);

    require Complete::Util;
    Complete::Util::complete_array_elem(
        word  => $word,
        array => \@modules,
    );
}

sub _complete_function {
    my %args = @_;
    my $word    = $args{word} // '';
    my $cmdline = $args{cmdline};
    my $r       = $args{r};

    return undef unless $cmdline;

    # force reading config file
    $r->{read_config} = 1;
    my $res = $cmdline->parse_argv($r);

    my $args = $res->[2];
    my $unparsed = _get_scenario(parent_args=>$args);
    my $parsed = _parse_scenario(
        scenario=>$unparsed,
        parent_args=>$args,
        apply_filters => $args{apply_filters},
    );

    my @functions = _get_participant_functions($parsed);

    require Complete::Util;
    Complete::Util::complete_array_elem(
        word  => $word,
        array => \@functions,
    );
}

sub _complete_participant {
    my %args = @_;
    my $word    = $args{word} // '';
    my $cmdline = $args{cmdline};
    my $r       = $args{r};

    return undef unless $cmdline;

    # force reading config file
    $r->{read_config} = 1;
    my $res = $cmdline->parse_argv($r);

    my $args = $res->[2];
    my $unparsed = _get_scenario(parent_args=>$args);
    my $parsed = _parse_scenario(
        scenario=>$unparsed,
        parent_args=>$args,
        apply_filters => $args{apply_filters},
    );

    require Complete::Util;
    Complete::Util::complete_array_elem(
        word  => $word,
        array => [grep {defined}
                      map {($_->{seq}, $_->{name}, $_->{_name})}
                      @{$parsed->{participants}}],
    );
}

sub _complete_participant_tags {
    my %args = @_;
    my $word    = $args{word} // '';
    my $cmdline = $args{cmdline};
    my $r       = $args{r};

    return undef unless $cmdline;

    # force reading config file
    $r->{read_config} = 1;
    my $res = $cmdline->parse_argv($r);

    my $args = $res->[2];
    my $unparsed = _get_scenario(parent_args=>$args);
    my $parsed = _parse_scenario(
        scenario=>$unparsed,
        parent_args=>$args,
        apply_filters => $args{apply_filters},
    );

    my %tags;
    for my $p (@{ $parsed->{participants} }) {
        if ($p->{tags}) {
            $tags{$_}++ for @{ $p->{tags} };
        }
    }

    require Complete::Util;
    Complete::Util::complete_array_elem(
        word  => $word,
        array => [keys %tags],
    );
}

sub _complete_dataset {
    my %args = @_;
    my $word    = $args{word} // '';
    my $cmdline = $args{cmdline};
    my $r       = $args{r};

    return undef unless $cmdline;

    # force reading config file
    $r->{read_config} = 1;
    my $res = $cmdline->parse_argv($r);

    my $args = $res->[2];
    my $unparsed = _get_scenario(parent_args=>$args);
    my $parsed = _parse_scenario(
        scenario=>$unparsed,
        parent_args=>$args,
        apply_filters => $args{apply_filters},
    );

    require Complete::Util;
    Complete::Util::complete_array_elem(
        word  => $word,
        array => [grep {defined}
                      map {($_->{seq}, $_->{name}, $_->{_name})}
                      @{$parsed->{datasets}}],
    );
}

sub _complete_dataset_tags {
    my %args = @_;
    my $word    = $args{word} // '';
    my $cmdline = $args{cmdline};
    my $r       = $args{r};

    return undef unless $cmdline;

    # force reading config file
    $r->{read_config} = 1;
    my $res = $cmdline->parse_argv($r);

    my $args = $res->[2];
    my $unparsed = _get_scenario(parent_args=>$args);
    my $parsed = _parse_scenario(
        scenario=>$unparsed,
        parent_args=>$args,
        apply_filters => $args{apply_filters},
    );

    my %tags;
    for my $p (@{ $parsed->{datasets} }) {
        if ($p->{tags}) {
            $tags{$_}++ for @{ $p->{tags} };
        }
    }

    require Complete::Util;
    Complete::Util::complete_array_elem(
        word  => $word,
        array => [keys %tags],
    );
}

sub _complete_item {
    my %args = @_;
    my $word    = $args{word} // '';
    my $cmdline = $args{cmdline};
    my $r       = $args{r};

    return undef unless $cmdline;

    # force reading config file
    $r->{read_config} = 1;
    my $res = $cmdline->parse_argv($r);

    my $args = $res->[2];
    my $unparsed = _get_scenario(parent_args=>$args);
    my $parsed = _parse_scenario(
        scenario=>$unparsed,
        parent_args=>$args,
        apply_filters => $args{apply_filters},
    );
    $res = _gen_items(
        scenario=>$parsed,
        parent_args=>$args,
        apply_filters => $args{apply_filters},
    );
    return undef unless $res->[0] == 200;
    my $items = $res->[2];

    require Complete::Util;
    Complete::Util::complete_array_elem(
        word  => $word,
        array => [map {($_->{seq}, $_->{name}, $_->{_name})} @$items],
    );
}

# list installed perls, and check each perl if Bencher::Backend is installed
sub _list_perls {
    require Capture::Tiny;

    eval { require App::perlbrew; 1 };
    return undef if $@;

    my $pb = App::perlbrew->new;
    my @perls = $pb->installed_perls;
    for my $perl (@perls) {
        my @cmd = (
            $perl->{executable},
            "-MBencher::Backend",
            "-e'print \$Bencher::Backend::VERSION'",
        );
        my ($stdout, $stderr, @res) =
            &Capture::Tiny::capture(sub { system @cmd });
        if ($stderr || $?) {
            $perl->{has_bencher} = 0;
            $perl->{bencher_version} = undef;
        } else {
            $perl->{has_bencher} = 1;
            $perl->{bencher_version} = $stdout;
        }
    }

    @perls;
}

sub _complete_perl {
    no warnings 'once';
    require Complete::Util;

    my %args = @_;
    my $word    = $args{word} // '';

    my @perls = _list_perls();

    local $Complete::Common::OPT_FUZZY = 0;
    Complete::Util::complete_array_elem(
        word => $word,
        array => [map {$_->{name}} grep {$_->{has_bencher}} @perls],
    );
}

my $_alias_spec_add_dataset = {
    summary => 'Add a dataset',
    code => sub {
        require JSON::MaybeXS;

        my $args = shift;
        push @{ $args->{datasets} },
            JSON::MaybeXS::decode_json($_[0]);
    },
};

my $_alias_spec_add_participant = {
    summary => 'Add a participant',
    code => sub {
        require JSON::MaybeXS;

        my $args = shift;
        push @{ $args->{participants} },
            JSON::MaybeXS::decode_json($_[0]);
    },
};

$SPEC{format_result} = {
    v => 1.1,
    summary => 'Format bencher result',
    args => {
        envres => {
            summary => 'Enveloped result from bencher',
            schema => 'array*', # XXX envres
            req => 1,
            pos => 0,
        },
        formatters => {
            summary => 'Formatters specification',
            schema => ['array*', of=>[
                'any*', of=>[
                    'str*',
                    ['array*', len=>2, elems=>['str*', 'hash*']],
                ]
            ]],
            req => 1,
            pos => 1,
        },
    },
    args_as => 'array',
};
sub format_result {
    require POSIX;

    my ($envres, $formatters, $opts) = @_;

    $opts //= {};

    $formatters //= [
        'AddVsSlowestField',
        ['Sort', {by=>$opts->{sort}}],
        'ScaleTime',
        'ScaleRate',
        'ScaleSize',
        'RoundNumbers',
        ($envres->[3]{'func.module_startup'} ? ('ModuleStartup') : ()),
        'DeleteConstantFields',
        'DeleteNotesFieldIfEmpty',
        'DeleteSeqField',

        'RenderAsTextTable',
    ];

    # load all formatter modules
    my @fmtobjs;
    for my $fmt (@$formatters) {
        my ($fmtname, $fmtargs);
        if (ref($fmt)) {
            $fmtname = $fmt->[0];
            $fmtargs = $fmt->[1];
        } else {
            $fmtname = $fmt;
            $fmtargs = {};
        }
        my $fmtmod = "Bencher::Formatter::$fmtname";
        my $fmtmod_pm = $fmtmod; $fmtmod_pm =~ s!::!/!g; $fmtmod_pm .= ".pm";
        require $fmtmod_pm;
        push @fmtobjs, $fmtmod->new(%$fmtargs);
    }

    # run all munge_result()
    for my $fmtobj (@fmtobjs) {
        next unless $fmtobj->can("munge_result");
        $fmtobj->munge_result($envres);
    }

    # return the first render_result()
    for my $fmtobj (@fmtobjs) {
        next unless $fmtobj->can("render_result");
        return $fmtobj->render_result($envres);
    }
}

$SPEC{bencher} = {
    v => 1.1,
    summary => 'A benchmark framework',
    description => <<'_',

Bencher is a benchmark framework. You specify a *scenario* (either in a
`Bencher::Scenario::*` Perl module, or a Perl script, or over the command-line)
containing list of *participants* and *datasets*. Participants are codes or
commands to run, and datasets are arguments for the codes/commands. Bencher will
permute the participants and datasets into benchmark items, ready to run.

You can choose to include only some participants, datasets, or items. And there
are options to view your scenario's participants/datasets/items/mentioned
modules, run benchmark against multiple perls and module versions, and so on.
Bencher comes as a CLI script as well as Perl module. See the `Bencher::Backend`
documentation for more information.

_
    args_rels => {
        # XXX precision & precision_limit is only relevant when action=bench
        # XXX note is only relevant when action=bench
        # XXX sort is only relevant when action=bench and format=text
        # XXX include_perls & exclude_perls are only relevant when multiperl=1
    },
    args => {
        scenario_file => {
            summary => 'Load a scenario from a Perl file',
            description => <<'_',

Perl file will be do()'ed and the last expression should be a hash containing
the scenario specification.

_
            schema => ['str*'],
            cmdline_aliases => {f=>{}},
        },
        scenario_module => {
            summary => 'Load a scenario from a Bencher::Scenario:: Perl module',
            description => <<'_',

Will try to load module `Bencher::Scenario::<NAME>` and expect to find a package
variable in the module called `$scenario` which should be a hashref containing
the scenario specification.

_
            schema => ['str*', match=>qr!\A\w+((?:::|/)\w+)*\z!],
            cmdline_aliases => {m=>{}},
            completion => sub { _complete_scenario_module(@_) },
        },
        participants => {
            'summary' => 'Add participants',
            'x.name.is_plural' => 1,
            schema => ['array*', of=>['hash*']],
            cmdline_aliases => {
                participant => $_alias_spec_add_participant,
                p => $_alias_spec_add_participant,
            },
        },
        datasets => {
            summary => 'Add datasets',
            'x.name.is_plural' => 1,
            schema => ['array*', of=>['hash*']],
            cmdline_aliases => {
                dataset => $_alias_spec_add_dataset,
                d => $_alias_spec_add_dataset,
            },
        },
        precision => {
            summary => 'Precision, will be passed to Benchmark::Dumb',
            description => <<'_',

This setting overrides `default_precision` property in the scenario.

_
            schema => ['float*', min=>0],
        },
        precision_limit => {
            summary => 'Set maximum (=smallest number) precision',
            description => <<'_',

Instead of setting `precision` which forces a single value, you can also set
this `precision_limit` setting. If the precision in the scenario is higher
(=number is smaller) than this limit, then this limit is used. For example, if
the scenario specifies `default_precision` 0.001 and `precision_limit` is set to
0.005 then 0.005 is used.

This setting is useful on slower computers which might not be able to reach the
required precision before hitting maximum number of iterations.

_
            schema => ['float*', between=>[0,1]],
        },
        action => {
            schema => ['str*', {
                in=>[qw/
                           list-perls
                           list-scenario-modules
                           show-scenario
                           list-participants
                           list-participant-modules
                           list-datasets
                           list-items
                           show-items-codes
                           show-items-results
                           show-items-results-sizes
                           show-items-outputs
                           bench
                       /]
                    # list-functions
            }],
            default => 'bench',
            cmdline_aliases => {
                a => {},
                list_perls => {
                    is_flag => 1,
                    summary => 'Shortcut for -a list-perls',
                    code => sub { $_[0]{action} = 'list-perls' },
                },
                list_scenario_modules => {
                    is_flag => 1,
                    summary => 'Shortcut for -a list-scenario-modules',
                    code => sub { $_[0]{action} = 'list-scenario-modules' },
                },
                L => {
                    is_flag => 1,
                    summary => 'Shortcut for -a list-scenario-modules',
                    code => sub { $_[0]{action} = 'list-scenario-modules' },
                },
                show_scenario => {
                    is_flag => 1,
                    summary => 'Shortcut for -a show-scenario',
                    code => sub { $_[0]{action} = 'show-scenario' },
                },
                list_participants => {
                    is_flag => 1,
                    summary => 'Shortcut for -a list-participants',
                    code => sub { $_[0]{action} = 'list-participants' },
                },
                list_participant_modules => {
                    is_flag => 1,
                    summary => 'Shortcut for -a list-participant-modules',
                    code => sub { $_[0]{action} = 'list-participant-modules' },
                },
                list_datasets => {
                    is_flag => 1,
                    summary => 'Shortcut for -a list-datasets',
                    code => sub { $_[0]{action} = 'list-datasets' },
                },
                list_permutes => {
                    is_flag => 1,
                    summary => 'Shortcut for -a list-permutes',
                    code => sub { $_[0]{action} = 'list-permutes' },
                },
                list_items => {
                    is_flag => 1,
                    summary => 'Shortcut for -a list-items',
                    code => sub { $_[0]{action} = 'list-items' },
                },
                show_items_codes => {
                    is_flag => 1,
                    summary => 'Shortcut for -a show-items-codes',
                    code => sub { $_[0]{action} = 'show-items-codes' },
                },
                show_items_results => {
                    is_flag => 1,
                    summary => 'Shortcut for -a show-items-results',
                    code => sub { $_[0]{action} = 'show-items-results' },
                },
                show_items_results_sizes => {
                    is_flag => 1,
                    summary => 'Shortcut for -a show-items-results-sizes',
                    code => sub { $_[0]{action} = 'show-items-results-sizes' },
                },
                show_items_outputs => {
                    is_flag => 1,
                    summary => 'Shortcut for -a show-items-outputs',
                    code => sub { $_[0]{action} = 'show-items-outputs' },
                },
            },
            tags => ['category:action'],
        },
        raw => {
            summary => 'Show "raw" data',
            schema => ['bool'],
            description => <<'_',

When action=show-items-result, will print result as-is instead of dumping as
Perl.

_
        },
        module_startup => {
            schema => ['bool*', is=>1],
            summary => 'Benchmark module startup overhead instead of normal benchmark',
            tags => ['category:action'],
        },
        detail => {
            schema => ['bool*'],
            cmdline_aliases => {l=>{}},
        },

        include_modules => {
            'x.name.is_plural' => 1,
            summary => 'Only include modules specified in this list',
            'summary.alt.plurality.singular' => 'Add module to include list',
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_participant_module(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },
        include_module_pattern => {
            summary => 'Only include modules matching this regex pattern',
            schema => ['re*'],
            tags => ['category:filtering'],
        },
        exclude_modules => {
            'x.name.is_plural' => 1,
            summary => 'Exclude modules specified in this list',
            'summary.alt.plurality.singular' => 'Add module to exclude list',
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_participant_module(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },
        exclude_module_pattern => {
            summary => 'Exclude module(s) matching this regex pattern',
            schema => ['re*'],
            tags => ['category:filtering'],
        },

        include_functions => {
            'x.name.is_plural' => 1,
            summary => 'Only include functions specified in this list',
            'summary.alt.plurality.singular' => 'Add function to include list',
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_function(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },
        include_function_pattern => {
            summary => 'Only include functions matching this regex pattern',
            schema => ['re*'],
            tags => ['category:filtering'],
        },
        exclude_functions => {
            'x.name.is_plural' => 1,
            summary => 'Exclude functions specified in this list',
            'summary.alt.plurality.singular' => 'Add function to exclude list',
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_function(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },
        exclude_function_pattern => {
            summary => 'Exclude function(s) matching this regex pattern',
            schema => ['re*'],
            tags => ['category:filtering'],
        },

        include_participants => {
            'x.name.is_plural' => 1,
            summary => 'Only include participants whose seq/name matches this',
            'summary.alt.plurality.singular' => 'Add participant (by name/seq) to include list',
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_participant(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },
        include_participant_names => {
            'x.name.is_plural' => 1,
            summary => 'Only include participants whose name matches this',
            'summary.alt.plurality.singular' => 'Add participant (by name) to include list',
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_participant(@_, apply_filters=>0, seq=>0) },
            tags => ['category:filtering'],
        },
        include_participant_seqs => {
            'x.name.is_plural' => 1,
            'x.name.singular' => 'include_participant_seq',
            summary => 'Only include participants whose sequence number matches this',
            'summary.alt.plurality.singular' => 'Add participant (by sequence number) to include list',
            schema => ['array*', of=>['int*', min=>0]],
            element_completion => sub { _complete_participant(@_, apply_filters=>0, name=>0) },
            tags => ['category:filtering'],
        },
        include_participant_pattern => {
            summary => 'Only include participants matching this regex pattern',
            schema => ['re*'],
            tags => ['category:filtering'],
        },
        include_participant_tags => {
            'x.name.is_plural' => 1,
            summary => 'Only include participants whose tag matches this',
            'summary.alt.plurality.singular' => 'Add a tag to participants include tag list',
            description => <<'_',

You can specify `A & B` to include participants that have _both_ tags A and B.

_
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_participant_tags(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },
        exclude_participants => {
            'x.name.is_plural' => 1,
            summary => 'Exclude participants whose seq/name matches this',
            'summary.alt.plurality.singular' => 'Add participant (by name/seq) to exclude list',
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_participant(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },
        exclude_participant_names => {
            'x.name.is_plural' => 1,
            summary => 'Exclude participants whose name matches this',
            'summary.alt.plurality.singular' => 'Add participant (by name) to exclude list',
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_participant(@_, apply_filters=>0, seq=>0) },
            tags => ['category:filtering'],
        },
        exclude_participant_seqs => {
            'x.name.is_plural' => 1,
            'x.name.singular' => 'exclude_participant_seq',
            summary => 'Exclude participants whose sequence number matches this',
            'summary.alt.plurality.singular' => 'Add participant (by sequence number) to exclude list',
            schema => ['array*', of=>['int*', min=>0]],
            element_completion => sub { _complete_participant(@_, apply_filters=>0, name=>0) },
            tags => ['category:filtering'],
        },
        exclude_participant_pattern => {
            summary => 'Exclude participants matching this regex pattern',
            schema => ['re*'],
            tags => ['category:filtering'],
        },
        exclude_participant_tags => {
            'x.name.is_plural' => 1,
            summary => 'Exclude participants whose tag matches this',
            'summary.alt.plurality.singular' => 'Add a tag to participants exclude tag list',
            description => <<'_',

You can specify `A & B` to exclude participants that have _both_ tags A and B.

_
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_participant_tags(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },

        include_items => {
            'x.name.is_plural' => 1,
            summary => 'Only include items whose seq/name matches this',
            'summary.alt.plurality.singular' => 'Add item (by name/seq) to include list',
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_item(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },
        include_item_names => {
            'x.name.is_plural' => 1,
            summary => 'Only include items whose name matches this',
            'summary.alt.plurality.singular' => 'Add item (by name) to include list',
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_item(@_, apply_filters=>0, seq=>0) },
            tags => ['category:filtering'],
        },
        include_item_seqs => {
            'x.name.is_plural' => 1,
            'x.name.singular' => 'include_item_seq',
            summary => 'Only include items whose sequence number matches this',
            'summary.alt.plurality.singular' => 'Add item (by sequence number) to include list',
            schema => ['array*', of=>['int*', min=>0]],
            element_completion => sub { _complete_item(@_, apply_filters=>0, name=>0) },
            tags => ['category:filtering'],
        },
        include_item_pattern => {
            summary => 'Only include items matching this regex pattern',
            schema => ['re*'],
            tags => ['category:filtering'],
        },
        exclude_items => {
            'x.name.is_plural' => 1,
            summary => 'Exclude items whose seq/name matches this',
            'summary.alt.plurality.singular' => 'Add item (by name/seq) to exclude list',
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_item(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },
        exclude_item_names => {
            'x.name.is_plural' => 1,
            summary => 'Exclude items whose name matches this',
            'summary.alt.plurality.singular' => 'Add item (by name) to exclude list',
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_item(@_, apply_filters=>0, seq=>0) },
            tags => ['category:filtering'],
        },
        exclude_item_seqs => {
            'x.name.is_plural' => 1,
            'x.name.singular' => 'exclude_item_seq',
            summary => 'Exclude items whose sequence number matches this',
            'summary.alt.plurality.singular' => 'Add item (by sequence number) to exclude list',
            schema => ['array*', of=>['int*', min=>0]],
            element_completion => sub { _complete_item(@_, apply_filters=>0, name=>0) },
            tags => ['category:filtering'],
        },
        exclude_item_pattern => {
            summary => 'Exclude items matching this regex pattern',
            schema => ['re*'],
            tags => ['category:filtering'],
        },

        include_datasets => {
            'x.name.is_plural' => 1,
            summary => 'Only include datasets whose seq/name matches this',
            'summary.alt.plurality.singular' => 'Add dataset (by name/seq) to include list',
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_dataset(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },
        include_dataset_names => {
            'x.name.is_plural' => 1,
            summary => 'Only include datasets whose name matches this',
            'summary.alt.plurality.singular' => 'Add dataset (by name) to include list',
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_dataset(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },
        include_dataset_seqs => {
            'x.name.is_plural' => 1,
            'x.name.singular' => 'include_dataset_seq',
            summary => 'Only include datasets whose sequence number matches this',
            'summary.alt.plurality.singular' => 'Add dataset (by sequence number) to include list',
            schema => ['array*', of=>['int*', min=>0]],
            element_completion => sub { _complete_dataset(@_, apply_filters=>0, name=>0) },
            tags => ['category:filtering'],
        },
        include_dataset_pattern => {
            summary => 'Only include datasets matching this regex pattern',
            schema => ['re*'],
            tags => ['category:filtering'],
        },
        exclude_datasets => {
            'x.name.is_plural' => 1,
            summary => 'Exclude datasets whose seq/name matches this',
            'summary.alt.plurality.singular' => 'Add dataset (by name/seq) to exclude list',
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_dataset(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },
        exclude_dataset_names => {
            'x.name.is_plural' => 1,
            summary => 'Exclude datasets whose name matches this',
            'summary.alt.plurality.singular' => 'Add dataset (by name) to exclude list',
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_dataset(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },
        exclude_dataset_seqs => {
            'x.name.is_plural' => 1,
            'x.name.singular' => 'exclude_dataset_seq',
            summary => 'Exclude datasets whose sequence number matches this',
            'summary.alt.plurality.singular' => 'Add dataset (by sequence number) to exclude list',
            schema => ['array*', of=>['int*', min=>0]],
            element_completion => sub { _complete_dataset(@_, apply_filters=>0, name=>0) },
            tags => ['category:filtering'],
        },
        exclude_dataset_pattern => {
            summary => 'Exclude datasets matching this regex pattern',
            schema => ['re*'],
            tags => ['category:filtering'],
        },
        include_dataset_tags => {
            'x.name.is_plural' => 1,
            summary => 'Only include datasets whose tag matches this',
            'summary.alt.plurality.singular' => 'Add a tag to dataset include tag list',
            description => <<'_',

You can specify `A & B` to include datasets that have _both_ tags A and B.

_
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_dataset_tags(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },
        exclude_dataset_tags => {
            'x.name.is_plural' => 1,
            summary => 'Exclude datasets whose tag matches this',
            'summary.alt.plurality.singular' => 'Add a tag to dataset exclude tag list',
            description => <<'_',

You can specify `A & B` to exclude datasets that have _both_ tags A and B.

_
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_dataset_tags(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },
        include_dataset_tags => {
            'x.name.is_plural' => 1,
            summary => 'Only include datasets whose tag matches this',
            'summary.alt.plurality.singular' => 'Add a tag to dataset include tag list',
            description => <<'_',

You can specify `A & B` to include datasets that have _both_ tags A and B.

_
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_dataset_tags(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },
        multiperl => {
            summary => 'Benchmark against multiple perls',
            schema => ['bool'],
            default => 0,
            description => <<'_',

Requires `App::perlbrew` to be installed. Will use installed perls from the
perlbrew installation. Each installed perl must have `Bencher::Backend` module
installed (in addition to having all modules that you want to benchmark,
obviously).

By default, only perls having Bencher::Backend will be included. Use
`--include-perl` and `--exclude-perl` to include and exclude which perls you
want.

Also note that due to the way this is currently implemented, benchmark code that
contains closures (references to variables outside the code) won't work.

_
        },
        include_perls => {
            'x.name.is_plural' => 1,
            'x.name.singular' => 'include_perl',
            summary => 'Only include some perls',
            'summary.alt.plurality.singular' => 'Add specified perl to include list',
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_perl(@_) },
            tags => ['category:filtering'],
        },
        exclude_perls => {
            'x.name.is_plural' => 1,
            'x.name.singular' => 'exclude_perl',
            summary => 'Exclude some perls',
            'summary.alt.plurality.singular' => 'Add specified perl to exclude list',
            schema => ['array*', of=>['str*']],
            element_completion => sub { _complete_perl(@_) },
            tags => ['category:filtering'],
        },

        multimodver => {
            summary => 'Benchmark multiple module versions',
            schema => ['str*'],
            description => <<'_',

If set to a module name, will search for all (instead of the first occurrence)
of the module in `@INC`. Then will generate items for each version.

Currently only one module can be multi version.

_
            completion => sub { _complete_participant_module(@_, apply_filters=>0) },
        },
        include_path => {
            summary => 'Additional module search paths',
            'summary.alt.plurality.singular' => 'Add path to module search path',
            schema => ['array*', of=>['str*']],
            description => <<'_',

Used when searching for scenario module, or when in multimodver mode.

_
            cmdline_aliases => {I=>{}},
        },
        # XXX include-mod-version
        # XXX exclude-mod-version

        on_failure => {
            summary => "What to do when there is a failure",
            schema => ['str*', in=>[qw/die skip/]],
            description => <<'_',

For a command participant, failure means non-zero exit code. For a Perl-code
participant, failure means Perl code dies or (if expected result is specified)
the result is not equal to the expected result.

The default is "die". When set to "skip", will first run the code of each item
before benchmarking and trap command failure/Perl exception and if that happens,
will "skip" the item.

_
        },
        on_result_failure => {
            summary => "What to do when there is a result failure",
            schema => ['str*', in=>[qw/die skip warn/]],
            description => <<'_',

This is like `on_failure` except that it specifically refer to the failure of
item's result not being equal to expected result.

There is an extra choice of `warn` for this type of failure, which is to print a
warning to STDERR and continue.

_
        },

        sort => {
            schema => ['array*', of=>['str*'], min_len=>1],
            default => ['-time'],
        },

        include_result_size => {
            summary => "Also return memory usage of each item code's result (return value)",
            schema => 'bool',
            description => <<'_',

Memory size is measured using `Devel::Size`.

_
        },

        capture_stdout => {
            summary => 'Trap output to stdout',
            schema => 'bool',
        },
        capture_stderr => {
            summary => 'Trap output to stderr',
            schema => 'bool',
        },

        include_process_size => {
            summary => "Also return process size information for each item",
            schema => 'bool',
            description => <<'_',

This is done by dumping each item's code into a temporary file and running the
file with a new perl interpreter process and measuring the process size at the
end (so it does not need to load Bencher itself or the other items). Currently
only works on Linux because process size information is retrieved from
`/proc/PID/smaps`. Not all code can work, e.g. if the code tries to access a
closure or outside data or extra modules (modules not specified in the
participant or loaded by the code itself). Usually does not make sense to use
this on external command participants.

_
        },

        return_meta => {
            summary => 'Whether to return extra metadata',
            description => <<'_',

When set to true, will return extra metadata such as platform information, CPU
information, system load before & after the benchmark, system time, and so on.
This is put in result metadata under `func.*` keys.

The default is to true (return extra metadata) unless when run as CLI and format
is text (where the extra metadata is not shown).

_
            schema => ['bool'],
            tags => ['category:result'],
        },

        save_result => {
            summary => 'Whether to save benchmark result to file',
            schema => 'bool*',
            description => <<'_',

Will also be turned on automatically if `BENCHER_RESULT_DIR` environment
variabl is defined.

When this is turned on, will save a JSON file after benchmark, containing the
result along with metadata. The directory of the JSON file will be determined
from the `results_dir` option, while the filename from the `results_filename`
option.

_
        },

        result_dir => {
            summary => 'Directory to use when saving benchmark result',
            schema => 'str*',
            'x.schema.entity' => 'dirname',
            tags => ['category:result'],
            description => <<'_',

Default is from `BENCHER_RESULT_DIR` environment variable, or the home
directory.

_
        },
        result_filename => {
            summary => 'Filename to use when saving benchmark result',
            schema => 'str*',
            'x.schema.entity' => 'filename',
            tags => ['category:result'],
            description => <<'_',

Default is:

    <NAME>.<yyyy-dd-dd-"T"HH-MM-SS>.json

or, when running in module startup mode:

    <NAME>.module_startup.<yyyy-dd-dd-"T"HH-MM-SS>.json

where <NAME> is scenario module name, or `NO_MODULE` if scenario is not from a
module. The `::` (double colon in the module name will be replaced with `-`
(dash).

_
        },

        note => {
            summary => 'Put additional note in the result',
            schema => ['str*'],
            tags => ['category:result'],
        },
    },
};
sub bencher {
    my %args = @_;

    my $action = $args{action};
    my $envres;

    my $is_cli_and_text_format;
    {
        my $r = $args{-cmdline_r};
        $is_cli_and_text_format = 1 if $r && ($r->{format} // 'text') =~ /text/;
    }

    if ($action eq 'list-perls') {
        my @perls = _list_perls();
        my @res;
        for my $perl (@perls) {
            if ($args{detail}) {
                push @res, {
                    name            => $perl->{name},
                    version         => $perl->{version},
                    has_bencher     => $perl->{has_bencher},
                    bencher_version => $perl->{bencher_version},
                };
            } else {
                push @res, $perl->{name};
            }
        }
        my %resmeta;
        $resmeta{'table.fields'} = [
            'name',
            'version',
            'has_bencher',
            'bencher_version',
        ] if $args{detail};
        $envres =
            [200, "OK", \@res, \%resmeta];
        goto L_END;
    }

    if ($action eq 'list-scenario-modules') {
        require PERLANCAR::Module::List;
        local @INC = @INC;
        unshift @INC, $_ for @{ $args{include_path} // [] };
        my $mods = PERLANCAR::Module::List::list_modules(
            'Bencher::Scenario::', {list_modules=>1, recurse=>1});
        $envres =
            [200, "OK",
             [map {s/^Bencher::Scenario:://; $_} sort keys %$mods]];
        goto L_END;
    }

    my $unparsed = _get_scenario(parent_args=>\%args);

    if ($action eq 'show-scenario') {
        $envres = [200, "OK", $unparsed];
        goto L_END;
    }

    my $stash = {};

    my $aibdf;
    $aibdf = 0 if $action =~ /\A(list-(datasets|participants))\z/;

    if ($unparsed->{before_parse_scenario}) {
        $log->infof("Executing before_parse_scenario hook ...");
        $unparsed->{before_parse_scenario}->(
            hook_name => 'before_parse_scenario',
            scenario  => $unparsed,
            stash     => $stash,
        );
    }

    my $parsed = _parse_scenario(
        scenario=>$unparsed,
        parent_args=>\%args,
        apply_include_by_default_filter => $aibdf,
    );

    if ($parsed->{after_parse_scenario}) {
        $log->infof("Executing after_parse_scenario hook ...");
        $parsed->{after_parse_scenario}->(
            hook_name => 'after_parse_scenario',
            scenario  => $parsed,
            stash     => $stash,
        );
    }

    my $module_startup = $args{module_startup} // $parsed->{module_startup};

    if ($parsed->{before_list_datasets}) {
        $log->infof("Executing before_list_datasets hook ...");
        $parsed->{before_list_datasets}->(
            hook_name => 'before_list_datasets',
            scenario  => $parsed,
            stash     => $stash,
        );
    }

    if ($action eq 'list-datasets') {
        unless ($parsed->{datasets}) {
            $envres = [200, "OK", undef];
            goto L_END;
        }
        my @res;
        my $has_summary = 0;
        for my $ds (@{ $parsed->{datasets} }) {
            if ($args{detail}) {
                my $rec = {
                    seq      => $ds->{seq},
                    include_by_default => $ds->{include_by_default},
                    name     => $ds->{name},
                    tags     => join(", ", @{ $ds->{tags} // []}),
                };
                if (defined $ds->{summary}) {
                    $has_summary = 1;
                    $rec->{summary} = $ds->{summary};
                }
                push @res, $rec;
            } else {
                push @res, $ds->{name};
            }
        }
        my %resmeta;
        $resmeta{'table.fields'} = [
            'seq',
            'include_by_default',
            'name',
            ('summary') x $has_summary,
            'tags',
        ]
            if $args{detail};
        $envres = [200, "OK", \@res, \%resmeta];
        goto L_END;
    }

    if ($parsed->{before_list_participants}) {
        $log->infof("Executing before_list_participants hook ...");
        $parsed->{before_list_participants}->(
            hook_name => 'before_list_participants',
            scenario  => $parsed,
            stash     => $stash,
        );
    }

    if ($action eq 'list-participant-modules') {
        my @modules = _get_participant_modules($parsed);
        $envres = [200, "OK", \@modules];
        goto L_END;
    }

    if ($action eq 'list-participants') {
        my @res;
        my $has_summary = 0;
        for my $p (@{ $parsed->{participants} }) {

            my $cmdline;
            if ($p->{cmdline_template}) {
                $cmdline = "#TEMPLATE: ".
                    (ref($p->{cmdline_template}) eq 'ARRAY' ? join(" ", @{$p->{cmdline_template}}) : $p->{cmdline_template});
            } elsif ($p->{cmdline}) {
                $cmdline =
                    (ref($p->{cmdline}) eq 'ARRAY' ? join(" ", @{$p->{cmdline}}) : $p->{cmdline});
            } elsif ($p->{perl_cmdline_template}) {
                $cmdline = "#TEMPLATE: #perl ".
                    (ref($p->{perl_cmdline_template}) eq 'ARRAY' ? join(" ", @{$p->{perl_cmdline_template}}) : $p->{perl_cmdline_template});
            } elsif ($p->{cmdline}) {
                $cmdline = "#perl ".
                    (ref($p->{perl_cmdline}) eq 'ARRAY' ? join(" ", @{$p->{perl_cmdline}}) : $p->{perl_cmdline});
            }
            my $rec = {
                seq      => $p->{seq},
                type     => $p->{type},
                include_by_default => $p->{include_by_default},
                name     => $p->{name} // $p->{_name},
                function => $p->{function},
                module   => $p->{modules} ? join("+", @{$p->{modules}}) : $p->{module},
                cmdline  => $cmdline,
                tags     => join(", ", @{$p->{tags} // []}),
            };
            if (defined $p->{summary}) {
                $has_summary = 1;
                $rec->{summary} = $p->{summary};
            }
            push @res, $rec;
        }

        unless ($args{detail}) {
            @res = map {$_->{name}} @res;
        }
        my %resmeta;
        $resmeta{'table.fields'} = [
            'seq',
            'type',
            'include_by_default',
            'name',
            ('summary') x $has_summary,
            'module',
            'function',
            'cmdline',
            'tags',
        ]
            if $args{detail};
        $envres = [200, "OK", \@res, \%resmeta];
        goto L_END;
    }

    my $items;
  GEN_ITEMS:
    {
        if ($parsed->{items}) {
            $items = $parsed->{items};
            last;
        }
        if ($parsed->{before_gen_items}) {
            $log->infof("Executing before_gen_items hook ...");
            $parsed->{before_gen_items}->(
                hook_name => 'before_gen_items',
                scenario  => $parsed,
                stash     => $stash,
            );
        }

        my $res = _gen_items(scenario=>$parsed, parent_args=>\%args);
        unless ($res->[0] == 200) {
            $envres = $res;
            goto L_END;
        }
        $items = $res->[2];
    }

    if ($action eq 'list-items') {
        my @rows;
        my @columns;
        for my $it0 (@$items) {
            my $it = {%$it0};
            delete $it->{$_} for grep {/^_/} keys %$it;
            if (!@columns) {
                push @columns, sort keys %$it;
            }
            push @rows, $it;
        }
        unless ($args{detail}) {
            for (@rows) {
                $_ = $_->{seq};
            }
        }
        my %resmeta;
        $resmeta{'table.fields'} = \@columns if $args{detail};
        $envres = [200, "OK", \@rows, \%resmeta];
        goto L_END;
    }

    if ($action eq 'show-items-codes') {
        $envres = [200, "OK", join(
            "",
            map {(
                "#$_->{seq} ($_->{_name}):\n",
                dmp($_->{_code}),
                "\n\n",
            )} @$items
        )];
        goto L_END;
    }

    if ($action =~ /\A(show-items-results-sizes|show-items-results|show-items-outputs|bench)\z/) {
        require Capture::Tiny;
        require Module::Load;
        require Time::HiRes;

        my $participants = $parsed->{participants};
        my $datasets = $parsed->{datasets};
        $envres = [200, "OK", [], {}];

        my $result_dir = $args{result_dir}
            // $ENV{BENCHER_RESULT_DIR} // $ENV{HOME};
        my $save_result = $args{save_result} // !!$result_dir;
        my $return_meta = $args{return_meta} // ($save_result ? 1:undef) //
            (
                $args{-cmdline_r} && (($args{-cmdline_r}{format}//'') !~ /json/) ?
                    0 : 1
                  );
        my $capture_stdout = $args{capture_stdout} // $parsed->{capture_stdout} // 0;
        $capture_stdout = 1 if $action eq 'show-items-outputs';
        my $capture_stderr = $args{capture_stderr} // $parsed->{capture_stderr} // 0;
        $capture_stderr = 1 if $action eq 'show-items-outputs';

        $envres->[3]{'func.module_startup'} = $module_startup;
        $envres->[3]{'func.module_versions'}{perl} = "$^V" if $return_meta;

        my $code_load = sub {
            no strict 'refs';
            my $mod = shift;
            $log->tracef("Loading module: %s", $mod);
            Module::Load::load($mod);
            if ($return_meta) {
                # we'll just use ${"$mod\::VERSION"} because we are already
                # loading the module
                $envres->[3]{'func.module_versions'}{$mod} =
                    ${"$mod\::VERSION"};
            }
        };

        $code_load->('Benchmark::Dumb');
        $code_load->('Devel::Platform::Info') if $return_meta;
        $code_load->('Sys::Info')             if $return_meta;
        $code_load->('Sys::Load')             if $return_meta;

        # load all participant modules
        {
            my %seen;
            my @modules = _get_participant_modules($parsed);
            for my $mod (@modules) {
                $code_load->($mod);
            }
            for my $mod (keys %{$parsed->{modules}}) {
                next if $mod eq 'perl';
                $code_load->($mod);
            }
        }

        # loading extra modules
        $code_load->($_) for @{ $parsed->{extra_modules} // [] };

        if ($parsed->{before_bench}) {
            $log->infof("Executing before_bench hook ...");
            $parsed->{before_bench}->(
                hook_name => 'before_bench',
                scenario  => $parsed,
                stash     => $stash,
            );
        }

        my $include_process_size = $args{include_process_size} //
            $parsed->{include_process_size};

        # test code first
        my $on_failure = $args{on_failure} // $parsed->{on_failure} // 'die';
        my $on_result_failure = $args{on_result_failure} //
            $parsed->{on_result_failure} // $on_failure;
        my $include_result_size = $args{include_result_size} //
            $parsed->{include_result_size} // 0;
        $include_result_size = 1 if $action eq 'show-items-results-sizes';
        $include_result_size = 0 if $module_startup;
        {
            last if $args{multiperl} || $args{multimodver};
            my $fitems = [];
            for my $it (@$items) {
                $log->tracef("Testing code for item #%d (%s) ...",
                             $it->{seq}, $it->{_name});
                eval {
                    my $participant = _find_record_by_seq($participants, $it->{_permute}{participant});
                    my $result_is_list = $participant->{result_is_list} // 0;
                    if ($capture_stdout && $capture_stderr) {
                        my ($stdout, $stderr, @res) = &Capture::Tiny::capture($it->{_code});
                        $it->{_stdout} = $stdout;
                        $it->{_stderr} = $stderr;
                        $it->{_result} = $result_is_list ? \@res : $res[0];
                    } elsif ($capture_stdout) {
                        my ($stdout, @res) = &Capture::Tiny::capture_stdout($it->{_code});
                        $it->{_stdout} = $stdout;
                        $it->{_result} = $result_is_list ? \@res : $res[0];
                    } elsif ($capture_stderr) {
                        my ($stderr, @res) = &Capture::Tiny::capture_stderr($it->{_code});
                        $it->{_stderr} = $stderr;
                        $it->{_result} = $result_is_list ? \@res : $res[0];
                    } else {
                        $it->{_result} = $result_is_list ?
                            [$it->{_code}->()] : $it->{_code}->();
                    }
                };
                my $err = $@;

                if ($err) {
                    if ($on_failure eq 'skip' || $action eq 'show-items-results') {
                        warn "Skipping item #$it->{seq} ($it->{_name}) ".
                            "due to failure: $err\n";
                        next;
                    } else {
                        die "Item #$it->{seq} ($it->{_name}) fails: $err\n";
                    }
                }

                $err = "";
                if (exists $it->{_permute}{dataset}) {
                    my $dataset = _find_record_by_seq($datasets, $it->{_permute}{dataset});
                    if (exists $dataset->{result}) {
                        my $dmp_result = dmp($it->{_result});
                        my $dmp_exp_result = dmp($dataset->{result});
                        if ($dmp_result ne $dmp_exp_result) {
                            $err = "Result ($dmp_result) is not as expected ($dmp_exp_result)";
                        }
                    }
                }

                if ($err) {
                    if ($on_result_failure eq 'skip') {
                        warn "Skipping item #$it->{seq} ($it->{_name}) ".
                            "due to failure (2): $err\n";
                        next;
                    } elsif ($on_result_failure eq 'warn' || $action eq 'show-items-results') {
                        warn "Warning: item #$it->{seq} ($it->{_name}) ".
                            "has failure (2): $err\n";
                    } else {
                        die "Item #$it->{seq} ($it->{_name}) fails (2): $err\n";
                    }
                }
                $it->{_code_error} = $err;

                if ($include_result_size) {
                    require Devel::Size;
                    $it->{_result_size} = Devel::Size::total_size($it->{_result});
                }

                if ($include_process_size) {
                    _get_process_size($parsed, $it);
                }

                push @$fitems, $it;
            }
            $items = $fitems;
        }

        if ($action eq 'show-items-results') {
            die "show-items-results currently not supported on multiperl or multimodver\n" if $args{multiperl} || $args{multimodver};
            if ($is_cli_and_text_format) {
                require Data::Dump;
                $envres->[3]{'cmdline.skip_format'} = 1;
                $envres->[2] = join(
                    "",
                    map {(
                        "#$_->{seq} ($_->{_name}):\n",
                        $args{raw} ? $_->{_result} : Data::Dump::dump($_->{_result}),
                        "\n\n",
                    )} @$items
                );
            } else {
                $envres->[2] = [map {$_->{_result}} @$items];
            }
            goto RETURN_RESULT;
        }

        if ($action eq 'show-items-results-sizes') {
            die "show-items-results currently not supported on multiperl or multimodver\n" if $args{multiperl} || $args{multimodver};
            if ($is_cli_and_text_format) {
                $envres->[3]{'cmdline.skip_format'} = 1;
                $envres->[2] = join(
                    "",
                    map {(
                        "#$_->{seq} ($_->{_name}):\n",
                        $_->{_result_size},
                        "\n\n",
                    )} @$items
                );
            } else {
                $envres->[2] = [map {$_->{_result_size}} @$items];
            }
            goto RETURN_RESULT;
        }

        if ($action eq 'show-items-outputs') {
            die "show-items-outputs currently not supported on multiperl or multimodver\n" if $args{multiperl} || $args{multimodver};
            if ($is_cli_and_text_format) {
                $envres->[3]{'cmdline.skip_format'} = 1;
                $envres->[2] = join(
                    "",
                    map {(
                        "#$_->{seq} ($_->{_name}) stdout (", length($_->{_stdout} // ''), " bytes):\n",
                        ($_->{_stdout} // ''),
                        "\n\n",
                        "#$_->{seq} ($_->{_name}) stderr (", length($_->{_stderr} // ''), " bytes):\n",
                        ($_->{_stderr} // ''),
                        "\n\n",
                    )} @$items
                );
            } else {
                $envres->[2] = [map {$_->{_result_size}} @$items];
            }
            goto RETURN_RESULT;
        }

        my $time_start = Time::HiRes::time();
        if ($return_meta) {
            $envres->[3]{'func.bencher_version'} = $Bencher::VERSION;
            $envres->[3]{'func.bencher_args'} = {
                map {$_=>$args{$_}} grep {!/\A-/} keys %args};
            if ($args{scenario_file}) {
                $envres->[3]{'func.scenario_file'} = $args{scenario_file};
            } elsif (my $mod = $args{scenario_module}) {
                $mod = "Bencher::Scenario::$mod";
                no strict 'refs';
                $envres->[3]{'func.scenario_module'} = $mod;
                $envres->[3]{'func.module_versions'}{$mod} =
                    ${"$mod\::VERSION"};
            }
            $envres->[3]{'func.sysload_before'} = [Sys::Load::getload()];
            $envres->[3]{'func.time_start'} = $time_start;
        }

        my $precision = $args{precision} //
            $parsed->{precision} // $parsed->{default_precision} // 0;
        if (defined($args{precision_limit}) && $precision < $args{precision_limit}) {
            $precision = $args{precision_limit};
        }
        $envres->[3]{'func.precision'} = $precision if $return_meta;

        $log->tracef("Running benchmark (precision=%g) ...", $precision);

        my @columns = (qw/seq participant dataset/);
        my @rows;
        if ($args{multiperl} || $args{multimodver}) {
            require Data::Clone;
            require Devel::Size;
            my %perl_exes;
            my %perl_opts;
            for my $it (@$items) {
                $perl_exes{$it->{perl}} = $it->{_perl_exe};
                $perl_opts{$it->{modver}} = $it->{_perl_opts} if defined $it->{modver};
            }
            if (!keys(%perl_opts)) {
                $perl_opts{""} = [];
            }

            my $sc = Data::Clone::clone($parsed);
            for (keys %$sc) { delete $sc->{$_} if /^(before|after)_/ } # remove all hooks

            my %item_mems; # key = item seq
            for my $perl (sort keys %perl_exes) {
                for my $modver (sort keys %perl_opts) {
                    my $scd_path = _get_tempfile_path("scenario-$perl");
                    $sc->{items} = [];
                    for my $it (@$items) {
                        next unless $it->{perl} eq $perl;
                        next unless !length($it->{modver}) ||
                            $it->{modver} eq $modver;
                        next if $item_mems{$it->{seq}}++; # avoid duplicate item
                        push @{$sc->{items}}, $it;
                    }
                    #use DD; dd {perl=>$perl, modver=>$modver, items=>$sc->{items}};
                    $log->debugf("Creating scenario dump file for %s (modver %s) at %s", $perl, $modver, $scd_path);
                    open my($fh), ">", $scd_path or die "Can't open file $scd_path: $!";
                    print $fh dmp($sc), ";\n";
                    close $fh;
                    my $res_path = _get_tempfile_path("benchresult-$perl");
                    my $cmd = join(
                        " ",
                        $perl_exes{$perl},
                        "-MBencher::Backend",
                        "-MData::Dmp",
                        @{ $perl_opts{$modver} // [] },
                        "-e'print dmp(Bencher::Backend::bencher(action=>q[bench], precision=>$precision, scenario_file=>q[$scd_path], include_result_size=>q[$include_result_size], return_meta=>0, capture_stdout=>$capture_stdout, capture_stderr=>$capture_stderr))' > '$res_path'",
                    );
                    $log->debugf("Running %s ...", $cmd);
                    system $cmd;
                    die "Failed running bencher for perl $perl (1)" if $?;
                    my $res = do $res_path;
                    die "Failed running bencher for perl $perl (2): can't parse result: $@" if $@;
                    die "Failed running bencher for perl $perl (3): result not an enveloped result" if ref($res) ne 'ARRAY';
                    die "Failed running bencher for perl $perl (4): $res->[0] - $res->[1]" if $res->[0] != 200;

                    for my $row (@{ $res->[2] }) {
                        $row->{perl} = $perl;
                        push @columns, "perl" unless grep {$_ eq 'perl'} @columns;
                        if (length $modver) {
                            $row->{modver} = $modver;
                            push @columns, "modver" unless grep {$_ eq 'modver'} @columns;
                        }
                        push @rows, $row;
                    }
                } # for modver
            } # for perl
        } else {
            my $tres;
            my $doit = sub {
                $tres = Benchmark::Dumb::_timethese_guts(
                    $precision,
                    {
                        map { $_->{seq} => $_->{_code} } @$items
                    },
                    "silent",
                );
            };

            if ($capture_stdout && $capture_stderr) {
                my ($stdout, $stderr, @res) = &Capture::Tiny::capture($doit);
            } elsif ($capture_stdout) {
                my ($stdout, @res) = &Capture::Tiny::capture_stdout($doit);
            } elsif ($capture_stderr) {
                my ($stdout, @res) = &Capture::Tiny::capture_stderr($doit);
            } else {
                $doit->();
            }

            if ($return_meta) {
                $envres->[3]{'func.time_end'} = Time::HiRes::time();
                $envres->[3]{'func.elapsed_time'} =
                    $envres->[3]{'func.time_end'} - $envres->[3]{'func.time_start'};
                $envres->[3]{'func.sysload_after'} = [Sys::Load::getload()];
            }

            for my $seq (sort {$a<=>$b} keys %$tres) {
                my $it = _find_record_by_seq($items, $seq);
                my $row = {
                    rate    => 1 / $tres->{$seq}{result}{num},
                    time    => $tres->{$seq}{result}{num},

                    (result_size => $it->{_result_size}) x !!$include_result_size,

                    errors  => $tres->{$seq}{result}{errors}[0],
                    samples => $tres->{$seq}{result}{_dbr_nsamples},
                    notes   => $it->{_code_error},
                };

                for my $k (sort keys %$it) {
                    next unless $k =~ /^(seq|participant|dataset|perl|modver|item_.+|arg_.+|proc_.+)$/;
                    push @columns, $k unless grep {$k eq $_} @columns;
                    $row->{$k} = $it->{$k};
                }
                push @rows, $row;
            }
        }

        push @columns, qw/seq rate time/;
        push @columns, qw/result_size/ if $include_result_size;
        # XXX proc_* fields should be put here
        push @columns, qw/errors samples notes/;

        $envres->[2] = \@rows;
        $envres->[3]{'table.fields'} = \@columns;

        if ($parsed->{after_bench}) {
            $log->infof("Executing after_bench hook ...");
            $parsed->{after_bench}->(
                hook_name => 'after_bench',
                scenario  => $parsed,
                stash     => $stash,
                result    => $envres,
            );
        }

        if ($return_meta) {
            $envres->[3]{'func.platform_info'} =
                Devel::Platform::Info->new->get_info;
            my $info = Sys::Info->new;
            $envres->[3]{'func.cpu_info'} = [$info->device('CPU')->identify];
            $envres->[3]{'func.note'} = $args{note} if exists $args{note};
        }

        # XXX separate to sub?
        if ($save_result) {
            require Data::Clean::JSON;
            require File::Slurper;
            require JSON::MaybeXS;
            require POSIX;

            my $result_filename = $args{result_filename} // do {
                my $mod = $args{scenario_module} // "NO_MODULE";
                $mod =~ s!(::|/)!-!g;
                sprintf(
                    "%s%s.%s.json",
                    $mod,
                    $module_startup ? ".module_startup" : "",
                    POSIX::strftime("%Y-%m-%dT%H-%M-%S",
                                    localtime($time_start)),
                );
            };
            my $path = "$result_dir/$result_filename";
            my $cleanser = Data::Clean::JSON->get_cleanser;
            $log->tracef("Saving result to %s ...", $path);
            File::Slurper::write_text(
                $path,
                JSON::MaybeXS::encode_json(
                    $cleanser->clone_and_clean($envres)
                )
            );
        }

      FORMAT:
        {
            last unless $is_cli_and_text_format;

            $envres = [
                200, "OK",
                format_result($envres, undef, {sort=>$args{sort}}),
                {
                    "cmdline.skip_format" => 1,
                },
            ];
        }

      RETURN_RESULT:

        goto L_END;

    }

    $envres = [400,"Unknown action"];

  L_END:

    if ($parsed->{before_return}) {
        $log->infof("Executing before_return hook ...");
        $parsed->{before_return}->(
            hook_name => 'before_return',
            scenario  => $parsed,
            stash     => $stash,
            result    => $envres,
        );
    }

    $envres;
}

$SPEC{parse_scenario} = {
    v => 1.1,
    summary => 'Parse scenario (fill in default values, etc)',
    args => {
        scenario => {
            summary => 'Unparsed scenario',
            schema  => 'hash*',
        },
    },
};
sub parse_scenario {
    my %args = @_;

    _parse_scenario(scenario => $args{scenario}, parent_args => {});
}

1;
#ABSTRACT: Backend for Bencher

=head1 ENVIRONMENT

=head2 BENCHER_RESULT_DIR => str

Set default for C<--results-dir>.


=head1 SEE ALSO

L<bencher>
