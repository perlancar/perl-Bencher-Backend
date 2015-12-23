package Bencher;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;
use Log::Any::IfLOG '$log';

use Benchmark::Dumb qw(timethese);
use Data::Dmp;
use List::MoreUtils qw(firstidx minmax uniq);
use Time::HiRes qw(time);

our %SPEC;

sub _uniquify_names {
    my $recs = shift;

    my $names = [map {$_->{name}} @{ $recs }];
    if ((grep {!defined} @$names) || scalar(@{[ uniq(@$names) ]}) < @$names) {
        my $i = -1;
        my %seen;
        for my $rec (@$recs) {
            $i++;
            $rec->{name} //= '';
            if ($seen{ $rec->{name} }++ || $rec->{name} eq '') {
                $rec->{name} .= (length($rec->{name}) ? " " : "") . "#$i";
                # XXX actually still doesn't guarantee unique name
            }
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
    my $include = $args{include};
    my $exclude = $args{exclude};
    my $include_pattern = $args{include_pattern};
    my $exclude_pattern = $args{exclude_pattern};
    my $include_tags = $args{include_tags};
    my $exclude_tags = $args{exclude_tags};
    my $aibdf = $args{apply_include_by_default_filter} // 1;

    my $frecs = [];

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
                    if ($rec->{name} eq $inc) {
                        $included++;
                        last INC;
                    }
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
                    next REC if $rec->{name} eq $exc;
                }
            }
        }
        if ($include_pattern) {
            next REC unless $rec->{seq} =~ /$include_pattern/i ||
                $rec->{name} =~ /$include_pattern/i;
            $explicitly_included++;
        }
        if ($exclude_pattern) {
            next REC if $rec->{seq} =~ /$exclude_pattern/i ||
                $rec->{name} =~ /$exclude_pattern/i;
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
    } elsif (defined $pargs->{scenario_module}) {
        my $m = "Bencher::Scenario::$pargs->{scenario_module}"; $m =~ s!/!::!g;
        my $mp = $m; $mp =~ s!::!/!g; $mp .= ".pm";
        require $mp;
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
                if ($p->{cmdline}) {
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

            # try to come up with a default name for the participant
            unless (defined($p->{name})) {
                if ($p->{type} eq 'command') {
                    my $cmdline = ref($p->{cmdline}) eq 'ARRAY' ?
                        join(" ", $p->{cmdline}) : $p->{cmdline};
                    $p->{name} = substr($cmdline, 0, 12);
                } elsif ($p->{type} eq 'perl_code') {
                    if ($p->{function}) {
                        $p->{name} = ($p->{module} ? "$p->{module}::" : "").
                            $p->{function};
                    } elsif ($p->{module}) {
                        $p->{name} = $p->{module};
                    }
                }
            }

            push @{ $parsed->{participants} }, $p;
        } # for each participant

        _uniquify_names($parsed->{participants});

        # filter participants by include/exclude module/function
        if ($apply_filters) {
            if ($pargs->{include_modules} && @{ $pargs->{include_modules} }) {
                $parsed->{participants} = [grep {
                    defined($_->{module}) && $_->{module} ~~ @{ $pargs->{include_modules} }
                } @{ $parsed->{participants} }];
            }
            if ($pargs->{exclude_modules} && @{ $pargs->{exclude_modules} }) {
                $parsed->{participants} = [grep {
                    !defined($_->{module}) || !($_->{module} ~~ @{ $pargs->{exclude_modules} })
                } @{ $parsed->{participants} }];
            }
            if ($pargs->{include_module_pattern}) {
                $parsed->{participants} = [grep {
                    defined($_->{module}) && $_->{module} =~ qr/$pargs->{include_module_pattern}/i
                } @{ $parsed->{participants} }];
            }
            if ($pargs->{exclude_module_pattern}) {
                $parsed->{participants} = [grep {
                    !defined($_->{module}) || $_->{module} !~ qr/$pargs->{exclude_module_pattern}/i
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
            records => $parsed->{participants},
            include => $pargs->{include_participants},
            exclude => $pargs->{exclude_participants},
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
        for my $ds0 (@{ $unparsed->{datasets} }) {
            $i++;
            my $ds = { %$ds0, seq=>$i };
            $ds->{include_by_default} //= 1;

            # try to come up with a default name for the dataset
            unless (defined($ds->{name})) {
                if ($ds->{args}) {
                    $ds->{name} = substr(dmp($ds->{args}), 0, 14);
                } elsif ($ds->{argv}) {
                    $ds->{name} = substr(dmp($ds->{argv}), 0, 14);
                }
            }
            push @{ $parsed->{datasets} }, $ds;
        } # for each dataset

        _uniquify_names($parsed->{datasets}) if $parsed->{datasets};

        $parsed->{datasets} = _filter_records(
            records => $parsed->{datasets},
            include => $pargs->{include_datasets},
            exclude => $pargs->{exclude_datasets},
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
        next unless defined $p->{module};
        push @modules, $p->{module} unless $p->{module} ~~ @modules;
    }

    @modules;
}

sub _get_participant_functions {
    use experimental 'smartmatch';

    my $parsed = shift;

    my @modules;
    for my $p (@{ $parsed->{participants} }) {
        next unless defined $p->{function};
        push @modules, $p->{function} unless $p->{function} ~~ @modules;
    }

    @modules;
}

sub _gen_items {
    require Permute::Named::Iter;

    my %args = @_;

    my $parsed = $args{scenario};
    my $pargs  = $args{parent_args};
    my $apply_filters = $args{apply_filters} // 1;

    $parsed->{items} = [];
    my @permute;

    # XXX allow permutation of perl path
    # XXX allow permutation of module path

    my $participants;
    my $datasets;
    my $module_startup = $pargs->{module_startup} // $parsed->{module_startup};

    if ($module_startup) {
        my @modules = _get_participant_modules($parsed);

        return [412, "There are no modules to benchmark ".
                    "the startup overhead of"]
            unless @modules;

        {
            # push perl as base-line
            push @$participants, {
                seq  => 0,
                name => "perl -e1 (baseline)",
                type => 'command',
                cmdline => [$^X, "-e1"],
            };

            my $i = 0;
            for my $mod (@modules) {
                $i++;
                push @$participants, {
                    seq  => $i,
                    name => $mod,
                    type => 'command',
                    cmdline => [$^X, "-M$mod", "-e1"],
                };
            }
        }
    } else {
        return [412, "Please load a scenario (-m, -f) or ".
                    "include at least one participant (-p)"]
            unless @{$parsed->{participants}};
        $participants = $parsed->{participants};
        $datasets = $parsed->{datasets} if $parsed->{datasets};
    }

    push @permute, "participant", [map {$_->{seq}} @$participants];

    if ($datasets && @$datasets) {
        push @permute, "dataset", [map {$_->{seq}} @$datasets];
    }

    my $extra_permutes = $parsed->{permutes} // {};
    if (keys %$extra_permutes) {
        for my $pm (keys %$extra_permutes) {
            push @permute, "permute:$pm", $extra_permutes->{$pm};
        }
    }

    $log->debugf("permute: %s", \@permute);

    my $iter = Permute::Named::Iter::permute_named_iter(@permute);
    my $i = -1;
    my $items = [];
  ITER:
    while (my $h = $iter->()) {
        $log->tracef("iter returns: %s", $h);
        $i++;
        my $item_name;

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

        {
            # convert participant's & dataset index to name temporarily, for
            # nicer item name
            my %h = %$h;
            $h{participant} = $p->{name};
            $h{dataset} = $ds->{name} if $ds;
            my @k = keys %h;
            if (@k == 1) {
                $item_name = $h{$k[0]};
            } else {
                $item_name = dmp(\%h);
            }
            #$log->tracef("Set item name to: %s", $item_name);
        }

        my $code;
        if ($p->{type} eq 'command') {
            my @cmd;
            my $shell;
            if (ref($p->{cmdline}) eq 'ARRAY') {
                @cmd = @{ $p->{cmdline} };
                $shell = 0;
            } else {
                @cmd = ($p->{cmdline});
                $shell = 1;
            }
            $code = sub {
                if ($shell) {
                    system $cmd[0];
                } else {
                    system {$cmd[0]} @cmd;
                }
                die "Command failed (child error=$?, os error=$!)\n"
                    if $?;
            };
        } elsif ($p->{type} eq 'perl_code') {
            if ($p->{code}) {
                if ($ds) {
                    if ($ds->{argv}) {
                        $code = sub { $p->{code}->(@{$ds->{argv}}) };
                    } elsif ($ds->{args}) {
                        $code = sub { $p->{code}->(%{$ds->{args}}) };
                    } else {
                        return [400, "Participant #$p->{seq}, dataset #$h->{dataset}: No argv/args supplied for code"];
                    }
                } else {
                    $code = $p->{code};
                }
            } elsif (my $template = $p->{code_template} || $p->{fcall_template}) {
                my $template_vars;
                if ($ds->{args}) {
                    $template_vars = { %{$ds->{args}} };
                } elsif ($ds->{argv}) {
                    $template_vars = { map {$_=>$ds->{argv}[$_]}
                                           0..$#{ $ds->{argv} } };
                } else {
                    #warn "Item #$i: participant specifies code_template/fcall_template but there is no args/argv in the dataset #$h->{dataset}\n";
                }
                # add template variables from extra permutes
                for (grep {/\Apermute:/} keys %$h) { $template_vars->{$_} = $h->{$_} }

                if ($template_vars) {
                    $template =~ s/\<(\w+(?::\w+)?)\>/dmp($template_vars->{$1})/eg;
                }
                my $code_str = "sub { $template }";
                $log->debugf("Item #%d: code=%s", $i, $code_str);
                $code = eval $code_str;
                return [400, "Item #$i: code compile error: $@ (code: $code_str)"] if $@;
            }
        } else {
            return [400, "Unknown participant type '$p->{type}'"];
        }

        push @$items, {
            seq  => $i,
            name => $item_name,
            code => $code,
            _permute => $h,
        };
    } # ITER

    $items = _filter_records(
        records => $items,
        include => $pargs->{include_items},
        exclude => $pargs->{exclude_items},
        include_pattern => $pargs->{include_item_pattern},
        exclude_pattern => $pargs->{exclude_item_pattern},
    ) if $apply_filters;

    [200, "OK", $items];
}

sub _complete_module {
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
        array => [map {($_->{seq}, $_->{name})} @{$parsed->{participants}}],
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
        array => [map {($_->{seq}, $_->{name})} @{$parsed->{datasets}}],
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
        array => [grep {!/\{/} # remove names that are too unwieldy
                      map {($_->{seq}, $_->{name})} @$items],
    );
}

my $_alias_spec_add_dataset = {
    summary => 'Add a dataset',
    code => sub {
        require JSON;

        my $args = shift;
        push @{ $args->{datasets} },
            JSON::decode_json($_[0]);
    },
};

my $_alias_spec_add_participant = {
    summary => 'Add a participant',
    code => sub {
        require JSON;

        my $args = shift;
        push @{ $args->{participants} },
            JSON::decode_json($_[0]);
    },
};

$SPEC{bencher} = {
    v => 1.1,
    summary => 'A benchmark framework',
    args => {
        scenario_file => {
            summary => 'Load a scenario from a Perl file',
            description => <<'_',

Perl file will be do()'ed and the last expression should be a hash containing
the scenario specification.

_
            schema => 'str*',
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
            completion => sub {
                require Complete::Module;
                my %args = @_;
                Complete::Module::complete_module(
                    word=>$args{word}, ns_prefix=>'Bencher::Scenario');
            },
        },
        participants => {
            'summary' => 'Add participants',
            'x.name.is_plural' => 1,
            schema => ['array*', of=>'hash*'],
            cmdline_aliases => {
                participant => $_alias_spec_add_participant,
                p => $_alias_spec_add_participant,
            },
        },
        datasets => {
            summary => 'Add datasets',
            'x.name.is_plural' => 1,
            schema => ['array*', of=>'hash*'],
            cmdline_aliases => {
                dataset => $_alias_spec_add_dataset,
                d => $_alias_spec_add_dataset,
            },
        },
        permutes => {
            summary => 'Add permutes',
            schema => ['hash*', each_value=>'array*'],
        },
        action => {
            schema => ['str*', {
                in=>[qw/
                           list-scenario-modules
                           show-scenario
                           list-participants
                           list-participant-modules
                           list-datasets
                           list-permutes
                           list-items
                           show-items-results
                           bench
                       /]
                    # list-functions
            }],
            default => 'bench',
            cmdline_aliases => {
                a => {},
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
                show_items_results => {
                    is_flag => 1,
                    summary => 'Shortcut for -a show-items-results',
                    code => sub { $_[0]{action} = 'show-items-results' },
                },
            },
            tags => ['category:action'],
        },
        raw => {
            summary => 'Show "raw" data',
            schema => 'bool',
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
            schema => 'bool*',
            cmdline_aliases => {l=>{}},
        },

        include_modules => {
            'x.name.is_plural' => 1,
            summary => 'Only include modules specified in this list',
            'summary.alt.plurality.singular' => 'Add module to include list',
            schema => ['array*', of=>'str*'],
            element_completion => sub { _complete_module(@_, apply_filters=>0) },
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
            schema => ['array*', of=>'str*'],
            element_completion => sub { _complete_module(@_, apply_filters=>0) },
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
            schema => ['array*', of=>'str*'],
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
            schema => ['array*', of=>'str*'],
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
            'summary.alt.plurality.singular' => 'Add participant to include list',
            schema => ['array*', of=>'str*'],
            element_completion => sub { _complete_participant(@_, apply_filters=>0) },
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
            schema => ['array*', of=>'str*'],
            element_completion => sub { _complete_participant_tags(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },
        exclude_participants => {
            'x.name.is_plural' => 1,
            summary => 'Exclude participants whose seq/name matches this',
            'summary.alt.plurality.singular' => 'Add participant to include list',
            schema => ['array*', of=>'str*'],
            element_completion => sub { _complete_participant(@_, apply_filters=>0) },
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
            schema => ['array*', of=>'str*'],
            element_completion => sub { _complete_participant_tags(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },

        include_items => {
            'x.name.is_plural' => 1,
            summary => 'Only include items whose seq/name matches this',
            'summary.alt.plurality.singular' => 'Add item to include list',
            schema => ['array*', of=>'str*'],
            element_completion => sub { _complete_item(@_, apply_filters=>0) },
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
            'summary.alt.plurality.singular' => 'Add item to exclude list',
            schema => ['array*', of=>'str*'],
            element_completion => sub { _complete_item(@_, apply_filters=>0) },
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
            'summary.alt.plurality.singular' => 'Add dataset to include list',
            schema => ['array*', of=>'str*'],
            element_completion => sub { _complete_dataset(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },
        include_dataset_pattern => {
            summary => 'Only include datasets matching this regex pattern',
            schema => 're*',
            tags => ['category:filtering'],
        },
        exclude_datasets => {
            'x.name.is_plural' => 1,
            summary => 'Exclude datasets whose seq/name matches this',
            'summary.alt.plurality.singular' => 'Add dataset to exclude list',
            schema => ['array*', of=>'str*'],
            element_completion => sub { _complete_dataset(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },
        exclude_dataset_pattern => {
            summary => 'Exclude datasets matching this regex pattern',
            schema => 're*',
            tags => ['category:filtering'],
        },
        include_dataset_tags => {
            'x.name.is_plural' => 1,
            summary => 'Only include datasets whose tag matches this',
            'summary.alt.plurality.singular' => 'Add a tag to dataset include tag list',
            description => <<'_',

You can specify `A & B` to include datasets that have _both_ tags A and B.

_
            schema => ['array*', of=>'str*'],
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
            schema => ['array*', of=>'str*'],
            element_completion => sub { _complete_dataset_tags(@_, apply_filters=>0) },
            tags => ['category:filtering'],
        },

        on_failure => {
            summary => "What to do when command fails or Perl code dies",
            schema => ['str*', in=>[qw/die skip/]],
            description => <<'_',

The default is "die". When set to "skip", will first run the code of each item
before benchmarking and trap command failure/Perl exception and if that happens,
will "skip" the item.

_
        },
    },
};
sub bencher {
    my %args = @_;

    my $action = $args{action};
    my $envres;

    if ($action eq 'list-scenario-modules') {
        require PERLANCAR::Module::List;
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

    my $parsed = _parse_scenario(
        scenario=>$unparsed,
        parent_args=>\%args,
        apply_include_by_default_filter => $aibdf,
    );
    my $module_startup = $args{module_startup} // $parsed->{module_startup};

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

    if ($action eq 'list-participant-modules') {
        my @modules = _get_participant_modules($parsed);
        $envres = [200, "OK", \@modules];
        goto L_END;
    }

    if ($action eq 'list-participants') {
        my @res;
        my $has_summary = 0;
        for my $p (@{ $parsed->{participants} }) {
            if ($args{detail}) {
                my $rec = {
                    seq      => $p->{seq},
                    type     => $p->{type},
                    include_by_default => $p->{include_by_default},
                    name     => $p->{name},
                    function => $p->{function},
                    module   => $p->{module},
                    cmdline  => ref($p->{cmdline}) eq 'ARRAY' ? join(" ", @{$p->{cmdline}}) : $p->{cmdline},
                    tags     => join(", ", @{$p->{tags} // []}),
                };
                if (defined $p->{summary}) {
                    $has_summary = 1;
                    $rec->{summary} = $p->{summary};

                }
                push @res, $rec;
            } else {
                push @res, $p->{name};
            }
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
    my $items = $res->[2];

    if ($action eq 'list-items') {
        my @res;
        for my $it (@$items) {
            if ($args{detail}) {
                push @res, {
                    seq      => $it->{seq},
                    name     => $it->{name},
                };
            } else {
                push @res, $it->{name};
            }
        }
        my %resmeta;
        $resmeta{'table.fields'} = [qw/seq name/]
            if $args{detail};
        $envres = [200, "OK", \@res, \%resmeta];
        goto L_END;
    }

    if ($action =~ /\A(show-items-results|bench)\z/) {
        require Module::Load;

        my $participants = $parsed->{participants};
        $envres = [200, "OK", [], {}];

        my $return_resmeta =
            $args{-cmdline_r} && (($args{-cmdline_r}{format} // '') !~ /json/) ?
            0 : 1;

        $envres->[3]{'func.module_versions'}{perl} = $^V if $return_resmeta;

        my $code_load = sub {
            no strict 'refs';
            my $mod = shift;
            $log->tracef("Loading module: %s", $mod);
            Module::Load::load($mod);
            if ($return_resmeta) {
                # we'll just use ${"$mod\::VERSION"} because we are already
                # loading the module
                $envres->[3]{'func.module_versions'}{$mod} =
                    ${"$mod\::VERSION"};
            }
        };

        $code_load->('Benchmark::Dumb');
        $code_load->('Devel::Platform::Info') if $return_resmeta;
        $code_load->('Sys::Info')             if $return_resmeta;
        $code_load->('Sys::Load')             if $return_resmeta;

        # load all modules
        {
            my %seen;
            my @modules = _get_participant_modules($parsed);
            for my $mod (@modules) {
                $code_load->($mod);
            }
        }

        my $on_failure = $args{on_failure} // $parsed->{on_failure} // 'die';
        {
            my $fitems = [];
            for my $it (@$items) {
                $log->tracef("Testing code for item #%d (%s) ...",
                             $it->{seq}, $it->{name});
                eval {
                    my $result_is_list = $participants->[
                        $it->{_permute}{participant} ]{result_is_list} // 0;
                    $it->{_result} = $result_is_list ?
                        [$it->{code}->()] : $it->{code}->();
                };
                my $err = $@;
                if ($err) {
                    if ($on_failure eq 'skip' || $action eq 'show-items-results') {
                        warn "Skipping item #$it->{seq} ($it->{name}) ".
                            "due to failure: $err\n";
                        next;
                    } else {
                        die "Item #$it->{seq} ($it->{name}) fails: $err\n";
                    }
                }
                push @$fitems, $it;
            }
            $items = $fitems;
        }

        if ($action eq 'show-items-results') {
            if ($return_resmeta) {
                $envres->[2] = [map {$_->{_result}} @$items];
            } elsif ($args{raw}) {
                $envres->[2] = join(
                    "",
                    map {(
                        "#$_->{seq} ($_->{name}):\n",
                        $_->{_result},
                        "\n\n",
                    )} @$items
                );
            } else {
                require Data::Dump;
                $envres->[2] = join(
                    "",
                    map {(
                        "#$_->{seq} ($_->{name}):\n",
                        Data::Dump::dump($_->{_result}),
                        "\n\n",
                    )} @$items
                );
            }
            goto RETURN_RESULT;
        }

        if ($parsed->{before_bench}) {
            $log->infof("Executing before_bench hook ...");
            $parsed->{before_bench}->(
                hook_name => 'before_bench',
                scenario  => $parsed,
                stash     => $stash,
            );
        }

        if ($return_resmeta) {
            $envres->[3]{'func.sysload_before'} = [Sys::Load::getload()];
            $envres->[3]{'_time_start'} = time();
        }

        my $tres = Benchmark::Dumb::_timethese_guts(
            0,
            {
                map { $_->{seq} => $_->{code} } @$items
            },
            "silent",
        );

        if ($return_resmeta) {
            $envres->[3]{'func.elapsed_time'} =
                time() - $envres->[3]{'_time_start'};
            delete $envres->[3]{'_time_start'};
            $envres->[3]{'func.sysload_after'} = [Sys::Load::getload()];
        }

        for my $seq (sort {$a<=>$b} keys %$tres) {
            my $it = _find_record_by_seq($items, $seq);
            push @{$envres->[2]}, {
                seq     => $seq,
                name    => $it->{name},
                time    => $tres->{$seq}{result}{num},
                rate    => 1 / $tres->{$seq}{result}{num},
                samples => $tres->{$seq}{result}{_dbr_nsamples},
                errors  => $tres->{$seq}{result}{errors}[0],
            };
        }
        $envres->[3]{'table.fields'} =
            [qw/seq name rate time samples errors/];

        if ($parsed->{after_bench}) {
            $log->infof("Executing after_bench hook ...");
            $parsed->{after_bench}->(
                hook_name => 'after_bench',
                scenario  => $parsed,
                stash     => $stash,
                result    => $envres,
            );
        }

      FORMAT:
        {
            my $r = $args{-cmdline_r};
            last unless $r && ($r->{format} // 'text') =~ /text/;

            my $ff = $envres->[3]{'table.fields'};

            # for module_startup mode: remove 'rate', add 'mod_overhead_time'
            if ($module_startup) {
                my $rit_baseline = _find_record_by_seq($envres->[2], 0);
                for my $rit (@{$envres->[2]}) {
                    delete $rit->{rate};
                    if ($rit_baseline) {
                        $rit->{mod_overhead_time} = $rit->{time} - $rit_baseline->{time};
                    }
                }
                splice @$ff, (firstidx {$_ eq 'rate'} @$ff), 1;
                splice @$ff, (firstidx {$_ eq 'time'} @$ff)+1, 0, "mod_overhead_time";
            }

            # sort by default from slowest to fastest
            $envres->[2] = [sort {$b->{time} <=> $a->{time}} @{$envres->[2]}];

            # pick an appropriate time unit & format the time
            my ($min, $max) = minmax(map {$_->{time}} @{$envres->[2]});

            my ($unit, $factor);
            if ($max <= 1.5e-6) {
                $unit = "ns";
                $factor = 1e9;
            } elsif ($max <= 1.5e-3) {
                $unit = "\x{03bc}s"; # XXX enable utf
                $factor = 1e6;
            } elsif ($max <= 1.5) {
                $unit = "ms";
                $factor = 1e3;
            }

            if ($unit) {
                for my $rit (@{$envres->[2]}) {
                    # XXX format number of decimal digits of 'time' and 'rate'
                    # based on sigma
                    $rit->{time} = sprintf(
                        "%.5f%s", $rit->{time} * $factor, $unit);
                    if (exists $rit->{mod_overhead_time}) {
                        $rit->{mod_overhead_time} = sprintf(
                            "%.5f%s", $rit->{mod_overhead_time} * $factor, $unit);
                    }
                }
            }

        } # FORMAT

        if ($return_resmeta) {
            $envres->[3]{'func.platform_info'} =
                Devel::Platform::Info->new->get_info;
            my $info = Sys::Info->new;
            $envres->[3]{'func.cpu_info'} = [$info->device('CPU')->identify];
        }

      RETURN_RESULT:

        goto L_END;

    }

    $envres = [304,"No action"];

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

=head2 participants

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

Other properties you can add to a participant: C<include_by_default> (bool,
default true, can be set to false if you want to exclude participant by default
when running benchmark, unless the participant is explicitly included).

Or, if you are benchmarking commands, you specify C<cmdline> (array or strings,
or strings) instead. An array cmdline will not use shell, while the string
version will use shell. See L<Bencher::Scenario::Interpreters>.

=over

=item * name

=item * summary

=item * module (str)

=item * function (str)

=item * fcall_template (str)

=item * result_is_list (bool, default 0)

=back

=head2 datasets

B<datasets> (array) lists the function inputs (or command-line arguments). You
can C<name> each dataset too, to be able to refer to it more easily.

Other properties you can add to a dataset: C<include_by_default> (bool, default
true, can be set to false if you want to exclude dataset by default when running
benchmark, unless the dataset is explicitly included).

=over

=item * name

=item * summary

=item * description

=item * args

=item * argv

=item * include_by_default (bool, default 1)

=back

=head3 Other properties

Other known scenario properties (keys):

=over

=item * name

From DefHash, scenario name (usually short and one word).

=item * summary

From DefHash, a one-line plaintext summary.

=item * description (str)

From DefHash, longer description in Markdown.

=item * permutes (hash)

Extra permutations. Bencher will create a permutation for every key in this
hash. You can use this to benchmark variation of options/arguments. Each hash
value must be an array of values. Example:

 permutes => {
     return_type => ['bool', 'str', 'full'],
 }

You use it when declaring participants, e.g.:

 participants => [
     fcall_template => 'Data::Sah::gen_validator(<schema>, {return_type=><permute:return_type>})'
 ]

When generating benchmark items, for this participant and for each dataset there
will be three items generated, one for every different C<return_type>.

=item * on_failure (str, "skip"|"die")

The default is "die". When set to "skip", will first run the code of each item
before benchmarking and trap command failure/Perl exception and if that happens,
will "skip" the item.

Can be overriden in the CLI with C<--on-failure> option.

=item * before_gen_items (code)

If specified, then this code will be called before generating items. You can use
this hook to, e.g.: generate datasets dynamically. Code will be given hash
argument with the following keys: C<hook_name> (str, set to
C<before_gen_items>), C<scenario>, C<stash> (hash, which you can use to pass
data between hooks).

=item * before_bench (code)

If specified, then this code will be called before starting the benchmark. Code
will be given hash argument with the following keys: C<hook_name> (str, set to
C<before_bench>), C<scenario>, C<stash>.

=item * after_bench (code)

If specified, then this code will be called after completing benchmark. You can
use this hook to, e.g.: do some custom formatting/modification to the result.
Code will be given hash argument with the following keys: C<hook_name> (str, set
to C<before_bench>), C<scenario>, C<stash>, C<result> (array, enveloped result).

=item * before_return (code)

If specified, then this code will be called before displaying/returning the
result. You can use this hook to, e.g.: modify the result in some way. Code will
be given hash argument with the following keys: C<hook_name> (str, set to
C<before_bench>), C<scenario>, C<stash>, C<result>.

=back


=head1 SEE ALSO

L<bencher>

B<BenchmarkAnything>. There are lot of overlaps of goals between Bencher and
this project. I hope to reuse or interoperate parts of BenchmarkAnything, e.g.
storing Bencher results in a BenchmarkAnything storage backend, sending Bencher
results to a BenchmarkAnything HTTP server, and so on.

L<Benchmark>, L<Benchmark::Dumb> (L<Dumbbench>)

C<Bencher::Scenario::*> for examples of scenarios.
