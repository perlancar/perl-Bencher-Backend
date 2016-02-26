package Bencher;

# DATE
# VERSION

use 5.010001;
use strict;
use warnings;
use Log::Any::IfLOG '$log';

use Data::Dmp;
use List::MoreUtils qw(firstidx minmax uniq);

our %SPEC;

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
                    if (($rec->{name} // $rec->{_name} eq '') eq $inc) {
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
                    next REC if (($rec->{name} // $rec->{_name} // '') eq $exc);
                }
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

            # try to come up with a nicer name for the participant (not
            # necessarily unique)
            unless (defined($p->{name})) {
                if ($p->{type} eq 'command') {
                    if (ref($p->{cmdline}) eq 'ARRAY') {
                        $p->{_name} = substr($p->{cmdline}[0], 0, 20);
                    } else {
                        $p->{cmdline} =~ /(\S+)/;
                        $p->{_name} = substr($1, 0, 20);
                    }
                } elsif ($p->{type} eq 'perl_code') {
                    if ($p->{function}) {
                        $p->{_name} =
                            ($p->{module} ? "$p->{module}::" : "").
                            $p->{function};
                    } elsif ($p->{module}) {
                        $p->{_name} = $p->{module};
                    }
                }
            }

            push @{ $parsed->{participants} }, $p;
        } # for each participant

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
            push @{ $parsed->{datasets} }, $ds;
        } # for each dataset

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

    $log->debugf("permute: %s", \@permute);

    # to store multiple argument values that are hash, e.g.
    # {args=>{sizes=>{"1M"=>1024**2, "1G"=>1024**3, "1T"=>1024**4}}} instead of
    # array: {args=>{sizes=>[1024**2, 1024**3, 1024**4]}}
    my %ds_arg_values; # key=ds seq, val=hash(key=arg name, val=arg values)

    my $iter = Permute::Named::Iter::permute_named_iter(@permute);
    my $item_seq = -1;
    my $items = [];
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

      ITER_ARGS:
        while (my $h_args = $iter_args->()) {
            $item_seq++;
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

                    if ($template_vars) {
                        $template =~ s/\<(\w+(?::\w+)?)\>/dmp($template_vars->{$1})/eg;
                    }
                    my $code_str = "sub { $template }";
                    $log->debugf("Item #%d: code=%s", $item_seq, $code_str);
                    $code = eval $code_str;
                    return [400, "Item #$item_seq: code compile error: $@ (code: $code_str)"] if $@;
                }
            } else {
                return [400, "Unknown participant type '$p->{type}'"];
            }

            my $item = {
                seq  => $item_seq,
                _code => $code,
                _permute => $h,
                ((_permute_args => $h_args) x !!$ds->{args}),
            };
            for my $k (keys %$h) {
                if ($k eq 'dataset') {
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
        array => [grep {defined}
                      map {($_->{seq}, $_->{name})} @{$parsed->{participants}}],
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
                      map {($_->{seq}, $_->{name})} @{$parsed->{datasets}}],
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
        array => [map {($_->{seq}, $_->{_name})} @$items],
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
        'RoundNumbers',
        ['Sort', {by=>$opts->{sort}}],
        'ScaleTime',
        'ScaleRate',
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
    args_rels => {
        # XXX precision & precision_limit is only relevant when action=bench
        # XXX note is only relevant when action=bench
        # XXX sort is only relevant when action=bench and format=text
    },
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
                           list-scenario-modules
                           show-scenario
                           list-participants
                           list-participant-modules
                           list-datasets
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
            schema => ['array*', of=>'str*', min_len=>1],
            default => ['-time'],
        },

        note => {
            summary => 'Put additional note in the result',
            schema => 'str*',
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
            my $rec = {
                seq      => $p->{seq},
                type     => $p->{type},
                include_by_default => $p->{include_by_default},
                name     => $p->{name} // $p->{_name},
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

    if ($action =~ /\A(show-items-results|bench)\z/) {
        require Module::Load;
        require Time::HiRes;

        my $participants = $parsed->{participants};
        my $datasets = $parsed->{datasets};
        $envres = [200, "OK", [], {}];

        my $return_resmeta =
            $args{-cmdline_r} && (($args{-cmdline_r}{format} // '') !~ /json/) ?
            0 : 1;

        $envres->[3]{'func.module_startup'} = $module_startup;
        $envres->[3]{'func.module_versions'}{perl} = "$^V" if $return_resmeta;

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
            for my $mod (keys %{$parsed->{modules}}) {
                next if $mod eq 'perl';
                $code_load->($mod);
            }
        }

        if ($parsed->{before_bench}) {
            $log->infof("Executing before_bench hook ...");
            $parsed->{before_bench}->(
                hook_name => 'before_bench',
                scenario  => $parsed,
                stash     => $stash,
            );
        }

        # test code first
        my $on_failure = $args{on_failure} // $parsed->{on_failure} // 'die';
        my $on_result_failure = $args{on_result_failure} //
            $parsed->{on_result_failure} // $on_failure;
        {
            my $fitems = [];
            for my $it (@$items) {
                $log->tracef("Testing code for item #%d (%s) ...",
                             $it->{seq}, $it->{_name});
                eval {
                    my $participant = _find_record_by_seq($participants, $it->{_permute}{participant});
                    my $result_is_list = $participant->{result_is_list} // 0;
                    $it->{_result} = $result_is_list ?
                        [$it->{_code}->()] : $it->{_code}->();
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
                        "#$_->{seq} ($_->{_name}):\n",
                        $_->{_result},
                        "\n\n",
                    )} @$items
                );
            } else {
                require Data::Dump;
                $envres->[2] = join(
                    "",
                    map {(
                        "#$_->{seq} ($_->{_name}):\n",
                        Data::Dump::dump($_->{_result}),
                        "\n\n",
                    )} @$items
                );
            }
            goto RETURN_RESULT;
        }

        if ($return_resmeta) {
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
            $envres->[3]{'func.time_start'} = Time::HiRes::time();
        }

        my $precision = $args{precision} //
            $parsed->{precision} // $parsed->{default_precision} // 0;
        if (defined($args{precision_limit}) && $precision < $args{precision_limit}) {
            $precision = $args{precision_limit};
        }
        $envres->[3]{'func.precision'} = $precision if $return_resmeta;

        $log->tracef("Running benchmark (precision=%g) ...", $precision);
        my $tres = Benchmark::Dumb::_timethese_guts(
            $precision,
            {
                map { $_->{seq} => $_->{_code} } @$items
            },
            "silent",
        );

        if ($return_resmeta) {
            $envres->[3]{'func.time_end'} = Time::HiRes::time();
            $envres->[3]{'func.elapsed_time'} =
                $envres->[3]{'func.time_end'} - $envres->[3]{'func.time_start'};
            $envres->[3]{'func.sysload_after'} = [Sys::Load::getload()];
        }

        my @columns = (qw/seq participant dataset/);
        my @rows;
        for my $seq (sort {$a<=>$b} keys %$tres) {
            my $it = _find_record_by_seq($items, $seq);
            my $row = {
                time    => $tres->{$seq}{result}{num},
                rate    => 1 / $tres->{$seq}{result}{num},
                samples => $tres->{$seq}{result}{_dbr_nsamples},
                errors  => $tres->{$seq}{result}{errors}[0],
                notes   => $it->{_code_error},
            };

            for my $k (sort keys %$it) {
                next unless $k =~ /^(seq|participant|dataset|item_.+|arg_.+)$/;
                push @columns, $k unless grep {$k eq $_} @columns;
                $row->{$k} = $it->{$k};
            }

            push @rows, $row;
        }
        push @columns, qw/seq rate time errors samples notes/;
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

      FORMAT:
        {
            my $r = $args{-cmdline_r};
            last unless $r && ($r->{format} // 'text') =~ /text/;

            $envres = [
                200, "OK",
                format_result($envres, undef, {sort=>$args{sort}}),
                {
                    "cmdline.skip_format" => 1,
                },
            ];
        }

        if ($return_resmeta) {
            $envres->[3]{'func.platform_info'} =
                Devel::Platform::Info->new->get_info;
            my $info = Sys::Info->new;
            $envres->[3]{'func.cpu_info'} = [$info->device('CPU')->identify];
            $envres->[3]{'func.note'} = $args{note} if exists $args{note};
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

=item * name (str)

From DefHash.

=item * summary (str)

From DefHash.

=item * description (str)

From DefHash.

=item * tags (array of str)

From DefHash. Define tag(s) for this participant. Can be used to include/exclude
groups of participants having the same tags.

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

=item * name (str)

From DefHash.

=item * summary (str)

From DefHash.

=item * description (str)

From DefHash.

=item * tags (array of str)

From DefHash. Define tag(s) for this dataset. Can be used to include/exclude
groups of datasets having the same tags.

=item * args (hash)

Example:

 {filename=>"ujang.txt", size=>10}

You can supply multiple argument values by adding C<@> suffix to the argument
name. You then supply an array for the values, example:

 {filename=>"ujang.txt", 'size@'=>[10, 100, 1000]}

This means, for each participant mentioning C<size>, three benchmark items will
be generated, one for each value of C<size>.

Aside from array, you can also use hash for the multiple values. This has a nice
effect of showing nicer names (in the hash keys) for the argument value, e.g.:

 {filename=>"ujang.txt", 'size@'=>{"1M"=>1024*2, "1G"=>1024**3, "1T"=>1024**4}}

=item * argv (array)

=item * include_by_default (bool, default 1)

=item * include_participant_tags (array of str)

Only include participants having all these tags.

=item * exclude_participant_tags (array of str)

Exclude participants having any of these tags.

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

=item * module_startup (bool)

=item * default_precision (float, between=>[0,1])

Precision to pass to Benchmark::Dumb. Default is 0. Can be overriden via
C<--precision> (CLI).

=item * on_failure (str, "skip"|"die")

For a command participant, failure means non-zero exit code. For a Perl-code
participant, failure means Perl code dies or (if expected result is specified)
the result is not equal to the expected result.

The default is "die". When set to "skip", will first run the code of each item
before benchmarking and trap command failure/Perl exception and if that happens,
will "skip" the item.

Can be overriden in the CLI with C<--on-failure> option.

=item * on_result_failure (str, "skip"|"die"|"warn")

This is like C<on_failure> except that it specifically refer to the failure of
item's result not being equal to expected result.

The default is the value of C<on_failure>.

There is an extra choice of `warn` for this type of failure, which is to print a
warning to STDERR and continue.

Can be overriden in the CLI with C<--on-result-failure> option.

=item * before_parse_scenario (code)

If specified, then this code will be called before parsing scenario. Code will
be given hash argument with the following keys: C<hook_name> (str, set to
C<before_gen_items>), C<scenario> (hash, unparsed scenario), C<stash> (hash,
which you can use to pass data between hooks).

=item * after_parse_scenario (code)

If specified, then this code will be called after parsing scenario. Code will be
given hash argument with the following keys: C<hook_name>, C<scenario> (hash,
parsed scenario), C<stash>.

=item * before_list_datasets (code)

If specified, then this code will be called before enumerating datasets from
scenario. Code will be given hash argument with the following keys:
C<hook_name>, C<scenario>, C<stash>.

You can use this hook to, e.g.: generate datasets dynamically.

=item * before_list_participants (code)

If specified, then this code will be called before enumerating participants from
scenario. Code will be given hash argument with the following keys:
C<hook_name>, C<scenario>, C<stash>.

You can use this hook to, e.g.: generate participants dynamically.

=item * before_gen_items (code)

If specified, then this code will be called before generating items. Code will
be given hash argument with the following keys: C<hook_name>, C<scenario>,
C<stash>.

You can use this hook to, e.g.: modify datasets/participants before being
permuted into items.

=item * before_bench (code)

If specified, then this code will be called before starting the benchmark. Code
will be given hash argument with the following keys: C<hook_name>, C<scenario>,
C<stash>.

=item * after_bench (code)

If specified, then this code will be called after completing benchmark. Code
will be given hash argument with the following keys: C<hook_name>, C<scenario>,
C<stash>, C<result> (array, enveloped result).

You can use this hook to, e.g.: do some custom formatting/modification to the
result.

=item * before_return (code)

If specified, then this code will be called before displaying/returning the
result. Code will be given hash argument with the following keys: C<hook_name>,
C<scenario>, C<stash>, C<result>.

You can use this hook to, e.g.: modify the result in some way.

=back


=head1 SEE ALSO

L<bencher>

B<BenchmarkAnything>. There are lot of overlaps of goals between Bencher and
this project. I hope to reuse or interoperate parts of BenchmarkAnything, e.g.
storing Bencher results in a BenchmarkAnything storage backend, sending Bencher
results to a BenchmarkAnything HTTP server, and so on.

L<Benchmark>, L<Benchmark::Dumb> (L<Dumbbench>)

C<Bencher::Scenario::*> for examples of scenarios.
