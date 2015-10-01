package Bencher::Scenario::Example;

# DATE
# VERSION

our $scenario = {
    participants => [
        {fcall_template => "PERLANCAR::Text::Levenshtein::editdist(<word1>, <word2>)"},
        {module=>'Text::LevenshteinXS'},
        {type=>'command', cmdline=>'ls -l 2>&1 >/dev/null'},
    ],
    datasets => [
        { args => {word1=>"a"      , word2=>"aa"},      result => 1 },
        { args => {word1=>"foo"    , word2=>"bar"},     result => 3 },
        { args => {word1=>"program", word2=>"porgram"}, result => 2 },
    ],
};

1;
# ABSTRACT: An example scenario
