project('FuzzyMatchSublime', ['cpp'],
        version : '0.0.1',
        default_options : ['warning_level=3', 'cpp_std=c++23'])

libfuzzy_inc = include_directories('.')
libfuzzy_lib = library('fuzzy',
                       'fuzzy.cpp',
                       'fuzzy.h',
                       include_directories : libfuzzy_inc)
libfuzzy_dep = declare_dependency(
    link_with : libfuzzy_lib,
    include_directories : libfuzzy_inc)

test('t', executable('t', 'tests.cpp', dependencies : [libfuzzy_dep]))
