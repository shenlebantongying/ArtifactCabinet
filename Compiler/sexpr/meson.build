project(
    'sexpr',
    'cpp',
    version: '0.1',
    meson_version: '>= 1.3.0',
    default_options: ['warning_level=3', 'cpp_std=gnu++26'],
)

gtest = dependency('gtest')

lib = library('sexpr', ['sexpr.cpp', 'sexpr_rec.cpp'], install: true)

test_rec = executable(
    'test_rec',
    ['sexpr_rec_tests.cpp'],
    link_with: lib,
    dependencies: [gtest],
)

test('test_rec', test_rec)
