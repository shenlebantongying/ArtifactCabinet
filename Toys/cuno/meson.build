project(
    'cuno',
    'cpp',
    version: '0.1',
    meson_version: '>= 1.3.0',
    default_options: ['warning_level=3', 'cpp_std=c++14'],
)

dependencies = [dependency('SDL3'), dependency('pangocairo'), dependency('fmt')]

exe = executable('cuno', 'cuno.cpp', install: true, dependencies: dependencies)

test('basic', exe)
