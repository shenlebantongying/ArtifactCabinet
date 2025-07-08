project(
    'Z_ui',
    'cpp',
    version: '0.1',
    meson_version: '>= 1.3.0',
    default_options: ['warning_level=3', 'cpp_std=c++23'],
)

deps = [dependency('sdl3'), dependency('cairo')]

lib = library(
    'Z_ui',
    ['Z_app.cpp', 'Z_button.cpp', 'Z_widget.cpp'],
    install: true,
    dependencies: deps,
)

inc = include_directories('.')


z_dep = declare_dependency(link_with: lib, include_directories: inc)

t1 = executable('t1', ['./tests/nest_layout.cpp'], dependencies: [deps, z_dep])

