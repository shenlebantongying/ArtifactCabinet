project(
    'pipe',
    'cpp',
    version: '0.1',
    meson_version: '>= 1.3.0',
    default_options: ['warning_level=3', 'cpp_std=c++23'],
)

fs = import('fs')

dependencies = [dependency('fmt')]

exe = executable(
    'pipe',
    'randomSender.cpp',
    install: true,
    dependencies: dependencies,
)

copy = fs.copyfile('randomReceiver.rkt', 'randomReceiver.rkt')
