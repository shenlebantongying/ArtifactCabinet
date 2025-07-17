project(
    'play',
    'cpp',
    meson_version : '>= 1.3.0',
    version : '0.1',
    default_options : ['warning_level=3','std=gnu++26'],
)

dependencies = [
    dependency('sdl3')
]

exe = executable(
    'play',
    'play.cpp',
    dependencies : dependencies,
    install : true,
)
