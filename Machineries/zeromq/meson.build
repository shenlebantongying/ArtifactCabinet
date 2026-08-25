project(
    'zmq_play',
    'cpp',
    version: '1.0.0',
    default_options: ['warning_level=3', 'cpp_std=gnu++26'],
)

d = [dependency('libzmq'), dependency('fmt')]


executable('01_server', '01_pub_sub/server.cpp', dependencies: d)

executable('01_client', '01_pub_sub/client.cpp', dependencies: d)

executable(
    '02_echo_inproc',
    '02_echo_inproc/tri-threads-inproc.cpp',
    dependencies: d,
)

executable('03_server', '03_large_data/server.cpp', dependencies: d)
executable('03_client', '03_large_data/client.cpp', dependencies: d)
