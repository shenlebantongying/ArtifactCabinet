project(
    'playground',
    'cpp',
    version: '0.1',
    meson_version: '>= 1.3.0',
    default_options: ['warning_level=3', 'cpp_std=gnu++26'],
)

d = [dependency('sdl3'), dependency('pangocairo'), dependency('fmt')]

w = library('my_sdl_window', 'SDL_window.cpp', dependencies: d)

executable(
    'SDL_drag_resize_rect',
    'SDL_drag_resize_rect.cpp',
    dependencies: d,
    link_with: w,
)
executable('SDL_drawText', 'SDL_drawText.cpp', dependencies: d, link_with: w)
executable(
    'SDL_keyboards_ev',
    'SDL_keyboards_ev.cpp',
    dependencies: d,
    link_with: w,
)
executable('SDL_quarter', 'SDL_quarter.cpp', dependencies: d, link_with: w)
executable('SDL_scrolling', 'SDL_scrolling.cpp', dependencies: d, link_with: w)
executable('SDL_sinWave', 'SDL_sinWave.cpp', dependencies: d, link_with: w)
executable(
    'SDL_text_cursor_move',
    'SDL_text_cursor_move.cpp',
    dependencies: d,
    link_with: w,
)
executable(
    'SDL_text_editing',
    'SDL_text_editing.cpp',
    dependencies: d,
    link_with: w,
)
executable('pango_basic', 'pango_basic.cpp', dependencies: d, link_with: w)
executable(
    'pango_text_attr',
    'pango_text_attr.cpp',
    dependencies: d,
    link_with: w,
)
executable(
    'pango_text_wrapping',
    'pango_text_wrapping.cpp',
    dependencies: d,
    link_with: w,
)
