use sdl3::event::Event;
use sdl3::keyboard::Keycode;
use sdl3::pixels::Color;
use sdl3::rect::Rect;

pub fn main() {
    let sdl = sdl3::init().unwrap();

    let window = sdl
        .video()
        .unwrap()
        .window("Pong game", 800, 600)
        .build()
        .unwrap();

    let mut canvas = window.into_canvas();

    let mut a = Rect::new(0,100,100,100);
    canvas.set_draw_color(Color::RGB(0, 255, 255));
    canvas.clear();
    canvas.present();

    let mut event_pump = sdl.event_pump().unwrap();
    let mut i: u8 = 0;
    let mut inc_i_q: bool = true;
    'running: loop {
        match i {
            u8::MAX => inc_i_q = false,
            u8::MIN => inc_i_q = true,
            _ => {}
        }

        match inc_i_q {
            true => i += 1,
            false => i -= 1,
        }

        canvas.set_draw_color(Color::RGB(i, 64, 255 - i));
        canvas.clear();

        canvas.set_draw_color(Color::RGB(0, 255, 255));
        canvas.fill_rect(a).unwrap();

        // TODO: what if 2 keys holding together??

        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. } => break 'running,
                Event::KeyDown {
                    keycode: Some(Keycode::W),
                    ..
                } => a.y -= 10,
                Event::KeyDown {
                    keycode: Some(Keycode::S),
                    ..
                } => a.y += 10,
                Event::KeyDown {
                    keycode: Some(Keycode::A),
                    ..
                } => a.x -= 10,
                Event::KeyDown {
                    keycode: Some(Keycode::D),
                    ..
                } => a.x += 10,
                Event::KeyDown { .. } => println!("Rad key pressed"),
                _ => {}
            }
        }

        canvas.present();
        sdl3::timer::delay(10);
    }
}
