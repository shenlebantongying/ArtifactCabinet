#[repr(C)]
struct Sab {
    a: i32,
    b: i32,
}

unsafe extern "C" {
    fn plus_sab(s: *mut Sab) -> i32;
    #[link_name = "printf"]
    fn printf(fmt: *const libc::c_char, ...) -> libc::c_int;
}

fn main() {
    let mut o = Box::new(Sab { a: 5, b: 14 });
    unsafe {
        printf(c"Hello, world!\n".as_ptr());

        printf(c"%d\n".as_ptr(), plus_sab(&mut *o));
    }
}
