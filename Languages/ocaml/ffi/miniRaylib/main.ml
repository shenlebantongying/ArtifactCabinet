open C.Functions
open C.Types

let () =
  (* The Unsigned is from ocaml-integers package *)
  let white_color = Ctypes.make color in
  Ctypes.setf white_color color_r (Unsigned.UInt8.of_int 255);
  Ctypes.setf white_color color_g (Unsigned.UInt8.of_int 255);
  Ctypes.setf white_color color_b (Unsigned.UInt8.of_int 255);
  Ctypes.setf white_color color_a (Unsigned.UInt8.of_int 0);

  init_window 300 300 "hello, world!";

  print_int C.Types.raylib_version;
  print_endline "";

  while not (window_should_close ()) do
    begin_drawing ();
    clear_background white_color;
    end_drawing ()
  done
