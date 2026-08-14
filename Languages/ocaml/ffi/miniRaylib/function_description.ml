open Ctypes
module Types = Types_generated

module Functions (F : Ctypes.FOREIGN) = struct
  open F

  let init_window =
    foreign "InitWindow" (int @-> int @-> string @-> returning void)

  let window_should_close = foreign "WindowShouldClose" (void @-> returning bool)
  let begin_drawing = foreign "BeginDrawing" (void @-> returning void)
  let end_drawing = foreign "EndDrawing" (void @-> returning void)

  let clear_background =
    foreign "ClearBackground" (Types.color @-> returning void)
end
