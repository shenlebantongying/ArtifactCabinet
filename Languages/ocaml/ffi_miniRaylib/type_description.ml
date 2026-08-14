open Ctypes

module Types (F : Ctypes.TYPE) = struct
  open F

  let raylib_version = constant "RAYLIB_VERSION_MAJOR" int

  (* Really fancy way to define a struct. *)
  type color

  let color : color structure typ = structure "Color"
  let color_r = field color "r" uint8_t
  let color_g = field color "g" uint8_t
  let color_b = field color "b" uint8_t
  let color_a = field color "a" uint8_t
  let () = seal color
end
