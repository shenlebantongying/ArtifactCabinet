package main

import "core:log"
import "vendor:raylib"

dim: f32 : 300
rad: f32 : dim / 2.5
pos_offset: raylib.Vector2 : {dim / 2, dim / 2}


main :: proc() {

	context.logger = log.create_console_logger()

	raylib.InitWindow(i32(dim), i32(dim), "hello, world!")
	raylib.SetTargetFPS(60)
	defer raylib.CloseWindow()

	pos_rotate: raylib.Vector2 = {rad, 0}
	pos: raylib.Vector2

	for (!raylib.WindowShouldClose()) {

		pos_rotate = raylib.Vector2Rotate(pos_rotate, raylib.GetFrameTime())
		log.info(raylib.GetFrameTime(), pos_rotate, sep = " -> ")

		raylib.BeginDrawing()
		defer raylib.EndDrawing()

		drawOrbit(pos_rotate)
	}
}

drawOrbit :: proc(pos_rotate: raylib.Vector2) {
	raylib.ClearBackground(raylib.RAYWHITE)

	raylib.DrawCircleLinesV(pos_offset, rad, raylib.BLUE)
	raylib.DrawCircleV(pos_rotate + pos_offset, 5, raylib.RED)
}
