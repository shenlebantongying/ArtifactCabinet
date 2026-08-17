package LockPickGame

import "core:log"
import "vendor:raylib"

dim: f32 : 300
rad: f32 : dim / 2.5
center: raylib.Vector2 : {dim / 2, dim / 2}

Player :: struct {
	angle: f32,
	dir:   f32,
}

Segment :: struct {
	start_angle: f32,
	end_angle:   f32,
}
Segment_Null := Segment{0, 0}
SegIndexList :: [dynamic]int

main :: proc() {
	context.logger = log.create_console_logger()

	raylib.SetRandomSeed(0)
	raylib.SetConfigFlags({.VSYNC_HINT, .WINDOW_HIGHDPI})
	raylib.InitWindow(i32(dim), i32(dim), "hello, world!")
	defer raylib.CloseWindow() // TODO:can we put init in a proc, then do deferred_out?

	raylib.SetTargetFPS(60)

	// Init Game Logic
	player: Player = Player {
		angle = 0,
		dir   = 1,
	}

	segs_all: [dynamic]Segment = {}
	random_insert_segment(&segs_all)

	log.info(&segs_all)

	for (!raylib.WindowShouldClose()) {

		if raylib.IsKeyPressed(raylib.KeyboardKey.SPACE) {
			player.dir *= -1
			hit, index := check_player_hit_any(&player, &segs_all)
			if hit {
				ordered_remove(&segs_all, index)
				{
					new_gen, new_seg := gen_a_segment(&segs_all)
					if new_gen {
						append(&segs_all, new_seg)
						random_insert_segment(&segs_all)
					}
				}
			}
		}

		if player.angle > 360 {
			player.angle -= 360
		} else if player.angle < 0 {
			player.angle += 360
		}
		player.angle += player.dir * radian_to_degree(3 * raylib.GetFrameTime())

		raylib.BeginDrawing()
		defer raylib.EndDrawing()

		draw_orbit()
		draw_segments(&segs_all)
		draw_player(&player)
		raylib.DrawCircleV(center, rad * 1 / 3, raylib.BLUE)
	}
}

// Move to center
mv :: proc(v: raylib.Vector2) -> raylib.Vector2 {
	return v + center
}

draw_orbit :: proc() {
	raylib.ClearBackground(raylib.RAYWHITE)

	raylib.DrawCircleV(center, rad, raylib.Color{0, 121, 241, 100})
}

draw_segments :: proc(segs: ^[dynamic]Segment) {
	color := raylib.GOLD
	for s, index in segs {
		raylib.DrawCircleSector(center, rad, s.start_angle, s.end_angle, i32(s.end_angle - s.start_angle), color)
	}
}

draw_player :: proc(p: ^Player) {
	pos_rotate: raylib.Vector2 = {rad, 0}
	pos_rotate = raylib.Vector2Rotate({rad, 0}, degree_to_radian(p.angle))
	raylib.DrawCircleV(mv(pos_rotate), 5, raylib.RED)
}

check_player_hit_any :: proc(p: ^Player, segs: ^[dynamic]Segment) -> (hit: bool = false, index: int = -1) {
	for s, i in segs^ {
		if s.start_angle < p.angle && p.angle < s.end_angle {
			hit = true
			index = i
			return
		}
	}
	return
}

gen_a_segment :: proc(segs: ^[dynamic]Segment) -> (has_value: bool, seg: Segment) {
	if len(segs) == 0 {
		mid: f32 = f32(raylib.GetRandomValue(0, 360 - 15))
		end := mid + 15
		start := mid - 15

		return true, Segment{start, end}
	}
	return false, Segment_Null
}

random_insert_segment :: proc(segs: ^[dynamic]Segment) {
	new_gen, new_seg := gen_a_segment(segs)
	if new_gen {
		append(segs, new_seg)
	}
}
