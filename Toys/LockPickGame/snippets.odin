package LockPickGame

import "vendor:raylib"

dynamic_array_remove_multiple :: proc($A: typeid/[]$T, indexes: [dynamic]int) -> (res: [dynamic]T) {
	for i in indexes {
		append(res, A[i])
	}
}

radian_to_degree :: proc(radian: f32) -> f32 {
	return radian / raylib.PI * 180
}

degree_to_radian :: proc(degree: f32) -> f32 {
	return degree / 180 * raylib.PI
}
