// zig test ./a01_bubble_sort.zig

const std = @import("std");
const expectEqual = std.testing.expectEqual;

pub fn swap(l: []i32, p1: usize, p2: usize) void {
    const temp = l[p1];
    l[p1] = l[p2];
    l[p2] = temp;
}

pub fn bubble_sort(l: []i32) void {
    var i = l.len;
    while (i > 0) : (i -= 1) {
        for (0..(i - 1)) |j| {
            if (l[j + 1] > l[j]) {
                swap(l, j + 1, j);
            }
        }
    }
}

test "Is my loop correct?" {
    const solution = [_]i32{ 5, 4, 3, 2, 1 };
    var arr = [_]i32{ 1, 2, 3, 4, 5 };
    var arr2 = [_]i32{ 4, 3, 2, 1, 5 };

    bubble_sort(&arr);
    try expectEqual(arr, solution);

    bubble_sort(&arr2);
    try expectEqual(arr2, solution);
}
