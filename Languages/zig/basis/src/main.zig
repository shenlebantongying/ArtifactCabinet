const std = @import("std");
const Io = std.Io;

extern fn add(a: i32, b: i32) i32;

pub fn main(init: std.process.Init) !void {
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});
    std.debug.print("1+2 -> {} \n", .{add(1, 2)});

    const io = init.io;
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_file_writer: Io.File.Writer = .init(.stdout(), io, &stdout_buffer);
    const stdout_writer = &stdout_file_writer.interface;

    try stdout_writer.print("Run `zig build test` to run the tests.\n", .{});
    try stdout_writer.flush();
}

test "simple test" {
    const gpa = std.testing.allocator;
    var list = std.ArrayList(i32).empty;
    defer list.deinit(gpa);
    try list.append(gpa, 42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
