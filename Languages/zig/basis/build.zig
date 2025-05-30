const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mylib = b.addSharedLibrary(.{
        .name = "basis",
        .root_source_file = b.path("src/mylib.zig"),
        .target = target,
        .optimize = optimize,
    });

    const basis_exe = b.addExecutable(.{
        .name = "basis",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(mylib);
    b.installArtifact(basis_exe);
    basis_exe.linkLibrary(mylib);

    // custom run step
    const run_step = b.step("myrun", "Run basis");
    const basis_run = b.addRunArtifact(basis_exe);
    run_step.dependOn(&basis_run.step);

    // Unit tests
    // zig build test --summary all
    const lib_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/mylib.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_exe_unit_tests.step);
}
