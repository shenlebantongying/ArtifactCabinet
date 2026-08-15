const std = @import("std");

pub fn build(b: *std.Build) void {
    const module_lib =
        b.createModule(.{ .target = b.graph.host, .root_source_file = b.path("src/mylib.zig") });
    const module_main =
        b.createModule(.{ .target = b.graph.host, .root_source_file = b.path("src/main.zig") });

    const mylib = b.addLibrary(.{ .linkage = .dynamic, .name = "basis", .root_module = module_lib });

    const basis_exe = b.addExecutable(.{ .name = "basis", .root_module = module_main });

    b.installArtifact(mylib);
    b.installArtifact(basis_exe);
    basis_exe.root_module.linkLibrary(mylib);

    // custom run step
    // zig build myrun
    const run_step = b.step("myrun", "Run basis");
    const basis_run = b.addRunArtifact(basis_exe);
    run_step.dependOn(&basis_run.step);

    // Unit tests
    // zig build mytest --summary all
    const lib_unit_tests = b.addTest(.{ .root_module = module_lib });
    const exe_unit_tests = b.addTest(.{ .root_module = module_main });

    const lib_unit_tests_run = b.addRunArtifact(lib_unit_tests);
    const exe_unit_tests_run = b.addRunArtifact(exe_unit_tests);

    const test_step = b.step("mytest", "Run unit tests");
    test_step.dependOn(&lib_unit_tests_run.step);
    test_step.dependOn(&exe_unit_tests_run.step);
}
