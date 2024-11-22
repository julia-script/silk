const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const options = b.addOptions();
    options.addOption([]const []const u8, "log_scopes", &.{ "MirBuilder", "Mir" });
    // options.addOption(bool, "have_libfoo", enable_foo);
    const test_filter = b.option(
        []const u8,
        "test-filter",
        "Only run tests with names matching the given substring.",
    ) orelse "";

    const lib = b.addStaticLibrary(.{
        .name = "lang",
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    lib.root_module.addOptions("options", options);

    b.installArtifact(lib);
    const lib_unit_tests = b.addTest(.{
        .name = "test",
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
        .filter = test_filter,
    });
    b.installArtifact(lib_unit_tests);
    lib_unit_tests.root_module.addOptions("options", options);

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);

    const debug_step = b.step("debug", "Debug unit tests");
    debug_step.dependOn(&lib_unit_tests.step);
}
