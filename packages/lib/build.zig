const std = @import("std");

pub inline fn queue(steps: anytype) void {
    var prev: *std.Build.Step = @constCast(steps[0]);
    inline for (1..steps.len) |i| {
        steps[i].dependOn(prev);
        prev = @constCast(steps[i]);
    }
}
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const options = b.addOptions();

    options.addOption([]const []const u8, "log_scopes", &.{
        // "MirBuilder",
        // "Mir",
        // "Compilation",
    });

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
    b.installArtifact(lib);
    lib.root_module.addOptions("options", options);

    const lib_unit_tests = b.addTest(.{
        .name = "test",
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
        .filter = test_filter,
    });
    b.installArtifact(lib_unit_tests);
    lib_unit_tests.root_module.addOptions("options", options);

    queue(.{
        &lib_unit_tests.step,
        &b.addRunArtifact(lib_unit_tests).step,
        b.step("test", "Run unit tests"),
    });

    const wasm_name = "lang";
    const wasm_exe = b.addExecutable(.{
        .name = wasm_name,
        .root_source_file = b.path("src/root-wasm.zig"),
        .target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .freestanding,
        }),
        .optimize = optimize: {
            // Wasm does not support debug info
            if (optimize == .Debug) {
                break :optimize .ReleaseSmall;
            }
            break :optimize optimize;
        },
        .unwind_tables = true,
    });
    wasm_exe.root_module.addOptions("options", options);

    wasm_exe.entry = .disabled;
    wasm_exe.export_table = true;
    wasm_exe.rdynamic = true;
    wasm_exe.import_memory = true;

    const install_wasm = b.addInstallArtifact(wasm_exe, .{});

    queue(.{
        &install_wasm.step,

        b.step("wasm", "Install wasm"),
    });

    const wasi_name = "lang-wasi";
    const wasi_exe = b.addExecutable(.{
        .name = wasi_name,
        .root_source_file = b.path("src/main-wasi.zig"),
        .target = b.resolveTargetQuery(.{
            .cpu_arch = .wasm32,
            .os_tag = .wasi,
        }),
        .optimize = optimize: {
            // Wasm does not support debug info
            if (optimize == .Debug) {
                break :optimize .ReleaseSmall;
            }
            break :optimize optimize;
        },
        .unwind_tables = true,
    });
    wasi_exe.root_module.addOptions("options", options);

    wasi_exe.entry = .default;
    // wasi_exe.export_table = true;
    wasi_exe.rdynamic = true;
    // wasi_exe.import_memory = true;

    const install_wasi = b.addInstallArtifact(wasi_exe, .{});

    queue(.{
        &install_wasi.step,

        b.step("wasi", "Install wasi"),
    });
}
