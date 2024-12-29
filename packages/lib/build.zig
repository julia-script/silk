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

    const enable_tracer = b.option(bool, "emit-trace", "Enable tracer") orelse false;
    const print_tracer = b.option(bool, "print-trace", "Print trace") orelse false;
    const log_scopes = b.option([]const u8, "log-scopes", "ast, hir, sema") orelse &.{};
    const update_snapshots = b.option(bool, "update", "Update snapshots") orelse false;
    const test_filter = b.option(
        []const u8,
        "test-filter",
        "Only run tests with names matching the given substring.",
    ) orelse "";

    const options = b.addOptions();

    options.addOption(bool, "enable_tracer", enable_tracer);
    options.addOption(bool, "update_snapshots", update_snapshots);
    options.addOption(bool, "print_trace", print_tracer);
    options.addOption([]const u8, "test_filter", test_filter);

    options.addOption([]const u8, "log_scopes", log_scopes);
    const cmd_dep = b.dependency("zig-cmd", .{});
    const cmd_module = cmd_dep.module("cmd");
    const expect_dep = b.dependency("expect", .{});
    const expect_module = expect_dep.module("expect");

    const lib = b.addStaticLibrary(.{
        .name = "lang",
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
    });
    lib.root_module.addImport("cmd", cmd_module);
    lib.root_module.addImport("expect", expect_module);
    lib.root_module.addOptions("options", options);
    b.installArtifact(lib);

    const lib_unit_tests = b.addTest(.{
        .name = "test",
        .root_source_file = b.path("src/root.zig"),
        .target = target,
        .optimize = optimize,
        .filter = test_filter,
    });
    lib_unit_tests.root_module.addImport("cmd", cmd_module);
    b.installArtifact(lib_unit_tests);
    lib_unit_tests.root_module.addOptions("options", options);

    queue(.{
        &lib_unit_tests.step,
        &b.addRunArtifact(lib_unit_tests).step,
        b.step("test", "Run unit tests"),
    });
    queue(.{
        &lib.step,
        &b.addInstallArtifact(lib_unit_tests, .{}).step,
        b.step("testdebug", "Install debug test"),
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

    wasm_exe.root_module.addImport("cmd", cmd_module);
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
    wasi_exe.root_module.addImport("cmd", cmd_module);

    wasi_exe.entry = .default;
    // wasi_exe.export_table = true;
    wasi_exe.rdynamic = true;
    // wasi_exe.import_memory = true;

    const install_wasi = b.addInstallArtifact(wasi_exe, .{});

    queue(.{
        &install_wasi.step,

        b.step("wasi", "Install wasi"),
    });

    const test_cases = b.addExecutable(.{
        .name = "snapshots",
        .root_source_file = b.path("src/snapshots.zig"),

        .target = target,

        .optimize = optimize,
    });

    // test_cases.root_module.addImport("cmd", cmd_module);
    test_cases.root_module.addOptions("options", options);

    b.installArtifact(test_cases);

    test_cases.root_module.addImport("expect", expect_module);
    queue(.{
        &test_cases.step,
        &b.addRunArtifact(test_cases).step,
        b.step("snapshots", "Run snapshots"),
    });
}
