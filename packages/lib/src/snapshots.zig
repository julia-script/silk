const std = @import("std");
const Ast = @import("Ast.zig");
const Hir = @import("Hir.zig");
const Mir = @import("Mir.zig");
const Sema = @import("sema/Sema.zig");
const ErrorManager = @import("ErrorManager.zig");
const options = @import("options");
const expect = @import("expect").expect;

const CASES_DIR_PATH = "./src/tests/cases";
const SNAPSHOTS_DIR_PATH = "./src/tests/snapshots";
const SILK_EXTENSION = ".sk";
test "Snapshots" {
    const allocator = std.testing.allocator;
    const cases_dir = try std.fs.cwd().openDir(CASES_DIR_PATH, .{});
    var walker = try cases_dir.walk(allocator);
    defer walker.deinit();
    if (options.test_filter.len > 0) {
        std.debug.print("Filtering test cases with '{s}'\n", .{options.test_filter});
    }

    while (try walker.next()) |entry| {
        if (!std.mem.endsWith(u8, entry.path, SILK_EXTENSION)) continue;
        if (std.mem.endsWith(u8, entry.path, ".ignore.sk")) continue;
        if (comptime options.test_filter.len > 0) {
            if (!std.mem.containsAtLeast(u8, entry.path, 1, options.test_filter)) continue;
        }
        std.debug.print("Running '{s}'\n", .{entry.path});
        runTestCases(allocator, cases_dir, entry.path) catch |err| {
            std.debug.print("Error on '{s}': {s}\n", .{ entry.path, @errorName(err) });
            return err;
        };
    }
}
fn runTestCases(allocator: std.mem.Allocator, dir: std.fs.Dir, path: []const u8) !void {
    const source = try dir.readFileAlloc(allocator, path, std.math.maxInt(usize));
    defer allocator.free(source);

    var errors_manager = try ErrorManager.init(allocator);
    defer errors_manager.deinit();

    const path_without_extension = try std.fs.path.join(allocator, &.{ CASES_DIR_PATH, path[0 .. path.len - SILK_EXTENSION.len] });
    defer allocator.free(path_without_extension);

    const trace_dir = try std.mem.join(allocator, "", &.{ path_without_extension, "-trace" });
    defer allocator.free(trace_dir);
    var ast = try Ast.parse(
        allocator,
        &errors_manager,
        source,
        .{
            .trace_dir = trace_dir,
            .trace_name = "ast",
            .unique_trace_name = true,
        },
    );
    defer ast.deinit();

    var ast_output = std.ArrayList(u8).init(allocator);
    defer ast_output.deinit();
    try ast.format(ast_output.writer().any(), 0, .{
        .show_slice = false,
        .show_node_index = true,
    });

    try checkSnapshot(allocator, ast_output.items, path, ".ast");

    var hir = try Hir.build(allocator, &ast, &errors_manager, .{
        .trace_dir = trace_dir,
        .trace_name = "hir",
        .unique_trace_name = true,
    });
    defer hir.deinit();
    var hir_output = std.ArrayList(u8).init(allocator);
    defer hir_output.deinit();
    try hir.format("", .{}, hir_output.writer().any());

    try checkSnapshot(allocator, hir_output.items, path, ".hir");

    var sema = try Sema.build(allocator, &hir, &errors_manager, .{
        // .tracer =
        // .trace_dir = trace_dir,
        // .trace_name = "sema",
        // .unique_trace_name = true,
    });
    defer sema.deinit();

    try sema.compileAll();

    var sema_output = std.ArrayList(u8).init(allocator);
    defer sema_output.deinit();
    // try Sema.format(&sema, std.io.getStdErr().writer().any());
    try Sema.format(&sema, sema_output.writer().any());

    try checkSnapshot(allocator, sema_output.items, path, ".sema");

    // try checkSnapshot(allocator, dir, sema_output.items, path, ".sema");
}

pub fn writeOutput(allocator: std.mem.Allocator, dir: std.fs.Dir, path: []const u8, data: []const u8, extension: []const u8) !void {
    const path_without_extension = path[0 .. path.len - SILK_EXTENSION.len];
    const ast_output_path = try std.mem.join(allocator, "", &.{ path_without_extension, extension });
    defer allocator.free(ast_output_path);
    // try dir.makePath(path_without_extension);
    try dir.writeFile(.{
        .sub_path = ast_output_path,
        .data = data,
        .flags = .{ .truncate = true },
    });
}

const diff = @import("patience_diff.zig").diff;
pub fn checkSnapshot(allocator: std.mem.Allocator, actual: []const u8, path: []const u8, extension: []const u8) !void {
    const file_name = try std.mem.join(allocator, "", &.{ std.fs.path.stem(path), extension });
    defer allocator.free(file_name);

    const expected_path = try std.fs.path.join(allocator, &.{ SNAPSHOTS_DIR_PATH, std.fs.path.dirname(path) orelse "", file_name });
    try std.fs.cwd().makePath(std.fs.path.dirname(expected_path) orelse ".");
    defer allocator.free(expected_path);

    if (options.update_snapshots) {
        std.debug.print("Updating snapshot {s}\n", .{expected_path});
        try std.fs.cwd().writeFile(.{
            .sub_path = expected_path,
            .data = actual,
            .flags = .{ .truncate = true },
        });
        return;
    }
    const expected_file = std.fs.cwd().openFile(expected_path, .{}) catch |err| {
        switch (err) {
            error.FileNotFound => {
                std.debug.print("File not found, writing to {s}\n", .{expected_path});
                try std.fs.cwd().writeFile(.{
                    .sub_path = expected_path,
                    .data = actual,
                    .flags = .{ .truncate = true },
                });
                return;
            },
            else => return err,
        }
    };
    defer expected_file.close();

    const expected = try expected_file.reader().readAllAlloc(allocator, std.math.maxInt(usize));

    defer allocator.free(expected);

    if (std.mem.eql(u8, actual, expected)) return;
    std.debug.print("Snapshot mismatch on {s}\n", .{expected_path});

    try expect(@as(@TypeOf(actual), expected)).toBeEqualString(actual);
}

// const Snapshot = struct {
//     file_path: []const u8,
//     allocator: std.mem.Allocator,
//     actual: ?[]const u8 = null,
//     expected: ?[]const u8 = null,
//     pub fn init(allocator: std.mem.Allocator, file_path: []const u8) Snapshot {
//         return .{ .file_path = file_path, .allocator = allocator };
//     }
//     pub fn readActual(self: *Snapshot, fun: fn (self: *Snapshot) anyerror![]const u8) !void {
//         _ = self; // autofix
//         _ = fun; // autofix
//     }
//     pub fn writeActual(self: Snapshot, data: []const u8) !void {
//         try std.fs.cwd().writeFile(.{
//             .sub_path = self.file_path,
//             .data = data,
//             .flags = .{ .truncate = true },
//         });
//     }
// };
