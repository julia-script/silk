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
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    {
        // std.debug.print("{s}\n", .{options.log_scopes});
        const cases_dir = try std.fs.cwd().openDir(CASES_DIR_PATH, .{});
        var walker = try cases_dir.walk(allocator);

        defer walker.deinit();
        if (options.test_filter.len > 0) {
            std.debug.print("Filtering test cases with '{s}'\n", .{options.test_filter});
        }

        while (try walker.next()) |entry| {
            var entry_gpa = std.heap.GeneralPurposeAllocator(.{}){};
            const entry_allocator = entry_gpa.allocator();
            if (!std.mem.endsWith(u8, entry.path, SILK_EXTENSION)) continue;
            if (std.mem.endsWith(u8, entry.path, ".todo.sk")) continue;
            if (comptime options.test_filter.len > 0) {
                if (!std.mem.containsAtLeast(u8, entry.path, 1, options.test_filter)) continue;
            }
            std.debug.print("Running '{s}'\n", .{entry.path});
            runTestCases(entry_allocator, cases_dir, entry.path) catch |err| {
                std.debug.print("Error on '{s}': {s}\n", .{ entry.path, @errorName(err) });
                return err;
            };
            _ = entry_gpa.detectLeaks();
        }
    }
    _ = gpa.detectLeaks();
}
fn runTestCases(allocator: std.mem.Allocator, dir: std.fs.Dir, path: []const u8) !void {
    const source = try dir.readFileAlloc(allocator, path, std.math.maxInt(usize));
    defer allocator.free(source);

    var errors_manager = try ErrorManager.init(allocator);
    defer errors_manager.deinit();

    const path_without_extension = try std.fs.path.join(allocator, &.{ CASES_DIR_PATH, path[0 .. path.len - SILK_EXTENSION.len] });
    defer allocator.free(path_without_extension);

    var sema = try Sema.init(allocator, &errors_manager, .{});
    defer sema.deinit();

    const root_path = try sema.makeRootSource(source, path);

    try sema.compileAll(root_path);
    const root = sema.getSource(root_path);

    var output = std.ArrayList(u8).init(allocator);
    defer output.deinit();

    const writer = output.writer().any();

    try root.ast.format(writer, 0, .{
        .show_slice = false,
        .show_node_index = true,
    });

    try checkSnapshot(allocator, output.items, path, ".ast");

    output.clearRetainingCapacity();

    try root.hir.format("", .{}, writer);
    try checkSnapshot(allocator, output.items, path, ".hir");

    output.clearRetainingCapacity();

    try sema.format(writer);
    try checkSnapshot(allocator, output.items, path, ".sema");
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
