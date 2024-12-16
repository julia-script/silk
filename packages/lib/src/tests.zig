const std = @import("std");
const Ast = @import("Ast.zig");
const Hir = @import("Hir.zig");
const Mir = @import("Mir.zig");
const ErrorManager = @import("ErrorManager.zig");

const CASES_DIR_PATH = "./src/tests/cases";
const SILK_EXTENSION = ".silk";
test "TestCases" {
    const allocator = std.testing.allocator;
    const cases_dir = try std.fs.cwd().openDir(CASES_DIR_PATH, .{});
    var walker = try cases_dir.walk(allocator);
    defer walker.deinit();

    while (try walker.next()) |entry| {
        if (!std.mem.endsWith(u8, entry.path, SILK_EXTENSION)) continue;
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

    // var header_section_end: usize = 0;

    // for (0..source.len) |i| {
    //     if ((i == 0 or source[i - 1] == '\n') and std.mem.startsWith(u8, source[i..], "///")) {
    //         header_section_end = i;
    //         break;
    //     }
    // }
    // const is_expression = std.mem.containsAtLeast(u8, source, 1, "/// !is_expression");

    // const header_section = source[0..header_section_end];
    // _ = header_section; // autofix
    // const body_section = source[header_section_end + 1 ..];
    // _ = body_section; // autofix

    const path_without_extension = try std.fs.path.join(allocator, &.{ CASES_DIR_PATH, path[0 .. path.len - SILK_EXTENSION.len] });
    defer allocator.free(path_without_extension);

    var ast = try Ast.parse(
        allocator,
        &errors_manager,
        source,
        .{
            .trace_dir = path_without_extension,
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

    std.debug.print("{s}\n", .{ast_output.items});
    try writeOutput(
        allocator,
        dir,
        path,
        ast_output.items,
        ".ast",
    );

    var hir = try Hir.build(allocator, &ast, &errors_manager, .{
        .trace_dir = path_without_extension,
        .trace_name = "hir",
        .unique_trace_name = true,
    });
    defer hir.deinit();

    var hir_output = std.ArrayList(u8).init(allocator);
    defer hir_output.deinit();
    try hir.format("", .{}, hir_output.writer().any());
    std.debug.print("{s}\n", .{hir_output.items});
    try writeOutput(
        allocator,
        dir,
        path,
        hir_output.items,
        ".hir",
    );

    var mir = try Mir.build(allocator, &hir, &errors_manager, .{
        .trace_dir = path_without_extension,
        .trace_name = "mir",
        .unique_trace_name = true,
    });
    defer mir.deinit();

    var mir_output = std.ArrayList(u8).init(allocator);
    defer mir_output.deinit();
    try mir.formatMir(mir_output.writer().any(), .{ .color = false });
    std.debug.print("{s}\n", .{mir_output.items});
    try writeOutput(
        allocator,
        dir,
        path,
        mir_output.items,
        ".mir",
    );
}

pub fn writeOutput(allocator: std.mem.Allocator, dir: std.fs.Dir, path: []const u8, data: []const u8, extension: []const u8) !void {
    const path_without_extension = path[0 .. path.len - SILK_EXTENSION.len];
    const ast_output_path = try std.mem.join(allocator, "", &.{ path_without_extension, extension });
    defer allocator.free(ast_output_path);
    try dir.writeFile(.{
        .sub_path = ast_output_path,
        .data = data,
    });
}
