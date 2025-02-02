// Changed Jan 29, 2025 to accomodate latest Zig changes
// See history if you're using an older version of Zig.

// in your build.zig, you can specify a custom test runner:
// const tests = b.addTest(.{
//   .target = target,
//   .optimize = optimize,
//   .test_runner = .{ .path = b.path("test_runner.zig"), .mode = .simple }, // add this line
//   .root_source_file = b.path("src/main.zig"),
// });

const std = @import("std");
const builtin = @import("builtin");

const Allocator = std.mem.Allocator;

const BORDER = "=" ** 80;

// use in custom panic handler
var current_test: ?[]const u8 = null;

pub fn main() !void {
    var mem: [8192]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&mem);

    const allocator = fba.allocator();

    const env = Env.init(allocator);
    defer env.deinit(allocator);

    var slowest = SlowTracker.init(allocator, 5);
    defer slowest.deinit();

    var pass: usize = 0;
    var fail: usize = 0;
    var skip: usize = 0;
    var leak: usize = 0;

    const printer = Printer.init();
    printer.fmt("\r\x1b[0K", .{}); // beginning of line and clear to end of line

    for (builtin.test_functions) |t| {
        if (isSetup(t)) {
            t.func() catch |err| {
                printer.status(.fail, "\nsetup \"{s}\" failed: {}\n", .{ t.name, err });
                return err;
            };
        }
    }

    for (builtin.test_functions) |t| {
        if (isSetup(t) or isTeardown(t)) {
            continue;
        }

        var status = Status.pass;
        slowest.startTiming();

        const is_unnamed_test = isUnnamed(t);
        if (env.filter) |f| {
            if (!is_unnamed_test and std.mem.indexOf(u8, t.name, f) == null) {
                continue;
            }
        }

        const friendly_name = blk: {
            const name = t.name;
            var it = std.mem.splitScalar(u8, name, '.');
            while (it.next()) |value| {
                if (std.mem.eql(u8, value, "test")) {
                    const rest = it.rest();
                    break :blk if (rest.len > 0) rest else name;
                }
            }
            break :blk name;
        };

        current_test = friendly_name;
        std.testing.allocator_instance = .{};
        const result = t.func();
        current_test = null;

        const ns_taken = slowest.endTiming(friendly_name);

        if (std.testing.allocator_instance.deinit() == .leak) {
            leak += 1;
            printer.status(.fail, "\n{s}\n\"{s}\" - Memory Leak\n{s}\n", .{ BORDER, friendly_name, BORDER });
        }

        if (result) |_| {
            pass += 1;
        } else |err| switch (err) {
            error.SkipZigTest => {
                skip += 1;
                status = .skip;
            },
            else => {
                status = .fail;
                fail += 1;
                printer.status(.fail, "\n{s}\n\"{s}\" - {s}\n{s}\n", .{ BORDER, friendly_name, @errorName(err), BORDER });
                if (@errorReturnTrace()) |trace| {
                    std.debug.dumpStackTrace(trace.*);
                }
                if (env.fail_first) {
                    break;
                }
            },
        }

        if (env.verbose) {
            const ms = @as(f64, @floatFromInt(ns_taken)) / 1_000_000.0;
            printer.status(status, "{s} ({d:.2}ms)\n", .{ friendly_name, ms });
        } else {
            printer.status(status, ".", .{});
        }
    }

    for (builtin.test_functions) |t| {
        if (isTeardown(t)) {
            t.func() catch |err| {
                printer.status(.fail, "\nteardown \"{s}\" failed: {}\n", .{ t.name, err });
                return err;
            };
        }
    }

    const total_tests = pass + fail;
    const status = if (fail == 0) Status.pass else Status.fail;
    printer.status(status, "\n{d} of {d} test{s} passed\n", .{ pass, total_tests, if (total_tests != 1) "s" else "" });
    if (skip > 0) {
        printer.status(.skip, "{d} test{s} skipped\n", .{ skip, if (skip != 1) "s" else "" });
    }
    if (leak > 0) {
        printer.status(.fail, "{d} test{s} leaked\n", .{ leak, if (leak != 1) "s" else "" });
    }
    printer.fmt("\n", .{});
    try slowest.display(printer);
    printer.fmt("\n", .{});
    std.posix.exit(if (fail == 0) 0 else 1);
}

const Printer = struct {
    out: std.fs.File.Writer,

    fn init() Printer {
        return .{
            .out = std.io.getStdErr().writer(),
        };
    }

    fn fmt(self: Printer, comptime format: []const u8, args: anytype) void {
        std.fmt.format(self.out, format, args) catch unreachable;
    }

    fn status(self: Printer, s: Status, comptime format: []const u8, args: anytype) void {
        const color = switch (s) {
            .pass => "\x1b[32m",
            .fail => "\x1b[31m",
            .skip => "\x1b[33m",
            else => "",
        };
        const out = self.out;
        out.writeAll(color) catch @panic("writeAll failed?!");
        std.fmt.format(out, format, args) catch @panic("std.fmt.format failed?!");
        self.fmt("\x1b[0m", .{});
    }
};

const Status = enum {
    pass,
    fail,
    skip,
    text,
};

const SlowTracker = struct {
    const SlowestQueue = std.PriorityDequeue(TestInfo, void, compareTiming);
    max: usize,
    slowest: SlowestQueue,
    timer: std.time.Timer,

    fn init(allocator: Allocator, count: u32) SlowTracker {
        const timer = std.time.Timer.start() catch @panic("failed to start timer");
        var slowest = SlowestQueue.init(allocator, {});
        slowest.ensureTotalCapacity(count) catch @panic("OOM");
        return .{
            .max = count,
            .timer = timer,
            .slowest = slowest,
        };
    }

    const TestInfo = struct {
        ns: u64,
        name: []const u8,
    };

    fn deinit(self: SlowTracker) void {
        self.slowest.deinit();
    }

    fn startTiming(self: *SlowTracker) void {
        self.timer.reset();
    }

    fn endTiming(self: *SlowTracker, test_name: []const u8) u64 {
        var timer = self.timer;
        const ns = timer.lap();

        var slowest = &self.slowest;

        if (slowest.count() < self.max) {
            // Capacity is fixed to the # of slow tests we want to track
            // If we've tracked fewer tests than this capacity, than always add
            slowest.add(TestInfo{ .ns = ns, .name = test_name }) catch @panic("failed to track test timing");
            return ns;
        }

        {
            // Optimization to avoid shifting the dequeue for the common case
            // where the test isn't one of our slowest.
            const fastest_of_the_slow = slowest.peekMin() orelse unreachable;
            if (fastest_of_the_slow.ns > ns) {
                // the test was faster than our fastest slow test, don't add
                return ns;
            }
        }

        // the previous fastest of our slow tests, has been pushed off.
        _ = slowest.removeMin();
        slowest.add(TestInfo{ .ns = ns, .name = test_name }) catch @panic("failed to track test timing");
        return ns;
    }

    fn display(self: *SlowTracker, printer: Printer) !void {
        var slowest = self.slowest;
        const count = slowest.count();
        printer.fmt("Slowest {d} test{s}: \n", .{ count, if (count != 1) "s" else "" });
        while (slowest.removeMinOrNull()) |info| {
            const ms = @as(f64, @floatFromInt(info.ns)) / 1_000_000.0;
            printer.fmt("  {d:.2}ms\t{s}\n", .{ ms, info.name });
        }
    }

    fn compareTiming(context: void, a: TestInfo, b: TestInfo) std.math.Order {
        _ = context;
        return std.math.order(a.ns, b.ns);
    }
};

const Env = struct {
    verbose: bool,
    fail_first: bool,
    filter: ?[]const u8,

    fn init(allocator: Allocator) Env {
        return .{
            .verbose = readEnvBool(allocator, "TEST_VERBOSE", true),
            .fail_first = readEnvBool(allocator, "TEST_FAIL_FIRST", false),
            .filter = readEnv(allocator, "TEST_FILTER"),
        };
    }

    fn deinit(self: Env, allocator: Allocator) void {
        if (self.filter) |f| {
            allocator.free(f);
        }
    }

    fn readEnv(allocator: Allocator, key: []const u8) ?[]const u8 {
        const v = std.process.getEnvVarOwned(allocator, key) catch |err| {
            if (err == error.EnvironmentVariableNotFound) {
                return null;
            }
            std.log.warn("failed to get env var {s} due to err {}", .{ key, err });
            return null;
        };
        return v;
    }

    fn readEnvBool(allocator: Allocator, key: []const u8, deflt: bool) bool {
        const value = readEnv(allocator, key) orelse return deflt;
        defer allocator.free(value);
        return std.ascii.eqlIgnoreCase(value, "true");
    }
};
const native_arch = builtin.cpu.arch;
const native_os = builtin.os.tag;
const io = std.io;
pub fn dumpCurrentStackTrace(start_addr: ?usize) void {
    nosuspend {
        if (builtin.target.isWasm()) {
            if (native_os == .wasi) {
                const stderr = io.getStdErr().writer();
                stderr.print("Unable to dump stack trace: not implemented for Wasm\n", .{}) catch return;
            }
            return;
        }
        const stderr = io.getStdErr().writer();
        if (builtin.strip_debug_info) {
            stderr.print("Unable to dump stack trace: debug info stripped\n", .{}) catch return;
            return;
        }
        const debug_info = std.debug.getSelfDebugInfo() catch |err| {
            stderr.print("Unable to dump stack trace: Unable to open debug info: {s}\n", .{@errorName(err)}) catch return;
            return;
        };
        writeCurrentStackTrace(stderr, debug_info, io.tty.detectConfig(io.getStdErr()), start_addr) catch |err| {
            stderr.print("Unable to dump stack trace: {s}\n", .{@errorName(err)}) catch return;
            return;
        };
    }
}

pub fn writeCurrentStackTrace(
    out_stream: anytype,
    debug_info: *std.debug.SelfInfo,
    tty_config: io.tty.Config,
    start_addr: ?usize,
) !void {
    if (native_os == .windows) {
        var context: std.debug.ThreadContext = undefined;
        std.debug.assert(std.debug.getContext(&context));
        return std.debug.writeStackTraceWindows(out_stream, debug_info, tty_config, &context, start_addr);
    }
    var context: std.debug.ThreadContext = undefined;
    const has_context = std.debug.getContext(&context);

    var it = (if (has_context) blk: {
        break :blk std.debug.StackIterator.initWithContext(start_addr, debug_info, &context) catch null;
    } else null) orelse std.debug.StackIterator.init(start_addr, null);
    defer it.deinit();
    std.debug.print("it: {any}\n", .{it});

    while (it.next()) |return_address| {
        // std.debug.print("return_address: {any}\n", .{return_address});
        // std.debug.printLastUnwindError(&it, debug_info, out_stream, tty_config);

        //     // On arm64 macOS, the address of the last frame is 0x0 rather than 0x1 as on x86_64 macOS,
        //     // therefore, we do a check for `return_address == 0` before subtracting 1 from it to avoid
        //     // an overflow. We do not need to signal `StackIterator` as it will correctly detect this
        //     // condition on the subsequent iteration and return `null` thus terminating the loop.
        //     // same behaviour for x86-windows-msvc
        const address = return_address -| 1;

        try printSourceAtAddress(debug_info, out_stream, address, tty_config);
    } else {
        // std.debug.printLastUnwindError(&it, debug_info, out_stream, tty_config);

    }
}
const AnsiColors = enum(u8) {
    black = 30,
    red = 31,
    green = 32,
    yellow = 33,
    blue = 34,
    magenta = 35,
    cyan = 36,
    white = 37,
    bright_black = 90,
    bright_red = 91,
    bright_green = 92,
    bright_yellow = 93,
    bright_blue = 94,
    bright_magenta = 95,
    bright_cyan = 96,
    bright_white = 97,
    pub fn toInt(self: AnsiColors) u8 {
        return @intFromEnum(self);
    }
};
// const SQUIGLY_UNDERSCORE = "\x1b[4m";
// <ESC>[4:0m  # no underline
// <ESC>[4:1m  # straight underline
// <ESC>[4:2m  # double underline
// <ESC>[4:3m  # curly underline
// <ESC>[4:4m  # dotted underline
// <ESC>[4:5m  # dashed underline
// <ESC>[4m    # straight underline (for backwards compat)
// <ESC>[24m   # no underline (for backwards compat)

pub fn writeHighlighted(line: []const u8, writer: std.io.AnyWriter, comptime max_column: usize, highlight_at: ?usize) !void {
    var buf: [max_column + 1]u8 = undefined;
    const copy_len = @min(line.len, max_column);
    std.mem.copyForwards(u8, buf[0..copy_len], line);
    buf[copy_len] = 0;
    const null_terminated: [:0]const u8 = @ptrCast(buf[0..copy_len]);
    //
    var tokenizer = std.zig.Tokenizer.init(null_terminated);
    var last_col: usize = 0;
    while (true) {
        const token: std.zig.Token = tokenizer.next();
        if (token.tag == .eof) {
            break;
        }
        const is_highlighted = if (highlight_at) |h|
            token.loc.start >= h and token.loc.end <= h
        else
            false;

        try writer.writeAll(line[last_col..token.loc.start]);
        const token_str = line[token.loc.start..token.loc.end];
        const tag_name = @tagName(token.tag);

        if (is_highlighted) {
            try writer.writeAll("\x1b[4:3m");
        }
        switch (token.tag) {
            .l_paren, .r_paren => {
                try writer.print("\x1b[{d}m{s}\x1b[0m", .{ AnsiColors.cyan.toInt(), token_str });
            },
            .l_brace, .r_brace => {
                try writer.print("\x1b[{d}m{s}\x1b[0m", .{ AnsiColors.magenta.toInt(), token_str });
            },
            .l_bracket, .r_bracket => {
                try writer.print("\x1b[{d}m{s}\x1b[0m", .{ AnsiColors.blue.toInt(), token_str });
            },
            .comma => {
                try writer.print("\x1b[{d}m{s}\x1b[0m", .{ AnsiColors.green.toInt(), token_str });
            },
            .equal,

            .equal_angle_bracket_right,

            .plus_equal,
            .minus_equal,
            .pipe_equal,
            .caret_equal,
            .bang_equal,
            => {
                try writer.print("\x1b[{d}m{s}\x1b[0m", .{ AnsiColors.red.toInt(), token_str });
            },
            .semicolon => {
                try writer.print("\x1b[{d}m{s}\x1b[0m", .{ AnsiColors.bright_white.toInt(), token_str });
            },
            .colon => {
                try writer.print("\x1b[{d}m{s}\x1b[0m", .{ AnsiColors.bright_white.toInt(), token_str });
            },

            .identifier => {
                try writer.print("\x1b[{d}m{s}\x1b[0m", .{ AnsiColors.bright_white.toInt(), token_str });
            },
            .string_literal => {
                try writer.print("\x1b[{d}m{s}\x1b[0m", .{ AnsiColors.bright_green.toInt(), token_str });
            },
            .number_literal => {
                try writer.print("\x1b[{d}m{s}\x1b[0m", .{ AnsiColors.bright_blue.toInt(), token_str });
            },
            else => {
                if (std.mem.startsWith(u8, tag_name, "keyword")) {
                    try writer.print("\x1b[{d}m{s}\x1b[0m", .{ AnsiColors.yellow.toInt(), token_str });
                } else {
                    try writer.writeAll(token_str);
                }
            },
        }
        // switch (token.tag) {
        //     // .keywor => {
        //     //     try writer.print("\x1b[32m{s}\x1b[0m", .{token_str});
        //     // },
        //     .bu => {
        //         try writer.print("\x1b[33m{s}\x1b[0m", .{token_str});
        //     },
        //     .string_literal => {
        //         try writer.print("\x1b[34m{s}\x1b[0m", .{token_str});
        //     },
        //     else => {
        //         try writer.writeAll(token_str);
        //     },
        // }
        last_col = token.loc.end;
        // std.debug.print("token: {any}\n", .{token});
    }
}

pub fn printSourceAtAddress(debug_info: *std.debug.SelfInfo, out_stream: anytype, address: usize, tty_config: io.tty.Config) !void {
    const module = debug_info.getModuleForAddress(address) catch |err| switch (err) {
        error.MissingDebugInfo, error.InvalidDebugInfo => return std.debug.printUnknownSource(debug_info, out_stream, address, tty_config),
        else => return err,
    };

    const symbol_info = module.getSymbolAtAddress(debug_info.allocator, address) catch |err| switch (err) {
        error.MissingDebugInfo, error.InvalidDebugInfo => return std.debug.printUnknownSource(debug_info, out_stream, address, tty_config),
        else => return err,
    };
    defer if (symbol_info.source_location) |sl| debug_info.allocator.free(sl.file_name);

    return std.debug.printLineInfo(
        out_stream,
        symbol_info.source_location,
        address,
        symbol_info.name,
        symbol_info.compile_unit_name,
        tty_config,
        std.debug.printLineFromFileAnyOs,
    );
}
pub fn printLineFromFileAnyOs(out_stream: std.io.AnyWriter, source_location: std.debug.SourceLocation, vertical_padding_lines: usize) !void {
    // Need this to always block even in async I/O mode, because this could potentially
    // be called from e.g. the event loop code crashing.
    var f = try std.fs.cwd().openFile(source_location.file_name, .{});
    defer f.close();
    // TODO fstat and make sure that the file has the correct size
    const start_line = if (source_location.line < vertical_padding_lines) 0 else source_location.line - vertical_padding_lines;
    const end_line = source_location.line + vertical_padding_lines;

    var buf: [std.mem.page_size]u8 = undefined;
    var amt_read = try f.read(buf[0..]);
    const line_start = seek: {
        var current_line_start: usize = 0;
        var next_line: usize = 1;
        while (next_line != start_line) {
            const slice = buf[current_line_start..amt_read];
            if (std.mem.indexOfScalar(u8, slice, '\n')) |pos| {
                next_line += 1;
                if (pos == slice.len - 1) {
                    amt_read = try f.read(buf[0..]);
                    current_line_start = 0;
                } else current_line_start += pos + 1;
            } else if (amt_read < buf.len) {
                return error.EndOfFile;
            } else {
                amt_read = try f.read(buf[0..]);
                current_line_start = 0;
            }
        }
        break :seek current_line_start;
    };
    var slice = buf[line_start..amt_read];
    var written_lines: usize = 0;
    // try out_stream.writeAll("----\n");
    try out_stream.writeBytesNTimes("┄", 40);
    try out_stream.writeAll("\n");

    const max_digits = std.math.log10(end_line);
    while (written_lines < vertical_padding_lines * 2 + 1 and slice.len > 0) {
        const pos = std.mem.indexOfScalar(u8, slice, '\n') orelse slice.len;
        const line = slice[0..pos];
        std.mem.replaceScalar(u8, line, '\t', ' ');
        const line_number = start_line + written_lines;
        const digits = std.math.log10(line_number);
        //dim
        // const dim_color = "\x1b[2m";
        try out_stream.writeByteNTimes(' ', max_digits - digits);
        try out_stream.print("\x1b[2m{d}", .{
            line_number,
        });
        try out_stream.writeAll("┆ ");
        try out_stream.print("\x1b[0m", .{});

        const is_main_line = line_number == source_location.line;
        try writeHighlighted(line, out_stream, 1024, if (is_main_line) source_location.column else null);
        try out_stream.writeAll("\n");
        if (is_main_line) {
            try out_stream.writeByteNTimes(' ', max_digits);
            try out_stream.writeAll("\x1b[2m");
            try out_stream.writeAll(" ┆ ");
            try out_stream.writeAll("\x1b[0m");

            try out_stream.writeByteNTimes(' ', source_location.column - 1);
            try out_stream.writeAll("\x1b[31m^\x1b[0m\n");
        }

        slice = slice[pos + 1 ..];
        written_lines += 1;
    }
    try out_stream.writeBytesNTimes("┄", 40);
    try out_stream.writeAll("\n");
    // for (slice) |c| {
    //     if (c == '\n') {
    // if (std.mem.indexOfScalar(u8, slice, '\n')) |pos| {
    //     const line = slice[0 .. pos + 1];
    //     std.mem.replaceScalar(u8, line, '\t', ' ');
    //     return out_stream.writeAll(line);
    // } else { // Line is the last inside the buffer, and requires another read to find delimiter. Alternatively the file ends.
    //     std.mem.replaceScalar(u8, slice, '\t', ' ');
    //     try out_stream.writeAll(slice);
    //     while (amt_read == buf.len) {
    //         amt_read = try f.read(buf[0..]);
    //         if (std.mem.indexOfScalar(u8, buf[0..amt_read], '\n')) |pos| {
    //             const line = buf[0 .. pos + 1];
    //             std.mem.replaceScalar(u8, line, '\t', ' ');
    //             return out_stream.writeAll(line);
    //         } else {
    //             const line = buf[0..amt_read];
    //             std.mem.replaceScalar(u8, line, '\t', ' ');
    //             try out_stream.writeAll(line);
    //         }
    //     }
    // Make sure printing last line of file inserts extra newline
    // try out_stream.writeByte('\n');
    // }
}
const TraceSymbolIterator = struct {
    it: std.debug.StackIterator,
    debug_info: *std.debug.SelfInfo,

    pub fn init(address: usize) !TraceSymbolIterator {
        var context: std.debug.ThreadContext = undefined;
        const has_context = std.debug.getContext(&context);
        const debug_info = try std.debug.getSelfDebugInfo();
        if (has_context) {
            return .{
                .it = try std.debug.StackIterator.initWithContext(address, debug_info, &context),
                .debug_info = debug_info,
            };
        }
        return .{
            .it = std.debug.StackIterator.init(address, null),
            .debug_info = debug_info,
        };
    }
    const TraceSymbolEntry = struct {
        debug_info: *std.debug.SelfInfo,
        address: usize,
        symbol: std.debug.Symbol,
        module: *std.debug.SelfInfo.Module,
        pub fn printSource(self: TraceSymbolEntry, writer: std.io.AnyWriter) !void {
            const tty_config = io.tty.detectConfig(std.io.getStdErr());
            try std.debug.printSourceAtAddress(
                self.debug_info,
                writer,
                self.address,
                tty_config,
            );
        }
    };
    pub fn next(self: *TraceSymbolIterator) ?TraceSymbolEntry {
        const address = self.it.next() orelse return null;
        const module = self.debug_info.getModuleForAddress(address) catch {
            return null;
        };
        const symbol = module.getSymbolAtAddress(self.debug_info.allocator, address) catch {
            return null;
        };

        return TraceSymbolEntry{
            .debug_info = self.debug_info,
            .address = address,
            .symbol = symbol,
            .module = module,
        };
    }

    pub fn deinit(self: *TraceSymbolIterator) void {
        self.it.deinit();
        self.debug_info.deinit();
    }
};

fn iterTraceSymbols(address: usize) !TraceSymbolIterator {
    return try TraceSymbolIterator.init(address);
}
const ignore_list = [_][]const u8{
    "zig/lib/std/debug.zig",
};
fn findFirstRelevantLocation(address: usize) !?std.debug.SourceLocation {
    var it = iterTraceSymbols(address) catch {
        return null;
    };
    defer it.deinit();

    lp: while (it.next()) |entry| {
        const src_file = entry.symbol.source_location orelse continue;
        for (ignore_list) |ignore| {
            if (std.mem.endsWith(u8, src_file.file_name, ignore)) {
                continue :lp;
            }
        }
        return src_file;
    }
    return null;
}

pub const panic = std.debug.FullPanic(struct {
    pub fn panicFn(msg: []const u8, first_trace_addr: ?usize) noreturn {
        // const first_trace_location = blk: {
        //     // break :blk findFirstRelevantLocation(first_trace_addr orelse @returnAddress()) catch {
        //     //     break :blk null;
        //     // };
        // };
        // _ = first_trace_location; // autofix
        const stderr = io.getStdErr().writer().any();
        if (current_test) |ct| {
            std.debug.print("\x1b[31m\n\n", .{});
            std.debug.print("   Panicked running \"{s}\":\n\n", .{ct});
            std.debug.print("\x1b[0m\n", .{});
            std.debug.print("   \x1b[31;1mError:\x1b[0m {s}\n\n", .{msg});

            var relative_path_buffer: [std.fs.max_path_bytes]u8 = undefined;
            const cwd = std.fs.cwd().realpath(".", relative_path_buffer[0..]) catch "";
            var maybe_it = iterTraceSymbols(first_trace_addr orelse @returnAddress()) catch null;
            if (maybe_it) |*it| {
                while (it.next()) |entry| {
                    const loc = entry.symbol.source_location orelse continue;
                    const symbol_name = entry.symbol.name;
                    if (std.mem.startsWith(u8, loc.file_name, cwd) and !std.mem.startsWith(u8, symbol_name, "assert")) {
                        // bold yellow
                        std.debug.print("\x1b[1;33m", .{});
                        std.debug.print("   @ {s}:{d}:{d} \x1b[2m{s}\x1b[0m\n\n", .{ loc.file_name[cwd.len + 1 ..], loc.line, loc.column, entry.symbol.name });
                        std.debug.print("\x1b[0m", .{});

                        printLineFromFileAnyOs(stderr, loc, 2) catch {};
                        std.debug.print("\x1b[0m\n", .{});
                    } else {
                        // paths outside of the cwd are dimmed
                        std.debug.print("\x1b[2m", .{});
                        std.debug.print("   @ {s}:{d}:{d}\n", .{ loc.file_name, loc.line, loc.column });
                        std.debug.print("\x1b[0m", .{});
                    }
                }
            }
            std.debug.print("\n", .{});
        }
        std.debug.defaultPanic(msg, first_trace_addr);
    }
}.panicFn);

fn isUnnamed(t: std.builtin.TestFn) bool {
    const marker = ".test_";
    const test_name = t.name;
    const index = std.mem.indexOf(u8, test_name, marker) orelse return false;
    _ = std.fmt.parseInt(u32, test_name[index + marker.len ..], 10) catch return false;
    return true;
}

fn isSetup(t: std.builtin.TestFn) bool {
    return std.mem.endsWith(u8, t.name, "tests:beforeAll");
}

fn isTeardown(t: std.builtin.TestFn) bool {
    return std.mem.endsWith(u8, t.name, "tests:afterAll");
}
