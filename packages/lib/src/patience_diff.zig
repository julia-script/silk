const std = @import("std");
const Array = std.ArrayList;
const Map = std.StringArrayHashMap;
const Patience = @This();
const tw = @import("color.zig").tw;

operations: Array(Operation),
a_lines: Array([]const u8),
b_lines: Array([]const u8),
a_moved: Array([]const u8),
a_moved_index: Array(usize),
b_moved: Array([]const u8),
b_moved_index: Array(usize),

allocator: std.mem.Allocator,
inserted: usize = 0,
deleted: usize = 0,

const Operation = union(enum) {
    insertion: struct {
        line: []const u8,
        index: usize,
    },
    deletion: struct {
        line: []const u8,
        index: usize,
    },
    move: struct {
        line: []const u8,
        a_index: isize,
        b_index: isize,
    },
};

const UniqueCommonMap = Map(UniqueCommonMapEntry);
const UniqueCommonMapEntry = struct {
    count: usize,
    index: usize,
};

pub fn addToResults(self: *Patience, a_index: isize, b_index: isize) !void {
    if (b_index < 0) {
        try self.a_moved.append(self.a_lines.items[@intCast(a_index)]);
        try self.a_moved_index.append(self.operations.items.len);
        self.deleted += 1;
        try self.operations.append(.{ .deletion = .{
            .line = self.a_lines.items[@intCast(a_index)],
            .index = @intCast(a_index),
        } });
        return;
    }
    if (a_index < 0) {
        try self.b_moved.append(self.b_lines.items[@intCast(b_index)]);
        try self.b_moved_index.append(self.operations.items.len);
        self.inserted += 1;
        try self.operations.append(.{ .insertion = .{
            .line = self.b_lines.items[@intCast(b_index)],
            .index = @intCast(b_index),
        } });
        return;
    }
    if (a_index == b_index) {
        return;
    }
    try self.operations.append(.{ .move = .{
        .line = self.b_lines.items[@intCast(b_index)],
        .a_index = @intCast(a_index),
        .b_index = @intCast(b_index),
    } });
}

fn addSubMatch(
    self: *Patience,
    _a_lo: isize,
    _a_hi: isize,
    _b_lo: isize,
    _b_hi: isize,
) !void {
    var a_lo = _a_lo;
    var a_hi = _a_hi;
    var b_lo = _b_lo;
    var b_hi = _b_hi;

    while (a_lo <= a_hi and b_lo <= b_hi and std.mem.eql(u8, self.a_lines.items[@intCast(a_lo)], self.b_lines.items[@intCast(b_lo)])) {
        try self.addToResults(a_lo, b_lo);
        a_lo += 1;
        b_lo += 1;
    }

    const a_hi_temp = a_hi;

    while (a_lo <= a_hi and b_lo <= b_hi and std.mem.eql(u8, self.a_lines.items[@intCast(a_hi)], self.b_lines.items[@intCast(b_hi)])) {
        a_hi -= 1;
        b_hi -= 1;
    }
    const unique_common_map = try self.uniqueCommon(self.a_lines, a_lo, a_hi, self.b_lines, b_lo, b_hi);
    if (unique_common_map.count() == 0) {
        while (a_lo <= a_hi) {
            try self.addToResults(a_lo, -1);
            a_lo += 1;
        }
        while (b_lo <= b_hi) {
            try self.addToResults(-1, b_lo);
            b_lo += 1;
        }
    } else {
        try self.recurseLCS(a_lo, a_hi, b_lo, b_hi, unique_common_map);
    }

    while (a_hi < a_hi_temp) {
        a_hi += 1;
        b_hi += 1;
        try self.addToResults(a_hi, b_hi);
    }
}

pub fn splitLines(allocator: std.mem.Allocator, source: []const u8) !Array([]const u8) {
    var lines = Array([]const u8).init(allocator);
    var i: usize = 0;
    var start: usize = 0;
    while (i < source.len) {
        if (source[i] == '\n') {
            try lines.append(source[start..i]);
            start = i + 1;
        }
        i += 1;
    }
    try lines.append(source[start..]);
    return lines;
}

pub fn init(allocator: std.mem.Allocator, source_a: []const u8, source_b: []const u8) !Patience {
    var difference = Patience{
        .operations = Array(Operation).init(allocator),
        .a_lines = try splitLines(allocator, source_a),
        .b_lines = try splitLines(allocator, source_b),
        .a_moved = Array([]const u8).init(allocator),
        .a_moved_index = Array(usize).init(allocator),
        .b_moved = Array([]const u8).init(allocator),
        .b_moved_index = Array(usize).init(allocator),
        .allocator = allocator,
    };
    try difference.recurseLCS(
        0,
        @as(isize, @intCast(difference.a_lines.items.len)) - 1,
        0,
        @as(isize, @intCast(difference.b_lines.items.len)) - 1,
        null,
    );
    return difference;
}

fn findUnique(
    self: *Patience,
    arr: Array([]const u8),
    lo: isize,
    hi: isize,
) !UniqueCommonMap {
    var line_map = UniqueCommonMap.init(self.allocator);
    // var i: isize = lo;
    if (lo > hi) return line_map;
    for (@intCast(lo)..@intCast(hi)) |i| {
        // while (i <= hi) {
        const line = arr.items[@intCast(i)];
        if (line_map.getPtr(line)) |_| {
            // item.count += 1;
            // item.index = @intCast(i);
            _ = line_map.orderedRemove(line);
        } else {
            try line_map.put(line, .{ .count = 1, .index = @intCast(i) });
        }
        // i += 1;
    }

    // for (line_map.keys()) |key| {
    //     std.debug.print("{s}\n", .{key});
    //     if (line_map.get(key)) |item| if (item.count != 1) {
    //         _ = line_map.orderedRemove(key);
    //     };
    // }
    return line_map;
}

const MatchEntry = struct {
    index_a: isize,
    index_b: isize,
};

fn uniqueCommon(
    self: *Patience,
    a_array: Array([]const u8),
    a_lo: isize,
    a_hi: isize,
    b_array: Array([]const u8),
    b_lo: isize,
    b_hi: isize,
) !Map(MatchEntry) {
    var ma = try self.findUnique(a_array, a_lo, a_hi);
    defer ma.deinit();
    var mb = try self.findUnique(b_array, b_lo, b_hi);
    defer mb.deinit();
    var map = Map(MatchEntry).init(self.allocator);
    for (ma.keys()) |key| {
        if (mb.get(key)) |b_value| {
            try map.put(key, .{
                .index_a = @intCast(ma.get(key).?.index),
                .index_b = @intCast(b_value.index),
            });
        }
    }

    return map;
}

const LCSEntry = struct {
    index_a: isize,
    index_b: isize,
    prev: ?*LCSEntry = null,
};

inline fn getAt(ja: *Array(Array(LCSEntry)), i: usize, j: usize) *LCSEntry {
    std.debug.assert(i < ja.items.len);
    std.debug.assert(j < ja.items[i].items.len);

    return &ja.items[i].items[j];
}

inline fn has(ja: *Array(Array(LCSEntry)), i: usize) bool {
    return i < ja.items.len;
}

fn longestCommonSequence(self: *Patience, ab_map: Map(MatchEntry)) !Array(LCSEntry) {
    var arena = std.heap.ArenaAllocator.init(self.allocator);
    defer arena.deinit();
    const allocator_alloc = arena.allocator();
    var ja = Array(Array(LCSEntry)).init(allocator_alloc);

    for (ab_map.values()) |entry| {
        var i: usize = 0;

        while (has(&ja, i) and
            getAt(
            &ja,
            i,
            ja.items[i].items.len - 1,
        ).index_b < entry.index_b) {
            i += 1;
        }
        const val = LCSEntry{
            .index_a = entry.index_a,
            .index_b = entry.index_b,
            .prev = if (0 < i) getAt(&ja, i - 1, ja.items[i - 1].items.len - 1) else null,
        };
        if (!has(&ja, i)) {
            var new = Array(LCSEntry).init(allocator_alloc);
            try new.append(val);
            try ja.append(new);
        } else {
            try ja.items[i].append(val);
        }
    }

    var lcs = Array(LCSEntry).init(self.allocator);

    if (0 < ja.items.len) {
        const n = ja.items.len - 1;
        try lcs.append(ja.items[n].items[ja.items[n].items.len - 1]);

        while (lcs.items[lcs.items.len - 1].prev) |prev| {
            try lcs.append(prev.*);
        }
    }

    std.mem.reverse(LCSEntry, lcs.items);
    return lcs;
}
const DiffError = error{
    OutOfMemory,
};
pub fn recurseLCS(
    self: *Patience,
    a_lo: isize,
    a_hi: isize,
    b_lo: isize,
    b_hi: isize,
    unique_common_map: ?Map(MatchEntry),
) DiffError!void {
    var map = unique_common_map orelse try self.uniqueCommon(self.a_lines, @intCast(a_lo), @intCast(a_hi), self.b_lines, @intCast(b_lo), @intCast(b_hi));
    const x = try self.longestCommonSequence(map);
    defer x.deinit();
    defer map.deinit();

    if (x.items.len == 0) {
        try self.addSubMatch(a_lo, a_hi, b_lo, b_hi);
    } else {
        if (a_lo < x.items[0].index_a or b_lo < x.items[0].index_b) {
            try self.addSubMatch(a_lo, x.items[0].index_a - 1, b_lo, x.items[0].index_b - 1);
        }
        var i: usize = 0;
        while (i < x.items.len - 1) {
            try self.addSubMatch(
                x.items[i].index_a,
                x.items[i + 1].index_a - 1,
                x.items[i].index_b,
                x.items[i + 1].index_b - 1,
            );
            i += 1;
        }

        if (x.items[i].index_a <= a_hi or x.items[i].index_b <= b_hi) {
            try self.addSubMatch(x.items[i].index_a, a_hi, x.items[i].index_b, b_hi);
        }
    }
}
pub const Diff = struct {
    operations: []const Operation,
    insertions: usize,
    deletions: usize,
    allocator: std.mem.Allocator,
    pub fn deinit(self: *Diff) void {
        self.allocator.free(self.operations);
    }
    pub fn format(self: *Diff, writer: std.io.AnyWriter, options: struct {
        color: bool = true,
    }) !void {
        for (self.operations) |operation| {
            switch (operation) {
                .insertion => |op| {
                    if (options.color) {
                        try writer.print("\x1b[32m + {s}\x1b[0m\n", .{op.line});
                    } else {
                        try writer.print(" + {s}\n", .{op.line});
                    }
                },
                .deletion => |op| {
                    if (options.color) {
                        try writer.print("\x1b[31m - {s}\x1b[0m\n", .{op.line});
                    } else {
                        try writer.print(" - {s}\n", .{op.line});
                    }
                },
                .move => |op| {
                    if (options.color) {
                        //dim
                        try writer.print("\x1b[2m   {s}\x1b[0m\n", .{op.line});
                    } else {
                        try writer.print("   {s}\n", .{op.line});
                    }
                },
            }
        }
    }
};

pub fn diff(allocator: std.mem.Allocator, source_a: []const u8, source_b: []const u8) !Diff {
    var difference = try Patience.init(allocator, source_a, source_b);
    defer difference.deinit();
    return .{
        .operations = try difference.operations.toOwnedSlice(),
        .insertions = difference.inserted,
        .deletions = difference.deleted,
        .allocator = allocator,
    };
}

pub fn deinit(self: *Patience) void {
    self.a_lines.deinit();
    self.b_lines.deinit();
    self.a_moved.deinit();
    self.a_moved_index.deinit();
    self.b_moved.deinit();
    self.b_moved_index.deinit();
}

test "Patience" {
    var res = try Patience.diff(
        std.testing.allocator,
        \\a
        \\Hello,
        \\World!
        \\I'm
        \\Julia
    ,
        \\Hello,
        \\World!
        \\My
        \\name
        \\is
        \\Julia
        ,
    );
    defer res.deinit();
    // try res.format(std.io.getStdErr().writer().any(), .{});
}
