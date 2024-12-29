const std = @import("std");

pub const Visibility = enum {
    public,
    private,
};

pub const BuiltinNameSpace = enum {
    std,
    cmp,
    pub fn fromSlice(slice: []const u8) BuiltinNameSpace {
        const fields = std.meta.fields(BuiltinNameSpace);
        inline for (fields) |field| {
            if (std.mem.eql(u8, field.name, slice)) {
                return @enumFromInt(field.value);
            }
        }
        std.debug.panic("Builtin namespace not found: {s}", .{slice});
    }
};

pub const BuiltinGlobal = enum {
    comptime_log,

    pub fn fromSlice(slice: []const u8) ?BuiltinGlobal {
        const fields = std.meta.fields(BuiltinGlobal);
        inline for (fields) |field| {
            if (std.mem.eql(u8, field.name, slice)) {
                return @enumFromInt(field.value);
            }
        }
        return null;
    }
};
