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
    as,
    Result,

    float_demote, // Convert (demote) f64 to f32.
    int_extend, // Convert (extend) i32 to i64.
    int_wrap, // Convert (wrap) i64 to i32.
    float_to_int,
    int_to_float,
    reinterpret,

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

pub const BuiltinMember = enum {
    len,
    as,
};
