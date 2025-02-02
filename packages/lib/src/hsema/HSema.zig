const std = @import("std");
const InternedList = @import("../interned-lists.zig").InternedLists;
pub const Lists = InternedList(usize);
pub const Strings = InternedList(u8);
const Self = @This();

lists: Lists,
strings: Strings,
allocator: std.mem.Allocator,
blocks: std.ArrayListUnmanaged(Block) = .{},

pub const Block = struct {
    instructions: std.ArrayListUnmanaged(Instruction) = .{},
    inputs: std.ArrayListUnmanaged(Instruction) = .{},
    outputs: std.ArrayListUnmanaged(Instruction) = .{},
};

pub const Instruction = struct {
    op: Op,
    // type: Type,
    pub const Op = enum {
        constant,
    };
};

pub const Type = union(enum) {
    type,
    float,
    int,
    u8,
    i8,
    i16,
    i32,
    i64,
    u16,
    u32,
    u64,
    f32,
    f64,
    bchar,
    bool,
    void,

    complex: Type.Index,

    pub const Index = usize;
    pub const Complex = struct {
        size: usize,
        data: Data,
        const Data = union(enum) {
            @"struct": struct {
                fields: std.ArrayListUnmanaged(Type.Index) = .{},
            },
            array: struct {
                child: Type.Index,
                size: usize,
            },
            function: struct {
                args: std.ArrayListUnmanaged(Type.Index) = .{},
                return_type: Type.Index,
            },
        };
    };
};

pub const Value = union(enum) {
    pub const Index = usize;
};

pub const TypedValue = struct {
    value: Value,
    type: Type,
};
