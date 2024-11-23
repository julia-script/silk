const std = @import("std");
const Array = std.ArrayListUnmanaged;
const PackedLists = @import("PackedLists.zig").new;
const ErrorManager = @import("ErrorManager.zig");
const Allocator = std.mem.Allocator;
const Ast = @import("Ast.zig");
const IrGen = @import("IrGen.zig");
const InternedStrings = @import("InternedStrings.zig");
const InternedSlice = InternedStrings.InternedSlice;
const assert = std.debug.assert;
const Ir = @This();
pub const IrLists = PackedLists(u32, std.math.maxInt(u32));

// All arrays of indexes, just aliases to make usage intention clearer
// pub const TypeLists = IrLists;
// pub const ValueLists = IrLists;
// pub const DefLists = IrLists;

strings: InternedStrings,
inst: Array(Inst) = .{},
lists: IrLists = .{},
types: Array(Type) = .{},
values: Array(Value) = .{},
defs: Array(Def) = .{},
ast: *Ast,
allocator: std.mem.Allocator,

pub fn gen(allocator: Allocator, ast: *Ast, error_manager: *ErrorManager) !Ir {
    var ir = Ir{
        .allocator = allocator,
        .ast = ast,
        .strings = try InternedStrings.init(allocator),
    };
    var irgen = try IrGen.init(allocator, &ir, error_manager);
    try irgen.gen();
    defer irgen.deinit();
    return ir;
}
pub fn deinit(self: *Ir) void {
    self.strings.deinit();
    self.inst.deinit(self.allocator);
    self.types.deinit(self.allocator);
    self.values.deinit(self.allocator);
    self.defs.deinit(self.allocator);
    self.lists.deinit(self.allocator);
}

pub const Def = struct {
    pub const Index = u32;
    pub const List = IrLists.List;
    hash: u64 = 0,
    name: InternedSlice,
    ty: Type.Index = .unknown,
    visibility: Visibility = .private,
    external: bool = false,
    exported: bool = false,
    mutable: bool = false,
    is_local: bool = false,
    init: ?Inst.Index = null,
    pub const Visibility = enum {
        public,
        private,
    };
};

pub const Inst = union(enum) {
    pub const Index = u32;
    pub const ListIndex = usize;
    add: Bin,
    sub: Bin,
    mul: Bin,
    div: Bin,
    mod: Bin,
    block: Block,
    inline_block: Block,
    global: ValueType,
    local: ValueType,
    constant: ValueType,
    as: ValueType,
    ret: InstType,

    pub const Bin = struct {
        ty: Type.Index,
        lhs: Index,
        rhs: Index,
    };
    pub const Block = struct {
        instructions: ListIndex,
    };
    pub const ValueType = struct {
        value: Value.Index,
        ty: Type.Index,
    };
    pub const InstType = struct {
        inst: Inst.Index,
        ty: Type.Index,
    };
    // tag: Tag = .sentinel,
    // data: Data.Index = 0,
    // pub const Index = u32;

    // pub const Tag = enum {
    //     sentinel,

    //     add,
    //     sub,
    //     mul,
    //     div,
    //     mod,

    //     @"if",
    //     block,
    // };

    // pub const Data = union {
    //     const Index = u32;
    //     bin: struct {
    //         lhs: u32,
    //         rhs: u32,
    //     },
    //     un: struct {
    //         lhs: u32,
    //         rhs: u32,
    //     },
    //     ret: struct {
    //         val: u32,
    //     },
    //     extend: struct {
    //         val: u32,
    //         ty: Type,
    //     },
    //     void: void,
    // };
};
pub const Type = union(enum) {
    option: struct {
        child: Index,
    },
    function: struct {
        params_ty: Index,
        params_len: u32,
        ret: Index,
    },
    param: struct {
        name: InternedSlice,
        ty: Index,
    },

    tuple: Tuple,

    pub const Index = enum(u32) {
        invalid,
        unknown,
        number,
        int256,
        string,
        boolean,

        void,

        // some,

        _,
        pub const INDEX_START: u32 = std.meta.tags(Type.Index).len;

        pub fn toIndex(self: Index) u32 {
            return @intCast(@intFromEnum(self));
        }

        pub fn asInt(self: Index) u32 {
            return @intCast(@intFromEnum(self));
        }
        pub fn asTypeIndex(self: u32) Type.Index {
            return @enumFromInt(self);
        }
        pub fn fromArrayIndex(index: usize) Index {
            return @enumFromInt(
                @as(u32, @intCast(index + INDEX_START)),
            );
        }

        pub fn toArrayIndex(self: Index) usize {
            return @intCast(@intFromEnum(self) - INDEX_START);
        }

        pub inline fn hash(self: Index) []const u8 {
            return std.mem.asBytes(&self);
        }
    };
    pub const ListIndex = usize;
    inline fn getHasher(tag: []const u8) std.hash.Wyhash {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update(tag);
        return hasher;
    }

    pub const Tuple = struct {
        items_list: ListIndex,

        pub fn fromList(items_list: ListIndex) @This() {
            return .{
                .items_list = items_list,
            };
        }
        pub const WipTuple = struct {
            items: IrLists.List,
            hash: u64 = std.hash.Wyhash.hash(0, "tuple"),
            pub fn append(self: *WipTuple, index: Type.Index) !void {
                try self.items.append(index.toIndex());
                // const hash: [48]u8 = undefined;
                // const hash = std.fmt.hex(std.mem.asBytes(&key)item.hash);

                const prev_hash = self.hash;
                self.hash = std.hash.Wyhash.hash(self.hash, std.mem.asBytes(&index));
                std.debug.print("hash: {} {}\n", .{ prev_hash, self.hash });
                // std.AutoArrayHashMap(comptime V: type)

                // self.hash = std.hash.Wyhash.init(self.hash, item.hash);
            }
            pub fn commit(self: *WipTuple) !struct {
                tuple: Tuple,
                hash: u64,
            } {
                std.debug.print("commit: {}\n", .{self.hash});
                return .{
                    .tuple = .{
                        .items_list = @intCast(try self.items.commit()),
                    },
                    .hash = self.hash,
                };
            }
        };
    };

    pub const Hashed = struct {
        index: Ir.Type.Index,
        hash: u64,

        pub const tyNumber = Hashed{
            .index = Ir.Type.Index.number,
            .hash = std.hash.Wyhash.hash(0, "number"),
        };
        pub const tyBoolean = Hashed{
            .index = Ir.Type.Index.boolean,
            .hash = std.hash.Wyhash.hash(0, "boolean"),
        };
        pub const tyString = Hashed{
            .index = Ir.Type.Index.string,
            .hash = std.hash.Wyhash.hash(0, "string"),
        };

        pub const tyVoid = Hashed{
            .index = Ir.Type.Index.void,
            .hash = std.hash.Wyhash.hash(0, "void"),
        };

        pub const tyUnknown = Hashed{
            .index = Ir.Type.Index.unknown,
            .hash = std.hash.Wyhash.hash(0, "unknown"),
        };
        // pub fn hash(self: Hashed, other: u64) u64 {
        //     return std.hash.Wyhash.hash(self.hash, @truncate(Wyhash.hash(0, std.mem.asBytes(&other))));
        // }
    };

    // pub fn unwrap(: Type) Type {
    //     return @intToEnum(Type, @enumToInt(self) + 1);
    // }
};
test Type {
    // @compileLog(std.meta.tags(Type.Index).len);
}
pub const Value = union(enum) {
    comptime_number: struct {
        val: InternedSlice,
    },
    number: struct {
        val: f64,
    },
    big_number: struct {
        val: i256,
    },
    string: struct {
        val: InternedSlice,
    },
    ref: struct {
        index: Def.Index,
    },
    pub const List = IrLists.List;
    pub const Index = enum(u32) {
        invalid,

        true,
        false,
        none,

        _,
        const INDEX_START: u32 = std.meta.tags(Type.Index).len;

        pub fn toIndex(self: Index) u32 {
            return @intCast(@intFromEnum(self));
        }
        pub fn fromArrayIndex(index: usize) Index {
            return @enumFromInt(
                @as(u32, @intCast(index + INDEX_START)),
            );
        }

        pub fn toArrayIndex(self: Index) usize {
            return @intCast(@intFromEnum(self) - INDEX_START);
        }

        pub inline fn hash(self: Index) []const u8 {
            return std.mem.asBytes(&self);
        }
    };
    // ty: Type.Index,
    // inst: Inst.Index,
};

pub fn init(allocator: std.mem.Allocator, ast: *Ast) !Ir {
    return .{
        .allocator = allocator,
        .ast = ast,
        .inst = try Array(Inst).initCapacity(allocator, 0),
    };
}
const Builder = @import("codegen/llvm/Builder.zig");

const FormatOptions = struct {};
fn formatType(self: *Ir, writer: std.io.AnyWriter, options: FormatOptions, index: Type.Index) !void {
    try writer.print("\x1b[2m[{d}]\x1b[0m", .{@intFromEnum(index)});
    switch (index) {
        .number => try writer.print("number", .{}),
        .boolean => try writer.print("bool", .{}),
        .string => try writer.print("string", .{}),
        .void => try writer.print("void", .{}),
        .unknown => try writer.print("unknown", .{}),
        .int256 => try writer.print("int256", .{}),
        .invalid => try writer.print("invalid", .{}),

        else => {
            const ty: Type = self.types.items[index.toArrayIndex()];
            switch (ty) {
                .function => |func| {
                    try writer.writeAll("(");
                    if (func.params_len > 0) {
                        const params_ty = self.types.items[func.params_ty.toArrayIndex()];
                        assert(std.meta.activeTag(params_ty) == .tuple);

                        var params_iter = self.lists.iterList(params_ty.tuple.items_list);
                        var i: usize = 0;
                        while (params_iter.next()) |param_ty_index| {
                            if (i > 0) try writer.writeAll(", ");
                            try formatType(self, writer, options, @enumFromInt(param_ty_index));
                            i += 1;
                        }
                        // try formatType(self, writer, options, params_ty);
                    }
                    // var params_iter = self.lists.iterList(func.params_ty);
                    // while (params_iter.next()) |param| {
                    //     try formatType(self, writer, options, param);
                    // }
                    try writer.writeAll(") -> ");
                    try formatType(self, writer, options, func.ret);

                    // try writer.print("fn({}) -> {}", .{ func.params_len, func.ret });
                },
                else => {
                    try writer.writeAll(@tagName(ty));
                    // std.debug.print("unknown[]\n", .{ty});
                },
            }
        },
    }
}
fn formatValue(self: *Ir, writer: std.io.AnyWriter, options: FormatOptions, value_index: Value.Index) !void {
    if (@intFromEnum(value_index) < Value.Index.INDEX_START) {
        try writer.print("{s}", .{@tagName(value_index)});
        return;
    }
    const value: Value = self.values.items[value_index.toArrayIndex()];
    switch (value) {
        .comptime_number => |comptime_number| {
            try writer.print("{s}", .{self.strings.getSlice(comptime_number.val)});
        },
        .number => |number| {
            try writer.print("{d}", .{number.val});
        },
        .big_number => |big_number| {
            try writer.print("{d}", .{big_number.val});
        },
        .string => |string| {
            try writer.print("{s}", .{self.strings.getSlice(string.val)});
        },
        .ref => |ref| {
            const def = self.defs.items[ref.index];
            try writer.print("%{s}", .{self.strings.getSlice(def.name)});
        },
        // _ => {
        //     try writer.print("{s}", .{@tagName(value)});
        // },
    }
    _ = options; // autofix

}
pub fn formatInst(self: *Ir, writer: std.io.AnyWriter, options: FormatOptions, inst: Inst.Index) !void {
    const inst_data = self.inst.items[inst];
    switch (inst_data) {
        .block, .inline_block => |block_inst| {
            try writer.print("block_{d}:\n", .{inst});
            var iter = self.lists.iterList(block_inst.instructions);
            while (iter.next()) |inst_index| {
                // try writer.writeAll("\n");
                try formatInst(self, writer, options, inst_index);
            }
            // try formatInst(self, writer, options, block_inst);
        },
        .local => |local_inst| {
            // const value: Ir.Value = self.values.items[local_inst.value];
            // const ty = self.types.items[local_inst.ty];

            try writer.print("  %{d} = local {s} ", .{ inst, @tagName(local_inst.ty) });
            try formatValue(self, writer, options, local_inst.value);
            try writer.writeAll("\n");
        },
        .add => |add_inst| {
            // const ty = self.types.items[add_inst.ty.toArrayIndex()];
            try writer.print("  %{d} = add ", .{inst});
            try formatType(self, writer, options, add_inst.ty);
            try writer.print(" %{any} %{any}\n", .{ add_inst.lhs, add_inst.rhs });
        },
        .ret => |ret_inst| {
            try writer.print("  ret ", .{});
            try formatType(self, writer, options, ret_inst.ty);
            try writer.print(" %{any}\n", .{ret_inst.inst});
        },
        else => {
            try writer.print("  {s}\n", .{@tagName(inst_data)});
        },
    }
    // try writer.print("{s}", .{@tagName(self.inst.items[inst])});
}
pub fn format(self: *Ir, writer: std.io.AnyWriter, options: FormatOptions) !void {
    for (self.defs.items) |def| {
        if (@intFromEnum(def.ty) < Type.Index.INDEX_START) {
            try writer.print("%{s} ", .{self.strings.getSlice(def.name)});
            try formatType(self, writer, options, def.ty);

            if (def.init) |init_inst| {
                try writer.print(" = %{d}\n", .{init_inst});
                // try writer.writeAll(" =  ");
                try formatInst(self, writer, options, init_inst);
            }
            try writer.writeAll("\n");
            continue;
        }
        const ty = self.types.items[def.ty.toArrayIndex()];
        _ = ty; // autofix
        // switch (def.ty) {

        // }
        try writer.print("define @{s}", .{self.strings.getSlice(def.name)});
        try formatType(self, writer, options, def.ty);
        try writer.writeAll("\n");

        if (def.init) |init_inst| try formatInst(self, writer, options, init_inst);
        try writer.writeAll("\n");
        // try def.ty.format(writer);
    }
}
test "llvm" {
    const test_allocator = std.testing.allocator;
    const target = std.Target.Query{};
    _ = target; // autofix
    const builtin = @import("builtin");
    var builder = try Builder.init(.{
        .allocator = test_allocator,
        .name = "test",
        .strip = false,
        .target = builtin.target,

        .triple = "test",
    });
    defer builder.deinit();

    const v = try builder.addVariable(.empty, .i32, .default);
    _ = v; // autofix
    const f = try builder.addFunction(
        try builder.fnType(.i32, &.{.i32}, .normal),
        try builder.strtabString("testfn"),
        .default,
    );

    var wip = try Builder.WipFunction.init(&builder, .{
        .function = f,
        .strip = false,
    });
    defer wip.deinit();
    // const block_index = try wip.block(0, "uuu");
    wip.cursor = .{ .block = try wip.block(0, "entry") };
    const lhs = wip.arg(0);
    const bin = try wip.bin(.add, lhs, lhs, "test");
    const ret = try wip.ret(bin);

    _ = ret; // autofix
    const init_val = try builder.intConst(.i32, 123);
    const wip_const = try builder.addVariable(try builder.strtabString("test_var"), .i32, .default);
    try wip_const.setInitializer(init_val, &builder);

    try wip.finish();
    builder.dump();
}
