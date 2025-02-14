const Op = @import("./opcodes.zig").Op;
const Value = @import("./val.zig").Value;
const std = @import("std");
const activeTag = std.meta.activeTag;
const utils = @import("./utils.zig");
const Block = @import("./Block.zig");
const Definition = @import("./Definition.zig");
const InstData = @import("./inst.zig").InstData;
const Module = @import("./Module.zig");

fn assertFormatsAs(value: anytype, comptime expected: []const u8) !void {
    var buf: [expected.len * 2]u8 = undefined;
    var fbo = std.io.fixedBufferStream(&buf);
    const writer = fbo.writer();
    try writer.print("{}", .{value});
    const actual = fbo.getWritten();
    return try std.testing.expectEqualStrings(actual, expected);
}

pub const Ty = union(enum) {
    void,
    f16,
    f32,
    f64,

    i8,
    i16,
    i32,
    i64,

    u8,
    u16,
    u32,
    u64,

    int,
    float,

    bool,

    type,
    unresolved,
    global: Module.Decl.Ref,
    ref: Ref,

    pub const Ref = utils.MakeRef(.ty, Ty, "ty{d}");
    pub const TyData = union(enum) {
        func: Function,
        array: Array,
        @"struct": Struct,
        // property_of: PropertyOf,

        // decl: Module.Decl.Ref,
        pub const Array = struct {
            type: Ty,
            size: Module.TypedValue,
        };
        pub const Function = struct {
            signature: Module.Signature.Ref,
            declaration: Module.Decl.Ref,
        };
        pub const Struct = struct {
            fields: []const Field,
            sealed: bool,
            associated_ns: ?Module.Namespace.Ref,
            pub const Field = struct {
                name: []const u8,
                ty: Ty,
                source_order_index: u32,
            };
        };
        pub const PropertyOf = struct {
            ty: Ty,
            name: []const u8,
        };
        pub fn displayFn(self: TyData, writer: std.io.AnyWriter, module: *const Module) anyerror!void {
            switch (self) {
                .func => |func| {
                    try writer.print("func({})", .{func.signature});
                },
                .array => |array| {
                    try writer.print("array({}, {})", .{ array.type.display(module), array.size.display(module) });
                },
                .@"struct" => |struct_ty| {
                    try writer.print("struct{{ ", .{});
                    for (struct_ty.fields, 0..) |field, i| {
                        if (i > 0) {
                            try writer.print(", ", .{});
                        }
                        try writer.print("{s}: {}", .{ field.name, field.ty.display(module) });
                    }
                    try writer.print(" }}", .{});
                },
                // .property_of => |property_of| {
                //     try writer.print("{}['{s}']", .{ property_of.ty.display(module), property_of.name });
                // },
            }
        }
        pub fn display(self: TyData, module: *const Module) Module.utils.MakeDisplay(TyData, TyData.displayFn) {
            return .{ .value = self, .module = module };
        }
    };

    const Hasher = std.hash.Wyhash;
    var UNIQUE: u64 = 0;
    pub fn hashTyData(ty_data: TyData, mod: *const Module) u64 {
        return switch (ty_data) {
            .func => |func| {
                var hasher = Hasher.init(0);
                hasher.update(std.mem.asBytes(&@intFromEnum(ty_data)));
                hasher.update(std.mem.asBytes(&func.signature));
                hasher.update(std.mem.asBytes(&func.declaration));
                return hasher.final();
            },
            .array => |array| {
                var hasher = Hasher.init(0);
                hasher.update(std.mem.asBytes(&@intFromEnum(ty_data)));
                const type_hash = Ty.hash(array.type, mod);

                hasher.update(std.mem.asBytes(&type_hash));
                hasher.update(std.mem.asBytes(&array.size.ty.hash(mod)));
                switch (array.size.value) {
                    .bytes => |bytes| {
                        hasher.update(std.mem.asBytes(&bytes));
                    },
                    else => {
                        hasher.update(std.mem.asBytes(&array.size));
                    },
                }
                return hasher.final();
            },
            .@"struct" => {
                UNIQUE += 1;
                var hasher = Hasher.init(UNIQUE);
                hasher.update(std.mem.asBytes(&@intFromEnum(ty_data)));
                return hasher.final();
            },
            // .property_of => |property_of| {
            //     var hasher = Hasher.init(0);
            //     hasher.update(std.mem.asBytes(&@intFromEnum(ty_data)));
            //     hasher.update(std.mem.asBytes(&property_of.name));
            //     hasher.update(std.mem.asBytes(&property_of.ty.hash(mod)));
            //     return hasher.final();
            // },
        };
    }
    pub fn hash(self: Ty, mod: *const Module) u64 {
        switch (self) {
            .ref => |ref| return Ty.hashTyData(mod.tys.get(ref), mod),
            .global => |ref| {
                var hasher = Hasher.init(0);
                hasher.update(std.mem.asBytes(&@intFromEnum(self)));
                hasher.update(std.mem.asBytes(&ref));
                return hasher.final();
            },

            else => {
                var hasher = Hasher.init(0);
                hasher.update(std.mem.asBytes(&@intFromEnum(self)));
                return hasher.final();
            },
        }
    }
    pub fn eql(self: Ty, other: Ty) bool {
        const self_tag = std.meta.activeTag(self);
        const other_tag = std.meta.activeTag(other);
        if (self_tag == other_tag) {
            return switch (self) {
                .ref => |ref| ref.idx == other.ref.idx,
                else => true,
            };
        }
        return false;
    }
    pub fn makeRef(self: Ty) Ref {
        return .{ .ref = Ref.from(@intFromEnum(self)) };
    }
    // pub fn toRef(self: Ty) Ref {
    //     const as_int: u32 = @intFromEnum(self);
    //     return Ref.from(as_int);
    // }
    // pub fn fromRef(ref: Ref) Ty {
    //     return @enumFromInt(ref.ref);
    // }
    // pub inline fn asIndex(self: Ty) u32 {
    //     return @intFromEnum(self) - FIRST_REF;
    // }
    // pub inline fn isComplex(self: Ty) bool {
    //     return @intFromEnum(self) >= FIRST_REF;
    // }

    pub fn accept(self: Ty, other: std.meta.Tag(Ty)) ?Ref {
        if (self == other) {
            return self.makeRef();
        }
        return null;
    }
    pub fn toVal(self: Ty) Value {
        return Value.ImmTy(self);
    }
    pub fn getSize(self: Ty, module: *const Module) u32 {
        return switch (self) {
            .i8, .u8 => 8,
            .i16, .u16 => 16,
            .i32, .u32 => 32,
            .i64, .u64 => 64,
            .int, .float => 0,
            .bool => 1,
            .f32 => 32,
            .f64 => 64,
            .f16 => 16,
            .void => 0,

            else => {
                std.debug.panic("can't get size of '{}'", .{self.display(module)});
            },
        };
    }

    // pub fn format(
    //     self: Ty,
    //     comptime _: []const u8,
    //     _: std.fmt.FormatOptions,
    //     writer: std.io.AnyWriter,
    // ) !void {
    //     switch (self) {
    //         .ref => |ref| {
    //             try writer.print("{}", .{ref});
    //         },
    //         .global => |ref| {
    //             try writer.print("{}", .{ref});
    //         },

    //         else => {
    //             try writer.print("{s}", .{@tagName(self)});
    //         },
    //     }
    // }

    fn displayFn(self: Ty, writer: std.io.AnyWriter, module: *const Module) anyerror!void {
        // try writer.print("{}", .{self});
        switch (self) {
            .ref => |ref| {
                const ty_data = module.tys.get(ref);
                try writer.print("{}", .{ty_data.display(module)});
            },
            .global => |ref| {
                try writer.print("{}", .{ref});
            },
            else => {
                try writer.print("{s}", .{@tagName(self)});
            },
        }
    }
    pub fn display(self: Ty, module: *const Module) Module.utils.MakeDisplay(Ty, displayFn) {
        return .{ .value = self, .module = module };
    }
    pub fn isInt(self: Ty) bool {
        return switch (self) {
            .i8,
            .i16,
            .i32,
            .i64,
            .u8,
            .u16,
            .u32,
            .u64,
            // float and int literal don't have define type yet so they can be coerced to both
            // int and float is just how they are currently represented
            // we may introduce other untyped numbers in the future like bigint and decimals
            .int,
            .float,
            => true,
            else => false,
        };
    }
    pub fn isFloat(self: Ty) bool {
        return switch (self) {
            .f16,
            .f32,
            .f64,
            .float,
            => true,
            else => false,
        };
    }
    pub fn isSigned(self: Ty) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64, .int => true,
            else => false,
        };
    }
    pub fn isUntypedNumber(self: Ty) bool {
        return switch (self) {
            .int, .float => true,
            else => false,
        };
    }
    pub fn bits(self: Ty) u32 {
        return switch (self) {
            .i8, .u8 => 8,
            .i16, .u16 => 16,
            .i32, .u32 => 32,
            .i64, .u64 => 64,
            .int => 0,
            .float => 0,

            else => 0,
        };
    }
    /// There might be cases where the expression hasn't been evaluated yet,
    /// which my be a numeric type but we don't know yet
    pub fn isResolvedNumeric(self: Ty) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .int, .float => true,
            else => false,
        };
    }
    pub fn getTyData(self: Ty, mod: *Module) ?TyData {
        return switch (self) {
            .ref => |ref| mod.tys.get(ref),
            else => null,
        };
    }
    pub fn getChildTy(self: Ty, mod: *Module) ?Ty {
        const ty_data = self.getTyData(mod) orelse return null;
        return switch (ty_data) {
            .array => |array| array.type,
            else => null,
        };
    }
    pub fn resolveGlobalType(global_decl_ref: Module.Decl.Ref, mod: *Module) ?Ty {
        if (!mod.definition_map.contains(global_decl_ref)) {
            return .{ .global = global_decl_ref };
        }
        const def = mod.getDefinitionByDeclRef(global_decl_ref);
        const result = def.dfg.result orelse return null;
        if (!result.isType()) {
            std.debug.panic("can't resolve global type for {}", .{global_decl_ref});
        }

        switch (result.value) {
            .ty => |ty| {
                return ty;
            },
            .global => |global| {
                return resolveGlobalType(global, mod) orelse return .{ .global = global };
            },
            else => {
                return null;
            },
        }
    }
    pub fn resolveGlobal(self: Ty, mod: *Module) ?Ty {
        return switch (self) {
            .global => |ref| {
                return resolveGlobalType(ref, mod);
                // const def = mod.getDefinitionByDeclRef(ref);
                // const result = def.dfg.result orelse return null;
                // if (!result.isType()) {
                //     std.debug.panic("can't resolve global type for {}", .{ref});
                // }
                // switch (result.value) {
                //     .ty => |ty| {
                //         return ty;
                //     },
                //     .global => |global| {
                //         return resolveGlobalType(global, mod);
                //     },
                //     else => {
                //         return null;
                //     },
                // }
            },
            else => null,
        };
    }
    pub fn getFieldByName(self: Ty, mod: *Module, name: []const u8) ?TyData.Struct.Field {
        var ty = self;
        switch (self) {
            .global => |ref| {
                ty = resolveGlobalType(ref, mod) orelse return null;
            },
            else => {},
        }
        const ty_data = ty.getTyData(mod) orelse return null;
        return switch (ty_data) {
            .@"struct" => |struct_ty| {
                for (struct_ty.fields) |field| {
                    if (std.mem.eql(u8, field.name, name)) {
                        return field;
                    }
                }
                return null;
            },

            else => null,
        };
    }
    pub fn getAssociatedNs(self: Ty, mod: *Module) ?Module.Namespace.Ref {
        const ty_data = self.getTyData(mod) orelse return null;
        return switch (ty_data) {
            .@"struct" => |struct_ty| struct_ty.associated_ns,
            else => null,
        };
    }
    pub fn getAssociatedDeclByName(self: Ty, mod: *Module, name: []const u8) ?Module.Decl.Ref {
        const ns_ref = self.getAssociatedNs(mod) orelse return null;
        const ns = mod.namespaces.getPtr(ns_ref);
        var iter = ns.declarations.hashmap.iterator();
        while (iter.next()) |entry| {
            const decl_ref = entry.key_ptr.*;
            const decl = mod.getDeclaration(decl_ref);
            if (std.mem.eql(u8, decl.getName(), name)) {
                return decl_ref;
            }
        }
        return null;
    }
};
test "Ty" {
    try assertFormatsAs(Ty.fromRef(1), "ty#1");
    try assertFormatsAs(Ty.fromRef(Ty.Ref.from(1)), "ty#1");
    try assertFormatsAs(Ty.f32, "f32");
    try std.testing.expectEqual(@intFromEnum(Ty.fromRef(0)), Ty.FIRST_REF);
    try std.testing.expectEqual(Ty.fromRef(0).encode(), Ty.FIRST_REF);
    try std.testing.expectEqual(Ty.decode(Ty.FIRST_REF), Ty.fromRef(0));
    try std.testing.expectEqual(Ty.decode(Ty.FIRST_REF + 1), Ty.fromRef(1));
}
