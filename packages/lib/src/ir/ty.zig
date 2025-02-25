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
    any,
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
    def: Definition.Ref,
    global: Module.Decl.Ref,
    inst: Module.InstData.Ref,
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
            len: Module.TypedValue,
            size: ?u32,
        };
        pub const Function = struct {
            signature: Module.Signature.Ref,
            declaration: Module.Decl.Ref,
        };
        pub const Struct = struct {
            fields: []const Field,
            sealed: bool,
            associated_ns: ?Module.Namespace.Ref,
            size: ?u32,
            // size: u32,
            pub const Field = struct {
                name: []const u8,
                ty: Ty,
                source_order_index: u32,
                size: ?u32,
                offset: ?u32,
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
                    try writer.print("array({}, {})", .{ array.type.display(module), array.len.display(module) });
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
                hasher.update(std.mem.asBytes(&array.len.ty.hash(mod)));
                switch (array.len.value) {
                    .bytes => |bytes| {
                        hasher.update(std.mem.asBytes(&bytes));
                    },
                    else => {
                        hasher.update(std.mem.asBytes(&array.len));
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
    pub fn getSize(self: Ty, module: *Module) u32 {
        return self.maybeGetSize(module) orelse std.debug.panic("can't get size of '{}'", .{self.display(module)});
    }

    pub fn maybeGetSize(self: Ty, mod: *Module) ?u32 {
        return switch (self) {
            .void => 0,
            .any => null,
            .f16 => 2,
            .f32 => 4,
            .f64 => 8,

            .i8 => 1,
            .i16 => 2,
            .i32 => 4,
            .i64 => 8,

            .u8 => 1,
            .u16 => 2,
            .u32 => 4,
            .u64 => 8,

            .int => null,
            .float => null,

            .bool => 1,

            .type => null,
            .unresolved => null,
            .ref => |ref| {
                const ty_data = mod.tys.get(ref);
                return switch (ty_data) {
                    .array => |array| array.size,
                    .@"struct" => |struct_ty| struct_ty.size,
                    .func => 0,
                };
            },
            .global => |ref| {
                const ty = Ty.resolveGlobalType(ref, mod) orelse return null;
                return ty.maybeGetSize(mod);
            },
            .def => null,
            .inst => null,
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
            .inst => |inst| {
                try writer.print("{}", .{inst});
            },
            .def => |ref| {
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
    pub fn isNumber(self: Ty) bool {
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
    pub fn getChildTy(self: Ty, mod: *Module) ?Ty {
        const ty_data = self.getTyData(mod) orelse return null;
        return switch (ty_data) {
            .array => |array| array.type,
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
    fn fieldLessThanFn(mod: *Module, lhs: TyData.Struct.Field, rhs: TyData.Struct.Field) bool {
        _ = mod; // autofix
        const lhs_size = lhs.size.?;
        const rhs_size = rhs.size.?;
        return lhs_size < rhs_size;
    }

    pub fn tryComputeSize(fields: []TyData.Struct.Field, mod: *Module) ?u32 {
        for (fields) |*field| {
            const size = field.ty.maybeGetSize(mod);
            if (size == null) {
                return null;
            }
            field.size = size;
        }
        std.mem.sortUnstable(TyData.Struct.Field, fields, mod, fieldLessThanFn);

        var offset: u32 = 0;
        const alignment: u32 = switch (mod.arch) {
            .@"32-bit" => 4,
            .@"64-bit" => 8,
        };
        for (fields) |*field| {
            field.offset = offset;
            offset += field.size.?;
            if (offset % alignment != 0) {
                offset = offset + alignment - (offset % alignment);
            }
            std.debug.print("field: {s}, type: {}, offset: {d} size: {d}\n", .{ field.name, field.ty.display(mod), field.offset.?, field.size.? });
        }

        return offset;
    }
    pub fn satisfies(self: Ty, mod: *Module, other: Ty) bool {
        _ = mod; // autofix
        if (self.eql(other)) return true;
        if (other.eql(.any)) return true;
        const is_numeric = other.isNumber();
        if (is_numeric) {
            if (self.isUntypedNumber()) return true;
            if (!self.isNumber()) return false;
            const is_float = other.isFloat();
            if (is_float and !self.isFloat()) return false;
            const is_signed = other.isSigned();
            if (is_signed != self.isSigned()) return false;

            return self.bits() <= other.bits();
        }

        return false;
    }
    pub fn isResolved(self: Ty, _: *Module) bool {
        return switch (self) {
            .unresolved,
            .def,
            .global,
            .inst,
            => false,
            else => true,
        };
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
