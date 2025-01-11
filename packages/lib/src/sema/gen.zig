const std = @import("std");
const Tracer = @import("../Tracer2.zig");
const Sema = @import("./Sema.zig");
const Hir = @import("../Hir.zig");
const ErrorManager = @import("../ErrorManager.zig");
const Array = std.ArrayListUnmanaged;
const ArrayHashMap = std.AutoArrayHashMapUnmanaged;
const ChunkedArray = @import("../chunked_array.zig").ChunkedArray;
const Ast = @import("../Ast.zig");
const Source = @import("../Compilation.zig").Source;
const InstContext = @import("./instructions/InstContext.zig");
const Index = @import("./instructions/inst-index.zig");
const Box = @import("../pointer.zig").Box;

inline fn formatHirIndex(hir: *Hir, index: Hir.Inst.Index) ![]const u8 {
    const hir_inst = getHirInst(hir, index);
    var buf: [256]u8 = undefined;
    const slice = try std.fmt.bufPrint(buf[0..], "hir = {d}:.{s}", .{ index, @tagName(hir_inst) });

    return slice;
}

fn getHirInst(hir: *Hir, index: Hir.Inst.Index) Hir.Inst {
    return hir.insts.items[index];
}

pub const Error = error{
    CircularDependency,
    SymbolNotFound,
    NoSpaceLeft,
    SymbolAlreadyExists,
    Overflow,
} || std.mem.Allocator.Error || std.fmt.ParseIntError || std.fmt.ParseFloatError || std.io.AnyWriter.Error;

pub const Hasher = struct {
    hasher: std.hash.Wyhash,
    var MAGIC_NUMBER: u64 = std.hash.Wyhash.hash(0, "magic");
    pub fn new(value: anytype) Hasher {
        var hasher = Hasher{ .hasher = std.hash.Wyhash.init(0) };
        hasher.update(value);
        return hasher;
    }

    pub fn unique() u64 {
        Hasher.MAGIC_NUMBER = std.hash.Wyhash.hash(Hasher.MAGIC_NUMBER, "magic");
        return Hasher.MAGIC_NUMBER;
    }
    pub inline fn update(self: *Hasher, value: anytype) void {
        const T = @TypeOf(value);
        switch (@typeInfo(T)) {
            .pointer => {
                self.hasher.update(value);
            },
            else => {
                self.hasher.update(std.mem.asBytes(&value));
            },
        }
        // switch (@TypeOf(value)) {
        //     []const u8 => {
        //         self.hasher.update(value);
        //     },
        //     else => {
        //         std.debug.print("update: {any}\n", .{@TypeOf(value)});
        //         self.hasher.update(std.mem.asBytes(&value));
        //     },
        // }
    }
    pub fn hash(value: anytype) u64 {
        var hasher = Hasher.new(value);
        return hasher.final();
    }
    pub fn final(self: *Hasher) u64 {
        return self.hasher.final();
    }
};
pub const Builder = struct {
    // sema: *Sema,
    entities: std.ArrayListUnmanaged(Box(Entity)) = .{},

    // strings: Sema.Strings,
    arena: std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,
    tracer: Tracer,
    errors_manager: *ErrorManager,
    // hir: *Hir,
    sema: *Sema,

    symbols: std.AutoArrayHashMapUnmanaged(Sema.Strings.Range, Entity.Key) = .{},
    symbols_by_hir_inst: std.AutoArrayHashMapUnmanaged(Hir.Inst.Index, Entity.Key) = .{},
    // values: ArrayHashMap(u64, Sema.Value) = .{},
    pes: ArrayHashMap(u64, Sema.Type) = .{},
    // lists: Sema.Lists,
    snapshot_scratch: std.ArrayListUnmanaged(u8) = .{},
    // declarations: std.ArrayListUnmanaged(Sema.Declaration) = .{},
    declarations_by_hir_inst: std.AutoArrayHashMapUnmanaged(Hir.Inst.Index, Sema.Declaration.Index) = .{},
    queue: std.AutoArrayHashMapUnmanaged(Entity.Key, void) = .{},

    pub const BuildOptions = struct {
        tracer: bool = false,
        trace_dir: ?[]const u8 = null,
        trace_name: ?[]const u8 = null,
    };
    const BuilderState = struct {
        // symbols: *std.meta.FieldType(Builder, .symbols),
    };
    pub fn getState(self: *Builder) []const u8 {
        self.snapshot_scratch.clearRetainingCapacity();
        const writer = self.snapshot_scratch.writer(self.arena.allocator()).any();

        writer.print("Types: {d} items\n", .{self.sema.types.count()}) catch {};
        writer.print("Values: {d} items\n", .{self.sema.values.count()}) catch {};
        writer.print("Declarations: {d} items\n", .{self.sema.declarations.items.len}) catch {};

        writer.print("\n", .{}) catch {};
        var symbols_iter = self.symbols.iterator();

        writer.print("Symbols: {d} items\n\n", .{self.symbols.count()}) catch {};
        while (symbols_iter.next()) |entry| {
            writer.print("- {s}: Ent({d})\n", .{ self.getSlice(entry.key_ptr.*), entry.value_ptr.* }) catch unreachable;
        }

        writer.print("\n", .{}) catch {};
        writer.print("Symbols by hir inst: {d} items\n\n", .{self.symbols_by_hir_inst.count()}) catch {};
        // var symbols_by_hir_inst_iter = self.symbols_by_hir_inst.iterator();
        // while (symbols_by_hir_inst_iter.next()) |entry| {
        //     writer.print("- {s} = Ent({d})\n", .{ formatHirIndex(self.hir, entry.key_ptr.*) catch unreachable, entry.value_ptr.* }) catch unreachable;
        // }

        writer.print("\n", .{}) catch {};
        writer.print("Entities: {d} items\n\n", .{self.entities.items.len}) catch unreachable;
        for (self.entities.items) |entity_box| {
            var entity = entity_box.ptr.*;
            writer.print("- {s}\n", .{entity.formatKey() catch unreachable}) catch unreachable;

            writer.print("  symbols = .{s}\n ", .{@tagName(entity.symbols)}) catch unreachable;
            switch (entity.type) {
                .resolved => |resolved| {
                    writer.print("  type = .{s} ", .{@tagName(entity.type)}) catch unreachable;
                    self.sema.formatType(writer, resolved) catch unreachable;
                    writer.print("\n", .{}) catch unreachable;
                },
                else => {
                    writer.print("  type = .{s}\n", .{@tagName(entity.type)}) catch unreachable;
                },
            }

            switch (entity.value) {
                .resolved => |resolved| {
                    _ = resolved; // autofix
                    // writer.print("  value = {s} ", .{@tagName(entry.value)}) catch unreachable;
                    // self.sema.formatTypedValue(writer, resolved, .{}) catch unreachable;
                    // writer.print("\n", .{}) catch unreachable;
                },
                else => {
                    writer.print("  value = {s}\n", .{@tagName(entity.value)}) catch unreachable;
                },
            }
            // writer.print("data = {s}\n", .{@tagName(entry.data.data)}) catch unreachable;
        }

        return self.snapshot_scratch.items;
    }

    pub fn getType(self: *Builder, key: Sema.Type.Key) ?Sema.Type {
        switch (key) {
            .simple => {
                return null;
            },
            .complex => |complex| {
                return self.sema.types.entries.items(.value)[complex];
            },
        }
    }
    pub fn getValue(self: *Builder, key: Sema.Value.Key) ?Sema.Value {
        switch (key) {
            .simple => {
                return null;
            },
            .complex => |complex| {
                return self.sema.values.entries.items(.value)[complex];
            },
        }
    }

    pub fn getSimpleTypeSize(self: *Builder, simple: Sema.Type.Simple) usize {
        _ = self; // autofix
        return switch (simple) {
            .bool => @sizeOf(bool),
            .boolean => @sizeOf(bool),
            .i8 => @sizeOf(i8),
            .u8 => @sizeOf(u8),
            .bchar => @sizeOf(u8),
            .i16 => @sizeOf(i16),
            .u16 => @sizeOf(u16),
            .i32 => @sizeOf(i32),
            .u32 => @sizeOf(u32),
            .i64 => @sizeOf(i64),
            .u64 => @sizeOf(u64),
            .usize => @sizeOf(u64), // TODO: this is platform dependent,
            .f32 => @sizeOf(f32),
            .f64 => @sizeOf(f64),
            .number => @sizeOf(f64),
            .int => @sizeOf(i64),
            .float => @sizeOf(f64),
            .void => 0,
            .unknown,
            .type,
            .infer,
            => 0,
            // else => unreachable,
        };
    }
    pub fn getTypeSize(self: *Builder, key: Sema.Type.Key) usize {
        switch (key) {
            .simple => |simple| return self.getSimpleTypeSize(simple),
            .complex => |complex| return self.sema.types.entries.items(.value)[complex].size,
        }
    }
    pub fn reserveDeclaration(self: *Builder) !Sema.Declaration.Index {
        const trace = self.tracer.begin(
            @src(),
            .{ "reserveDeclaration", "Builder.reserveDeclaration()", .{} },
            .{},
        );
        const index = self.sema.declarations.items.len;
        defer trace.end(.{
            .index = index,
        });
        try self.sema.declarations.append(self.sema.allocator, undefined);
        return index;
    }

    pub fn setDeclaration(self: *Builder, index: Sema.Declaration.Index, declaration: Sema.Declaration) void {
        self.sema.declarations.items[index] = declaration;
    }
    pub fn pushDeclaration(self: *Builder, declaration: Sema.Declaration) !Sema.Declaration.Index {
        const index = self.sema.declarations.items.len;
        try self.sema.declarations.append(self.sema.allocator, declaration);
        return index;
    }
    pub fn queueEntity(self: *Builder, key: Entity.Key) !void {
        const entity = self.getEntity(key);
        const trace = self.tracer.begin(
            @src(),
            .{ "queueEntity", "Builder.queueEntity({s})", .{try entity.formatKey()} },
            .{},
        );
        defer trace.end(.{});
        try self.queue.put(self.allocator, key, {});
    }
    pub fn getEntityKeyByHirInst(self: *Builder, hir_inst_index: Hir.Inst.Index) ?Entity.Key {
        return self.symbols_by_hir_inst.get(hir_inst_index);
    }
    pub fn getEntityByHirInst(self: *Builder, hir_inst_index: Hir.Inst.Index) *Entity {
        const entity_key = self.symbols_by_hir_inst.get(hir_inst_index) orelse std.debug.panic("no entity for hir_inst: {d}", .{hir_inst_index});
        return self.getEntity(entity_key);
    }
    pub fn getEntity(self: *Builder, key: Entity.Key) *Entity {
        return self.entities.items[key].ptr;
    }
    pub fn getEntityBySymbol(self: *Builder, name: []const u8) ?*Entity {
        const range = self.strings.getRange(name) orelse return null;
        const entity_key = self.symbols.get(range) orelse return null;
        return self.getEntity(entity_key);
    }

    pub fn getSlice(self: *Builder, range: Sema.Strings.Range) []const u8 {
        return self.sema.strings.getSlice(range);
    }
    pub fn internSlice(self: *Builder, slice: []const u8) Error!Sema.Strings.Range {
        return self.sema.strings.internSlice(slice);
    }
    inline fn internMultipleSlices(self: *Builder, slices: anytype) Error!Sema.Strings.Range {
        var list = self.sema.strings.new();
        inline for (slices) |slice| {
            if (@TypeOf(slice) == Sema.Strings.Range) {
                try list.appendSlice(self.getSlice(slice));
            } else {
                try list.appendSlice(slice);
            }
        }
        return try list.commit();
    }
    pub fn newList(self: *Builder) Sema.Lists.WorkingList {
        return self.sema.lists.new();
    }

    pub fn unwrapTypeValue(self: *Builder, value_key: Sema.Value.Key) Sema.Type.Key {
        switch (value_key) {
            .complex => |complex| {
                const value = self.sema.values.entries.items(.value)[complex];
                return switch (value.data) {
                    .type => |type_key| {
                        return type_key;
                    },
                    else => std.debug.panic("not a type value: {s}", .{@tagName(value.data)}),
                };
            },
            .simple => |simple| {
                switch (simple) {
                    inline else => |simple_value| {
                        const value_tag_name = @tagName(simple_value);
                        if (comptime std.mem.startsWith(u8, value_tag_name, "type_")) {
                            const type_tag_name = value_tag_name[5..];
                            return Sema.Type.simple(
                                std.meta.stringToEnum(Sema.Type.Simple, type_tag_name) orelse std.debug.panic(
                                    "TODO: unwrapTypeValue {s}",
                                    .{type_tag_name},
                                ),
                            );
                        }
                        std.debug.panic("not a type value: {s}", .{value_tag_name});
                    },
                }
            },
        }
    }
    pub fn unwrapPointerType(self: *Builder, type_key: Sema.Type.Key) ?Sema.Type.Key {
        const ty = self.getType(type_key) orelse return null;
        switch (ty.data) {
            .pointer => |pointer| {
                return pointer.child;
            },
            // .slice => |slice| {
            //     return slice.child;
            // },
            else => {
                return null;
            },
        }
    }
    pub fn isSliceEqual(self: *Builder, slice_range: Sema.Strings.Range, slice: []const u8) bool {
        if (slice_range.len != slice.len) return false;
        return std.mem.eql(u8, self.getSlice(slice_range), slice);
    }
    pub fn maybeGetChildType(self: *Builder, type_key: Sema.Type.Key) ?Sema.Type.Key {
        const ty = self.getType(type_key) orelse return null;
        return switch (ty.data) {
            .pointer => |pointer| pointer.child,
            .slice => |slice| slice.child,
            // .array => |array| array.child,

            else => null,
        };
    }
    pub fn maybeGetPointer(self: *Builder, typed_value: Sema.TypedValue) !?usize {
        const value = self.getValue(typed_value.value) orelse return null;
        return switch (value.data) {
            // .integer => |integer| @intCast(integer),
            .bytes => try self.readNumberAsType(u32, typed_value),
            .slice => |slice| if (slice.ptr.isComptimeKnown()) try self.readNumberAsType(u32, slice.ptr) else null,
            else => null,
        };
    }

    fn getTypeKeyHash(self: *Builder, type_key: Sema.Type.Key) u64 {
        switch (type_key) {
            .simple => |simple| {
                const hash = switch (simple) {
                    inline else => |simple_type| comptime std.hash.Wyhash.hash(0, @tagName(simple_type)),
                };
                return hash;
            },
            .complex => |complex| {
                const ty = self.sema.types.entries.items(.value)[complex];
                return ty.hash;
            },
        }
    }
    fn internType(self: *Builder, ty: Sema.Type) Error!Sema.Type.Key {
        const trace = self.tracer.begin(
            @src(),
            .{ "internType", "Builder.internType(.{s}, hash = {x})", .{ @tagName(ty.data), ty.hash } },
            .{
                .ty = ty,
            },
        );
        if (self.sema.types.getIndex(ty.hash)) |index| {
            const existing = Sema.Type.complex(index);

            trace.end(.{
                .type_key = existing,
                .is_new = false,
            });
            return existing;
        }
        const index = self.sema.types.count();
        try self.sema.types.put(self.sema.allocator, ty.hash, ty);
        const new_key = Sema.Type.complex(index);
        self.tracer.trace(
            @src(),
            .{ "internType", "[INTERN_NEW_TYPE(.{s}, index = {d}, hash = {x})]", .{ @tagName(ty.data), index, ty.hash } },
            .{},
        );
        trace.end(.{
            .type_key = new_key,
            .is_new = true,
        });
        return new_key;
    }

    pub fn internFlatUnionTypeData(self: *Builder, fields: []const Sema.Type.Key) Error!Sema.Type.Key {
        var fields_list = self.sema.lists.new();
        for (fields) |field| {
            try fields_list.append(field.encode());
        }
        return try self.internTypeData(.{
            .flat_union = .{
                .fields = try fields_list.commit(),
            },
        });
    }
    pub fn internTypeData(self: *Builder, data: Sema.Type.Data) Error!Sema.Type.Key {
        const trace = self.tracer.begin(
            @src(),
            .{ "internTypeData", "Builder.internTypeData(.{s})", .{@tagName(data)} },
            .{
                .data = data,
            },
        );
        defer trace.end(.{});
        switch (data) {
            .typeof => |typeof| {
                var hasher = Hasher.new("typeof");
                hasher.update(self.getTypeKeyHash(typeof.child));
                return try self.internType(.{
                    .hash = hasher.final(),
                    .data = data,
                    .alignment = 0,
                    .size = 0,
                });
            },
            .function => |function| {
                _ = function; // autofix
                // var hasher = Hasher.new("function");
                // for (self.sema.lists.getSlice(function.params)) |param| {
                //     const decoded = Sema.Type.Key.decode(param);
                //     hasher.update(self.getTypeKeyHash(decoded));
                // }
                // const hash = hasher.final();
                // return key;
                return try self.internType(.{
                    .hash = Hasher.unique(),
                    .data = data,
                    .alignment = 0,
                    .size = 0,
                });
            },
            .array => |array| {
                var hasher = Hasher.new("array");
                hasher.update(self.getTypeKeyHash(array.child));
                hasher.update(array.len);

                const alignment, const element_size = blk: {
                    if (self.getType(array.child)) |child_ty| {
                        break :blk .{
                            child_ty.alignment,
                            child_ty.size,
                        };
                    }
                    const child_alignment = self.getSimpleTypeSize(array.child.simple);
                    break :blk .{
                        child_alignment,
                        child_alignment,
                    };
                };

                return try self.internType(.{
                    .hash = hasher.final(),
                    .data = data,
                    .alignment = alignment,
                    .size = element_size * array.len,
                });
            },

            .pointer => |pointer| {
                var hasher = Hasher.new("pointer");
                hasher.update(self.getTypeKeyHash(pointer.child));
                const alignment = self.getSimpleTypeSize(.usize);
                return try self.internType(.{
                    .hash = hasher.final(),
                    .data = data,
                    .alignment = alignment,
                    .size = alignment,
                });
            },
            .slice => |slice| {
                var hasher = Hasher.new("slice");
                hasher.update(self.getTypeKeyHash(slice.child));

                const alignment = self.getSimpleTypeSize(.usize);
                return try self.internType(.{
                    .hash = hasher.final(),
                    .data = data,
                    .alignment = alignment,
                    .size = alignment * 2,
                });
            },
            .struct_field, .@"struct" => {
                // structs properties like alignment,offsets, size, are a bit more complicated to calculate so we resolve it before interning
                // also, they are always unique, so it doesn't really matter
                unreachable;
            },
            .flat_union => {
                var hasher = Hasher.new("flat_union");
                var alignment: usize = 0;
                var size: usize = 0;
                // TODO: flatten nested unions
                // TODO: promote types, ex, if f32 | number, promote to just number. If i32 | any, promote to just any.
                // TODO: make sure types are unique
                // TODO: should unions with a single type be promoted to that type? I guess so..
                // TODO: hash should not depend on order of fields
                for (self.sema.lists.getSlice(data.flat_union.fields)) |field| {
                    const field_type_key = Sema.Type.Key.decode(field);
                    hasher.update(self.getTypeKeyHash(field_type_key));
                    switch (field_type_key) {
                        .simple => |simple| {
                            const field_size = self.getSimpleTypeSize(simple);
                            alignment = @max(alignment, field_size);
                            size = @max(size, field_size);
                        },
                        .complex => |complex| {
                            const field_type = self.getComplexType(complex);
                            alignment = @max(alignment, field_type.alignment);
                            size = @max(size, field_type.size);
                        },
                    }
                }
                return try self.internType(.{
                    .hash = hasher.final(),
                    .data = data,
                    .alignment = alignment,
                    .size = size,
                });
            },
            .any => |any| {
                var hasher = Hasher.new("any");
                var alignment: usize = 0;
                var size: usize = 0;
                switch (any.concrete) {
                    .simple => |simple| {
                        hasher.update(self.getTypeKeyHash(any.concrete));
                        alignment = self.getSimpleTypeSize(simple);
                        size = alignment;
                    },
                    .complex => |complex| {
                        const ty = self.getComplexType(complex);
                        hasher.update(ty.hash);
                        alignment = ty.alignment;
                        size = ty.size;
                    },
                }

                // const value_arg_inst_index = self.getInstructionIndex(hir_args_list[0]);

                // return try self.pushMaybeCastInstruction(hir_inst_index, value_arg_inst_index, Sema.Type.simple(.f32));
                // return try self.pushInstruction(hir_inst_index, .{
                hasher.update(self.getTypeKeyHash(data.any.concrete));
                return try self.internType(.{
                    .hash = hasher.final(),
                    .data = data,
                    .alignment = alignment,
                    .size = size,
                });
            },
            .builtin_global => {
                var hasher = Hasher.new("builtin_global");
                hasher.update(@tagName(data.builtin_global));
                return try self.internType(.{
                    .hash = hasher.final(),
                    .data = data,
                    .alignment = 0,
                    .size = 0,
                });
            },
            .builtin_member => {
                var hasher = Hasher.new("builtin_member");
                hasher.update(@tagName(data.builtin_member.member));
                return try self.internType(.{
                    .hash = hasher.final(),
                    .data = data,
                    .alignment = 0,
                    .size = 0,
                });
            },
        }
        _ = type; // autofix
        std.debug.panic("TODO: internType {s}", .{@tagName(data)});
    }
    fn getValueKeyHash(self: *Builder, value_key: Sema.Value.Key) u64 {
        switch (value_key) {
            .simple => |simple| {
                const hash = switch (simple) {
                    inline else => |simple_type| comptime std.hash.Wyhash.hash(0, @tagName(simple_type)),
                };
                return hash;
            },
            .complex => |complex| {
                const value = self.sema.values.entries.items(.value)[complex];
                return value.hash;
            },
        }
    }

    pub inline fn numberAsBytes(value: anytype) [8]u8 {
        var bytes: [8]u8 = undefined;
        std.mem.copyForwards(u8, &bytes, std.mem.asBytes(&value));
        return bytes;
    }

    pub fn numberAsBytesValueKey(self: *Builder, value: anytype) !Sema.Value.Key {
        return try self.internValueData(.{ .bytes = numberAsBytes(value) });
    }
    fn internValue(self: *Builder, value: Sema.Value) Error!Sema.Value.Key {
        const trace = self.tracer.begin(
            @src(),
            .{ "internValue", "Builder.internValue(.{s}, hash = {x})", .{ @tagName(value.data), value.hash } },
            .{
                .value = value,
            },
        );
        if (self.sema.values.getIndex(value.hash)) |index| {
            const existing = Sema.Value.complex(index);

            trace.end(.{
                .value_key = existing,
                .is_new = false,
            });
            return existing;
        }
        const index = self.sema.values.count();
        try self.sema.values.put(self.sema.allocator, value.hash, value);
        const new_key = Sema.Value.complex(index);
        self.tracer.trace(
            @src(),
            .{ "internValue", "[INTERN_NEW_VALUE(.{s}, index = {d}, hash = {x})]", .{ @tagName(value.data), index, value.hash } },
            .{},
        );
        trace.end(.{
            .value_key = new_key,
            .is_new = true,
        });
        return new_key;
    }

    pub fn internRegisterValue(self: *Builder, value: anytype) Error!Sema.Value.Key {
        const slice = switch (@TypeOf(value)) {
            []const u8 => value,
            []u8 => value,
            else => std.mem.asBytes(&value),
        };

        const key = try self.internValue(.{
            .hash = std.hash.Wyhash.hash(0, slice),
            .data = .{ .comptime_register = undefined },
        });
        var result: []u8 = self.sema.values.entries.items(.value)[key.complex].data.comptime_register[0..];
        std.mem.copyForwards(u8, result[0..slice.len], slice);
        return key;
    }
    pub fn readRegisterAsType(self: *Builder, T: type, value_key: Sema.Value.Key) T {
        const slice = self.getValue(value_key).?.data.comptime_register;
        return std.mem.bytesToValue(T, slice[0..8]);
        // switch (@typeInfo(T)) {
        //     .integer => return std.mem.readInt(T, slice, .little),
        //     .float => return std.mem.bytesToValue(T, slice),
        //     else => unreachable,
        // }
        // return std.mem.readInt(T, slice, .little);
    }
    pub fn internSimpleValue(self: *Builder, comptime ty: Sema.Type.Simple, value: anytype) Error!Sema.Value.Key {
        switch (ty) {
            .f32, .f64, .number => {
                return try self.internValueData(.{
                    .float = @floatCast(value),
                });
            },
            .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .usize => {
                return try self.internValueData(.{
                    .integer = @intCast(value),
                });
            },
            .bool => {
                if (value) {
                    return Sema.Value.simple(.true);
                } else {
                    return Sema.Value.simple(.false);
                }
            },
            else => unreachable,
        }
    }
    pub fn internValueData(self: *Builder, data: Sema.Value.Data) Error!Sema.Value.Key {
        const trace = self.tracer.begin(
            @src(),
            .{ "internValueData", "Builder.internValueData(.{s})", .{@tagName(data)} },
            .{
                .data = data,
            },
        );
        defer trace.end(.{});
        switch (data) {
            .function => |function| {
                var hash = Hasher.new("function");
                const type_hash = self.getTypeKeyHash(function.type);
                hash.update(type_hash);
                if (function.init) |init| {
                    hash.update(init);
                }
                return try self.internValue(.{
                    .hash = hash.final(),
                    .data = data,
                });
            },
            // .integer => |integer| {
            //     var hasher = Hasher.new("integer");
            //     hasher.update(integer);
            //     return try self.internValue(.{
            //         .hash = hasher.final(),
            //         .data = data,
            //     });
            // },
            .bytes => |bytes| {
                var hasher = Hasher.new("bytes");
                hasher.update(bytes);
                return try self.internValue(.{
                    .hash = hasher.final(),
                    .data = data,
                });
            },
            // .float => |float| {
            //     var hasher = Hasher.new("float");

            //     hasher.update(float);
            //     return try self.internValue(.{
            //         .hash = hasher.final(),
            //         .data = data,
            //     });
            // },
            .type => |type_key| {
                const type_hash = self.getTypeKeyHash(type_key);
                var hasher = Hasher.new("type");
                hasher.update(type_hash);
                return try self.internValue(.{
                    .hash = hasher.final(),
                    .data = data,
                });
                // return switch (type_key) {
                //     .simple => |simple| {
                //         return switch (simple) {
                //             inline else => |simple_type| {
                //                 const simple_type_name = @tagName(simple_type);
                //                 const simple_value_name = "type_" ++ simple_type_name;
                //                 const simple_value = std.meta.stringToEnum(Sema.Value.Simple, simple_value_name) orelse {
                //                     std.debug.panic("TODO: internValueData {s}", .{simple_value_name});
                //                 };
                //                 return Sema.Value.simple(simple_value);
                //             },
                //         };
                //     },
                //     .complex => |complex| {
                //         var hasher = Hasher.new("complex");
                //         const type_hash = try self.getTypeKeyHash(complex);
                //         hasher.update(type_hash);
                //         return try self.internValue(.{
                //             .hash = hasher.final(),
                //             .data = data,
                //         });
                //     },
                // };
            },
            .global => |global| {
                _ = global; // autofix
                // var hasher = Hasher.new("global");
                // const type_hash = self.getTypeKeyHash(global.typed_value.type);
                // const value_hash = self.getValueKeyHash(global.typed_value.value);
                // hasher.update(type_hash);
                // hasher.update(value_hash);
                // if (global.init) |init| {
                //     hasher.update(init);
                // }
                return try self.internValue(.{
                    .hash = Hasher.unique(),
                    .data = data,
                });
            },
            // .comptime_pointer => |pointer| {
            //     var hasher = Hasher.new("comptime_pointer");
            //     hasher.update(pointer);
            //     return try self.internValue(.{
            //         .hash = hasher.final(),
            //         .data = data,
            //     });
            // },
            // .array_init => |array_init| {
            //     _ = array_init; // autofix
            //     // const list_slice = self.sema.lists.getSlice(array_init.items_list);
            //     // hasher.update(list_slice);
            //     // const type_key = try self.getTypeKeyHash(array_init.type);
            //     // hasher.update(type_key);

            //     return try self.internValue(.{
            //         .hash = Hasher.unique(),
            //         .data = data,
            //     });
            // },
            // .type_init => |type_init| {
            //     _ = type_init; // autofix
            //     return try self.internValue(.{
            //         .hash = Hasher.unique(),
            //         .data = data,
            //     });
            // },
            // .field_init => |field_init| {
            //     _ = field_init; // autofix
            //     return try self.internValue(.{
            //         .hash = Hasher.unique(),
            //         .data = data,
            //     });
            // },
            // .builtin_global => |builtin_global| {
            //     var hasher = Hasher.new("builtin_global");
            //     hasher.update(builtin_global.builtin);
            //     return try self.internValue(.{
            //         .hash = hasher.final(),
            //         .data = data,
            //     });
            // },
            .slice => |slice| {
                var hasher = Hasher.new("slice-value");
                hasher.update(self.getTypeKeyHash(slice.ptr.type));
                hasher.update(self.getValueKeyHash(slice.ptr.value));
                // hasher.update(self.getValueKeyHash(slice.len));
                switch (slice.len) {
                    .resolved => |constant| {
                        hasher.update(self.getTypeKeyHash(constant.type));
                        hasher.update(self.getValueKeyHash(constant.value));
                    },
                    .ref => |ref_inst_index| {
                        _ = ref_inst_index; // autofix

                        // const
                        // hasher.update(self.getTypeKeyHash(ref.type));
                    },
                }
                return try self.internValue(.{
                    .hash = hasher.final(),
                    .data = data,
                });
            },
            .string_literal => |string_literal| {
                var hasher = Hasher.new("string_literal");
                const slice = self.sema.strings.getSlice(string_literal);
                hasher.update(slice);
                return try self.internValue(.{
                    .hash = hasher.final(),
                    .data = data,
                });
            },
            .deref => |deref| {
                var hasher = Hasher.new("deref");
                hasher.update(self.getTypeKeyHash(deref.ptr.type));
                hasher.update(self.getValueKeyHash(deref.ptr.value));
                return try self.internValue(.{
                    .hash = hasher.final(),
                    .data = data,
                });
            },
            .flat_union => |flat_union| {
                _ = flat_union; // autofix
                var hasher = Hasher.new("flat_union");
                switch (data.flat_union.active_field) {
                    .ref => |ref| {
                        hasher.update("ref");
                        hasher.update(ref);
                    },
                    .resolved => |resolved| {
                        hasher.update("resolved");
                        hasher.update(self.getTypeKeyHash(resolved.type));
                        hasher.update(self.getValueKeyHash(resolved.value));
                    },
                }
                return try self.internValue(.{
                    .hash = hasher.final(),
                    .data = data,
                });
            },
            else => {
                std.debug.panic("TODO: internValueData {s}", .{@tagName(data)});
            },
        }
    }
    pub fn readPointerValue(self: *Builder, value_key: Sema.Value.Key) usize {
        const value = self.getValue(value_key) orelse unreachable;
        return value.data.comptime_pointer;
    }
    pub fn isComplexValue(self: *Builder, value_key: Sema.Value.Key, tag: std.meta.FieldEnum(Sema.Value.Data)) bool {
        return switch (value_key) {
            .simple => false,
            .complex => |complex| {
                const value = self.getComplexValue(complex);
                return std.meta.activeTag(value.data) == tag;
            },
        };
    }
    pub inline fn build(allocator: std.mem.Allocator, sema: *Sema, errors_manager: *ErrorManager, options: BuildOptions) Error!Builder {
        return Builder{
            // .strings = Sema.Strings.init(allocator),
            .errors_manager = errors_manager,
            // .memory = ComptimeMemory.init(allocator, self),
            .sema = sema,

            // .lists = Sema.Lists.init(allocator),
            .arena = std.heap.ArenaAllocator.init(allocator),
            .allocator = allocator,
            .tracer = Tracer.init(
                allocator,
                .sema,
                .{
                    .dir = options.trace_dir orelse "./.tmp/trace",
                },
            ) catch @panic("Tracer.init failed"),
        };

        // Collect root symbols
        // const root_entity = try Entity.init(builder, Hir.Inst.RootIndex);

        // const root_entity_key = try builder.makeEntity(.{
        //     .name = try builder.internSlice("root"),
        //     .hir_inst_index = Hir.Inst.RootIndex,
        //     .data = .{ .struct_declaration = .{} },
        // });
        // var root_entity = builder.getEntity(root_entity_key);
        // try root_entity.collectEntities();

        // try builder.collect(Hir.Inst.RootIndex);

        // return builder;
    }

    pub fn isSigned(self: *Builder, type_key: Sema.Type.Key) bool {
        _ = self; // autofix
        switch (type_key) {
            .simple => |simple| {
                return switch (simple) {
                    .float, .int, .number, .i8, .i16, .i32, .i64 => true,
                    .u8, .u16, .u32, .u64, .usize => false,
                    else => unreachable,
                };
            },
            else => unreachable,
        }
    }

    pub fn numberBits(self: *Builder, type_key: Sema.Type.Key) u8 {
        _ = self; // autofix
        switch (type_key) {
            .simple => |simple| {
                return switch (simple) {
                    .i8, .u8 => 8,
                    .i16, .u16 => 16,
                    .i32, .u32 => 32,
                    .i64, .u64 => 64,
                    .usize => 64, // TODO: platform dependent
                    .number => 64,
                    .float => 64,
                    .int => 64,
                    .f32 => 32,
                    .f64 => 64,
                    .bool => 1,
                    .bchar => 8,
                    else => std.debug.panic("TODO: numberBits {s}", .{@tagName(simple)}),
                };
            },
            else => unreachable,
        }
    }
    pub fn isFloat(self: *Builder, type_key: Sema.Type.Key) bool {
        _ = self; // autofix
        switch (type_key) {
            .simple => |simple| return switch (simple) {
                .f32, .f64 => true,
                else => false,
            },
            else => unreachable,
        }
    }
    // pub fn getNumberValueKeyAs(self: *Builder, comptime T: type, value_key: Sema.Value.Key) T {
    //     const value = self.getValue(value_key) orelse {
    //         std.debug.panic("error: value not found", .{});
    //     };
    //     if (T == f64 or T == f32) {
    //         return switch (value.data) {
    //             .float => |f| @floatCast(f),
    //             .integer => |i| @floatFromInt(i),
    //             // .big_integer => |i| @floatFromInt(i),
    //             else => unreachable,
    //         };
    //     }

    //     return switch (value.data) {
    //         .integer => |i| @intCast(i),
    //         // .big_integer => |i| @intCast(i),
    //         .float => |f| @intFromFloat(f),
    //         else => std.debug.panic("unreachable: getNumberValueKeyAs {s}", .{@tagName(value.data)}),
    //     };
    // }
    pub fn castBytesToType(bytes: []const u8, From: type, To: type) !To {
        const value = std.mem.bytesToValue(From, bytes);
        if (From == To) return value;
        const lhs_is_float = From == f64 or From == f32;
        const rhs_is_float = To == f64 or To == f32;
        if (lhs_is_float and rhs_is_float) {
            return @floatCast(value);
        }
        if (lhs_is_float and !rhs_is_float) {
            return @intFromFloat(value);
        }
        if (!lhs_is_float and rhs_is_float) {
            return @floatFromInt(value);
        }
        if (!lhs_is_float and !rhs_is_float) {
            return @intCast(value);
        }
        // return @intCast(value);
        // if (From == f64 or From == f32 and (To ))
    }
    pub inline fn readBytes(self: *Builder, typed_value: Sema.TypedValue) ![]const u8 {
        switch (typed_value.value) {
            .simple => |simple| switch (simple) {
                // .bytes => |bytes| bytes,
                .exec_time, .runtime => return error.CantReadRuntimeValues,
                // .runtime, .comptime => unreachable,
                else => return error.ExpectedBytesValue,
            },
            .complex => |complex| switch (self.getComplexValue(complex).data) {
                // .slice => |slice| slice.ptr.value,
                .bytes => |bytes| return bytes[0..self.getTypeSize(typed_value.type)],
                else => return error.ExpectedBytesValue,
            },
        }
    }
    pub fn readNumberAsType(self: *Builder, T: type, typed_value: Sema.TypedValue) !T {
        const bytes = try self.readBytes(typed_value);
        switch (typed_value.type) {
            .simple => |simple| switch (simple) {
                .i8 => return castBytesToType(bytes, i8, T),
                .i16 => return castBytesToType(bytes, i16, T),
                .i32 => return castBytesToType(bytes, i32, T),
                .i64 => return castBytesToType(bytes, i64, T),
                .u8, .bchar => return castBytesToType(bytes, u8, T),
                .u16 => return castBytesToType(bytes, u16, T),
                .u32 => return castBytesToType(bytes, u32, T),
                .u64 => return castBytesToType(bytes, u64, T),
                .usize => return castBytesToType(bytes, usize, T),
                .f32 => return castBytesToType(bytes, f32, T),
                .f64 => return castBytesToType(bytes, f64, T),
                .number => return castBytesToType(bytes, f64, T),
                .float => return castBytesToType(bytes, f64, T),
                .int => return castBytesToType(bytes, i64, T),
                else => @panic("TODO"),
            },
            .complex => return castBytesToType(bytes, usize, T),
        }
    }
    pub fn convertNumberType(self: *Builder, typed_value: Sema.TypedValue, target_type: Sema.Type.Key) !Sema.TypedValue {
        if (typed_value.type.isEqual(target_type)) {
            return typed_value;
        }
        var bytes: [8]u8 = undefined;
        switch (typed_value.type.simple) {
            inline .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .f32, .f64, .usize, .float, .int, .bchar => |src_tag| switch (target_type.simple) {
                inline .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .f32, .f64, .usize, .float, .int, .bchar => |dst_tag| {
                    const Src = src_tag.getNativeType();
                    const Dst = dst_tag.getNativeType();
                    const src_bytes = try self.readBytes(typed_value);
                    const result = try castBytesToType(src_bytes, Src, Dst);
                    std.mem.copyForwards(u8, &bytes, std.mem.asBytes(&result));
                },
                else => |dst_tag| std.debug.panic("unreachable: convertNumberType {s} -> {s}", .{ @tagName(src_tag), @tagName(dst_tag) }),
            },
            else => |target_tag| std.debug.panic("unreachable: convertNumberType {s}", .{@tagName(target_tag)}),
        }
        // const Src = typed_value.type.getNativeType();
        // const Dst = target_type.getNativeType();
        // const src_bytes = try self.readBytes(typed_value);
        // const result = try self.castBytesToType(src_bytes, Src, Dst);
        // var bytes: [8]u8 = undefined;
        // std.mem.copyForwards(u8, &bytes, &result);

        // const
        // const bytes = try self.readNumberAsType(f64, typedValue);
        return Sema.TypedValue{
            .type = target_type,
            .value = try self.internValueData(.{ .bytes = bytes }),
        };
    }
    pub fn getBooleanValueKeyAsBool(self: *Builder, value_key: Sema.Value.Key) bool {
        switch (value_key) {
            .simple => unreachable,
            .complex => |complex| switch (self.getComplexValue(complex).data) {
                .bytes => |bytes| return std.mem.bytesToValue(bool, bytes[0..@sizeOf(bool)]),
                else => unreachable,
            },
        }
    }
    pub fn doComparison(self: *Builder, op: Sema.Instruction.Op, lhs: Sema.TypedValue, rhs: Sema.TypedValue) !Sema.TypedValue {
        switch (lhs.type.simple) {
            inline .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .usize, .int => |t| {
                _ = t; // autofix

                const lhs_value = try self.readNumberAsType(i64, lhs);
                const rhs_value = try self.readNumberAsType(i64, rhs);
                const result = switch (op) {
                    .eq => lhs_value == rhs_value,
                    .ne => lhs_value != rhs_value,
                    .lt => lhs_value < rhs_value,
                    .le => lhs_value <= rhs_value,
                    .gt => lhs_value > rhs_value,
                    .ge => lhs_value >= rhs_value,
                    else => unreachable,
                };
                return Sema.TypedValue{
                    .type = Sema.Type.simple(.bool),
                    .value = if (result) try self.numberAsBytesValueKey(true) else try self.numberAsBytesValueKey(false),
                };
                // return try self.pushInstruction(hir_inst_index, .{
                //     .op = .constant,
                //     .type = Sema.Type.simple(.bool),
                //     .value = if (result) Sema.Value.simple(.true) else Sema.Value.simple(.false),
                //     .data = .void,
                // });
            },
            .f32, .f64, .float => {
                const lhs_value = try self.readNumberAsType(f64, lhs);
                const rhs_value = try self.readNumberAsType(f64, rhs);
                const result = switch (op) {
                    .eq => lhs_value == rhs_value,
                    .ne => lhs_value != rhs_value,
                    .lt => lhs_value < rhs_value,
                    .le => lhs_value <= rhs_value,
                    .gt => lhs_value > rhs_value,
                    .ge => lhs_value >= rhs_value,
                    else => unreachable,
                };
                return Sema.TypedValue{
                    .type = Sema.Type.simple(.bool),
                    .value = if (result) try self.numberAsBytesValueKey(true) else try self.numberAsBytesValueKey(false),
                };
                // return try self.pushInstruction(hir_inst_index, .{
                //     .op = .constant,
                //     .type = Sema.Type.simple(.bool),
                //     .value = if (result) Sema.Value.simple(.true) else Sema.Value.simple(.false),
                //     .data = .void,
                // });
            },
            else => unreachable,
        }
    }
    pub fn doArithmetic(self: *Builder, comptime T: type, op: Sema.Instruction.Op, lhs: Sema.TypedValue, rhs: Sema.TypedValue) Error!Sema.TypedValue {
        const lhs_value = try self.readNumberAsType(T, lhs);
        const rhs_value = try self.readNumberAsType(T, rhs);
        const result: T = switch (op) {
            .add => lhs_value + rhs_value,
            .sub => lhs_value - rhs_value,
            .mul => lhs_value * rhs_value,
            .div => @divExact(lhs_value, rhs_value),
            else => unreachable,
        };
        // var bytes: [8]u8 = undefined;
        // std.mem.copyForwards(u8, &bytes, &result);
        // return try self.numberAsBytesValueKey(result);
        return Sema.TypedValue{
            .type = lhs.type,
            .value = try self.numberAsBytesValueKey(result),
        };
        // switch (T) {
        //     f32, f64 => {
        //         const result: T = switch (op) {
        //             .add => lhs_value + rhs_value,
        //             .sub => lhs_value - rhs_value,
        //             .mul => lhs_value * rhs_value,
        //             .div => lhs_value / rhs_value,
        //             else => unreachable,
        //         };
        //         var bytes: [8]u8 = undefined;
        //         std.mem.copyForwards(u8, &bytes, &result);
        //         return Sema.TypedValue{
        //             .type = lhs.type,
        //             .value = try self.internValueData(.{ .bytes = bytes }),
        //         };
        //     },
        //     else => {
        //         const result: T = switch (op) {
        //             .add => lhs_value + rhs_value,
        //             .sub => lhs_value - rhs_value,
        //             .mul => lhs_value * rhs_value,
        //             .div => @divExact(lhs_value, rhs_value),
        //             else => unreachable,
        //         };
        //         return Sema.TypedValue{
        //             .type = lhs.type,
        //             .value = try self.internValueData(.{ .integer = result }),
        //         };
        // },
        // }
        // _ = T; // autofix
        // switch (lhs.type.simple) {
        //     .i8,
        //     .i16,
        //     .i32,
        //     .i64,
        //     .u8,
        //     .u16,
        //     .u32,
        //     .u64,
        //     .usize,
        //     => {
        //         const lhs_value = try self.readNumberAsType(i64, lhs);
        //         const rhs_value = try self.readNumberAsType(i64, rhs);
        //         const result: i64 = switch (op) {
        //             .add => lhs_value + rhs_value,
        //             .sub => lhs_value - rhs_value,
        //             .mul => lhs_value * rhs_value,
        //             .div => @divExact(lhs_value, rhs_value),
        //             else => unreachable,
        //         };
        //         return Sema.TypedValue{
        //             .type = lhs.type,
        //             .value = try self.internValueData(.{ .integer = result }),
        //         };
        //         // return try self.pushInstruction(hir_inst_index, .{
        //         //     .op = .constant,
        //         //     .type = data.type,
        //         //     .value = try self.builder.internValueData(.{ .integer = result }),
        //         //     .data = .void,
        //         // });
        //     },
        //     .f32, .f64, .number => {
        //         const lhs_value = try self.readNumberAsType(f64, lhs);
        //         const rhs_value = try self.readNumberAsType(f64, rhs);
        //         const result = switch (op) {
        //             .add => lhs_value + rhs_value,
        //             .sub => lhs_value - rhs_value,
        //             .mul => lhs_value * rhs_value,
        //             .div => lhs_value / rhs_value,
        //             else => unreachable,
        //         };
        //         return Sema.TypedValue{
        //             .type = lhs.type,
        //             .value = try self.internValueData(.{ .float = result }),
        //         };
        //     },
        //     else => unreachable,
        // }
    }
    pub fn deinit(self: *Builder) void {
        // self.strings.deinit();
        // self.lists.deinit();
        // self.entities.deinit();
        self.arena.deinit();
        // self.values.deinit(self.allocator);
        // self.types.deinit(self.allocator);
        self.symbols.deinit(self.allocator);
        self.symbols_by_hir_inst.deinit(self.allocator);
        self.tracer.deinit();
        // self.declarations.deinit(self.allocator);
        self.declarations_by_hir_inst.deinit(self.allocator);
        self.queue.deinit(self.allocator);
    }

    pub fn makeEntity(self: *Builder, input: Entity.EntityInput) !Entity.Key {
        const trace = self.tracer.begin(
            @src(),
            .{
                "makeEntity", "makeEntity({s})", .{
                    // try formatHirIndex(self.hir, input.hir_inst_index),
                    @tagName(input.data),
                },
            },
            .{
                // .input = input,
            },
        );
        const key = self.entities.items.len;
        defer trace.end(.{ .key = key, .post_state = self.getState() });
        if (self.symbols_by_hir_inst.contains(input.hir_inst_index)) {
            std.debug.panic("Symbol already exists", .{
                // try formatHirIndex(self.hir, input.hir_inst_index),
            });
        }
        const entity_box = try Box(Entity).init(self.arena.allocator());
        entity_box.ptr.* = try Entity.init(self, key, input);
        const entity = entity_box.ptr.*;
        try self.entities.append(self.arena.allocator(), entity_box);
        switch (input.data) {
            .global_declaration, .global_type_declaration, .function_declaration => {
                self.tracer.trace(
                    @src(),
                    .{ "makeEntity", "symbols.put({s}, {d})", .{ self.getSlice(entity.name), key } },
                    .{
                        // .entity = entity,
                    },
                );
                try self.symbols.put(self.allocator, entity.name, key);
            },
            else => {},
        }
        try self.symbols_by_hir_inst.put(self.allocator, input.hir_inst_index, key);

        return key;
    }

    pub fn compileDeclaration(self: *Builder, name: []const u8) !Sema.Declaration.Index {
        const trace = self.tracer.begin(
            @src(),
            .{ "compileSymbol", "Builder.compileSymbol('{s}')", .{name} },
            .{
                .name = name,
            },
        );
        defer trace.end(.{ .post_state = self.getState() });

        const entity = self.getEntityBySymbol(name) orelse return error.SymbolNotFound;

        const declaration = try entity.resolveDeclaration();
        try self.optimizeDeclaration(declaration);
        return declaration;
    }

    pub fn collectSource(self: *Builder, source: Sema.Strings.Range) !void {
        const root_entity_key = try self.makeEntity(.{
            .name = source,
            .source = source,
            .hir_inst_index = Hir.Inst.RootIndex,
            .data = .{ .module_declaration = .{} },
        });
        var root_entity = self.getEntity(root_entity_key);
        try root_entity.collectEntities();
    }
    pub fn compileAll(self: *Builder, source: Sema.Strings.Range) !void {
        const trace = self.tracer.begin(
            @src(),
            .{ "compileAll", "Builder.compileAll", .{} },
            .{},
        );
        defer trace.end(.{ .post_state = self.getState() });
        try self.collectSource(source);

        var i: usize = 0;
        var j: usize = 0;
        while (true) : (i += 1) {
            if (i >= self.entities.items.len) {
                // var symbols_iter = self.symbols.iterator();
                const symbols = self.symbols.entries.items(.value);
                while (j < symbols.len) : (j += 1) {
                    const entity = self.getEntity(symbols[j]);
                    const declaration = try entity.resolveDeclaration();
                    _ = declaration; // autofix
                }

                if (i >= self.entities.items.len) break;
            }
            var entity = self.entities.items[i].ptr.*;
            try entity.collectEntities();
        }
    }

    pub fn getComplexValue(self: *Builder, value_key: anytype) *Sema.Value {
        const index = switch (@TypeOf(value_key)) {
            Sema.Value.Key => value_key.complex,
            usize => value_key,
            else => @compileError("not a complex value"),
        };
        return &self.sema.values.entries.items(.value)[index];
    }
    pub fn getComplexType(self: *Builder, type_key: anytype) *Sema.Type {
        const index = switch (@TypeOf(type_key)) {
            Sema.Type.Key => type_key.complex,
            usize => type_key,
            else => @compileError("not a complex type"),
        };
        return &self.sema.types.entries.items(.value)[index];
    }
    const CastType = enum {
        allowed,
        unnecessary,
        not_allowed,
    };

    pub fn canCastImplicitlyInner(self: *Builder, from_type: Sema.Type.Key, to_type: Sema.Type.Key, complexity: usize) !CastType {
        if (complexity > 10) return error.TypeComplexityTooHigh;
        var c = complexity;
        if (from_type.isEqual(to_type)) return .unnecessary;
        // if (Sema.Type.isOneOfSimple(from_type, .{ )
        if (from_type.isOneOfSimple(&.{ .float, .int }) and to_type.isOneOfSimple(
            &.{
                .i8,
                .i16,
                .i32,
                .i64,
                .u8,
                .u16,
                .u32,
                .u64,
                .usize,
                .f32,
                .f64,
                .bchar,
                .float,
                .int,
            },
        )) return .allowed;
        const signed_types = &.{ .i8, .i16, .i32, .i64 };
        // const number_types = &.{ .number, .bchar };
        if (from_type.isOneOfSimple(signed_types) and to_type.isOneOfSimple(signed_types)) {
            const lhs_bits = self.numberBits(from_type);
            const rhs_bits = self.numberBits(to_type);
            return if (lhs_bits <= rhs_bits) .allowed else .not_allowed;
        }
        const unsigned_types = &.{ .u8, .u16, .u32, .u64, .usize };
        if (from_type.isOneOfSimple(unsigned_types) and to_type.isOneOfSimple(unsigned_types)) {
            const lhs_bits = self.numberBits(from_type);
            const rhs_bits = self.numberBits(to_type);
            return if (lhs_bits <= rhs_bits) .allowed else .not_allowed;
        }

        const float_types = &.{ .f32, .f64 };
        if (from_type.isOneOfSimple(float_types) and to_type.isOneOfSimple(float_types)) {
            const lhs_bits = self.numberBits(from_type);
            const rhs_bits = self.numberBits(to_type);
            return if (lhs_bits <= rhs_bits) .allowed else .not_allowed;
        }
        if (to_type.isEqualSimple(.type)) {
            if (self.getType(from_type)) |from_type_inst| switch (from_type_inst.data) {
                .typeof => return .allowed,
                else => {},
            };
        }

        switch (to_type) {
            .simple => |target_simple| {
                _ = target_simple; // autofix
            },
            .complex => |target_complex| {
                const target_complex_type = self.getComplexType(target_complex);
                switch (target_complex_type.data) {
                    .any => return .allowed,
                    .flat_union => {
                        const fields = self.sema.lists.getSlice(target_complex_type.data.flat_union.fields);
                        // var can_cast: CastType = .not_allowed;
                        for (fields) |field| {
                            c += 1;
                            const field_key = Sema.Type.Key.decode(field);
                            switch (try self.canCastImplicitlyInner(from_type, field_key, c + 1)) {
                                .allowed, .unnecessary => return .allowed,
                                .not_allowed => {},
                            }
                        }
                        return .not_allowed;
                        // return can_cast;
                    },
                    else => {},
                }
            },
        }

        return .not_allowed;
    }
    pub fn canCastImplicitly(self: *Builder, from_type: Sema.Type.Key, to_type: Sema.Type.Key) !CastType {
        return try self.canCastImplicitlyInner(from_type, to_type, 0);
    }

    const FormattableType = struct {
        type_key: Sema.Type.Key,
        sema: *Sema,
        pub fn format(self: FormattableType, comptime _: []const u8, _: std.fmt.FormatOptions, writer: std.io.AnyWriter) !void {
            try self.sema.formatType(writer, self.type_key);
        }
    };
    const FormattableTypedValue = struct {
        typed_value: Sema.TypedValue,
        sema: *Sema,
        pub fn format(self: FormattableTypedValue, comptime _: []const u8, _: std.fmt.FormatOptions, writer: std.io.AnyWriter) !void {
            try self.sema.formatTypedValue(writer, self.typed_value, .{});
        }
    };
    pub fn getFormattableType(self: *Builder, type_key: Sema.Type.Key) FormattableType {
        const type_inst = self.getType(type_key);

        _ = type_inst; // autofix
        return FormattableType{
            .type_key = type_key,
            .sema = self.sema,
        };
    }
    pub fn getFormattableTypedValue(self: *Builder, typed_value: Sema.TypedValue) FormattableTypedValue {
        return FormattableTypedValue{
            .typed_value = typed_value,
            .sema = self.sema,
        };
    }
    pub fn compContext(self: *Builder, context: anytype, is_comptime: bool) CompContext(@TypeOf(context)) {
        return CompContext(@TypeOf(context)).init(self, context, is_comptime);
    }
    pub fn CompContext(
        // self_Builder: *Builder,
        C: type,
    ) type {
        return struct {
            // instructions: []Sema.Instruction,
            is_comptime: bool,
            builder: *Builder,
            ctx: C,
            depth: usize = 0,
            pub fn init(builder: *Builder, ctx: C, is_comptime: bool) @This() {
                return .{
                    .builder = builder,
                    .ctx = ctx,
                    .is_comptime = is_comptime,
                };
            }

            pub fn getInstruction(self: *@This(), index: Sema.Instruction.Index) *Sema.Instruction {
                // return &self.instructions[index];
                return self.ctx.getInstruction(index);
            }
            pub fn getTypedValue(self: *@This(), index: Sema.Instruction.Index) Sema.TypedValue {
                return self.ctx.getTypedValue(index);
            }
            pub fn markDead(self: *@This(), index: Sema.Instruction.Index) void {
                if (comptime @hasDecl(C, "markDead")) {
                    self.ctx.markDead(index);
                }
            }
            pub fn setValue(self: *@This(), index: Sema.Instruction.Index, value: Sema.TypedValue) !void {
                try self.ctx.setValue(index, value);
            }
            pub fn goTo(self: *@This(), index: Sema.Instruction.Index) !void {
                std.debug.print("goTo: {d}\n", .{index});
                if (self.depth > 4) {
                    std.debug.panic("depth too big", .{});
                }
                self.depth += 1;
                try self.ctx.goTo(index);
            }
            pub fn computeInstruction(self: *@This(), inst_index: Sema.Instruction.Index) !Sema.TypedValue {
                const inst = self.getInstruction(inst_index);
                // const inst_value = self.getTypedValue(inst_index);
                const trace = self.builder.tracer.begin(
                    @src(),
                    .{ "computeInstructions", "Builder.computeInstructions", .{} },
                    .{},
                );
                defer trace.end(.{ .post_state = self.builder.getState() });
                // std.debug.print("computeInstruction: {any}\n", .{inst.op});
                const is_comptime = self.is_comptime;

                switch (inst.op) {
                    .@"if" => {
                        const condition_value = self.getTypedValue(inst.data.@"if".condition);
                        if (!self.is_comptime) return self.getTypedValue(inst_index);
                        const condition_result = self.builder.getBooleanValueKeyAsBool(condition_value.value);

                        // const condition_result = try self.builder.getBooleanValueKeyAsBool(condition_value);

                        // const inst = self.getInstruction(inst_index);
                        // const cond_value = self.getValue(inst.data.@"if".condition);

                        // self.incDepth();
                        // defer self.decDepth();

                        // const last_block_inst = inst.data.@"if".else_block orelse inst.data.@"if".then_block;
                        // const end = inst_index + self.getInstruction(last_block_inst).data.block.instructions_count;
                        const last_block = inst.data.@"if".else_block orelse inst.data.@"if".then_block;
                        _ = last_block; // autofix

                        if (condition_result) {
                            // try self.execInstruction(inst.data.@"if".then_block);
                            try self.goTo(inst.data.@"if".then_block);
                        } else {
                            if (inst.data.@"if".else_block) |else_block_inst_index| {
                                try self.goTo(else_block_inst_index);
                            }
                            // self.goTo(inst.data.@"if".else_block orelse end);
                        }
                        // if (cond_value.value.isEqualSimple(.false)) {
                        //     if (inst.data.@"if".else_block) |else_block_inst_index| {
                        //         try self.execInstruction(else_block_inst_index);
                        //         return;
                        //     }
                        // }
                        return .{
                            .type = Sema.Type.simple(.void),
                            .value = Sema.Value.simple(.void),
                        };
                    },
                    .loop => {
                        if (self.is_comptime) {
                            try self.goTo(inst.data.loop.body_block);
                        }
                        return .{
                            .type = Sema.Type.simple(.void),
                            .value = Sema.Value.simple(.void),
                        };
                    },
                    .store => {
                        var base_inst_value = self.getTypedValue(inst.data.operand_payload.operand);
                        var value_inst_value = self.getTypedValue(inst.data.operand_payload.payload);
                        if (!base_inst_value.isComptimeKnown()) return self.getTypedValue(inst_index);
                        if (!value_inst_value.isComptimeKnown()) return self.getTypedValue(inst_index);
                        const base = try self.builder.readNumberAsType(usize, base_inst_value);
                        try self.builder.sema.memory.store(base, value_inst_value);
                        self.markDead(inst.data.operand_payload.operand);
                        self.markDead(inst.data.operand_payload.payload);
                        return Sema.TypedValue.VOID;
                    },
                    .get_element_pointer => {
                        var base_inst_value = self.getTypedValue(inst.data.get_element_pointer.base);
                        var index_inst_value = self.getTypedValue(inst.data.get_element_pointer.index);
                        if (!base_inst_value.isComptimeKnown()) return self.getTypedValue(inst_index);
                        if (!index_inst_value.isComptimeKnown()) return self.getTypedValue(inst_index);

                        const index = try self.builder.readNumberAsType(usize, index_inst_value);
                        const ptr = try self.builder.readNumberAsType(usize, base_inst_value);
                        const unwrapped_type = self.builder.unwrapPointerType(base_inst_value.type) orelse std.debug.panic("not a pointer type", .{});
                        const element_type = switch (self.builder.getComplexType(unwrapped_type).data) {
                            .slice => |slice| slice.child,
                            .array => |array| array.child,
                            .@"struct" => |str| {
                                const fields = self.builder.sema.lists.getSlice(str.fields);
                                const field = fields[index];
                                const field_type = self.builder.getComplexType(Sema.Type.Key.decode(field)).data.struct_field;
                                // const element_size = self.getTypeSize(field_type.type);
                                // std.debug.panic("field_type: {any}\n", .{field_type});
                                return .{
                                    .type = field_type.type,
                                    .value = try self.builder.numberAsBytesValueKey(ptr + field_type.offset),
                                };
                            },
                            else => unreachable,
                        };
                        const element_size = self.builder.getTypeSize(element_type);
                        return .{
                            .type = inst.typed_value.type,
                            .value = try self.builder.numberAsBytesValueKey(ptr + index * element_size),
                        };
                    },
                    .alloc => {
                        std.debug.print("allocating... {any}\n", .{inst.data.alloc.type});
                        if (is_comptime or inst.data.alloc.mutable == false) {
                            // if (!is_comptime) return;
                            const ptr = try self.builder.sema.memory.stackCreate(inst.data.alloc.type);
                            return .{
                                .type = inst.typed_value.type,
                                .value = try self.builder.numberAsBytesValueKey(ptr),
                            };
                        }
                        return self.getTypedValue(inst_index);
                    },
                    .load => {
                        var ptr_inst_value = self.getTypedValue(inst.data.operand);
                        if (!ptr_inst_value.isComptimeKnown()) return self.getTypedValue(inst_index);
                        const ptr = try self.builder.readNumberAsType(usize, ptr_inst_value);
                        const type_to_load = self.builder.unwrapPointerType(ptr_inst_value.type) orelse std.debug.panic("not a pointer type", .{});
                        return .{
                            .type = inst.typed_value.type,
                            .value = try self.builder.sema.memory.load(type_to_load, ptr),
                        };
                    },
                    .add, .sub, .mul, .div => {
                        var lhs_inst_value = self.getTypedValue(inst.data.bin_op.lhs);
                        var rhs_inst_value = self.getTypedValue(inst.data.bin_op.rhs);
                        if (!lhs_inst_value.isComptimeKnown()) return self.getTypedValue(inst_index);
                        if (!rhs_inst_value.isComptimeKnown()) return self.getTypedValue(inst_index);
                        // const lhs = try self.builder.readNumberAsType(usize, lhs_inst_value);
                        // _ = lhs; // autofix
                        // const rhs = try self.builder.readNumberAsType(usize, rhs_inst_value);
                        // inst.value = try self.builder.numberAsBytesValueKey(lhs + rhs);

                        // lhs_inst.liveness = 0;
                        // rhs_inst.liveness = 0;
                        self.markDead(inst.data.bin_op.lhs);
                        self.markDead(inst.data.bin_op.rhs);

                        const op = inst.op;
                        switch (inst.typed_value.type.simple) {
                            inline .float, .int, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .usize => |type_| {
                                return try self.builder.doArithmetic(
                                    type_.getNativeType(),
                                    op,
                                    // .{ .type = inst.type, .value = lhs_inst_value.value },
                                    lhs_inst_value,
                                    rhs_inst_value,
                                    // .{ .type = inst.type, .value = rhs_inst_value.value },
                                );
                            },
                            else => unreachable,
                        }
                    },
                    .eq, .ne, .gt, .ge, .lt, .le => {
                        var lhs_inst_value = self.getTypedValue(inst.data.bin_op.lhs);
                        var rhs_inst_value = self.getTypedValue(inst.data.bin_op.rhs);
                        if (!lhs_inst_value.isComptimeKnown()) return self.getTypedValue(inst_index);
                        if (!rhs_inst_value.isComptimeKnown()) return self.getTypedValue(inst_index);

                        // lhs_inst.liveness = 0;
                        // rhs_inst.liveness = 0;
                        self.markDead(inst.data.bin_op.lhs);
                        self.markDead(inst.data.bin_op.rhs);

                        // std.debug.assert(lhs_inst.type.isEqual(rhs_inst.type));
                        const resolved = try self.builder.doComparison(inst.op, lhs_inst_value, rhs_inst_value);
                        return resolved;
                    },
                    .cast => {
                        var value_inst_value = self.getTypedValue(inst.data.operand);
                        if (!value_inst_value.isComptimeKnown()) return self.getTypedValue(inst_index);
                        const value = try self.builder.convertNumberType(value_inst_value, inst.typed_value.type);
                        return .{
                            .type = inst.typed_value.type,
                            .value = value.value,
                        };
                    },
                    .br => {
                        // const target_inst_index =
                        if (inst.data.br.payload) |payload| {
                            const operand_inst_value = self.getTypedValue(payload);
                            try self.setValue(inst.data.br.target, operand_inst_value);
                        }
                        // defer self.depth -= 1;
                        // const target_hir_inst_index = hir_inst.br.target;
                        // const target_hir_inst = self.entity.getHirInstruction(target_hir_inst_index);
                        const target_inst = self.getInstruction(inst.data.br.target);
                        if (self.is_comptime) {
                            const goto = switch (target_inst.op) {
                                .block => inst.data.br.target + target_inst.data.block.instructions_list.len,
                                .loop => inst.data.br.target,
                                else => unreachable,
                            };
                            try self.goTo(goto);
                        }
                        return .{
                            .type = Sema.Type.simple(.void),
                            .value = Sema.Value.simple(.void),
                        };
                        // const target_inst_index = inst.data.br.target;
                        // return self.getTypedValue(target_inst_index);
                    },
                    .fn_call => {
                        //   var value: Sema.Value.Key = Sema.Value.simple(.exec_time);
                        // std.debug.print("fn_call: {any}\n", .{inst});

                        const callee_value = self.getTypedValue(inst.data.fn_call.callee);
                        if (!self.is_comptime or !callee_value.isComptimeKnown()) return self.getTypedValue(inst_index);

                        // var all_args_resolved = true;
                        const args_list = self.builder.sema.lists.getSlice(inst.data.fn_call.args_list);
                        var arena = std.heap.ArenaAllocator.init(self.builder.allocator);
                        defer arena.deinit();
                        var args_typed_values: std.ArrayList(Sema.TypedValue) = std.ArrayList(Sema.TypedValue).init(arena.allocator());
                        for (args_list) |arg_index| {

                            // if (self.is_comptime and !callee_fn_value_key.isEqualSimple(.exec_time) and !callee_fn_value_key.isEqualSimple(.runtime) and all_args_resolved) {
                            // if (self.builder.getValue(callee_fn_value_key)) |callee_fn_value| {
                            //     if (callee_fn_value.data.function.init) |init_inst| {
                            const arg_value = self.getTypedValue(arg_index);
                            if (!arg_value.isComptimeKnown()) {
                                return self.getTypedValue(inst_index);
                            }
                            try args_typed_values.append(arg_value);
                        }
                        // const args_list = self.builder.sema.lists.getSlice(inst.data.fn_call.args_list);
                        // defer args_typed_values.deinit();
                        // for (args_list) |arg_index| {
                        //     const arg_inst = self.getInstruction(arg_index);
                        //     _ = arg_inst; // autofix
                        //     self.markDead(arg_index);
                        // }
                        std.debug.print("args_typed_values: {any}\n", .{self.builder.getComplexValue(callee_value.value)});
                        var exec_context = ExecContext.init(
                            arena.allocator(),
                            self.builder,
                            args_typed_values.items,
                            callee_value,
                            // init_inst,
                        );
                        //     }
                        // }
                        // }

                        // return self.handleFnCallInstruction(inst_index);
                        return .{
                            .type = self.getTypedValue(inst_index).type,
                            .value = try exec_context.exec(),
                        };
                    },
                    .typeof => {
                        const operand_inst_value = self.getTypedValue(inst.data.operand);
                        return .{
                            .type = Sema.Type.simple(.type),
                            .value = try self.builder.internValueData(.{ .type = operand_inst_value.type }),
                        };
                    },
                    else => {
                        std.debug.print("%{d} unhandled computeInstruction: {any}\n", .{ inst_index, inst.op });
                    },
                }
                return self.getTypedValue(inst_index);
            }
        };
    }
    pub const ComputeContext = struct {
        instructions: []Sema.Instruction,
        is_comptime: bool,
        should_update_instructions: bool = false,

        pub fn getInstruction(self: *ComputeContext, index: Sema.Instruction.Index) *Sema.Instruction {
            return &self.instructions[index];
        }
    };
    // pub fn computeInstruction(self: *Builder, inst_index: Sema.Instruction.Index, context: *ComputeContext) !Sema.TypedValue {
    //     var inst = context.getInstruction(inst_index);
    //     const trace = self.tracer.begin(
    //         @src(),
    //         .{ "computeInstructions", "Builder.computeInstructions", .{} },
    //         .{},
    //     );
    //     defer trace.end(.{ .post_state = self.getState() });
    //     // std.debug.print("computeInstruction: {any}\n", .{inst.op});
    //     const is_comptime = context.is_comptime;

    //     switch (inst.op) {
    //         .store => {
    //             var base_inst = context.getInstruction(inst.data.operand_payload.operand);
    //             var value_inst = context.getInstruction(inst.data.operand_payload.payload);
    //             if (!base_inst.value.isComptimeKnown()) return inst.getTypedValue();
    //             if (!value_inst.value.isComptimeKnown()) return inst.getTypedValue();
    //             const base = try self.readNumberAsType(usize, base_inst.getTypedValue());
    //             try self.sema.memory.store(base, value_inst.getTypedValue());
    //             return inst.getTypedValue();
    //         },
    //         .get_element_pointer => {
    //             var base_inst = context.getInstruction(inst.data.get_element_pointer.base);
    //             var index_inst = context.getInstruction(inst.data.get_element_pointer.index);
    //             if (!base_inst.value.isComptimeKnown()) return inst.getTypedValue();
    //             if (!index_inst.value.isComptimeKnown()) return inst.getTypedValue();

    //             const index = try self.readNumberAsType(usize, index_inst.getTypedValue());
    //             const ptr = try self.readNumberAsType(usize, base_inst.getTypedValue());
    //             const unwrapped_type = self.unwrapPointerType(base_inst.type) orelse std.debug.panic("not a pointer type", .{});
    //             const element_type = switch (self.getComplexType(unwrapped_type).data) {
    //                 .slice => |slice| slice.child,
    //                 .array => |array| array.child,
    //                 .@"struct" => |str| {
    //                     const fields = self.sema.lists.getSlice(str.fields);
    //                     const field = fields[index];
    //                     const field_type = self.getComplexType(Sema.Type.Key.decode(field)).data.struct_field;
    //                     // const element_size = self.getTypeSize(field_type.type);
    //                     // std.debug.panic("field_type: {any}\n", .{field_type});
    //                     return .{
    //                         .type = field_type.type,
    //                         .value = try self.sema.builder.numberAsBytesValueKey(ptr + field_type.offset),
    //                     };
    //                 },
    //                 else => unreachable,
    //             };
    //             const element_size = self.getTypeSize(element_type);
    //             return .{
    //                 .type = inst.type,
    //                 .value = try self.sema.builder.numberAsBytesValueKey(ptr + index * element_size),
    //             };
    //         },
    //         .alloc => {
    //             if (!is_comptime and inst.data.alloc.mutable) return inst.getTypedValue();
    //             // if (!is_comptime) return;
    //             const ptr = try self.sema..memory.stackCreate(inst.data.alloc.type);
    //             return .{
    //                 .type = inst.type,
    //                 .value = try self.sema.builder.numberAsBytesValueKey(ptr),
    //             };
    //         },
    //         .load => {
    //             var ptr_inst = context.getInstruction(inst.data.operand);
    //             if (!ptr_inst.value.isComptimeKnown()) return inst.getTypedValue();
    //             const ptr = try self.readNumberAsType(usize, ptr_inst.getTypedValue());
    //             const type_to_load = self.unwrapPointerType(ptr_inst.type) orelse std.debug.panic("not a pointer type", .{});
    //             return .{
    //                 .type = inst.type,
    //                 .value = try self.sema.memory.load(type_to_load, ptr),
    //             };
    //         },
    //         .add, .sub, .mul, .div => {
    //             var lhs_inst = context.getInstruction(inst.data.bin_op.lhs);
    //             var rhs_inst = context.getInstruction(inst.data.bin_op.rhs);
    //             if (!lhs_inst.value.isComptimeKnown()) return inst.getTypedValue();
    //             if (!rhs_inst.value.isComptimeKnown()) return inst.getTypedValue();
    //             const lhs = try self.readNumberAsType(usize, lhs_inst.getTypedValue());
    //             const rhs = try self.readNumberAsType(usize, rhs_inst.getTypedValue());
    //             inst.value = try self.sema.builder.numberAsBytesValueKey(lhs + rhs);

    //             lhs_inst.liveness = 0;
    //             rhs_inst.liveness = 0;

    //             const op = inst.op;
    //             switch (inst.type.simple) {
    //                 inline .number, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .usize => |type_| {
    //                     return try self.doArithmetic(
    //                         type_.getNativeType(),
    //                         op,
    //                         .{ .type = inst.type, .value = lhs_inst.value },
    //                         .{ .type = inst.type, .value = rhs_inst.value },
    //                     );
    //                 },
    //                 else => unreachable,
    //             }
    //         },
    //         .eq, .ne, .gt, .ge, .lt, .le => {
    //             var lhs_inst = context.getInstruction(inst.data.bin_op.lhs);
    //             var rhs_inst = context.getInstruction(inst.data.bin_op.rhs);
    //             if (!lhs_inst.value.isComptimeKnown()) return inst.getTypedValue();
    //             if (!rhs_inst.value.isComptimeKnown()) return inst.getTypedValue();

    //             lhs_inst.liveness = 0;
    //             rhs_inst.liveness = 0;

    //             // std.debug.assert(lhs_inst.type.isEqual(rhs_inst.type));
    //             const resolved = try self.doComparison(inst.op, lhs_inst.getTypedValue(), rhs_inst.getTypedValue());
    //             return resolved;
    //         },
    //         else => {},
    //     }
    //     return inst.getTypedValue();
    // }

};

// Module Entity:
// type: .type,
// value: Value { .type = Type.Key -> Type.struct }
// Module is a struct + declarations
// resolved type means nothing since module type is just type
// values resolved means all STRUCT fields are resolved
// (declarations are not technically part of the struct though,
// so they have their own entities and need to be resolved individually)
//
// Type Entity:
// type: .type,
// value: Value { .type = Type.Key  }
// resolved type means nothing since struct type is just type
// resolved value means all fields are resolved
//
// Field Entity:
// type: Type.Key,
// value: Value { ... }
// type is the type of the field
// value is the default value of the field (needs to compute the inline block instructions), if no default value, the default value is undefined
//
// Global Entity:
// type: Type.Key,
// value: Value { ... }
// type is the type of the global
// value is the result of the initializer expression. Unless it's extern, the initializer expression is required.
//
// Function Entity:
// type: Type.Key, // a type "fn" with the prototype of the function
// value: Value { ... } // a "fn" value that links to the type (prototype) and the body of instructions
//
// Parameter Entity:
// type: Type.Key,
// value: Value { ... }
// type is the type of the parameter
// value is the default value of the parameter if any.

pub const Entity = struct {
    parent: ?Entity.Key = null,
    name: Sema.Strings.Range,
    key: Entity.Key,
    hir_inst_index: Hir.Inst.Index,
    builder: *Builder,
    data: Data,
    is_pub: bool = false,
    is_export: bool = false,
    source: Sema.Strings.Range,

    type: union(Stage) {
        idle: void,
        resolving: void,
        resolved: Sema.Type.Key,
    } = .idle,
    value: union(Stage) {
        idle: void,
        resolving: void,
        resolved: Sema.Value.Key,
    } = .idle,
    symbols: union(Stage) {
        idle: void,
        resolving: void,
        resolved: void,
    } = .idle,
    declaration: union(Stage) {
        idle: void,
        resolving: void,
        resolved: Sema.Declaration.Index,
    } = .idle,
    dependencies: std.AutoArrayHashMapUnmanaged(Entity.Key, void) = .{},
    pub const Key = usize;
    pub const Kind = enum {
        module_declaration,
        global_declaration,
        global_type_declaration,
        function_declaration,
        field_declaration,
        parameter_declaration,

        if_statement,
        loop_statement,
        block_statement,
        type,
    };
    const Data = union(Kind) {
        module_declaration: struct {
            fields: std.AutoArrayHashMapUnmanaged(Sema.Strings.Range, struct {
                name: Sema.Strings.Range,
                entity: Entity.Key,
                index: usize,
                offset: usize,
            }) = .{},
            declarations: std.AutoArrayHashMapUnmanaged(Sema.Strings.Range, Entity.Key) = .{},
        },
        global_declaration: struct {
            type: Entity.Key,
            declaration_index: Sema.Declaration.Index,
        },
        global_type_declaration: struct {
            declaration_index: Sema.Declaration.Index,
        },
        function_declaration: struct {
            declaration_index: Sema.Declaration.Index,
            parameters: std.AutoArrayHashMapUnmanaged(Sema.Strings.Range, Entity.Key) = .{},
            return_type: Entity.Key = 0,
        },
        field_declaration: struct {
            type: Entity.Key,
        },
        parameter_declaration: struct {
            type: Entity.Key,
            index: usize,
        },

        if_statement: void,
        loop_statement: void,
        block_statement: void,
        type: void,
    };

    pub const Stage = enum {
        idle,
        resolving,
        resolved,
    };
    pub const EntityInput = struct {
        parent: ?Entity.Key = null,
        source: ?Sema.Strings.Range = null,
        name: Sema.Strings.Range,
        hir_inst_index: Hir.Inst.Index,

        data: Data,
    };

    pub fn getHir(self: *Entity) *Hir {
        return self.builder.sema.getHir(self.source);
    }
    pub fn getHirInstruction(self: *Entity, hir_inst_index: Hir.Inst.Index) Hir.Inst {
        const hir = self.builder.sema.getHir(self.source);
        return hir.insts.items[hir_inst_index];
    }
    pub fn getHirList(self: *Entity, hir_list_index: Hir.InternedLists.Range) []Hir.Inst.Index {
        const hir = self.builder.sema.getHir(self.source);
        return hir.lists.getSlice(hir_list_index);
    }

    pub fn internNode(self: *Entity, node: Ast.Node.Index) Error!Sema.Strings.Range {
        const hir = self.builder.sema.getHir(self.source);
        const slice = hir.ast.getNodeSlice(node);
        return try self.builder.sema.strings.internSlice(slice);
    }
    pub fn pushDependency(self: *Entity, key: Entity.Key) !void {
        try self.dependencies.put(self.builder.arena.allocator(), key, {});
        try self.builder.queueEntity(key);
    }
    pub fn resolveDependencies(self: *Entity) Error!void {
        while (self.dependencies.popOrNull()) |kv| {
            std.debug.print("resolveDependencies: {}\n", .{kv.key == self.key});
            const is_self = kv.key == self.key;
            _ = is_self; // autofix
            const entity = self.builder.getEntity(kv.key);
            _ = try entity.resolveType();
            const val = try entity.resolveValue();
            std.debug.print("resolveDependencies: {}\n", .{val});
        }
    }
    pub fn init(builder: *Builder, key: Key, input: EntityInput) !Entity {
        var ent = Entity{
            .name = if (input.parent) |parent| try builder.internMultipleSlices(&.{ builder.getEntity(parent).name, "::", input.name }) else input.name,
            .key = key,
            .hir_inst_index = input.hir_inst_index,
            .builder = builder,
            .data = input.data,
            .source = input.source orelse if (input.parent) |parent| builder.getEntity(parent).source else std.debug.panic("no source", .{}),
        };
        // switch (input.data) {
        //     .module_declaration => {
        //         ent.type = .{ .resolved = Sema.Type.simple(.type) };
        //     },
        //     .type, .global_type_declaration => {
        //         ent.symbols = .resolved;
        //         ent.type = .{ .resolved = Sema.Type.simple(.type) };
        //     },
        //     else => {},
        // }
        switch (ent.getHirInstruction(input.hir_inst_index)) {
            .global_decl => |global_decl| {
                ent.is_pub = global_decl.visibility == .public;
                ent.is_export = global_decl.exported;
            },

            else => {},
        }
        return ent;
    }
    const EntityState = struct {
        hir_inst_index: Hir.Inst.Index,
        // data: Data,
        type: std.meta.FieldType(Entity, .type),
        value: std.meta.FieldType(Entity, .value),
        // instruction: std.meta.FieldType(Entity, .instruction),
        symbols: std.meta.FieldType(Entity, .symbols),
    };
    pub fn getState(self: *Entity) EntityState {
        return .{
            .hir_inst_index = self.hir_inst_index,
            // .data = self.data,
            .type = self.type,
            .value = self.value,
            .symbols = self.symbols,
        };
    }
    pub inline fn formatKey(self: *Entity) Error![]const u8 {
        var buf: [256]u8 = undefined;
        const slice = try std.fmt.bufPrint(
            buf[0..],
            "Ent({s}: {d}, .{s}, {s})",
            .{
                self.builder.getSlice(self.name),
                self.key,
                @tagName(self.data),
                try formatHirIndex(self.getHir(), self.hir_inst_index),
            },
        );
        return slice;
    }

    pub fn collectEntities(self: *Entity) Error!void {
        switch (self.symbols) {
            .idle => {},
            .resolving => {
                return error.CircularDependency;
            },
            .resolved => {
                return;
            },
        }
        const entity_key = self.key;
        self.symbols = .resolving;
        var builder = self.builder;
        defer builder.getEntity(entity_key).symbols = .resolved;

        const trace = self.builder.tracer.begin(
            @src(),
            .{ "collectEntities", "{s}.collectEntities", .{
                try self.formatKey(),
            } },
            .{
                // .self = self.getState(),
                // .builder_state = self.builder.getState(),
            },
        );
        defer trace.end(.{
            .post_state = self.builder.getState(),
        });
        switch (self.data) {
            .module_declaration => try self.collectModuleSymbols(),
            .function_declaration => try self.collectFunctionSymbols(),
            .field_declaration => try self.collectFieldSymbols(),
            .global_declaration => {},
            // .function_declaration => {},
            .parameter_declaration => {},
            .global_type_declaration => {},
            .type => {},
            else => std.debug.panic("unhandled collectEntities: {s}\n", .{@tagName(self.data)}),
        }
        // self.symbols = .resolved;
    }
    pub fn collectModuleSymbols(self: *Entity) Error!void {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "collectModuleSymbols", "{s}.collectModuleSymbols", .{
                try self.formatKey(),
            } },
            .{
                // .self = self.getState(),
                // .builder_state = self.builder.getState(),
            },
        );
        defer trace.end(.{
            // .builder_state = self.builder.getState(),

        });

        const hir_inst = self.getHirInstruction(self.hir_inst_index).struct_decl;
        const fields_list = self.getHirList(hir_inst.fields_list);
        for (fields_list, 0..) |field_inst_index, i| {
            const field_inst = self.getHirInstruction(field_inst_index).struct_field;

            const name_slice_range = try self.internNode(field_inst.name_node);
            const name_slice = self.builder.getSlice(name_slice_range);

            const field_trace = self.builder.tracer.begin(
                @src(),
                .{ "collectModuleSymbols", "[COLLECT_FIELD={s}]", .{
                    name_slice,
                } },
                .{
                    // .self = self.getState(),
                    // .builder_state = self.builder.getState(),
                    .hir_inst_index = field_inst_index,
                    .hir_inst = field_inst,
                },
            );
            defer field_trace.end(.{});

            const field_entity_key = try self.builder.makeEntity(.{
                .parent = self.key,
                .name = name_slice_range,
                .hir_inst_index = field_inst_index,
                .data = .{ .field_declaration = .{ .type = 0 } },
            });
            try self.data.module_declaration.fields.put(self.builder.arena.allocator(), name_slice_range, .{
                .name = name_slice_range,
                .entity = field_entity_key,
                .index = i,
                .offset = 0,
            });
        }

        const declarations_list = self.getHirList(hir_inst.declarations_list);
        for (declarations_list) |declaration_inst_index| {
            switch (self.getHirInstruction(declaration_inst_index)) {
                .global_decl => |global_decl_inst| {
                    const name_slice_range = try self.internNode(global_decl_inst.name_node);
                    const decl_type = if (global_decl_inst.is_type) "TYPE" else if (global_decl_inst.is_fn) "FN" else "GLOBAL";
                    const declaration_trace = self.builder.tracer.begin(
                        @src(),
                        .{ "collectModuleSymbols", "[COLLECT_{s}_DECLARATION={s}]", .{
                            decl_type,
                            self.builder.getSlice(name_slice_range),
                        } },
                        .{
                            // .self = self.getState(),
                            .hir_inst_index = declaration_inst_index,
                            .hir_inst = global_decl_inst,
                        },
                    );
                    defer declaration_trace.end(.{});
                    // const interned_name = try self.builder.internSlice(name_slice);

                    if (global_decl_inst.is_type) {
                        const global_type_entity_key = try self.builder.makeEntity(.{
                            .parent = self.key,
                            .name = name_slice_range,
                            .hir_inst_index = declaration_inst_index,
                            .data = .{
                                .global_type_declaration = .{
                                    .declaration_index = try self.builder.pushDeclaration(.{
                                        .name = name_slice_range,
                                        .is_pub = global_decl_inst.visibility == .public,
                                        .is_export = global_decl_inst.exported,
                                        .type = Sema.Type.simple(.type),
                                        .value = Sema.Value.simple(.unknown),
                                    }),
                                },
                            },
                        });
                        try self.data.module_declaration.declarations.put(
                            self.builder.arena.allocator(),
                            name_slice_range,
                            global_type_entity_key,
                        );
                    } else if (global_decl_inst.is_fn) {
                        const global_fn_entity_key = try self.builder.makeEntity(.{
                            .parent = self.key,
                            .name = name_slice_range,
                            .hir_inst_index = declaration_inst_index,
                            .data = .{
                                .function_declaration = .{
                                    .declaration_index = try self.builder.pushDeclaration(.{
                                        .name = name_slice_range,
                                        .is_pub = global_decl_inst.visibility == .public,
                                        .is_export = global_decl_inst.exported,
                                        .type = Sema.Type.simple(.unknown),
                                        .value = Sema.Value.simple(.unknown),
                                    }),
                                },
                            },
                        });
                        try self.data.module_declaration.declarations.put(
                            self.builder.arena.allocator(),
                            name_slice_range,
                            global_fn_entity_key,
                        );
                    } else {
                        const global_entity_key = try self.builder.makeEntity(.{
                            .parent = self.key,
                            .name = name_slice_range,
                            .hir_inst_index = declaration_inst_index,
                            .data = .{
                                .global_declaration = .{
                                    .declaration_index = try self.builder.pushDeclaration(.{
                                        .name = name_slice_range,
                                        .is_pub = global_decl_inst.visibility == .public,
                                        .is_export = global_decl_inst.exported,
                                        .type = Sema.Type.simple(.unknown),
                                        .value = Sema.Value.simple(.unknown),
                                    }),
                                    .type = undefined,
                                },
                            },
                        });
                        // const type_entity = ;
                        self.builder.getEntity(global_entity_key).data.global_declaration.type = try self.builder.makeEntity(.{
                            .parent = global_entity_key,
                            .name = try self.builder.internSlice("%type"),
                            .hir_inst_index = global_decl_inst.type orelse @panic("global declaration needs explicit type"),
                            .data = .{ .type = {} },
                        });
                        try self.data.module_declaration.declarations.put(
                            self.builder.arena.allocator(),
                            name_slice_range,
                            global_entity_key,
                        );
                    }
                },
                else => unreachable,
            }
        }
    }
    pub fn collectFunctionSymbols(self: *Entity) Error!void {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "collectFunctionSymbols", "{s}.collectFunctionSymbols", .{
                try self.formatKey(),
            } },
            .{
                // .self = self.getState(),
            },
        );
        defer trace.end(.{});
        var builder = self.builder;
        const key = self.key;
        const hir_inst = self.getHirInstruction(self.hir_inst_index).global_decl;
        const fn_inst = self.getHirInstruction(hir_inst.type orelse unreachable).fn_decl;
        const parameters_list = self.getHirList(fn_inst.params_list);
        const data = &self.data.function_declaration;
        _ = data; // autofix
        for (parameters_list, 0..) |parameter_inst_index, i| {
            const parameter_inst = self.getHirInstruction(parameter_inst_index).param_decl;
            const name_slice_range = try self.internNode(parameter_inst.name_node);

            const parameter_trace = builder.tracer.begin(
                @src(),
                .{ "collectFunctionSymbols", "[COLLECT_PARAMETER='{s}']", .{
                    self.builder.getSlice(name_slice_range),
                } },
                .{
                    // .self = self.getState(),
                    // .builder_state = self.builder.getState(),
                    .hir_inst_index = parameter_inst_index,
                    .hir_inst = parameter_inst,
                },
            );

            const parameter_entity_key = try builder.makeEntity(.{
                .parent = key,
                .name = name_slice_range,
                .hir_inst_index = parameter_inst_index,
                .data = .{ .parameter_declaration = .{ .type = 0, .index = i } },
            });
            if (builder.getEntityKeyByHirInst(parameter_inst.type)) |parameter_type_entity_key| {
                const ent = builder.getEntity(parameter_entity_key);
                ent.data.parameter_declaration.type = parameter_type_entity_key;
            } else {
                const parameter_type_entity_key = try builder.makeEntity(.{
                    .parent = parameter_entity_key,
                    .name = try builder.internSlice("%type"),
                    .hir_inst_index = parameter_inst.type,
                    .data = .{ .type = {} },
                });
                // const _self = builder.getEntity(key);
                builder.getEntity(parameter_entity_key).data.parameter_declaration.type = parameter_type_entity_key;
                // try _self.data.function_declaration.parameters.put(builder.arena.allocator(), name_slice_range, parameter_type_entity_key);
            }

            defer parameter_trace.end(.{ .entity_key = parameter_entity_key });
            const _self = builder.getEntity(key);
            try _self.data.function_declaration.parameters.put(
                builder.arena.allocator(),
                name_slice_range,
                parameter_entity_key,
            );
        }

        if (builder.getEntityKeyByHirInst(fn_inst.return_type)) |ret_entity_key| {
            builder.getEntity(key).data.function_declaration.return_type = ret_entity_key;
        } else {
            const ret_entity_key = try builder.makeEntity(.{
                .parent = key,
                .name = try builder.internSlice("%return_type"),
                .hir_inst_index = fn_inst.return_type,
                .data = .{ .type = {} },
            });
            builder.getEntity(key).data.function_declaration.return_type = ret_entity_key;
        }
    }
    pub fn collectFieldSymbols(self: *Entity) Error!void {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "collectFieldSymbols", "{s}.collectFieldSymbols", .{
                try self.formatKey(),
            } },
            .{},
        );
        defer trace.end(.{});

        const hir_inst = self.getHirInstruction(self.hir_inst_index).struct_field;
        const type_hir_inst = hir_inst.type orelse std.debug.panic("field type not resolved: {any}", .{hir_inst.type});
        if (self.builder.getEntityKeyByHirInst(type_hir_inst)) |field_type_entity_key| {
            self.data.field_declaration.type = field_type_entity_key;
        } else {
            self.data.field_declaration.type = try self.builder.makeEntity(.{
                .parent = self.key,
                .name = try self.builder.internSlice("%type"),
                .hir_inst_index = type_hir_inst,
                .data = .{ .type = {} },
            });
        }
    }
    pub fn collectGlobalSymbols(self: *Entity) Error!void {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "collectGlobalSymbols", "{s}.collectGlobalSymbols", .{
                try self.formatKey(),
            } },
            .{},
        );
        defer trace.end(.{});

        const hir_inst = self.getHirInstruction(self.hir_inst_index).global_decl;
        const name_slice_range = try self.internNode(hir_inst.name_node);
        const name_slice = self.builder.getSlice(name_slice_range);
        _ = name_slice; // autofix

        const type_hir_inst = hir_inst.type orelse std.debug.panic("field type not resolved: {any}", .{hir_inst.type});

        // const type_hir_inst = hir_inst.type orelse std.debug.panic("field type not resolved: {any}", .{hir_inst.type});
        if (self.builder.getEntityKeyByHirInst(type_hir_inst)) |field_type_entity_key| {
            self.data.field_declaration.type = field_type_entity_key;
        } else {
            self.data.field_declaration.type = try self.builder.makeEntity(.{
                .parent = self.key,
                .name = try self.builder.internSlice("%type"),
                .hir_inst_index = type_hir_inst,
                .data = .{ .type = {} },
            });
        }
        // if (self.builder.getEntityKeyByHirInst(type_hir_inst)) |type_entity_key| {

        //     self.data.global_declaration.type = type_entity_key;
        // } else {
        //     self.data.global_declaration.type = try self.builder.makeEntity(.{
        //         .parent = self.key,
        //         .name = try self.builder.internSlice("%type"),
        //         .hir_inst_index = type_hir_inst,
        //         .data = .{ .type = {} },
        //     });
        // }

    }
    pub fn collectParameterSymbols(self: *Entity) Error!void {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "collectParameterSymbols", "{s}.collectParameterSymbols", .{
                try self.formatKey(),
            } },
            .{},
        );
        defer trace.end(.{});
    }
    pub fn resolveType(self: *Entity) Error!Sema.Type.Key {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "resolveType", "{s}.resolveType", .{
                try self.formatKey(),
            } },
            .{},
        );
        defer trace.end(.{ .type = self.type, .post_state = self.builder.getState() });
        switch (self.type) {
            .idle => {
                try self.collectEntities();
            },
            .resolving => {
                return error.CircularDependency;
            },
            .resolved => {
                return self.type.resolved;
            },
        }
        self.type = .resolving;

        self.type = .{
            .resolved = switch (self.data) {
                // .struct_declaration => {},
                .function_declaration => try self.resolveFunctionType(),
                .parameter_declaration => try self.resolveParameterType(),
                .field_declaration => try self.resolveFieldType(),
                .global_declaration => try self.resolveGlobalType(),
                //noop, type of type is always 'type'..the actual type is the type values
                .module_declaration,
                .type,
                .global_type_declaration,
                => {
                    // self.type = .{ .resolved = Sema.Type.simple(.type) };
                    self.type = .{
                        .resolved = try self.builder.internTypeData(.{
                            .typeof = .{
                                .child = self.builder.unwrapTypeValue(try self.resolveValue()),
                            },
                        }),
                    };
                    return self.type.resolved;
                },

                // .global_declaration => {
                //     return self.builder.unwrapTypeValue(try self.resolveValue());
                // },
                else => std.debug.panic("unhandled data: {s}", .{@tagName(self.data)}),
            },
        };
        return self.type.resolved;
    }

    pub fn resolveFunctionType(self: *Entity) Error!Sema.Type.Key {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "resolveFunctionType", "{s}.resolveFunctionType", .{
                try self.formatKey(),
            } },
            .{},
        );
        defer trace.end(.{});
        const params = self.data.function_declaration.parameters;
        var params_list = self.builder.newList();
        var iter = params.iterator();

        while (iter.next()) |param_entry| {
            const param_trace = self.builder.tracer.begin(
                @src(),
                .{ "resolveFunctionType", "[RESOLVE_PARAMETER_TYPE='{s}']", .{
                    self.builder.getSlice(param_entry.key_ptr.*),
                } },
                .{},
            );
            defer param_trace.end(.{});

            const param_type = try self.builder.getEntity(param_entry.value_ptr.*).resolveType();
            try params_list.append(param_type.encode());
        }
        const ret_type_value = try self.builder.getEntity(self.data.function_declaration.return_type).resolveValue();

        const ty = try self.builder.internTypeData(.{
            .function = .{
                .is_builtin = false,
                .entity = self.key,
                .params = try params_list.commit(),
                .ret = self.builder.unwrapTypeValue(ret_type_value),
            },
        });
        const declaration = &self.builder.sema.declarations.items[self.data.function_declaration.declaration_index];
        declaration.type = ty;
        return ty;
    }
    pub fn resolveParameterType(self: *Entity) Error!Sema.Type.Key {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "resolveParameterType", "{s}.resolveParameterType", .{
                try self.formatKey(),
            } },
            .{},
        );
        defer trace.end(.{});
        // const hir_inst = getHirInst(self.builder.hir, self.hir_inst_index).param_decl;
        const parameter_type_entity = self.builder.getEntity(self.data.parameter_declaration.type);
        const parameter_type_value = try parameter_type_entity.resolveValue();

        return self.builder.unwrapTypeValue(parameter_type_value);
    }
    pub fn resolveFieldType(self: *Entity) Error!Sema.Type.Key {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "resolveFieldType", "{s}.resolveFieldType", .{
                try self.formatKey(),
            } },
            .{},
        );
        defer trace.end(.{});
        // const field_type_entity = self.builder.getEntity(self.data.field_declaration);
        const field_type_entity = self.builder.getEntity(self.data.field_declaration.type);
        const field_type_value = try field_type_entity.resolveValue();
        return self.builder.unwrapTypeValue(field_type_value);
    }
    pub fn resolveGlobalType(self: *Entity) Error!Sema.Type.Key {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "resolveGlobalType", "{s}.resolveGlobalType", .{
                try self.formatKey(),
            } },
            .{},
        );
        defer trace.end(.{});
        // const field_type_entity = self.builder.getEntity(self.data.field_declaration);
        const field_type_entity = self.builder.getEntity(self.data.global_declaration.type);
        const field_type_value = try field_type_entity.resolveValue();
        return self.builder.unwrapTypeValue(field_type_value);
    }
    pub fn resolveValue(self: *Entity) Error!Sema.Value.Key {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "resolveValue", "{s}.resolveValue", .{
                try self.formatKey(),
            } },
            .{},
        );
        defer trace.end(.{ .value = self.value, .post_state = self.builder.getState() });
        const key = self.key;
        _ = key; // autofix
        switch (self.value) {
            .idle => {
                //         const value = switch (self.data) {

                //         }
                // _ = try self.resolveType();
            },
            .resolving => {
                return error.CircularDependency;
            },
            .resolved => {
                return self.value.resolved;
            },
        }
        self.value = .resolving;

        const value = switch (self.data) {
            .type => try self.resolveTypeValue(),
            .function_declaration => try self.resolveFunctionValue(),
            .global_type_declaration => try self.resolveGlobalTypeValue(),
            .module_declaration => try self.resolveModuleValue(),
            .global_declaration => try self.resolveGlobalValue(),
            else => std.debug.panic("unhandled value: {s}", .{@tagName(self.data)}),
        };
        self.value = .{ .resolved = value };
        if (self.dependencies.count() > 0) {
            try self.resolveDependencies();
            _ = try self.resolveValue();
        }
        return self.value.resolved;
    }

    pub fn resolveTypeValue(self: *Entity) Error!Sema.Value.Key {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "resolveTypeValue", "{s}.resolveTypeValue", .{
                try self.formatKey(),
            } },
            .{},
        );
        defer trace.end(.{});

        const hir_inst = self.getHirInstruction(self.hir_inst_index);
        switch (hir_inst) {
            .global_get => |global_get_inst| {
                const global_entity = self.builder.getEntityByHirInst(global_get_inst.operand);
                return try global_entity.resolveValue();
            },
            else => {
                // return try self.resolveValue();
            },
        }

        switch (std.meta.activeTag(hir_inst)) {
            // .global_get => |global_get_inst| {
            //     var xien = self.builder.getEntityByHirInst(hir_inst.) orelse std.debug.panic("global_get_inst is not a number", .{});

            //     // const global_get_inst = self.getInstruction(self.hir_inst_index);
            //     // const global_get_inst_value = self.builder.getValue(global_get_inst.value) orelse std.debug.panic("global_get_inst_value is not a number", .{});
            //     // const global_get_inst_value_int = self.builder.getNumberValueKeyAs(i64, global_get_inst_value.value);
            //     // return try self.pushInstruction(hir_inst_index, .{
            //     //     .op = .store,
            //     //     .type = Sema.Type.simple(.void),
            //     //     .value = Sema.Value.simple(.void),
            //     //     .data = .{ .operand_payload = .{
            //     //         .operand = pointer_inst_index,
            //     //         .payload = load_inst_value_int,
            //     //     } },
            //     // });
            //     return try global_get_inst.resolveValue();
            // },
            inline else => |tag| {
                const tag_name = @tagName(tag);
                if (comptime std.mem.startsWith(u8, tag_name, "ty_")) {
                    const value_tag_name = "type_" ++ tag_name[3..];
                    return Sema.Value.simple(
                        std.meta.stringToEnum(Sema.Value.Simple, value_tag_name) orelse std.debug.panic(
                            "TODO: resolveTypeValue {s}",
                            .{value_tag_name},
                        ),
                    );
                }
                // const type_entity = self.builder.getEntityByHirInst(self.hir_inst_index);

                // return self.builder.internValueData(.{ .type = try self.resolveType() });
                std.debug.panic("TODO: resolveTypeValue {d} {s}", .{ self.hir_inst_index, tag_name });
            },
        }
    }
    pub fn resolveFunctionValue(self: *Entity) Error!Sema.Value.Key {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "resolveFunctionValue", "{s}.resolveFunctionValue", .{
                try self.formatKey(),
            } },
            .{},
        );
        const key = self.key;
        _ = key; // autofix
        defer trace.end(.{});
        const hir_inst = self.getHirInstruction(self.hir_inst_index).global_decl;
        // const type_key = try self.resolveType();
        // const ty = self.builder.getType(type_key) orelse std.debug.panic("function type not resolved: {any}", .{type_key});

        if (hir_inst.init) |init_inst| {
            // const a = try self.gen(init_inst);

            var scope = Scope.init(self, self.builder.allocator);
            defer scope.deinit();
            const block_inst = try scope.genRootBlock(init_inst);
            // try block.handleBlockInstructionInner(init_inst);

            // const block_inst = try block.commit();

            return try self.builder.internValueData(.{
                .function = .{
                    .type = try self.resolveType(),
                    .init = block_inst,
                },
            });

            // if (scope.dependencies.count() > 0) {
            //     try scope.resolveDependencies();
            //     self_entity = self.builder.getEntity(key);
            //     var second_pass_scope = Scope.init(self_entity, self.builder.allocator);
            //     defer second_pass_scope.deinit();

            //     const second_pass_block_inst = try second_pass_scope.genRootBlock(init_inst);
            //     self_entity.value = .{ .resolved = try self.builder.internValueData(.{
            //         .function = .{
            //             .type = try self_entity.resolveType(),
            //             .init = second_pass_block_inst,
            //         },
            //     }) };
            // }

            // return self_entity.value.resolved;
        }

        return self.builder.internValueData(.{
            .function = .{
                .type = try self.resolveType(),
                .init = null,
            },
        });
    }
    // pub fn resolveBlock(self: *Entity) Error!Block {
    //     const trace = self.builder.tracer.begin(
    //         @src(),
    //         .{ "resolveBlock", "{s}.resolveBlock", .{
    //             try self.formatKey(),
    //         } },
    //         .{},
    //     );
    //     defer trace.end(.{});
    // }

    pub fn resolveGlobalTypeValue(self: *Entity) Error!Sema.Value.Key {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "resolveGlobalTypeValue", "{s}.resolveGlobalTypeValue", .{
                try self.formatKey(),
            } },
            .{},
        );
        defer trace.end(.{});
        const hir_inst = self.getHirInstruction(self.hir_inst_index).global_decl;

        const init_inst = hir_inst.init orelse std.debug.panic("global type declaration has no init", .{});
        var scope = Scope.init(self, self.builder.allocator);
        defer scope.deinit();
        // scope.is_comptime = true;
        const block_inst = try scope.genRootBlock(init_inst);

        const inst = self.builder.sema.instructions.items[block_inst];
        return inst.typed_value.value;
    }
    pub fn resolveGlobalValue(self: *Entity) Error!Sema.Value.Key {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "resolveGlobalValue", "{s}.resolveGlobalValue", .{
                try self.formatKey(),
            } },
            .{},
        );
        defer trace.end(.{});

        const hir_inst = self.getHirInstruction(self.hir_inst_index).global_decl;

        const init_inst = hir_inst.init orelse std.debug.panic("global type declaration has no init", .{});
        var scope = Scope.init(self, self.builder.allocator);
        defer scope.deinit();
        // scope.is_comptime = true;
        const block_inst = try scope.genRootBlock(init_inst);

        const inst = self.builder.sema.instructions.items[block_inst];
        // return inst.typed_value.value;
        return try self.builder.internValueData(.{
            .global = .{
                .typed_value = inst.typed_value,
                .init = block_inst,
            },
        });
    }
    const SortByAlignmentContext = struct {
        values: []const Sema.Type,
        pub fn lessThan(ctx: @This(), a_index: usize, b_index: usize) bool {
            return ctx.values[a_index].alignment > ctx.values[b_index].alignment;
        }
    };
    pub fn resolveModuleValue(self: *Entity) Error!Sema.Value.Key {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "resolveModuleValue", "{s}.resolveModuleValue", .{
                try self.formatKey(),
            } },
            .{},
        );
        defer trace.end(.{});

        var fields_list = self.builder.newList();

        var iter = self.data.module_declaration.fields.iterator();

        var fields_data = std.AutoArrayHashMap(Sema.Strings.Range, Sema.Type).init(self.builder.allocator);
        defer fields_data.deinit();
        var alignment: usize = 0;
        while (iter.next()) |field| {
            var field_entity = self.builder.getEntity(field.value_ptr.*.entity);
            const ty = try field_entity.resolveType();
            const field_size, const field_alignment = blk: {
                if (self.builder.getType(ty)) |field_ty| {
                    break :blk .{ field_ty.size, field_ty.alignment };
                } else {
                    const simple_size = self.builder.getSimpleTypeSize(ty.simple);
                    break :blk .{ simple_size, simple_size };
                }
            };

            alignment = @max(alignment, field_alignment);
            try fields_data.put(field.key_ptr.*, .{
                .hash = 0,
                .data = .{
                    .struct_field = .{
                        .type = ty,
                        .offset = 0,
                    },
                },
                .size = field_size,
                .alignment = field_alignment,
            });
        }
        fields_data.sort(SortByAlignmentContext{ .values = fields_data.values() });

        var iter_sorted_fields = fields_data.iterator();
        var i: usize = 0;

        var offset: usize = 0;
        var struct_hasher = Hasher.new(0);
        struct_hasher.update("struct");

        // For now I think it makes sense for struct/modules to be unique, so let's hash the entity key with it to make sure of that.
        // We may want to change this for simple structs, like, structs without declarations or tuples..let's see.
        struct_hasher.update(self.key);

        while (iter_sorted_fields.next()) |entry| {
            var field = entry.value_ptr.*;
            const padding = @mod(field.alignment - @mod(offset, field.alignment), field.alignment);
            offset += padding;
            field.data.struct_field.offset = offset;
            offset += field.size;

            var hasher = Hasher.new(0);
            hasher.update(field.data.struct_field.offset);
            hasher.update(self.builder.getTypeKeyHash(field.data.struct_field.type));

            field.hash = hasher.final();
            struct_hasher.update("field");

            struct_hasher.update(field.hash);

            const field_type = try self.builder.internType(field);
            try fields_list.append(field_type.encode());
            var field_obj = self.data.module_declaration.fields.getPtr(entry.key_ptr.*).?;

            field_obj.index = i;
            field_obj.offset = field.data.struct_field.offset;
            // self.data.module_declaration.fields.getPtr(entry.key_ptr.*).?.index = i;
            i += 1;
        }

        const trailing_padding = if (alignment > 0) @mod(alignment - @mod(offset, alignment), alignment) else 0;
        const struct_size = offset + trailing_padding;
        const struct_ty = try self.builder.internType(.{
            .hash = struct_hasher.final(),
            .data = .{
                .@"struct" = .{
                    .entity = self.key,
                    .fields = try fields_list.commit(),
                },
            },
            .size = struct_size,
            .alignment = alignment,
        });

        return try self.builder.internValueData(.{
            .type = struct_ty,
        });
    }

    pub fn resolveDeclaration(self: *Entity) Error!Sema.Declaration.Index {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "resolveDeclaration", "{s}.resolveDeclaration", .{
                try self.formatKey(),
            } },
            .{},
        );
        defer trace.end(.{});

        switch (self.declaration) {
            .idle => {
                _ = try self.resolveValue();
            },
            .resolving => {
                return error.CircularDependency;
            },
            .resolved => {
                return self.declaration.resolved;
            },
        }
        self.declaration = .resolving;

        switch (self.data) {
            .function_declaration => {
                self.builder.setDeclaration(self.data.function_declaration.declaration_index, .{
                    .name = self.name,
                    .is_export = self.is_export,
                    .is_pub = self.is_pub,
                    .type = try self.resolveType(),
                    .value = try self.resolveValue(),
                });
                self.declaration = .{ .resolved = self.data.function_declaration.declaration_index };
                while (self.builder.queue.popOrNull()) |entry| {
                    // try self.builder.queueEntity(entry.key_ptr.*);
                    var queued = self.builder.getEntity(entry.key);
                    const queue_trace = self.builder.tracer.begin(
                        @src(),
                        .{
                            "resolveDeclaration", "{s}.resolveQueue({s})", .{
                                try self.formatKey(),
                                try queued.formatKey(),
                            },
                        },
                        .{},
                    );
                    defer queue_trace.end(.{});
                    _ = try queued.resolveDeclaration();
                }

                return self.data.function_declaration.declaration_index;
            },
            .global_type_declaration => {
                self.builder.setDeclaration(self.data.global_type_declaration.declaration_index, .{
                    .name = self.name,
                    .is_export = self.is_export,
                    .is_pub = self.is_pub,
                    .type = try self.resolveType(),
                    .value = try self.resolveValue(),
                });
                self.declaration = .{ .resolved = self.data.global_type_declaration.declaration_index };
                while (self.builder.queue.popOrNull()) |entry| {
                    // try self.builder.queueEntity(entry.key_ptr.*);
                    var queued = self.builder.getEntity(entry.key);
                    _ = try queued.resolveDeclaration();
                }
                return self.data.global_type_declaration.declaration_index;
            },
            .global_declaration => {
                self.builder.setDeclaration(self.data.global_declaration.declaration_index, .{
                    .name = self.name,
                    .is_export = self.is_export,
                    .is_pub = self.is_pub,
                    .type = try self.resolveType(),
                    .value = try self.resolveGlobalValue(),
                });
                return self.data.global_declaration.declaration_index;
            },
            // .parameter_declaration => {
            //     // noop

            // },
            else => std.debug.panic("unhandled declaration: {s}", .{@tagName(self.data)}),
        }
    }
    fn getInstructionFn(self: *anyopaque, index: Sema.Instruction.Index) Sema.Instruction {
        _ = index; // autofix
        const entity: *Entity = @alignCast(@ptrCast(self));
        _ = entity; // autofix
        // return builder.sema.instructions.items[index];
        @panic("not implemented");
    }
    // pub fn gen(self: *Entity, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    //     const hir_inst = self.getHirInstruction(hir_inst_index);
    //     _ = hir_inst; // autofix
    //     // const hir_inst = self.

    //     const inst_context = InstContext{
    //         .builder = self.builder,
    //         .depth = 0,
    //         .context = @ptrCast(self),
    //         .getInstructionFn = getInstructionFn,
    //         .setInstructionFn = undefined,
    //         .pushInstructionFn = undefined,
    //         .goToFn = undefined,
    //     };
    //     _ = inst_context; // autofix
    //     // switch (hir_inst.op) {
    //     //     .block => {},
    //     //     else => {},
    //     // }

    //     @panic("not implemented");
    // }
};

pub const Scope = struct {
    instructions: std.ArrayListUnmanaged(Sema.Instruction) = .{},
    // instructions: std.AutoArrayHashMapUnmanaged(Hir.Inst.Index, Sema.Instruction) = .{}, // allocator: std.mem.Allocator,
    instructions_by_hir_inst: std.AutoArrayHashMapUnmanaged(Hir.Inst.Index, Sema.Instruction.Index) = .{},
    arena: std.heap.ArenaAllocator,

    entity: *Entity,
    builder: *Builder,
    trace: Tracer.EndTrace,
    depth: usize = 0,
    cursor: Hir.Inst.Index = 0,
    // is_comptime: bool = false,
    is_inline: bool = false,
    active_block_instructions: ?*Sema.Lists.WorkingList = null,

    // inst_context: InstContext,
    pub fn init(entity: *Entity, allocator: std.mem.Allocator) Scope {
        return Scope{
            // .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(allocator),
            .builder = entity.builder,
            .entity = entity,
            .trace = entity.builder.tracer.begin(
                @src(),
                .{ "newScope", "Scope.newScope", .{} },
                .{},
            ),
        };
    }

    fn getInstructionFn(self: *anyopaque, index: Sema.Instruction.Index) Sema.Instruction {
        const scope: *Scope = @alignCast(@ptrCast(self));
        const inst = scope.getInstruction(index);
        return inst.*;
    }
    fn getParamValueFn(context: *anyopaque, index: u32) Sema.Value.Key {
        _ = context; // autofix
        _ = index; // autofix
        return Sema.Value.simple(.exec_time);
        // const scope: *Scope = @alignCast(@ptrCast(context));
        // const ty =try  scope.entity.resolveType();
        // if (scope.builder.getType(ty)) |ty_info| {
        //     switch (ty_info.data) {
        //         .p => |param| {
        //             return param.value;
        //         },
        //         else => {},
        //     }
        // } else {
        // }
        // _ = context; // autofix
        // _ = index; // autofix

    }
    fn setInstructionFn(self: *anyopaque, index: Sema.Instruction.Index, instruction: Sema.Instruction) void {
        const scope: *Scope = @alignCast(@ptrCast(self));
        scope.setInstruction(index, instruction);
    }
    fn goToFn(self: *anyopaque, index: Sema.Instruction.Index) void {
        _ = index; // autofix
        const scope: *Scope = @alignCast(@ptrCast(self));
        _ = scope; // autofix
        // scope.goTo(index);
    }
    fn setIdMapFn(self: *anyopaque, id: u32, index: Sema.Instruction.Index) void {
        const scope: *Scope = @alignCast(@ptrCast(self));
        scope.instructions_by_hir_inst.put(scope.arena.allocator(), id, index) catch |err| {
            std.debug.panic("setIdMapFn: {s}", .{@errorName(err)});
        };
    }
    fn pushInstructionFn(self: *anyopaque, id: ?u32, instruction: Sema.Instruction) Sema.Instruction.Index {
        var scope: *Scope = @alignCast(@ptrCast(self));
        return scope.pushInstruction(id, instruction) catch |err| {
            std.debug.panic("pushInstructionFn: {s}", .{@errorName(err)});
        };
    }
    pub fn deinit(self: *Scope) void {
        self.trace.end(.{});

        self.arena.deinit();
    }
    // pub fn makeBlock(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Block {
    //     const trace = self.builder.tracer.begin(
    //         @src(),
    //         .{ "makeBlock", "Scope.makeBlock({s})", .{
    //             try formatHirIndex(self.builder.sema.getHir(self.entity.source), hir_inst_index),
    //         } },
    //         .{},
    //     );
    //     const root_hir_inst = self.entity.getHirInstruction(hir_inst_index);
    //     const is_comptime = switch (root_hir_inst) {
    //         .block, .inline_block => |inst| inst.is_comptime,
    //         else => false,
    //     };
    //     // const is_comptime
    //     const instruction_index = try self.pushInstruction(hir_inst_index, .{
    //         .op = .block,
    //         .type = Sema.Type.simple(.void),
    //         .value = Sema.Value.simple(.void),
    //         .data = .{ .block = .{
    //             .instructions_count = 0,
    //             .is_comptime = is_comptime,
    //         } },
    //     });
    //     defer trace.end(.{
    //         .instructions = self.instructions.items,
    //         .index = instruction_index,
    //     });
    //     return Block{
    //         .scope = self,
    //         .builder = self.builder,
    //         .root_hir_inst_index = hir_inst_index,
    //         .instruction_index = instruction_index,
    //         .is_comptime = is_comptime,
    //         .cursor = hir_inst_index,
    //     };
    // }
    pub fn reserveInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const index = self.instructions.items.len;
        try self.instructions.append(self.arena.allocator(), undefined);
        try self.instructions_by_hir_inst.put(self.arena.allocator(), hir_inst_index, index);
        if (self.active_block_instructions) |instructions_list| {
            try instructions_list.append(index);
        }
        return index;
    }
    pub fn setInstruction(self: *Scope, index: Sema.Instruction.Index, instruction: Sema.Instruction) void {
        self.instructions.items[index] = instruction;
    }

    const ScopeComputeContext = struct {
        scope: *Scope,
        pub fn getInstruction(self: @This(), index: Sema.Instruction.Index) *Sema.Instruction {
            return &self.scope.instructions.items[index];
        }
        pub fn getTypedValue(self: @This(), index: Sema.Instruction.Index) Sema.TypedValue {
            return self.getInstruction(index).typed_value;
        }
        pub fn setValue(self: @This(), index: Sema.Instruction.Index, value: Sema.TypedValue) !void {
            self.scope.instructions.items[index].typed_value = value;
        }
        pub fn markDead(self: @This(), index: Sema.Instruction.Index) void {
            std.debug.print("markDead: {d}\n", .{index});
            // self.scope.markDead(index);
            self.scope.instructions.items[index].liveness = 0;
        }
        pub fn goTo(self: @This(), index: Sema.Instruction.Index) !void {
            self.scope.cursor = @intCast(index);
        }
    };
    pub fn pushInstruction(self: *Scope, hir_inst_index: ?Hir.Inst.Index, instruction: Sema.Instruction) Error!Sema.Instruction.Index {
        const index = self.instructions.items.len;
        try self.instructions.append(self.arena.allocator(), instruction);
        if (hir_inst_index) |hir_index| {
            try self.instructions_by_hir_inst.put(self.arena.allocator(), hir_index, index);
        }
        // var compute_context = self.builder.compContext(ScopeComputeContext{
        //     .scope = self,
        // }, self.is_comptime);
        // const result = try compute_context.computeInstruction(index);

        // self.instructions.items[index].typed_value = result;
        if (self.active_block_instructions) |instructions_list| {
            try instructions_list.append(index);
        }

        return index;
    }
    pub fn getInstructionByHirIndex(self: *Scope, hir_inst_index: Hir.Inst.Index) *Sema.Instruction {
        const index = self.instructions_by_hir_inst.get(hir_inst_index).?;
        return &self.instructions.items[index];
    }
    pub fn getInstruction(self: *Scope, index: Sema.Instruction.Index) *Sema.Instruction {
        return &self.instructions.items[index];
    }
    pub fn getInstructionIndex(self: *Scope, hir_inst_index: Hir.Inst.Index) Sema.Instruction.Index {
        return self.instructions_by_hir_inst.get(hir_inst_index) orelse {
            std.debug.panic("unreachable: no instruction index for hir_inst_index: {d}", .{hir_inst_index});
        };
    }
    pub fn markDead(self: *Scope, index: Sema.Instruction.Index) void {
        self.getInstruction(index).liveness = 0;
    }
    pub fn markDeadIfComptimeKnown(self: *Scope, index: Sema.Instruction.Index) void {
        var inst = self.getInstruction(index);
        if (self.isComptimeKnown(index)) {
            inst.liveness = 0;
        }
    }

    pub fn genRootBlock(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        var inst_context = InstContext{
            .builder = self.builder,
            .depth = 0,
            .context = @ptrCast(self),
            .getInstructionFn = getInstructionFn,
            .setInstructionFn = setInstructionFn,
            .getParamValueFn = getParamValueFn,
            .pushInstructionFn = pushInstructionFn,
            .goToFn = goToFn,
            .setIdMapFn = setIdMapFn,
        };
        _ = switch (hir_inst) {
            .block, .inline_block => try Index.gen(&inst_context, self, hir_inst_index),
            else => |inst| {
                std.debug.panic("unhandled hir_inst: {s}", .{@tagName(inst)});
            },
        };
        // _ = try self.resolveInstruction(hir_inst_index);
        const index: Sema.Instruction.Index = self.builder.sema.instructions.items.len;
        try self.builder.sema.instructions.appendSlice(self.builder.allocator, self.instructions.items);
        return index;
    }

    pub fn resolveInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "resolveInstruction", "Scope.resolveInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{},
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });
        self.cursor = hir_inst_index;

        const inst = switch (hir_inst) {
            .block, .inline_block => self.handleBlockInstruction(hir_inst_index),
            .br => self.handleBrInstruction(hir_inst_index),
            .loop => self.handleLoopInstruction(hir_inst_index),
            .global_get => self.handleGlobalGet(hir_inst_index),
            // .builtin_get => self.handleBuiltinGetInstruction(hir_inst_index),

            .builtin_global_get => self.handleBuiltinGlobalGetInstruction(hir_inst_index),
            .number_literal,
            .boolean_literal,
            .string_literal,
            .char_literal,
            => self.handleConstantInstruction(hir_inst_index),
            // .string_literal => self.handleStringLiteralInstruction(hir_inst_index),

            .constant_int => self.handleConstantIntInstruction(hir_inst_index),
            .ty_i8,
            .ty_i16,
            .ty_i32,
            .ty_i64,
            .ty_i128,
            .ty_i256,
            .ty_u8,
            .ty_u16,
            .ty_u32,
            .ty_u64,
            .ty_u128,
            .ty_u256,
            .ty_usize,
            .ty_f64,
            .ty_f32,
            .ty_array,
            .ty_number,
            .ty_pointer,
            .ty_void,
            .ty_boolean,
            => self.handleTypeLiteralInstruction(hir_inst_index),

            .gt,
            .ge,
            .lt,
            .le,
            .eq,
            .ne,
            => self.handleComparisonInstruction(hir_inst_index),
            .add, .sub, .mul, .div => self.handleArithmeticInstruction(hir_inst_index),
            .alloc => self.handleAllocInstruction(hir_inst_index),
            .store => self.handleStoreInstruction(hir_inst_index),
            .param_get => self.handleParamGetInstruction(hir_inst_index),
            .if_expr => self.handleIfExprInstruction(hir_inst_index),
            .load => self.handleLoadInstruction(hir_inst_index),
            .ret => self.handleRetInstruction(hir_inst_index),
            .typeof => self.handleTypeOfInstruction(hir_inst_index),

            .fn_call => self.handleFnCallInstruction(hir_inst_index),
            .as => self.handleAsInstruction(hir_inst_index),
            .param_set => self.handleParamSetInstruction(hir_inst_index),
            .param => self.handleParamInstruction(hir_inst_index),
            .field_init => self.handleFieldInitInstruction(hir_inst_index),
            .get_property_pointer => self.handleGetPropertyPointerInstruction(hir_inst_index),
            .get_element_pointer => self.handleGetElementPointerInstruction(hir_inst_index),
            .struct_decl => self.handleStructDeclInstruction(hir_inst_index),
            .type_init => self.handleTypeInitInstruction(hir_inst_index),

            .select_expr => self.handleSelectExprInstruction(hir_inst_index),
            .array_init => self.handleArrayInitInstruction(hir_inst_index),
            // .global_decl => self.handleGlobalDeclInstruction(hir_inst_index),
            else => std.debug.panic("unhandled hir_inst: {s}", .{@tagName(hir_inst)}),
        };

        return inst;
    }
    pub fn handleBlockInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleBlockInstruction", "Block.handleBlockInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });
        const current_is_comptime = self.is_comptime;
        const current_working_list = self.active_block_instructions;

        // var block = try self.makeBlock(hir_inst_index);
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const block_index = try self.pushInstruction(hir_inst_index, .{
            .op = .block,
            .typed_value = Sema.TypedValue.VOID,
            .data = .{ .block = .{
                .instructions_list = Sema.Instruction.List.empty,
                .is_comptime = self.is_comptime,
            } },
        });

        const block_hir_data = switch (hir_inst) {
            .inline_block, .block => |inst| inst,
            else => unreachable,
        };
        const hir_inst_list = self.entity.getHir().lists.getSlice(block_hir_data.instructions_list);
        self.is_comptime = self.is_comptime or block_hir_data.is_comptime;
        // std.debug.print("is_comptime: {any}\n", .{is_comptime});
        // const end_inst = hir_inst_list[hir_inst_list.len - 1];

        self.cursor += 1;
        // while (true) {
        //     _ = try self.resolveInstruction(self.cursor);
        //     if (self.cursor >= end_inst) break;
        //     self.cursor += 1;
        // }
        // var instructions_list = self.builder.newList();

        var instructions_list = self.builder.newList();
        self.active_block_instructions = &instructions_list;
        for (hir_inst_list) |index| {
            // if (self.cursor > index) {
            //     continue;
            // }
            _ = try self.resolveInstruction(index);
            // try instructions_list.append(resolved_index);
        }

        // self.getInstruction(block_index).data.block.instructions_count = self.instructions.items.len - block_index;
        self.getInstruction(block_index).data.block.instructions_list = try instructions_list.commit();
        // self.setInstruction(block_index, .{
        //     .op = .block,
        //     .type = Sema.Type.simple(.void),
        //     .value = Sema.Value.simple(.void),
        //     .data = .{ .block = .{
        //         .instructions_count = self.instructions.items.len - block_index,
        //         .is_comptime = self.is_comptime,
        //     } },
        // });

        // try block.handleBlockInstructionInner(hir_inst_index);

        // self.getInstruction(block.instruction_index).data.block.instructions_count = block.instructions.items.len + 1;
        // var inst = self.getInstruction(block_index);
        // inst.data.block.instructions_count = block.instructions.items.len + 1;
        // inst.data.block.is_comptime = block.is_comptime;
        // inst.liveness = if (block.getIsLive()) 1 else 0;

        // try self.instructions.appendSlice(self.arena.allocator(), block.instructions.items);

        self.is_comptime = current_is_comptime;
        self.active_block_instructions = current_working_list;

        return block_index;
    }
    pub fn handleLoopInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleLoopInstruction", "Block.handleLoopInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });
        const index = try self.reserveInstruction(hir_inst_index);
        const current_active_block_instructions = self.active_block_instructions;
        self.active_block_instructions = null;
        if (self.is_comptime) {
            const inst = try self.pushInstruction(hir_inst_index, .{
                .op = .void,
                .typed_value = Sema.TypedValue.VOID,
                .data = .void,
                .liveness = 0,
            });
            _ = try self.resolveInstruction(hir_inst.loop.body);
            return inst;
        }

        const body_block_index = try self.resolveInstruction(hir_inst.loop.body);
        self.setInstruction(index, .{
            .op = .loop,
            .typed_value = Sema.TypedValue.VOID,
            .data = .{
                .loop = .{
                    .body_block = body_block_index,
                },
            },
        });
        self.active_block_instructions = current_active_block_instructions;

        return index;
    }

    pub fn handleBrInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleBrInstruction", "Block.handleBrInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });

        const target_index = self.getInstructionIndex(hir_inst.br.target);
        const payload_index: ?Sema.Instruction.Index = if (hir_inst.br.operand) |operand| self.getInstructionIndex(operand) else null;

        const break_index = try self.pushInstruction(hir_inst_index, .{
            .op = .br,
            .typed_value = Sema.TypedValue.VOID,
            .data = .{
                .br = .{
                    .target = target_index,
                    .payload = payload_index,
                },
            },
        });

        return break_index;
    }

    pub fn handleGlobalGet(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const global_entity = self.builder.getEntityByHirInst(hir_inst.global_get.operand);

        return self.pushGlobalGetInstruction(hir_inst_index, global_entity.key);
    }

    pub fn handleBuiltinGlobalGetInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        // return self.pushGlobalGetInstruction(hir_inst_index, global_entity.key);
        const ty = try self.builder.internTypeData(.{ .builtin_global = hir_inst.builtin_global_get.builtin });

        return try self.pushInstruction(hir_inst_index, .{
            .op = .constant,
            .typed_value = .{
                .type = ty,
                .value = try self.builder.internValueData(.{ .type = ty }),
            },
            .data = .void,
        });
        // switch (hir_inst.builtin_global_get.builtin) {
        //     .comptime_log => {
        //         // const ty = try self.builder.internTypeData(.{ .function = .{
        //         //     .is_builtin = true,
        //         //     .params = try self.builder.sema.lists.internSlice(
        //         //         &.{
        //         //             (try self.builder.internTypeData(.{ .any = .{} })).encode(),
        //         //         },
        //         //     ),
        //         //     .ret = Sema.Type.simple(.void),
        //         //     .entity = std.math.maxInt(Entity.Key),
        //         // } });
        //         const ty = try self.builder.internTypeData(.{ .builtin_global = .comptime_log    });

        //         return try self.pushInstruction(hir_inst_index, .{
        //             .op = .constant,
        //             .type = Sema.Type.simple(.type),
        //             .value = try self.builder.internValueData(.{ .builtin_global = .comptime_log }),
        //             .data = .void,
        //         });
        //     },
        //     .as => {
        //         // const ty = try self.builder.internTypeData(.{ .function = .{
        //         //     .is_builtin = true,
        //         //     .params = try self.builder.sema.lists.internSlice(
        //         //         &.{
        //         //             Sema.Type.simple(.type).encode(),
        //         //             (try self.builder.internTypeData(.{ .any = .{} })).encode(),
        //         //         },
        //         //     ),
        //         //     .ret = Sema.Type.simple(.type),
        //         //     .entity = std.math.maxInt(Entity.Key),
        //         // } });
        //         const ty = try self.builder.internTypeData(.{ .builtin_global = .as });

        //         return try self.pushInstruction(hir_inst_index, .{
        //             .op = .constant,
        //             .type = Sema.Type.simple(.type),
        //             .value = try self.builder.internValueData(.{ .type = ty }),
        //             .data = .void,
        //         });
        //     },
        // }
        // return try self.pushInstruction(hir_inst_index, .{
        //     .op = .builtin_global_get,
        //     .type = Sema.Type.simple(.type),
        //     .value = switch (hir_inst.builtin_global_get.builtin) {
        //         .comptime_log => try self.builder.internValueData(.{ .type = try self.builder.internType(.{
        //             .hash = Hasher.hash("builtin-comptime-log"),
        //             .data = .{ .function = .{

        //             } },
        //         }) }),
        //         // .std => std.debug.panic("TODO: std", .{}),
        //         // .cmp => try self.builder.internValueData(.{
        //         //     .type = try self.builder.internType(.{
        //         //         .hash = Hasher.hash("builtin-cmp"),
        //         //         .data = .{
        //         //             .@"struct" = .{
        //         //                 .entity = std.math.maxInt(Entity.Key),
        //         //                 .fields = try self.builder.sema.lists.internSlice(
        //         //                     &.{

        //         //                         // .{ .name = "eq", .type = .{ .simple = .bool } },
        //         //                         // .{ .name = "ne", .type = .{ .simple = .bool } },
        //         //                         // .{ .name = "lt", .type = .{ .simple = .bool } },
        //         //                         // .{ .name = "le", .type = .{ .simple = .bool } },
        //         //                         // .{ .name = "gt", .type = .{ .simple = .bool } },
        //         //                         // .{ .name = "ge", .type = .{ .simple = .bool } },
        //         //                     },
        //         //                 ),
        //         //             },
        //         //         },
        //         //         .size = 0,
        //         //         .alignment = 0,
        //         //     }),
        //         // }),
        //     },
        //     .data = .{ .builtin_namespace = .{ .namespace = hir_inst.builtin_global_get.namespace } },
        // });
    }

    pub fn pushGlobalGetInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index, entity_key: Entity.Key) Error!Sema.Instruction.Index {
        const entity = self.builder.getEntity(entity_key);
        // try self.scope.pushDependency(entity_key);
        const global_type = try entity.resolveType();
        switch (entity.data) {
            .function_declaration => |fn_decl| {
                // global_entity.data.function_declaration.declaration_index,
                return self.pushInstruction(hir_inst_index, .{
                    .op = .global_get,
                    .typed_value = .{
                        .type = global_type,
                        .value = try self.maybeResolveDependency(entity_key),
                    },
                    .data = .{ .declaration = fn_decl.declaration_index },
                });
            },
            .global_type_declaration => |type_decl| {
                return self.pushInstruction(hir_inst_index, .{
                    .op = .global_get,
                    .typed_value = .{
                        .type = global_type,
                        .value = try self.maybeResolveDependency(entity_key),
                    },
                    .data = .{ .declaration = type_decl.declaration_index },
                });
            },
            else => std.debug.panic("unhandled global_entity: {s}", .{@tagName(entity.data)}),
        }
    }
    pub fn maybeResolveDependency(self: *Scope, entity_key: Entity.Key) Error!Sema.Value.Key {
        const entity = self.builder.getEntity(entity_key);

        const val = entity.resolveValue() catch |e| {
            switch (e) {
                error.CircularDependency => {
                    try self.entity.pushDependency(entity_key);
                    return Sema.Value.simple(.exec_time);
                },
                else => return e,
            }
        };
        return val;
    }
    pub fn handleConstantInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleConstantNumber", "Block.handleConstantNumber({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .hir_inst_index = hir_inst_index,
                .hir_inst = hir_inst,
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });

        switch (hir_inst) {
            .number_literal => |ast_node| {
                const slice = self.entity.getHir().ast.getNodeSlice(ast_node.node);
                const is_float = std.mem.indexOf(u8, slice, ".") != null;
                _ = is_float; // autofix
                // const value: Sema.Value.Data = blk: {
                var bytes: [8]u8 = undefined;
                // if (is_float) {
                const f = try std.fmt.parseFloat(f64, slice);

                std.mem.copyForwards(u8, &bytes, std.mem.asBytes(&f));
                // } else {
                //     const i = try std.fmt.parseInt(i64, slice, 10);
                //     std.mem.copyForwards(u8, &bytes, std.mem.asBytes(&i));
                // }
                // };

                const value_index = try self.builder.internValueData(.{ .bytes = bytes });
                return self.pushInstruction(hir_inst_index, .{
                    .op = .constant,
                    .typed_value = .{
                        .type = Sema.Type.simple(.float),
                        .value = value_index,
                    },
                    .data = .void,
                });
            },
            .boolean_literal => |ast_node| {
                const tag = self.entity.getHir().ast.getNodeSlice(ast_node.node);
                const is_true = std.mem.eql(u8, tag, "true");
                return self.pushInstruction(hir_inst_index, .{
                    .op = .constant,
                    .typed_value = .{
                        .type = Sema.Type.simple(.bool),
                        .value = try self.builder.numberAsBytesValueKey(is_true),
                    },
                    .data = .void,
                });
            },

            .string_literal => |ast_node| {
                var slice = self.entity.getHir().ast.getNodeSlice(ast_node.node);
                if (slice[0] == '"') {
                    slice = slice[1 .. slice.len - 1];
                }

                // const pointer = try self.builder.sema.memory.alloc(Sema.Type.simple(.bchar), @intCast(slice.len));

                return self.pushInstruction(hir_inst_index, .{
                    .op = .constant,
                    .typed_value = .{
                        .type = try self.builder.internTypeData(.{ .array = .{
                            .child = Sema.Type.simple(.bchar),
                            .len = @intCast(slice.len),
                        } }),
                        .value = try self.builder.internValueData(.{ .string_literal = try self.builder.internSlice(slice) }),
                    },
                    .data = .void,
                });
            },
            .char_literal => |ast_node| {
                var slice = self.entity.getHir().ast.getNodeSlice(ast_node.node);
                slice = slice[1 .. slice.len - 1];
                const char: u32 = std.unicode.utf8Decode(slice) catch {
                    std.debug.panic("invalid char literal: {s}", .{slice});
                };
                const char_float: f64 = @floatFromInt(char);

                return self.pushInstruction(hir_inst_index, .{
                    .op = .constant,
                    .typed_value = .{
                        .type = Sema.Type.simple(.float),
                        .value = try self.builder.numberAsBytesValueKey(char_float),
                    },
                    .data = .void,
                });
            },

            else => std.debug.panic("unhandled hir_inst: {s}", .{@tagName(hir_inst)}),
        }
    }
    pub fn handleComparisonInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleComparisonInstruction", "Block.handleComparisonInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });
        const op: Sema.Instruction.Op, const bin_op = switch (hir_inst) {
            .gt => .{ .gt, hir_inst.gt },
            .ge => .{ .ge, hir_inst.ge },
            .lt => .{ .lt, hir_inst.lt },
            .le => .{ .le, hir_inst.le },
            .eq => .{ .eq, hir_inst.eq },
            .ne => .{ .ne, hir_inst.ne },
            else => unreachable,
        };
        var lhs_index = self.getInstructionIndex(bin_op.lhs);
        var rhs_index = self.getInstructionIndex(bin_op.rhs);
        const lhs_inst = self.getInstruction(lhs_index);
        const rhs_inst = self.getInstruction(rhs_index);

        const lhs_is_float = lhs_inst.typed_value.type.isEqualSimple(.float);
        const rhs_is_float = rhs_inst.typed_value.type.isEqualSimple(.float);
        const lhs_is_int = lhs_inst.typed_value.type.isEqualSimple(.int);
        const rhs_is_int = rhs_inst.typed_value.type.isEqualSimple(.int);
        const lhs_is_number = lhs_is_float or lhs_is_int;
        const rhs_is_number = rhs_is_float or rhs_is_int;

        if (lhs_is_number and !rhs_is_number) {
            // lhs_index =
            return try self.pushMaybeFoldComparison(hir_inst_index, .{
                .op = op,
                .typed_value = .{
                    .type = Sema.Type.simple(.bool),
                    .value = Sema.Value.simple(.exec_time),
                },
                .data = .{ .bin_op = .{
                    .lhs = try self.pushCastInstruction(hir_inst_index, lhs_index, rhs_inst.typed_value.type),
                    .rhs = rhs_index,
                } },
            });
        } else if (rhs_is_number and !lhs_is_number) {
            return try self.pushMaybeFoldComparison(hir_inst_index, .{
                .op = op,
                .typed_value = .{
                    .type = Sema.Type.simple(.bool),
                    .value = Sema.Value.simple(.exec_time),
                },
                .data = .{ .bin_op = .{
                    .lhs = lhs_index,
                    .rhs = try self.pushCastInstruction(hir_inst_index, rhs_index, lhs_inst.typed_value.type),
                } },
            });
        }
        const lhs_is_signed = self.builder.isSigned(lhs_inst.typed_value.type);
        const rhs_is_signed = self.builder.isSigned(rhs_inst.typed_value.type);
        if (lhs_is_signed != rhs_is_signed) {
            std.debug.panic("error: comparison of signed and unsigned types", .{});
        }
        const lhs_bits = self.builder.numberBits(lhs_inst.typed_value.type);
        const rhs_bits = self.builder.numberBits(rhs_inst.typed_value.type);

        var ty = lhs_inst.typed_value.type;
        if (lhs_bits > rhs_bits) {
            ty = lhs_inst.typed_value.type;
            rhs_index = try self.pushCastInstruction(hir_inst_index, rhs_index, lhs_inst.typed_value.type);
        } else if (rhs_bits > lhs_bits) {
            ty = rhs_inst.typed_value.type;
            lhs_index = try self.pushCastInstruction(hir_inst_index, lhs_index, rhs_inst.typed_value.type);
        }

        return self.pushInstruction(hir_inst_index, .{
            .op = op,
            .typed_value = .{
                .type = Sema.Type.simple(.bool),
                .value = Sema.Value.simple(.exec_time),
            },
            .data = .{ .bin_op = .{
                .lhs = lhs_index,
                .rhs = rhs_index,
            } },
        });
    }

    pub fn maybeCoerceValue(self: *Scope, typed_value: Sema.TypedValue, type_index: Sema.Type.Key) Error!Sema.TypedValue {
        // const ty = switch (type_index) {
        //     .simple => |simple| simple,
        //     .complex => return typed_value,
        // };
        switch (typed_value.value) {
            .simple => |simple| switch (simple) {
                .exec_time => return typed_value,
                .runtime => return typed_value,
                else => {},
            },
            else => {},
        }
        return try self.builder.convertNumberType(typed_value, type_index);
    }
    pub fn pushCastInstruction(
        self: *Scope,
        hir_inst_index: Hir.Inst.Index,
        instruction_index: Sema.Instruction.Index,
        type_index: Sema.Type.Key,
    ) Error!Sema.Instruction.Index {
        const instruction = self.getInstruction(instruction_index);
        _ = instruction; // autofix
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "pushCastInstruction", "Scope.pushCastInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });

        // const value = try self.maybeCoerceValue(instruction.value, instruction.type);
        // const value = try self.builder.convertNumberType(instruction.getTypedValue(), type_index);

        // self.markDeadIfComptimeKnown(instruction_index);
        const value_inst = self.getInstruction(instruction_index);
        const typed_value = try self.maybeCoerceValue(value_inst.typed_value, type_index);

        std.debug.assert(!type_index.isEqualSimple(.type));
        if (self.isComptimeKnown(instruction_index)) {
            self.markDead(instruction_index);
            // return self.pushInstruction(hir_inst_index, .{
            //     .op = .constant,
            //     .type = type_index,
            //     .value = try self.maybeCoerceValue(value_inst.getTypedValue(), type_index),
            //     .data = .void,
            // });
        }
        return try self.pushInstruction(hir_inst_index, .{
            .op = .cast,
            .typed_value = .{
                .type = typed_value.type,
                .value = typed_value.value,
            },
            .data = .{ .operand = instruction_index },
        });
    }

    pub fn pushMaybeCastInstruction(
        self: *Scope,
        hir_inst_index: Hir.Inst.Index,
        instruction_index: Sema.Instruction.Index,
        type_inst_index: Sema.Instruction.Index,
    ) Error!Sema.Instruction.Index {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "pushMaybeCastInstruction", "Scope.pushMaybeCastInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });
        const type_inst = self.getInstruction(type_inst_index);
        const type_index = self.builder.unwrapTypeValue(type_inst.typed_value.value);
        if (try self.pushMaybeCastInstructionToType(hir_inst_index, instruction_index, type_index)) |index| {
            self.markDead(type_inst_index);
            return index;
        }
        return instruction_index;
    }
    pub fn pushMaybeCastInstructionToType(
        self: *Scope,
        hir_inst_index: Hir.Inst.Index,
        instruction_index: Sema.Instruction.Index,
        type_index: Sema.Type.Key,
    ) Error!?Sema.Instruction.Index {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "pushMaybeCastInstructionToType", "Scope.pushMaybeCastInstructionToType({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });
        const instruction = self.getInstruction(instruction_index);

        if (self.builder.getType(type_index)) |expected_type| {
            switch (expected_type.data) {
                .any => {
                    if (self.builder.getType(instruction.typed_value.type)) |inst_type| {
                        switch (inst_type.data) {
                            .any => return null,
                            else => {},
                        }
                    }
                    return try self.pushCastInstruction(hir_inst_index, instruction_index, try self.builder.internTypeData(.{ .any = .{
                        .concrete = instruction.typed_value.type,
                    } }));
                },
                .flat_union => {
                    const fields = self.builder.sema.lists.getSlice(expected_type.data.flat_union.fields);
                    var active_field_index: ?Sema.Type.Key = null;
                    for (fields) |field| {
                        const field_key = Sema.Type.Key.decode(field);
                        switch (self.builder.canCastImplicitly(instruction.typed_value.type, field_key) catch |err| {
                            std.debug.panic("{s}", .{@errorName(err)});
                        }) {
                            .unnecessary => {
                                active_field_index = field_key;
                                break;
                            },
                            .allowed => {
                                active_field_index = field_key;
                            },
                            .not_allowed => {},
                        }
                    }

                    if (active_field_index) |field_key| {
                        return try self.pushInstruction(hir_inst_index, .{
                            .op = .cast,
                            .typed_value = .{
                                .type = type_index,
                                .value = try self.builder.internValueData(.{ .flat_union = .{
                                    .active_field = .{ .resolved = .{
                                        .type = field_key,
                                        .value = instruction.typed_value.value,
                                    } },
                                } }),
                            },
                            .data = .{ .operand = instruction_index },
                        });
                    }

                    std.debug.panic("TODO: pushMaybeCastInstructionToType", .{});
                },
                else => {},
            }
        }
        const can_cast = self.builder.canCastImplicitly(instruction.typed_value.type, type_index) catch |err| {
            std.debug.panic("{s}", .{@errorName(err)});
        };
        switch (can_cast) {
            .allowed => return try self.pushCastInstruction(hir_inst_index, instruction_index, type_index),
            .unnecessary => return instruction_index,
            .not_allowed => return null,
        }
    }
    // const CastType = enum {
    //     allowed,
    //     unnecessary,
    //     not_allowed,
    // };

    // pub fn canCastImplicitlyInner(self: *Scope, from_type: Sema.Type.Key, to_type: Sema.Type.Key, complexity: usize) !CastType {
    //     if (complexity > 10) return error.TypeComplexityTooHigh;
    //     if (from_type.isEqual(to_type)) return .unnecessary;
    //     // if (Sema.Type.isOneOfSimple(from_type, .{ )
    //     if (from_type.isEqualSimple(.number) and to_type.isOneOfSimple(
    //         &.{
    //             .i8,
    //             .i16,
    //             .i32,
    //             .i64,
    //             .u8,
    //             .u16,
    //             .u32,
    //             .u64,
    //             .usize,
    //             .f32,
    //             .f64,
    //             .bchar,
    //         },
    //     )) return .allowed;
    //     const signed_types = &.{ .i8, .i16, .i32, .i64 };
    //     // const number_types = &.{ .number, .bchar };
    //     if (from_type.isOneOfSimple(signed_types) and to_type.isOneOfSimple(signed_types)) {
    //         const lhs_bits = self.builder.numberBits(from_type);
    //         const rhs_bits = self.builder.numberBits(to_type);
    //         return if (lhs_bits <= rhs_bits) .allowed else .not_allowed;
    //     }
    //     const unsigned_types = &.{ .u8, .u16, .u32, .u64, .usize };
    //     if (from_type.isOneOfSimple(unsigned_types) and to_type.isOneOfSimple(unsigned_types)) {
    //         const lhs_bits = self.builder.numberBits(from_type);
    //         const rhs_bits = self.builder.numberBits(to_type);
    //         return if (lhs_bits <= rhs_bits) .allowed else .not_allowed;
    //     }

    //     const float_types = &.{ .f32, .f64 };
    //     if (from_type.isOneOfSimple(float_types) and to_type.isOneOfSimple(float_types)) {
    //         const lhs_bits = self.builder.numberBits(from_type);
    //         const rhs_bits = self.builder.numberBits(to_type);
    //         return if (lhs_bits <= rhs_bits) .allowed else .not_allowed;
    //     }
    //     if (to_type.isEqualSimple(.type)) {
    //         if (self.builder.getType(from_type)) |from_type_inst| switch (from_type_inst.data) {
    //             .typeof => return .allowed,
    //             else => {},
    //         };
    //     }

    //     switch (to_type) {
    //         .simple => |target_simple| {
    //             _ = target_simple; // autofix
    //         },
    //         .complex => |target_complex| {
    //             const target_complex_type = self.builder.getComplexType(target_complex);
    //             switch (target_complex_type.data) {
    //                 .any => return .allowed,
    //                 .flat_union => {
    //                     const fields = self.builder.sema.lists.getSlice(target_complex_type.data.flat_union.fields);
    //                     // var can_cast: CastType = .not_allowed;
    //                     for (fields) |field| {
    //                         const field_key = Sema.Type.Key.decode(field);
    //                         switch (try self.canCastImplicitlyInner(from_type, field_key, complexity + 1)) {
    //                             .allowed, .unnecessary => return .allowed,
    //                             .not_allowed => {},
    //                         }
    //                     }
    //                     return .not_allowed;
    //                     // return can_cast;
    //                 },
    //                 else => {},
    //             }
    //         },
    //     }

    //     return .not_allowed;
    // }
    // pub fn canCastImplicitly(self: *Scope, from_type: Sema.Type.Key, to_type: Sema.Type.Key) !CastType {
    //     return try self.canCastImplicitlyInner(from_type, to_type, 0);
    // }
    pub fn getInstructionAsType(self: *Scope, hir_inst_index: Hir.Inst.Index, instruction_index: Sema.Instruction.Index, type_index: Sema.Type.Key) Error!Sema.Instruction.Index {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "getInstructionAsType", "Scope.getInstructionAsType", .{} },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });

        return (try self.pushMaybeCastInstructionToType(
            hir_inst_index,
            instruction_index,
            type_index,
        )) orelse instruction_index;
    }
    pub fn getInstructionAsTypeByHirInst(self: *Scope, hir_inst_index: Hir.Inst.Index, type_index: Sema.Type.Key) Error!Sema.Instruction.Index {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "getInstructionAsTypeByHirInst", "Scope.getInstructionAsTypeByHirInst({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });
        const instruction_index = self.getInstructionIndex(hir_inst_index);

        return (try self.pushMaybeCastInstructionToType(
            hir_inst_index,
            instruction_index,
            type_index,
        )) orelse instruction_index;
    }
    pub fn isComptimeKnown(self: *Scope, index: Sema.Instruction.Index) bool {
        return self.getInstruction(index).typed_value.isComptimeKnown();
    }
    pub fn pushMaybeFoldComparison(self: *Scope, hir_inst_index: Hir.Inst.Index, instruction: Sema.Instruction) Error!Sema.Instruction.Index {
        if (!self.isComptimeKnown(instruction.data.bin_op.lhs) or !self.isComptimeKnown(instruction.data.bin_op.rhs)) {
            return try self.pushInstruction(hir_inst_index, instruction);
        }

        self.markDead(instruction.data.bin_op.lhs);
        self.markDead(instruction.data.bin_op.rhs);

        const lhs_inst = self.getInstruction(instruction.data.bin_op.lhs);
        const rhs_inst = self.getInstruction(instruction.data.bin_op.rhs);

        std.debug.assert(lhs_inst.typed_value.type.isEqual(rhs_inst.typed_value.type));
        const resolved = try self.builder.doComparison(
            instruction.op,
            // .{ .type = lhs_inst.typed_value.type, .value = lhs_inst.typed_value.value },
            lhs_inst.typed_value,
            rhs_inst.typed_value,
        );

        return try self.pushInstruction(hir_inst_index, .{
            .op = .constant,
            // .type = resolved.type,
            // .value = resolved.value,
            .typed_value = resolved,
            .data = .void,
        });
    }
    pub fn handleTypeLiteralInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleTypeLiteralInstruction", "Scope.handleTypeLiteralInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .hir_inst_index = hir_inst_index,
                .hir_inst = hir_inst,
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });

        const type_value_index = switch (std.meta.activeTag(hir_inst)) {
            .ty_i8 => Sema.Type.simple(.i8),
            .ty_i16 => Sema.Type.simple(.i16),
            .ty_i32 => Sema.Type.simple(.i32),
            .ty_i64 => Sema.Type.simple(.i64),
            // .ty_i128 => Sema.Type.simple(.i128),
            // .ty_i256 => Sema.Type.simple(.i256),
            .ty_u8 => Sema.Type.simple(.u8),
            .ty_u16 => Sema.Type.simple(.u16),
            .ty_u32 => Sema.Type.simple(.u32),
            .ty_u64 => Sema.Type.simple(.u64),
            // .ty_u128 => Sema.Type.simple(.u128),
            // .ty_u256 => Sema.Type.simple(.u256),
            .ty_usize => Sema.Type.simple(.usize),
            .ty_f64 => Sema.Type.simple(.f64),
            .ty_f32 => Sema.Type.simple(.f32),
            .ty_boolean => Sema.Type.simple(.bool),

            // inline .ty_i8,
            // .ty_i16,
            // .ty_i32,
            // .ty_i64,
            // .ty_i128,
            // .ty_i256,
            // .ty_u8,
            // .ty_u16,
            // .ty_u32,
            // .ty_u64,
            // .ty_u128,
            // .ty_u256,
            // .ty_usize,
            // .ty_f64,
            // .ty_f32,
            // .ty_boolean,
            // => |tag| Sema.Type.simple(std.meta.stringToEnum(Sema.Type.Simple, (comptime @tagName(tag)[3..])) orelse {
            //     return std.debug.panic("not implemented: resolveTypeLiteralInstruction '{s}'", .{@tagName(tag)});
            // }),
            .ty_array => ty: {
                const ty_array = hir_inst.ty_array;
                const type_inst_id = self.getInstructionIndex(ty_array.type);
                const type_inst = self.getInstruction(type_inst_id);
                const size_inst_id = try self.getInstructionAsTypeByHirInst(ty_array.size, Sema.Type.simple(.usize));
                const size_inst = self.getInstruction(size_inst_id);
                // const size = size_inst.value;
                // const size_value = self.builder.getValue(size) orelse std.debug.panic("Error: size_value is not a number", .{});
                const size_int = try self.builder.readNumberAsType(usize, size_inst.typed_value);
                const type_value_index = type_inst.typed_value.value;
                self.markDead(type_inst_id);
                self.markDead(size_inst_id);

                const type_index = try self.builder.internTypeData(.{ .array = .{
                    .child = self.builder.unwrapTypeValue(type_value_index),
                    .len = size_int,
                } });

                break :ty type_index;
            },
            .ty_pointer => ty: {
                const ty_pointer = hir_inst.ty_pointer;
                const type_inst = self.getInstructionByHirIndex(ty_pointer.operand);
                // const type_inst = self.getInstruction(type_inst_id);
                // const type_value_index = type_inst.value;
                const type_index = try self.builder.internTypeData(.{ .pointer = .{
                    .child = self.builder.unwrapTypeValue(type_inst.typed_value.value),
                } });
                break :ty type_index;
            },
            else => std.debug.panic("unhandled type_literal: {s}", .{@tagName(hir_inst)}),
        };

        const value = try self.builder.internValueData(.{ .type = type_value_index });
        // std.debug.print("value: {} type {}\n", .{ value, type_value_index });
        return self.pushInstruction(
            hir_inst_index,
            .{
                .op = .type,
                .typed_value = .{
                    .type = try self.builder.internTypeData(.{ .typeof = .{ .child = type_value_index } }),
                    .value = value,
                },
                .data = .void,
            },
        );
    }
    pub fn handleConstantIntInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const value = hir_inst.constant_int.value;
        var bytes: [8]u8 = undefined;
        std.mem.copyForwards(u8, &bytes, std.mem.asBytes(&value));
        return self.pushInstruction(hir_inst_index, .{
            .op = .constant,
            .typed_value = .{
                .type = Sema.Type.simple(.number),
                .value = try self.builder.internValueData(.{ .bytes = bytes }),
            },
            .data = .void,
        });
    }
    pub fn handleAllocInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleAllocInstruction", "Scope.handleAllocInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });

        const type_inst_index = self.getInstructionIndex(hir_inst.alloc.type);
        const type_inst = self.getInstruction(type_inst_index);
        self.markDead(type_inst_index);

        const type_to_alloc = self.builder.unwrapTypeValue(type_inst.typed_value.value);

        switch (type_to_alloc) {
            .complex => |complex| switch (self.builder.getComplexType(complex).data) {
                .array => |array| {
                    _ = array; // autofix
                    // const pointer = try self.builder.sema.memory.alloc(array.child, array.len);
                    // const value = try self.builder.numberAsBytesValueKey(pointer);

                    const ty = try self.builder.internTypeData(.{
                        .pointer = .{
                            .child = type_to_alloc,
                        },
                    });
                    return try self.pushInstruction(hir_inst_index, .{
                        .op = .alloc,
                        .typed_value = .{
                            .type = ty,
                            .value = Sema.Value.simple(.exec_time),
                        },
                        .data = .{ .alloc = .{
                            .type = type_to_alloc,
                            .mutable = hir_inst.alloc.mutable,
                        } },
                    });
                },
                // .slice => |slice| {
                //     _ = slice; // autofix

                //     return try self.pushInstruction(hir_inst_index, .{
                //         .op = .alloc,
                //         .type = type_to_alloc,
                //         .value = try self.builder.internValueData(.{ .slice = .{
                //             .ptr = Sema.Value.simple(.exec_time),
                //             .len = Sema.Value.simple(.exec_time),
                //         } }),
                //         .data = .{ .alloc = .{
                //             .type = type_to_alloc,
                //             .mutable = hir_inst.alloc.mutable,
                //         } },
                //     });
                // },
                // .string_literal => |string_literal| {
                //     const slice = self.builder.sema.strings.getSlice(string_literal);
                //     const pointer = try self.builder.sema.memory.alloc(slice, slice.len);
                //     const value = try self.builder.internValueData(.{ .integer = @intCast(pointer) });

                //     const ty = try self.builder.internTypeData(.{
                //         .pointer = .{
                //             .child = type_to_alloc,
                //         },
                //     });
                //     return try self.pushInstruction(hir_inst_index, .{
                //         .op = .alloc,
                //         .type = ty,
                //         .value = value,
                //         .data = .{ .alloc = .{
                //             .type = type_to_alloc,
                //             .mutable = hir_inst.alloc.mutable,
                //         } },
                //     });
                // },
                else => {},
            },
            .simple => |simple| switch (simple) {
                // .string_literal =>  {
                //     // const value = self.builder.getComplexValue(in);
                //     // const slice = self.builder.sema.strings.getSlice(string_literal);
                //     _ = slice; // autofix
                //     // const pointer = try self.builder.sema.memory.alloc(slice, slice.len);
                //     // const value = try self.builder.internValueData(.{ .integer = @intCast(pointer) });
                //     std.debug.panic("not implemented: handleAllocInstruction string_literal", .{});
                //     return try self.pushInstruction(hir_inst_index, .{
                //         .op = .alloc,
                //         .type = type_to_alloc,
                //         .value = value,
                //         .data = .{ .alloc = .{
                //             .type = try se,
                //             .mutable = hir_inst.alloc.mutable,
                //         } },
                //     });
                // },
                else => {},
            },
        }

        return self.pushInstruction(hir_inst_index, .{
            .op = .alloc,
            .typed_value = .{
                .type = try self.builder.internTypeData(.{
                    .pointer = .{
                        .child = type_to_alloc,
                    },
                }),
                .value = Sema.Value.simple(.exec_time),
            },
            .data = .{ .alloc = .{
                .type = type_to_alloc,
                .mutable = hir_inst.alloc.mutable,
            } },
        });
    }
    pub fn handleArithmeticInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleArithmeticInstruction", "Scope.handleArithmeticInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });
        const op: Sema.Instruction.Op, const bin_op = switch (hir_inst) {
            .add => .{ .add, hir_inst.add },
            .sub => .{ .sub, hir_inst.sub },
            .mul => .{ .mul, hir_inst.mul },
            .div => .{ .div, hir_inst.div },
            else => unreachable,
        };

        var lhs_index = self.getInstructionIndex(bin_op.lhs);
        var rhs_index = self.getInstructionIndex(bin_op.rhs);
        const lhs_inst = self.getInstruction(lhs_index);
        const rhs_inst = self.getInstruction(rhs_index);

        const lhs_is_number = lhs_inst.typed_value.type.isEqualSimple(.number);
        const rhs_is_number = rhs_inst.typed_value.type.isEqualSimple(.number);

        if (lhs_is_number and !rhs_is_number) {
            // lhs_index = try self.pushCastInstruction(hir_inst_index, lhs_index, rhs_inst.type);
            // ty = rhs_inst.type;
            return self.pushMaybeFoldArithmetic(hir_inst_index, .{
                .op = op,
                .typed_value = .{
                    .type = rhs_inst.typed_value.type,
                    .value = Sema.Value.simple(.exec_time),
                },
                .data = .{ .bin_op = .{
                    .lhs = try self.pushCastInstruction(
                        hir_inst_index,
                        lhs_index,
                        rhs_inst.typed_value.type,
                    ),
                    .rhs = rhs_index,
                } },
            });
        } else if (rhs_is_number and !lhs_is_number) {
            return self.pushMaybeFoldArithmetic(hir_inst_index, .{
                .op = op,
                .typed_value = .{
                    .type = lhs_inst.typed_value.type,
                    .value = Sema.Value.simple(.exec_time),
                },
                .data = .{ .bin_op = .{
                    .lhs = lhs_index,
                    .rhs = try self.pushCastInstruction(
                        hir_inst_index,
                        rhs_index,
                        lhs_inst.typed_value.type,
                    ),
                } },
            });
        }

        const lhs_is_signed = self.builder.isSigned(lhs_inst.typed_value.type);
        const rhs_is_signed = self.builder.isSigned(rhs_inst.typed_value.type);

        if (lhs_is_signed != rhs_is_signed) {
            std.debug.panic("error: arithmetic operands have different signedness", .{});
        }
        const lhs_bits = self.builder.numberBits(lhs_inst.typed_value.type);
        const rhs_bits = self.builder.numberBits(rhs_inst.typed_value.type);

        var ty = lhs_inst.typed_value.type;
        if (lhs_bits > rhs_bits) {
            ty = lhs_inst.typed_value.type;
            rhs_index = try self.pushCastInstruction(hir_inst_index, rhs_index, lhs_inst.typed_value.type);
        } else if (rhs_bits > lhs_bits) {
            ty = rhs_inst.typed_value.type;
            lhs_index = try self.pushCastInstruction(hir_inst_index, lhs_index, rhs_inst.typed_value.type);
        }

        return self.pushInstruction(hir_inst_index, .{
            .op = op,
            .typed_value = .{
                .type = ty,
                .value = Sema.Value.simple(.exec_time),
            },
            // .value = try self.builder.maybeFoldArithmetic(op, lhs_index, rhs_index),
            // .value = Sema.Value.simple(.exec_time),
            .data = .{ .bin_op = .{
                .lhs = lhs_index,
                .rhs = rhs_index,
            } },
        });
    }
    pub fn pushMaybeFoldArithmetic(self: *Scope, hir_inst_index: Hir.Inst.Index, data: Sema.Instruction) Error!Sema.Instruction.Index {
        if (!self.isComptimeKnown(data.data.bin_op.lhs) or !self.isComptimeKnown(data.data.bin_op.rhs)) {
            return try self.pushInstruction(hir_inst_index, data);
        }

        const lhs_inst_index = data.data.bin_op.lhs;
        const rhs_inst_index = data.data.bin_op.rhs;
        const lhs_inst = self.getInstruction(lhs_inst_index);
        const rhs_inst = self.getInstruction(rhs_inst_index);

        self.markDead(lhs_inst_index);
        self.markDead(rhs_inst_index);
        var result: Sema.TypedValue = undefined;
        const op = data.op;
        switch (data.typed_value.type.simple) {
            inline .float, .int, .number, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .usize => |type_| {
                result = try self.builder.doArithmetic(
                    type_.getNativeType(),
                    op,
                    // .{ .type = data.typed_value.type, .value = lhs_inst.typed_value.value },
                    lhs_inst.typed_value,
                    // .{ .type = data.typed_value.type, .value = rhs_inst.typed_value.value },
                    rhs_inst.typed_value,
                );
            },
            else => unreachable,
        }

        // const result = try self.builder.doArithmetic(
        //     data.op,

        //     data.type.getNativeType(),
        //     .{ .type = data.type, .value = lhs_inst.value },
        //     .{ .type = data.type, .value = rhs_inst.value },
        // );
        return try self.pushInstruction(hir_inst_index, .{
            .op = .constant,
            .typed_value = .{
                .type = data.typed_value.type,
                .value = result.value,
            },
            .data = .void,
        });
    }

    pub fn handleStoreInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleStoreInstruction", "Scope.handleStoreInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });

        const pointer_inst_index = self.getInstructionIndex(hir_inst.store.pointer);
        const pointer_inst = self.getInstruction(pointer_inst_index);
        const pointer_type = self.builder.unwrapPointerType(pointer_inst.typed_value.type) orelse std.debug.panic("unwrapped_pointer_inst is not a type {d}", .{pointer_inst_index});
        std.debug.print("pointer_type: {}\n", .{self.builder.getFormattableType(pointer_type)});

        switch (pointer_type) {
            .complex => |complex| switch (self.builder.getComplexType(complex).data) {
                .@"struct" => {
                    return try self.handleStoreStruct(hir_inst_index);
                },
                .array => {
                    return try self.handleStoreArray(hir_inst_index);
                },
                else => |data| {
                    std.debug.panic("unhandled store value: {s}", .{@tagName(data)});
                },
            },
            .simple => |simple| switch (simple) {
                .i8,
                .i16,
                .i32,
                .i64,

                .u8,
                .u16,
                .u32,
                .u64,
                .usize,

                .f32,
                .f64,
                .number,
                .bchar,
                .bool,
                .boolean,
                => {
                    const value_inst_index = try self.getInstructionAsTypeByHirInst(hir_inst.store.value, pointer_type);
                    return try self.pushInstruction(hir_inst_index, .{
                        .op = .store,
                        .typed_value = Sema.TypedValue.VOID,
                        .data = .{ .operand_payload = .{
                            .operand = pointer_inst_index,
                            .payload = value_inst_index,
                        } },
                    });
                },
                // .slice => |slice| {
                //     _ = slice;
                // },
                else => |data| {
                    std.debug.panic("unhandled store value: {s}", .{@tagName(data)});
                },
            },
        }
        // if (self.builder.getType(value_inst.type)) |type_| {
        // })
        // if (self.builder.getValue(value_inst.value)) |value| switch (value.data) {
        //     .array_init => |array_init| {
        //         self.markDead(value_inst_index);
        //         const list = self.builder.sema.lists.getSlice(array_init.items_list);
        //         const slice_type = self.builder.getType(pointer_inst.type) orelse std.debug.panic("type_to_store is not a type", .{});
        //         const element_type = slice_type.data.slice.child;

        //         const element_size = self.builder.getTypeSize(element_type);
        //         for (list, 0..) |item, i| {
        //             self.markDead(item);
        //             const item_inst_index = try self.pushMaybeCastInstructionToType(
        //                 hir_inst_index,
        //                 item,
        //                 element_type,
        //             ) orelse item;

        //             const offset: u32 = @intCast(i * element_size);
        //             const index_inst = try self.pushInstruction(hir_inst_index, .{
        //                 .op = .constant,
        //                 .type = Sema.Type.simple(.usize),
        //                 .value = try self.builder.internValueData(.{ .integer = @intCast(i) }),
        //                 .data = .void,
        //             });

        //             const get_element_pointer_inst = try self.pushInstruction(hir_inst_index, .{
        //                 .op = .get_element_pointer,
        //                 .type = try self.builder.internTypeData(.{ .pointer = .{ .child = element_type } }),
        //                 .value = if (ptr) |ptr_| try self.builder.internValueData(.{ .integer = ptr_ + offset }) else Sema.Value.simple(.exec_time),
        //                 .data = .{ .get_element_pointer = .{
        //                     .base = pointer_inst_index,
        //                     .index = index_inst,
        //                 } },
        //             });

        //             _ = try self.pushInstruction(hir_inst_index, .{
        //                 .op = .store,
        //                 .type = Sema.Type.simple(.void),
        //                 .value = Sema.Value.simple(.void),
        //                 .data = .{ .operand_payload = .{
        //                     .operand = get_element_pointer_inst,
        //                     .payload = item_inst_index,
        //                 } },
        //             });
        //             if (ptr) |ptr_| {
        //                 const item_value_inst = self.getInstruction(item_inst_index);
        //                 try self.builder.sema.memory.store(element_type, ptr_ + i * element_size, item_value_inst.value);
        //             }
        //         }
        //         return pointer_inst_index;
        //     },
        //     .type_init => |type_init| {
        //         self.markDead(value_inst_index);

        //         const list = self.builder.sema.lists.getSlice(type_init.field_init_list);
        //         const type_to_store = self.builder.unwrapPointerType(pointer_inst.type) orelse std.debug.panic("type_to_store is not a type", .{});
        //         const type_to_store_type = self.builder.getType(type_to_store) orelse std.debug.panic("type_to_store_type is not a type", .{});
        //         const module = self.builder.getEntity(type_to_store_type.data.@"struct".entity);
        //         const struct_fields = self.builder.sema.lists.getSlice(type_to_store_type.data.@"struct".fields);

        //         for (list, 0..) |field_inst_index, i| {
        //             self.markDead(field_inst_index);

        //             _ = i; // autofix
        //             const field_inst = self.getInstruction(field_inst_index);
        //             const field_inst_value = self.builder.getValue(field_inst.value) orelse std.debug.panic("field_inst_value is not a number", .{});
        //             const property_name_range = field_inst_value.data.field_init.field_name;
        //             const property = module.data.module_declaration.fields.get(property_name_range) orelse unreachable;
        //             const field_entity = self.builder.getEntity(property.entity);
        //             const field_type = try field_entity.resolveType();
        //             const field_value_inst_index = try self.pushMaybeCastInstructionToType(
        //                 hir_inst_index,
        //                 field_inst_value.data.field_init.value_inst,
        //                 field_type,
        //             ) orelse field_inst_index;

        //             const field_offset = self.builder.getType(Sema.Type.Key.decode(struct_fields[property.index])).?.data.struct_field.offset;

        //             if (ptr) |ptr_| {
        //                 const field_value_inst = self.getInstruction(field_value_inst_index);
        //                 try self.builder.sema.memory.store(field_type, ptr_ + field_offset, field_value_inst.value);
        //             }
        //             const element_pointer_inst = try self.pushInstruction(hir_inst_index, .{
        //                 .op = .get_element_pointer,
        //                 .type = try self.builder.internTypeData(.{ .pointer = .{ .child = field_type } }),
        //                 .value = if (ptr) |ptr_| try self.builder.internValueData(.{ .integer = ptr_ + field_offset }) else Sema.Value.simple(.exec_time),
        //                 .data = .{ .get_element_pointer = .{
        //                     .base = pointer_inst_index,
        //                     .index = property.index,
        //                 } },
        //             });
        //             _ = try self.pushInstruction(hir_inst_index, .{
        //                 .op = .store,
        //                 .type = Sema.Type.simple(.void),
        //                 .value = Sema.Value.simple(.void),
        //                 .data = .{ .operand_payload = .{
        //                     .operand = element_pointer_inst,
        //                     .payload = field_value_inst_index,
        //                 } },
        //             });
        //         }
        //         return pointer_inst_index;
        //     },
        //     .slice => {
        //         // if (ptr) |ptr_| {
        //         // self.markDead(value_inst_index);
        //         // const type_to_store = self.builder.getComplexType(type_key: anytype)
        //         // const ptr_ = try self.builder.sema.memory.dupe(value_inst.getTypedValue());

        //         // liveness = 0;
        //         return try self.pushInstruction(hir_inst_index, .{
        //             .op = .memcpy,
        //             .type = Sema.Type.simple(.void),
        //             .value = Sema.Value.simple(.void),
        //             .data = .{ .operand_payload = .{
        //                 .operand = pointer_inst_index,
        //                 .payload = value_inst_index,
        //             } },
        //         });
        //         // }
        //     },
        //     .integer => {
        //         // const value_int = self.builder.getNumberValueKeyAs(i64, value.data.integer);
        //         const type_to_store = self.builder.unwrapPointerType(pointer_inst.type) orelse std.debug.panic("type_to_store is not a type", .{});
        //         if (ptr) |ptr_| {
        //             self.markDead(value_inst_index);
        //             try self.builder.sema.memory.store(type_to_store, ptr_, value_inst.value);
        //             liveness = 0;
        //         }
        //     },
        //     .float => {
        //         if (ptr) |ptr_| {
        //             const type_to_store = self.builder.unwrapPointerType(pointer_inst.type) orelse std.debug.panic("type_to_store is not a type", .{});
        //             self.markDead(value_inst_index);
        //             try self.builder.sema.memory.store(type_to_store, ptr_, value_inst.value);
        //             liveness = 0;
        //         }
        //     },

        //     // .comptime_pointer => {
        //     //     if (ptr) |ptr_| {
        //     //         self.markDead(value_inst_index);
        //     //         try self.builder.sema.memory.storeAt([], ptr_, value_inst.value);
        //     //         liveness = 0;
        //     //     }
        //     // },
        //     else => {
        //         std.debug.panic("unhandled store value: {s}", .{@tagName(value.data)});
        //     },
        // };

        // return self.pushInstruction(hir_inst_index, .{
        //     .op = .store,
        //     .type = Sema.Type.simple(.void),
        //     .value = Sema.Value.simple(.void),
        //     .data = .{ .operand_payload = .{
        //         .operand = pointer_inst_index,
        //         .payload = value_inst_index,
        //     } },
        //     .liveness = liveness,
        // });
    }
    pub fn handleStoreStruct(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const pointer_inst_index = self.getInstructionIndex(hir_inst.store.pointer);

        const value_inst_index = self.getInstructionIndex(hir_inst.store.value);
        const value_inst = self.getInstruction(value_inst_index);

        switch (value_inst.op) {
            .type_init => {
                self.markDead(value_inst_index);
                const type_init = self.builder.getComplexValue(value_inst.typed_value.value).data.type_init;
                const list = self.builder.sema.lists.getSlice(type_init.field_init_list);
                for (list) |field_inst_index| {
                    _ = try self.pushSetInstructionField(hir_inst_index, pointer_inst_index, field_inst_index);
                }

                return pointer_inst_index;
            },
            .load => {
                // const load_inst = self.getInstruction(value_inst_index);
                // const load_inst_value = self.builder.getValue(load_inst.value) orelse std.debug.panic("load_inst_value is not a number", .{});
                // const load_inst_value_int = self.builder.getNumberValueKeyAs(i64, load_inst_value.value);
                return try self.pushInstruction(hir_inst_index, .{
                    .op = .store,
                    .typed_value = Sema.TypedValue.VOID,
                    .data = .{ .operand_payload = .{
                        .operand = pointer_inst_index,
                        .payload = value_inst_index,
                    } },
                });
            },
            else => {
                std.debug.panic("unhandled store value: {s}", .{@tagName(value_inst.op)});
            },
        }
    }
    pub fn handleStoreArray(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const pointer_inst_index = self.getInstructionIndex(hir_inst.store.pointer);
        const pointer_inst = self.getInstruction(pointer_inst_index);

        const value_inst_index = self.getInstructionIndex(hir_inst.store.value);
        const value_inst = self.getInstruction(value_inst_index);

        std.debug.print("storing array\n", .{});
        switch (value_inst.op) {
            .array_init => {
                self.markDead(value_inst_index);
                const array_init = self.builder.getComplexValue(value_inst.typed_value.value).data.array_init;
                const array_type = self.builder.unwrapPointerType(pointer_inst.typed_value.type) orelse std.debug.panic("array_type is not a type", .{});
                const element_type = self.builder.getComplexType(array_type).data.array.child;
                const element_size = self.builder.getTypeSize(element_type);

                const list = self.builder.sema.lists.getSlice(array_init.items_list);
                for (list, 0..) |item_inst_index, i| {
                    const offset: usize = @intCast(i * element_size);
                    _ = offset; // autofix
                    const index_inst = try self.pushInstruction(hir_inst_index, .{
                        .op = .constant,
                        .typed_value = .{
                            .type = Sema.Type.simple(.usize),
                            .value = try self.builder.numberAsBytesValueKey(i),
                        },
                        .data = .void,
                    });

                    const get_element_pointer_inst = blk: {
                        // const ty = try self.builder.internTypeData(.{ .pointer = .{ .child = element_type } });
                        const data: Sema.Instruction.Data = .{ .get_element_pointer = .{
                            .base = pointer_inst_index,
                            .index = index_inst,
                        } };

                        break :blk try self.pushInstruction(hir_inst_index, .{
                            .op = .get_element_pointer,
                            .typed_value = .{
                                .type = try self.builder.internTypeData(.{ .pointer = .{ .child = element_type } }),
                                .value = Sema.Value.simple(.exec_time),
                            },
                            .data = data,
                        });
                    };
                    const item_inst = try self.pushMaybeCastInstructionToType(hir_inst_index, item_inst_index, element_type) orelse item_inst_index;

                    _ = try self.pushInstruction(hir_inst_index, .{
                        .op = .store,
                        .typed_value = Sema.TypedValue.VOID,
                        .data = .{ .operand_payload = .{
                            .operand = get_element_pointer_inst,
                            .payload = item_inst,
                        } },
                    });
                }

                return pointer_inst_index;
            },

            .constant => {
                // self.markDeadIfComptimeKnown(value_inst_index);
                const type_to_store = self.builder.unwrapPointerType(pointer_inst.typed_value.type) orelse std.debug.panic("type_to_store is not a type", .{});

                const value_to_store_inst_index = try self.pushMaybeCastInstructionToType(
                    hir_inst.store.value,
                    value_inst_index,
                    type_to_store,
                ) orelse value_inst_index;
                const value_to_store_inst = self.getInstruction(value_to_store_inst_index);
                _ = value_to_store_inst; // autofix
                // const value_inst_index = self.getInstructionIndex(hir_inst.store.value);
                // const value_inst = self.getInstruction(value_inst_index);
                // std.debug.print("value_to_store_inst: {} {}\n", .{ value_inst, self.builder.getFormattableType(type_to_store) });
                // const value_inst = self.getInstruction(value_inst_index);
                // const constant_inst_value = self.builder.getValue(constant_inst.value) orelse std.debug.panic("constant_inst_value is not a number", .{});
                // const constant_inst_value_int = self.builder.getNumberValueKeyAs(i64, constant_inst_value.value);
                // try self.builder.sema.memory.store(element_type, element_ptr, constant_inst_value_int);
                // TODO: store values in mem
                // if ()

                // switch (value_inst.value) {
                //     .complex => |complex| switch (self.builder.getComplexValue(complex).data) {
                //         .string_literal => |range| {
                //             const slice = self.builder.getSlice(range);
                //             _ = slice; // autofix
                //             std.debug.print("storing string_literal\n", .{});
                //             const ptr = try self.builder.sema.memory.store(type_to_store);
                //             _ = ptr; // autofix
                //         },
                //         else => {},
                //     },
                //     else => {},
                // }
                // if (try self.builder.maybeGetPointer(pointer_inst.getTypedValue())) |ptr| {
                //     try self.builder.sema.memory.store(ptr, value_to_store_inst.getTypedValue());
                // }
                // self.builder.sema.memory.store(type_to_store, pointer_inst.getTypedValue(), value_to_store_inst_index);
                return try self.pushInstruction(hir_inst_index, .{
                    .op = .store,
                    .typed_value = Sema.TypedValue.VOID,
                    .data = .{ .operand_payload = .{
                        .operand = pointer_inst_index,
                        .payload = value_to_store_inst_index,
                    } },
                });
            },
            else => {
                std.debug.panic("unhandled store value: {s}", .{@tagName(value_inst.op)});
            },
        }
    }

    pub fn pushSetInstructionField(
        self: *Scope,
        hir_inst_index: Hir.Inst.Index,
        struct_inst_index: Sema.Instruction.Index,
        field_inst_index: Sema.Instruction.Index,
    ) Error!Sema.Instruction.Index {
        const struct_inst = self.getInstruction(struct_inst_index);

        const struct_unwrapped_pointer_type_key = self.builder.unwrapPointerType(struct_inst.typed_value.type) orelse std.debug.panic("struct_unwrapped_pointer_type is not a type", .{});
        const struct_type = self.builder.getType(struct_unwrapped_pointer_type_key) orelse std.debug.panic("type_to_store is not a type", .{});

        const module = self.builder.getEntity(struct_type.data.@"struct".entity);
        const field_init_inst = self.getInstruction(field_inst_index);
        const field_init_inst_value = self.builder.getValue(field_init_inst.typed_value.value) orelse std.debug.panic("field_init_inst_value is not a number", .{});
        const property_name_range = field_init_inst_value.data.field_init.field_name;
        self.markDead(field_inst_index);

        const property = module.data.module_declaration.fields.get(property_name_range) orelse unreachable;

        const field_entity = self.builder.getEntity(property.entity);
        const field_type = try field_entity.resolveType();
        const field_value_inst_index = try self.pushMaybeCastInstructionToType(
            hir_inst_index,
            field_init_inst_value.data.field_init.value_inst,
            field_type,
        ) orelse field_inst_index;

        const index_inst_index = try self.pushInstruction(hir_inst_index, .{
            .op = .constant,
            .typed_value = .{
                .type = Sema.Type.simple(.usize),
                .value = try self.builder.numberAsBytesValueKey(property.index),
            },
            .data = .void,
        });
        const get_element_pointer_inst_index = try self.pushInstruction(hir_inst_index, .{
            .op = .get_element_pointer,
            .typed_value = .{
                .type = try self.builder.internTypeData(.{ .pointer = .{ .child = field_type } }),
                .value = Sema.Value.simple(.exec_time),
            },
            .data = .{ .get_element_pointer = .{
                .base = struct_inst_index,
                .index = index_inst_index,
            } },
        });

        return try self.pushInstruction(hir_inst_index, .{
            .op = .store,
            .typed_value = .{
                .type = Sema.Type.simple(.void),
                .value = Sema.Value.simple(.void),
            },
            .data = .{ .operand_payload = .{
                .operand = get_element_pointer_inst_index,
                .payload = field_value_inst_index,
            } },
        });
    }
    pub fn handleParamGetInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleParamGetInstruction", "Scope.handleParamGetInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });
        const operand_inst_index = self.getInstructionIndex(hir_inst.param_get.operand);
        const operand_inst = self.getInstruction(operand_inst_index);
        // const param_type = self.builder.getType(operand_inst.type) orelse {
        // //     std.debug.panic("unreachable: should get a param type", .{});
        // // };

        return self.pushInstruction(hir_inst_index, .{
            .op = .param_get,
            .typed_value = operand_inst.typed_value,
            .data = .{ .operand = operand_inst_index },
        });
    }
    pub fn handleIfExprInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleIfExprInstruction", "Scope.handleIfExprInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });

        const condition_index = self.getInstructionIndex(hir_inst.if_expr.cond);
        const condition_inst = self.getInstruction(condition_index);

        if (self.isComptimeKnown(condition_index)) {
            const condition_result = self.builder.getBooleanValueKeyAsBool(condition_inst.typed_value.value);
            if (condition_result) {
                return try self.resolveInstruction(hir_inst.if_expr.then_body);
            } else {
                if (hir_inst.if_expr.else_body) |else_body| {
                    return try self.resolveInstruction(else_body);
                }
            }
        }

        const index = try self.reserveInstruction(hir_inst_index);
        const current_block_instructions = self.active_block_instructions;

        self.active_block_instructions = null;

        const then_block_index = try self.resolveInstruction(hir_inst.if_expr.then_body);
        const else_block_index = if (hir_inst.if_expr.else_body) |else_body| try self.resolveInstruction(else_body) else null;

        self.setInstruction(index, .{
            .op = .@"if",
            .typed_value = Sema.TypedValue.VOID,
            .data = .{ .@"if" = .{
                .condition = condition_index,
                .then_block = then_block_index,
                .else_block = else_block_index,
                .finally_block = null,
            } },
            // .liveness = if (condition_result == null) 1 else 0,
        });
        self.active_block_instructions = current_block_instructions;
        return index;
    }

    pub fn handleSelectExprInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleSelectExprInstruction", "Scope.handleSelectExprInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });

        const condition = self.getInstructionIndex(hir_inst.select_expr.cond);
        const then_block_index = try self.resolveInstruction(hir_inst.select_expr.then_body);
        const else_block_index = try self.resolveInstruction(hir_inst.select_expr.else_body.?);
        const then_block = self.getInstruction(then_block_index);

        return self.pushInstruction(hir_inst_index, .{
            .op = .select,
            .typed_value = .{
                .type = then_block.typed_value.type,
                .value = Sema.Value.simple(.exec_time),
            },
            .data = .{ .select = .{ .condition = condition, .then_block = then_block_index, .else_block = else_block_index } },
        });
    }
    pub fn handleLoadInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleLoadInstruction", "Scope.handleLoadInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });

        const pointer_inst_index = self.getInstructionIndex(hir_inst.load.operand);
        const pointer_inst = self.getInstruction(pointer_inst_index);
        // switch (pointer_inst.op) {
        //     .constant => {
        //         return try self.pushInstruction(hir_inst_index, .{
        //             .op = pointer_inst.op,
        //             .type = pointer_inst.type,
        //             .value = pointer_inst.value,
        //             .data = pointer_inst.data,
        //         });
        //     },
        //     else => {},
        // }

        const type_to_load = self.builder.unwrapPointerType(pointer_inst.typed_value.type) orelse {
            std.debug.panic("expected type not found", .{});
        };

        // switch (pointer_inst.op) {
        //     .constant => {
        //         return try self.pushInstruction(hir_inst_index, .{
        //             .op = pointer_inst.op,
        //             .type = pointer_inst.type,
        //             .value = pointer_inst.value,
        //             .data = pointer_inst.data,
        //         });
        //     },
        //     else => {},
        // }

        // const value = if (self.builder.getValue(pointer_inst.value)) |value| switch (value.data) {
        //     .bytes => |ptr| try self.builder.sema.memory.load(type_to_load, self.builder.readNumberAsType(usize, ptr)),
        //     else => Sema.Value.simple(.exec_time),
        // } else Sema.Value.simple(.exec_time);
        // const value = if (try self.builder.maybeGetPointer(pointer_inst.getTypedValue())) |ptr|
        //     try self.builder.sema.memory.load(type_to_load, ptr)
        // else
        //     Sema.Value.simple(.exec_time);

        // const value = switch (pointer_inst.value) {
        //     .comptime_pointer => |ptr| try self.builder.sema.memory.load(type_to_load, ptr),
        //     else => Sema.Value.simple(.exec_time),
        // };

        return self.pushInstruction(hir_inst_index, .{
            .op = .load,
            .typed_value = .{
                .type = type_to_load,
                .value = Sema.Value.simple(.exec_time),
            },
            .data = .{ .operand = pointer_inst_index },
        });
    }
    pub fn handleParamSetInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleParamSetInstruction", "Scope.handleParamSetInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });

        const lhs_index = self.getInstructionIndex(hir_inst.param_set.lhs);
        const rhs_index = self.getInstructionIndex(hir_inst.param_set.rhs);
        return try self.pushInstruction(hir_inst_index, .{
            .op = .param_set,
            .typed_value = Sema.TypedValue.VOID,
            .data = .{ .operand_payload = .{
                .operand = lhs_index,
                .payload = rhs_index,
            } },
        });
    }
    pub fn handleRetInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleRetInstruction", "Scope.handleRetInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });

        return self.pushInstruction(hir_inst_index, .{
            .op = .ret,
            .typed_value = Sema.TypedValue.VOID,
            .data = .{
                .maybe_operand = if (hir_inst.ret.operand) |operand| self.getInstructionIndex(operand) else null,
            },
        });
    }
    pub fn handleTypeOfInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleTypeOfInstruction", "Scope.handleTypeOfInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const operand_index = self.getInstructionIndex(hir_inst.typeof.operand);
        const operand_inst = self.getInstruction(operand_index);
        return self.pushInstruction(hir_inst_index, .{
            .op = .typeof,
            .typed_value = .{
                .type = Sema.Type.simple(.type),
                .value = try self.builder.internValueData(.{ .type = operand_inst.typed_value.type }),
            },
            .data = .{ .operand = operand_index },
        });
    }

    pub fn handleStructDeclInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        _ = hir_inst; // autofix
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleStructDeclInstruction", "Scope.handleStructDeclInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });

        // hir_inst.struct_decl.fields_list;
        const entity_key = try self.builder.makeEntity(.{
            .parent = self.entity.key,
            .hir_inst_index = hir_inst_index,
            .name = try self.builder.internSlice("mod"),
            .data = .{
                .module_declaration = .{},
            },
        });
        var entity = self.builder.getEntity(entity_key);

        // const ty = try entity.resolveValue();

        // std.debug.panic("todo", .{});
        // self.cursor = @intCast(self.instructions.items.len);
        const ty = try entity.resolveType();
        const value = try entity.resolveValue();
        return self.pushInstruction(hir_inst_index, .{
            .op = .type,
            .typed_value = .{
                .type = ty,
                .value = value,
            },
            .data = .void,
        });
    }
    pub fn getTypePropertyByName(self: *Scope, hir_inst_index: Hir.Inst.Index, name_range: Sema.Strings.Range) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const base_hir_index = hir_inst.get_property_pointer.base;

        const base_inst_index = self.getInstructionIndex(base_hir_index);
        const base_inst = self.getInstruction(base_inst_index);
        const base_type = self.builder.unwrapTypeValue(base_inst.typed_value.value);

        switch (base_type) {
            .simple => |simple_type| switch (simple_type) {
                // .type => |struct_type| {
                //     _ = struct_type; // autofix
                // },
                else => {
                    std.debug.panic("getTypePropertyByName: unhandled base type: {s}", .{@tagName(simple_type)});
                },
            },
            .complex => |complex_type| switch (self.builder.getComplexType(complex_type).data) {
                .@"struct" => |struct_type| {
                    const entity = self.builder.getEntity(struct_type.entity);
                    const declaration_entity_index = entity.data.module_declaration.declarations.get(name_range) orelse {
                        std.debug.panic("error: property '{s}' not found in struct '{s}'", .{
                            self.builder.getSlice(name_range),
                            self.builder.getSlice(entity.name),
                        });
                    };
                    return try self.pushGlobalGetInstruction(hir_inst_index, declaration_entity_index);
                },
                else => {
                    const ty = self.builder.getComplexType(complex_type);
                    std.debug.panic("getTypePropertyByName: unhandled complex type: {s}", .{@tagName(ty.data)});
                },
            },
        }
    }
    pub fn maybeUnwrapPointerType(self: *Scope, ty: Sema.Type.Key) Sema.Type.Key {
        if (self.builder.getType(ty)) |ty_| switch (ty_.data) {
            .pointer => |pointer_type| return pointer_type.child,
            else => return ty,
        } else return ty;
    }

    pub fn getPropertyByName(self: *Scope, hir_inst_index: Hir.Inst.Index, name_range: Sema.Strings.Range) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const base_hir_index = hir_inst.get_property_pointer.base;

        const base_inst_index = self.getInstructionIndex(base_hir_index);
        const base_inst = self.getInstruction(base_inst_index);
        const base_type = self.maybeUnwrapPointerType(base_inst.typed_value.type);
        const name_slice = self.builder.getSlice(name_range);
        if (base_type.isOneOfSimple(&.{ .number, .usize, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .f32, .f64 })) {
            if (self.builder.isSliceEqual(name_range, "as")) {
                const ty = try self.builder.internTypeData(.{ .builtin_member = .{ .member = .as } });
                return try self.pushInstruction(hir_inst_index, .{
                    .op = .constant,
                    .typed_value = .{
                        .type = ty,
                        .value = try self.builder.internValueData(.{ .type = ty }),
                    },
                    .data = .{ .operand = base_inst_index },
                });
            }
        }

        switch (base_type) {
            .simple => |simple_type| switch (simple_type) {
                .type => return try self.getTypePropertyByName(hir_inst_index, name_range),
                // .number,
                // .usize,
                // => {
                //     if (self.builder.isSliceEqual(name_range, "as")) {
                //         return try self.pushInstruction(hir_inst_index, .{
                //             .op = .get_builtin_fn_as,
                //             .type = Sema.Type.simple(.type),
                //             .value = Sema.Value.simple(.type_builtin_fn_as),
                //             .data = .{ .operand = base_inst_index },
                //         });
                //     }
                //     std.debug.panic("todo {s}", .{self.builder.getSlice(name_range)});
                // },

                else => {
                    std.debug.panic("getPropertyByName: {s} unhandled base type: {s}", .{
                        name_slice,
                        @tagName(simple_type),
                    });
                },
            },
            .complex => |complex_type| switch (self.builder.getComplexType(complex_type).data) {
                .@"struct" => |struct_type| {
                    const entity = self.builder.getEntity(struct_type.entity);

                    if (entity.data.module_declaration.declarations.get(name_range)) |declaration| {
                        return try self.pushGlobalGetInstruction(hir_inst_index, declaration);
                    }

                    const field = entity.data.module_declaration.fields.get(name_range) orelse {
                        std.debug.panic("error: property '{s}' not found in struct '{s}'", .{
                            self.builder.getSlice(name_range),
                            self.builder.getSlice(entity.name),
                        });
                    };
                    const field_entity = self.builder.getEntity(field.entity);
                    // const field_index = field
                    std.debug.print("pushing index instruction\n", .{});
                    // const base
                    var field_value: Sema.Value.Key = Sema.Value.simple(.exec_time);
                    // const field_type = try field_entity.resolveType();
                    std.debug.print("field_type: {any}\n", .{self.builder.getFormattableTypedValue(base_inst.typed_value)});
                    if (try self.builder.maybeGetPointer(base_inst.typed_value)) |ptr| {
                        // const offset = self.builder.getComplexType(field_type).data.struct_field.offset;

                        field_value = try self.builder.numberAsBytesValueKey(ptr + field.offset);
                    }
                    const index_inst_index = try self.pushInstruction(hir_inst_index, .{
                        .op = .constant,
                        .typed_value = .{
                            .type = Sema.Type.simple(.usize),
                            .value = try self.builder.numberAsBytesValueKey(field.index),
                        },
                        .data = .void,
                    });

                    return self.pushInstruction(hir_inst_index, .{
                        .op = .get_element_pointer,
                        .typed_value = .{
                            .type = try self.builder.internTypeData(.{ .pointer = .{ .child = try field_entity.resolveType() } }),
                            .value = field_value,
                        },
                        // .value = base_inst.value,
                        // .value = field_value,
                        .data = .{ .get_element_pointer = .{
                            .base = base_inst_index,
                            .index = index_inst_index,
                        } },
                    });
                    // std.debug.panic("todo {s}", .{self.builder.getSlice(name_range)});
                    // return try self.pushGlobalGetInstruction(hir_inst_index, field.entity);
                },
                .typeof => return try self.getTypePropertyByName(hir_inst_index, name_range),
                .array => |array_type| {
                    if (self.builder.isSliceEqual(name_range, "len")) {
                        return try self.pushInstruction(hir_inst_index, .{
                            .op = .constant,
                            .typed_value = .{
                                .type = Sema.Type.simple(.usize),
                                .value = try self.builder.numberAsBytesValueKey(array_type.len),
                            },
                            .data = .void,
                        });
                    }
                    std.debug.panic("todo {s}", .{self.builder.getSlice(name_range)});
                },
                else => {
                    const ty = self.builder.getComplexType(complex_type);
                    std.debug.panic("getPropertyByName: unhandled complex type: {s}", .{@tagName(ty.data)});
                },
            },
        }
        // const property_name_range = try self.entity.internNode(hir_inst.get_property_pointer.property_name_node);
        // const property_name_slice = self.builder.getSlice(property_name_range);

    }

    pub fn handleBuiltinPropertyAccessInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleBuiltinPropertyAccessInstruction", "Scope.handleBuiltinPropertyAccessInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });
        const base_hir_index = hir_inst.get_property_pointer.base;
        _ = base_hir_index; // autofix

        const property_name_range = try self.entity.internNode(hir_inst.get_property_pointer.property_name_node);
        // const property_name_slice = self.builder.getSlice(property_name_range);
        return try self.getPropertyByName(hir_inst_index, property_name_range);

        // const base_index = self.getInstructionIndex(base_hir_index);
        // const base_instruction = self.getInstruction(base_index);
        // if (self.builder.getType(base_instruction.type)) |base_type| {
        //     std.debug.panic("todo {s}", .{@tagName(base_type.data)});
        // }

        // // var a: u16 = 0xabcd; // runtime-known
        // // _ = &a;
        // // const b: u8 = @intCast(a);
        // // _ = b; // autofix

        // switch (base_instruction.type.simple) {
        //     .usize,
        //     .i8,
        //     .i16,
        //     .i32,
        //     .i64,
        //     .u8,
        //     .u16,
        //     .u32,
        //     .u64,
        //     .f32,
        //     .f64,

        //     .number,
        //     => {
        //         if (std.mem.eql(u8, property_name_slice, "as")) {
        //             return try self.pushInstruction(hir_inst_index, .{
        //                 .op = .get_builtin_fn_as,
        //                 .type = Sema.Type.simple(.builtin_fn_as),
        //                 .value = Sema.Value.simple(.type_builtin_fn_as),
        //                 .data = .{ .operand = base_index },
        //             });
        //         }
        //         std.debug.panic("todo {s}", .{property_name_slice});
        //     },
        //     else => |tag| std.debug.panic("unhandled base type: {s}", .{@tagName(tag)}),
        // }
        // std.debug.panic("unhandled base type", .{});
    }
    pub fn handleGetPropertyPointerInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleGetPropertyPointerInstruction", "Scope.handleGetPropertyPointerInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });
        const is_builtin = hir_inst.get_property_pointer.is_builtin;
        if (is_builtin) {
            return try self.handleBuiltinPropertyAccessInstruction(hir_inst_index);
        }
        const base_hir_index = hir_inst.get_property_pointer.base;
        _ = base_hir_index; // autofix
        const property_name_range = try self.entity.internNode(hir_inst.get_property_pointer.property_name_node);
        return try self.getPropertyByName(hir_inst_index, property_name_range);
        // const property_name_slice = self.builder.getSlice(property_name_range);

        // if (self.builder.getEntityKeyByHirInst(base_hir_index)) |base_entity_key| {
        //     // Is referecing a global
        //     // ;
        //     const base_type = try self.builder.getEntity(base_entity_key).resolveType();
        //     if (base_type.isEqualSimple(.type)) {
        //         const value = try self.builder.getEntity(base_entity_key).resolveValue();

        //         const module_type_key = self.builder.unwrapTypeValue(value);
        //         const module_type = self.builder.getType(module_type_key) orelse {
        //             std.debug.panic("unreachable: should get a base type", .{});
        //         };
        //         const module = self.builder.getEntity(module_type.data.@"struct".entity);
        //         const declaration_entity_key = module.data.module_declaration.declarations.get(property_name_range) orelse {
        //             std.debug.panic("unreachable: should get a declaration", .{});
        //         };
        //         return try self.pushGlobalGetInstruction(hir_inst_index, declaration_entity_key);
        //     }
        // }

        // const base_index = self.getInstructionIndex(base_hir_index);
        // const base_instruction = self.getInstruction(base_index);

        // if (self.builder.isComplexValue(base_instruction.value, .slice)) {
        //     if (!std.mem.eql(u8, property_name_slice, "len")) std.debug.panic("error: array property {s} doesn't exist", .{property_name_slice});
        //     const len = self.builder.getComplexValue(base_instruction.value).data.slice.len;

        //     return try self.pushInstruction(hir_inst_index, .{
        //         .op = .constant,
        //         .type = Sema.Type.simple(.usize),
        //         .value = len,
        //         .data = .void,
        //     });
        // }

        // const unwrapped_base_type_key = self.builder.unwrapPointerType(base_instruction.type) orelse {
        //     std.debug.panic("unreachable: should get a pointer type {}", .{(base_instruction)});
        // };

        // const base_type = self.builder.getType(unwrapped_base_type_key) orelse {
        //     std.debug.panic("unreachable: should get a base type", .{});
        // };
        // const module = self.builder.getEntity(base_type.data.@"struct".entity);
        // if (module.data.module_declaration.fields.get(property_name_range)) |field| {
        //     const field_entity = self.builder.getEntity(field.entity);
        //     return self.pushInstruction(hir_inst_index, .{
        //         .op = .get_element_pointer,
        //         .type = try self.builder.internTypeData(.{ .pointer = .{ .child = try field_entity.resolveType() } }),
        //         .value = base_instruction.value,
        //         .data = .{ .get_element_pointer = .{
        //             .base = base_index,
        //             .index = field.index,
        //         } },
        //     });
        // }
        // if (module.data.module_declaration.declarations.get(property_name_range)) |declaration| {
        //     // const declaration_entity = self.builder.getEntity(declaration.entity);
        //     return try self.pushGlobalGetInstruction(hir_inst_index, declaration);
        // }
        // std.debug.panic("unreachable: %{d} should get a field or a declaration", .{hir_inst_index});
    }
    pub fn handleGetElementPointerInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const base_index = self.getInstructionIndex(hir_inst.get_element_pointer.base);
        const base_instruction = self.getInstruction(base_index);

        const index_inst_index = try self.getInstructionAsTypeByHirInst(hir_inst.get_element_pointer.index, Sema.Type.simple(.usize));

        const type_to_access = if (self.builder.getType(base_instruction.typed_value.type)) |ty| switch (ty.data) {
            .array, .slice => ty,
            .pointer => |pointer_type| self.builder.getComplexType(pointer_type.child).*,
            else => std.debug.panic("unhandled base type: {s}", .{@tagName(ty.data)}),
        } else std.debug.panic("unreachable: should get a base type", .{});

        const element_type = switch (type_to_access.data) {
            .slice => |slice_type| {
                const index_inst = self.getInstruction(index_inst_index);
                _ = index_inst; // autofix
                return try self.pushInstruction(hir_inst_index, .{
                    .op = .get_element_pointer,
                    .typed_value = .{
                        .type = try self.builder.internTypeData(.{ .pointer = .{ .child = slice_type.child } }),
                        .value = Sema.Value.simple(.exec_time),
                    },
                    .data = .{ .get_element_pointer = .{
                        .base = base_index,
                        .index = index_inst_index,
                    } },
                });
            },
            .array => |array_type| {
                const index_inst = self.getInstruction(index_inst_index);
                _ = index_inst; // autofix
                return try self.pushInstruction(hir_inst_index, .{
                    .op = .get_element_pointer,
                    .typed_value = .{
                        .type = try self.builder.internTypeData(.{ .pointer = .{ .child = array_type.child } }),
                        .value = Sema.Value.simple(.exec_time),
                    },
                    .data = .{ .get_element_pointer = .{
                        .base = base_index,
                        .index = index_inst_index,
                    } },
                });
            },
            else => std.debug.panic("unhandled base type: {s}", .{@tagName(type_to_access.data)}),
        };
        _ = element_type; // autofix

    }

    pub const SignatureCheckResult = struct {
        args_list: Sema.Instruction.List,
        all_args_comptime_known: bool,
        scope: *Scope,

        pub fn getArg(self: *SignatureCheckResult, arg_index: usize) Sema.Instruction.Index {
            const list = self.scope.builder.sema.lists.getSlice(self.args_list);
            return list[arg_index];
        }
        pub fn getArgInstruction(self: *SignatureCheckResult, arg_index: usize) *Sema.Instruction {
            return self.scope.getInstruction(self.getArg(arg_index));
        }
    };

    pub fn checkSignature(self: *Scope, type_params: []const Sema.Type.Key, hir_args_list_range: Hir.Inst.List, comptime cast_if_needed: bool) Error!SignatureCheckResult {
        if (type_params.len != hir_args_list_range.len) {
            std.debug.panic("error: function has {d} params but {d} args were provided", .{ type_params.len, hir_args_list_range.len });
        }
        const hir_args_list = self.entity.getHirList(hir_args_list_range);

        var args_list = self.builder.newList();
        var all_args_comptime_known = true;
        for (type_params, hir_args_list) |type_param, hir_arg_index| {
            var arg_inst_index: Sema.Instruction.Index = undefined;

            if (cast_if_needed) {
                arg_inst_index = try self.getInstructionAsTypeByHirInst(hir_arg_index, type_param);
                try args_list.append(arg_inst_index);
            } else {
                arg_inst_index = self.getInstructionIndex(hir_arg_index);
                const arg_inst = self.getInstruction(arg_inst_index);
                const can_cast = self.builder.canCastImplicitly(arg_inst.typed_value.type, type_param) catch |err| {
                    std.debug.panic("{s}", .{@errorName(err)});
                };
                // std.debug.print("{} {} {}\n", .{ arg_inst.type, type_param, can_cast });

                if (can_cast == .not_allowed) {
                    const writer = std.io.getStdErr().writer().any();
                    try writer.print("error: argument type mismatch: ", .{});
                    try self.builder.sema.formatType(writer, arg_inst.typed_value.type);
                    try writer.print(" != ", .{});
                    try self.builder.sema.formatType(writer, type_param);
                    try writer.print("\n", .{});
                    @panic("Type mismatch");
                }
                try args_list.append(arg_inst_index);
            }

            if (!self.isComptimeKnown(arg_inst_index)) {
                all_args_comptime_known = false;
            }
        }

        return .{
            .args_list = try args_list.commit(),
            .all_args_comptime_known = all_args_comptime_known,
            .scope = self,
        };
    }
    pub fn handleBuiltinMemberCallInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const callee_inst_index = self.getInstructionIndex(hir_inst.fn_call.callee);
        const callee_inst = self.getInstruction(callee_inst_index);
        // const callee_value: Sema.Value = self.builder.getValue(callee_inst.value) orelse {
        //     std.debug.panic("error: builtin call value is not a builtin global", .{});
        // };
        const builtin_global_key = self.builder.unwrapTypeValue(callee_inst.typed_value.value);
        std.debug.print("callee_inst: {}\n", .{callee_inst});
        // const builtin_global_key = self.builder.unwrapTypeValue(callee_inst.value);
        // const any_type = try self.builder.internTypeData(.{ .any = .{} });
        switch (self.builder.getComplexType(builtin_global_key).data.builtin_member.member) {
            .as => {
                const lhs_inst_index = callee_inst.data.operand;
                const lhs_inst = self.getInstruction(lhs_inst_index);
                const lhs_type = lhs_inst.typed_value.type;

                var signature_check_result = try self.checkSignature(
                    &.{
                        try self.builder.internFlatUnionTypeData(&.{
                            try self.builder.internTypeData(.{ .typeof = .{
                                .child = Sema.Type.simple(.f64),
                            } }),
                            try self.builder.internTypeData(.{ .typeof = .{
                                .child = Sema.Type.simple(.f32),
                            } }),
                            try self.builder.internTypeData(.{ .typeof = .{
                                .child = Sema.Type.simple(.i8),
                            } }),
                            try self.builder.internTypeData(.{ .typeof = .{
                                .child = Sema.Type.simple(.i16),
                            } }),
                            try self.builder.internTypeData(.{ .typeof = .{
                                .child = Sema.Type.simple(.i32),
                            } }),
                            try self.builder.internTypeData(.{ .typeof = .{
                                .child = Sema.Type.simple(.i64),
                            } }),
                            try self.builder.internTypeData(.{ .typeof = .{
                                .child = Sema.Type.simple(.u8),
                            } }),
                            try self.builder.internTypeData(.{ .typeof = .{
                                .child = Sema.Type.simple(.u16),
                            } }),
                            try self.builder.internTypeData(.{ .typeof = .{
                                .child = Sema.Type.simple(.u32),
                            } }),
                            try self.builder.internTypeData(.{ .typeof = .{
                                .child = Sema.Type.simple(.u64),
                            } }),
                            try self.builder.internTypeData(.{ .typeof = .{
                                .child = Sema.Type.simple(.f32),
                            } }),
                            try self.builder.internTypeData(.{ .typeof = .{
                                .child = Sema.Type.simple(.f64),
                            } }),
                            try self.builder.internTypeData(.{ .typeof = .{
                                .child = Sema.Type.simple(.bchar),
                            } }),
                        }),
                    },
                    hir_inst.fn_call.args_list,
                    false,
                );
                const rhs_inst_index = signature_check_result.getArg(0);
                const rhs_inst = self.getInstruction(rhs_inst_index);
                const rhs_type = self.builder.unwrapTypeValue(rhs_inst.typed_value.value);

                if (lhs_type.isEqual(rhs_type)) {
                    std.debug.panic("error: unsupported cast", .{});
                }
                const lhs_bits = self.builder.numberBits(lhs_type);
                const rhs_bits = self.builder.numberBits(rhs_type);

                const lhs_is_float = self.builder.isFloat(lhs_type);
                const rhs_is_float = self.builder.isFloat(rhs_type);
                self.markDead(callee_inst_index);
                self.markDeadIfComptimeKnown(lhs_inst_index);
                self.markDead(rhs_inst_index);

                if (lhs_is_float != rhs_is_float) {
                    const typed_value = try self.maybeCoerceValue(lhs_inst.typed_value, rhs_type);
                    if (lhs_is_float) {
                        return try self.pushInstruction(hir_inst_index, .{
                            .op = .truncate_float_to_int,
                            .typed_value = typed_value,
                            .data = .{ .operand = lhs_inst_index },
                        });
                    } else {
                        return try self.pushInstruction(hir_inst_index, .{
                            .op = .convert_int_to_float,
                            .typed_value = typed_value,
                            .data = .{ .operand = lhs_inst_index },
                        });
                    }
                }

                switch (lhs_type.simple) {
                    .int, .float => {
                        return try self.pushMaybeCastInstruction(hir_inst_index, lhs_inst_index, rhs_inst_index);
                    },
                    .f32, .f64 => {
                        // const lhs_bits = self.builder.numberBits(lhs_type);
                        // const rhs_bits = self.builder.numberBits(rhs_type);
                        const typed_value = try self.maybeCoerceValue(lhs_inst.typed_value, rhs_type);
                        if (rhs_bits > lhs_bits) {
                            return try self.pushInstruction(hir_inst_index, .{
                                .op = .float_promote,
                                .typed_value = typed_value,
                                .data = .{ .operand = lhs_inst_index },
                            });
                        }

                        return try self.pushInstruction(hir_inst_index, .{
                            .op = .float_demote,
                            .typed_value = typed_value,
                            .data = .{ .operand = lhs_inst_index },
                        });
                    },
                    .u8, .u16, .u32, .u64, .usize => {
                        // const lhs_bits = self.builder.numberBits(lhs_type);
                        // const rhs_bits = self.builder.numberBits(rhs_type);
                        const typed_value = try self.maybeCoerceValue(lhs_inst.typed_value, rhs_type);
                        const rhs_signed = self.builder.isSigned(rhs_type);
                        if (rhs_signed) {
                            return try self.pushInstruction(hir_inst_index, .{
                                .op = .reinterpret,
                                .typed_value = typed_value,
                                .data = .{ .operand = lhs_inst_index },
                            });
                        }
                        if (rhs_bits > lhs_bits) {
                            return try self.pushInstruction(hir_inst_index, .{
                                .op = .int_extend,
                                .typed_value = typed_value,
                                .data = .{ .operand = lhs_inst_index },
                            });
                        }

                        return try self.pushInstruction(hir_inst_index, .{
                            .op = .int_wrap,
                            .typed_value = typed_value,
                            .data = .{ .operand = lhs_inst_index },
                        });
                    },

                    .i8, .i16, .i32, .i64 => {
                        // const lhs_bits = self.builder.numberBits(lhs_type);
                        // const rhs_bits = self.builder.numberBits(rhs_type);
                        const typed_value = try self.maybeCoerceValue(lhs_inst.typed_value, rhs_type);
                        const rhs_signed = self.builder.isSigned(rhs_type);
                        if (!rhs_signed) {
                            return try self.pushInstruction(hir_inst_index, .{
                                .op = .reinterpret,
                                .typed_value = typed_value,
                                .data = .{ .operand = lhs_inst_index },
                            });
                        }

                        if (rhs_bits > lhs_bits) {
                            return try self.pushInstruction(hir_inst_index, .{
                                .op = .int_extend,
                                .typed_value = typed_value,
                                .data = .{ .operand = lhs_inst_index },
                            });
                        }

                        return try self.pushInstruction(hir_inst_index, .{
                            .op = .int_wrap,
                            .typed_value = typed_value,
                            .data = .{ .operand = lhs_inst_index },
                        });
                    },

                    // .number => {},
                    else => {
                        std.debug.panic("error: cannot cast '{s}' to '{s}'", .{ @tagName(lhs_type.simple), @tagName(rhs_type.simple) });
                    },
                }
            },
            else => std.debug.panic("unimplemented builtin member call", .{}),
        }
    }
    pub fn handleBuiltinCallInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const callee_inst_index = self.getInstructionIndex(hir_inst.fn_call.callee);
        const callee_inst = self.getInstruction(callee_inst_index);
        // const callee_value: Sema.Value = self.builder.getValue(callee_inst.value) orelse {
        //     std.debug.panic("error: builtin call value is not a builtin global", .{});
        // };
        const builtin_global_key = self.builder.unwrapTypeValue(callee_inst.typed_value.value);
        const any_type = try self.builder.internTypeData(.{ .any = .{} });
        switch (self.builder.getComplexType(builtin_global_key).data.builtin_global) {
            .comptime_log => {
                // const writer = std.io.getStdErr().writer().any();
                // var args_list = self.builder.newList();
                // for (hir_args_list) |arg_hir_index| {
                //     const arg_inst_index = try self.getInstructionAsTypeByHirInst(arg_hir_index, any_type);
                //     try args_list.append(arg_inst_index);
                // }
                const signature_check_result = try self.checkSignature(
                    &.{any_type},
                    hir_inst.fn_call.args_list,
                    false,
                );
                self.markDead(callee_inst_index);

                return try self.pushInstruction(hir_inst_index, .{
                    .op = .fn_call,
                    .typed_value = Sema.TypedValue.VOID,
                    .data = .{ .builtin_call = .{
                        .builtin = .comptime_log,
                        .args_list = signature_check_result.args_list,
                    } },
                });
            },
            .as => {
                var signature_check_result = try self.checkSignature(
                    &.{
                        Sema.Type.simple(.type),
                        any_type,
                    },
                    hir_inst.fn_call.args_list,
                    false,
                );
                const type_arg_inst_index = signature_check_result.getArg(0);
                const value_arg_inst_index = signature_check_result.getArg(1);
                return try self.pushMaybeCastInstruction(hir_inst_index, value_arg_inst_index, type_arg_inst_index);
            },
            .float_demote => {
                var signature_check_result = try self.checkSignature(
                    &.{
                        try self.builder.internFlatUnionTypeData(&.{
                            try self.builder.internTypeData(.{ .typeof = .{
                                .child = Sema.Type.simple(.f64),
                            } }),
                            try self.builder.internTypeData(.{ .typeof = .{
                                .child = Sema.Type.simple(.f32),
                            } }),
                        }),
                        try self.builder.internFlatUnionTypeData(&.{
                            Sema.Type.simple(.f64),
                            Sema.Type.simple(.f32),
                        }),
                    },
                    hir_inst.fn_call.args_list,
                    false,
                );

                self.markDeadIfComptimeKnown(signature_check_result.getArg(0));
                self.markDeadIfComptimeKnown(signature_check_result.getArg(1));

                return try self.pushInstruction(hir_inst_index, .{
                    .op = .float_demote,
                    .typed_value = .{
                        .type = self.builder.unwrapTypeValue(signature_check_result.getArgInstruction(0).typed_value.value),
                        .value = signature_check_result.getArgInstruction(1).typed_value.value,
                    },
                    .data = .{ .operand = signature_check_result.getArg(1) },
                });
            },

            else => |builtin| {
                std.debug.panic("unimplemented builtin: @{s}", .{@tagName(builtin)});
            },
        }
    }
    pub fn doFloatDemote(self: *Scope, type_index: Sema.Type.Key, value_key: Sema.Value.Key) Error!Sema.Value.Key {
        _ = self; // autofix
        _ = type_index; // autofix
        _ = value_key; // autofix
        // if (type_index.isEqualSimple(.f64)) {
    }
    pub fn handleFnCallInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleFnCallInstruction", "Scope.handleFnCallInstruction({s})", .{
                try formatHirIndex(self.entity.getHir(), hir_inst_index),
            } },
            .{
                .before = self.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.instructions.items,
        });

        const callee_inst_index = self.getInstructionIndex(hir_inst.fn_call.callee);
        const callee_inst = self.getInstruction(callee_inst_index);

        const callee_fn_type_key = callee_inst.typed_value.type;
        const callee_fn_type = self.builder.getComplexType(callee_fn_type_key);

        const fn_entity = switch (callee_fn_type.data) {
            .builtin_global => return try self.handleBuiltinCallInstruction(hir_inst_index),
            .builtin_member => return try self.handleBuiltinMemberCallInstruction(hir_inst_index),
            .function => |function| self.builder.getEntity(function.entity),
            else => std.debug.panic("error: not callable {s}", .{@tagName(callee_fn_type.data)}),
        };
        const callee_fn_value_key = try self.maybeResolveDependency(fn_entity.key);
        _ = callee_fn_value_key; // autofix

        const hir_args_list = self.entity.getHirList(hir_inst.fn_call.args_list);
        const params_list = self.builder.sema.lists.getSlice(callee_fn_type.data.function.params);

        if (hir_args_list.len != params_list.len) {
            std.debug.panic("error: function has {d} params but {d} args were provided", .{ params_list.len, hir_args_list.len });
        }

        var args_list = self.builder.newList();
        // for (hir_args_list) |arg_hir_index| {
        //     const arg_inst_index = try self.getInstructionAsTypeByHirInst(
        //         arg_hir_index,
        //         params_list[0],
        //     );
        //     try args_list.append(arg_inst_index);
        // }

        var all_args_resolved = true;

        for (hir_args_list, params_list) |arg_hir_index, param_type_key| {
            const param_type = Sema.Type.Key.decode(param_type_key);
            const arg_index = try self.getInstructionAsTypeByHirInst(arg_hir_index, param_type);
            const arg_inst = self.getInstruction(arg_index);
            if (!arg_inst.typed_value.type.isEqual(param_type) and !param_type.isEqual(try self.builder.internTypeData(.{ .any = .{} }))) {
                std.debug.panic("error: argument type mismatch: {} != {}", .{ arg_inst.typed_value.type, param_type });
            }

            try args_list.append(arg_index);
            if (!self.isComptimeKnown(arg_index)) {
                all_args_resolved = false;
            }
        }

        // var value: Sema.Value.Key = Sema.Value.simple(.exec_time);
        // if (self.is_comptime and !callee_fn_value_key.isEqualSimple(.exec_time) and !callee_fn_value_key.isEqualSimple(.runtime) and all_args_resolved) {
        //     if (self.builder.getValue(callee_fn_value_key)) |callee_fn_value| {
        //         if (callee_fn_value.data.function.init) |init_inst| {
        //             var arena = std.heap.ArenaAllocator.init(self.builder.allocator);
        //             defer arena.deinit();
        //             var args_typed_values: std.ArrayList(Sema.TypedValue) = std.ArrayList(Sema.TypedValue).init(self.builder.allocator);
        //             defer args_typed_values.deinit();
        //             for (args_list.list.items) |arg_index| {
        //                 const arg_inst = self.getInstruction(arg_index);
        //                 self.markDead(arg_index);
        //                 try args_typed_values.append(.{
        //                     .type = arg_inst.type,
        //                     .value = arg_inst.value,
        //                 });
        //             }
        //             var exec_context = ExecContext.init(
        //                 arena.allocator(),
        //                 self.builder,
        //                 args_typed_values.items,
        //                 callee_fn_type_key,
        //                 callee_fn_value_key,
        //                 init_inst,
        //             );
        //             value = try exec_context.exec();
        //         }
        //     }
        // }
        return try self.pushInstruction(hir_inst_index, .{
            .op = .fn_call,
            .typed_value = .{
                .type = callee_fn_type.data.function.ret,
                .value = Sema.Value.simple(.exec_time),
            },
            .data = .{
                .fn_call = .{
                    .callee_entity = fn_entity.key,
                    .callee_declaration = fn_entity.data.function_declaration.declaration_index,
                    .callee = callee_inst_index,
                    .args_list = try args_list.commit(),
                },
            },
        });

        // const fn_entity_data = self.builder.getEntity(fn_entity_key);
        // const fn_entity_data_type = try fn_entity_data.resolveType();
        // std.debug.panic("TODO", .{});

        // const callee_type_key = blk: {
        //     // Resolve if callee is a global
        //     // Ex
        //     // fn foo(): void {}
        //     // fn main() void {
        //     //     foo();
        //     // }
        //     if (self.builder.getEntityKeyByHirInst(hir_inst.fn_call.callee)) |callee_entity_key| {
        //         break :blk try self.builder.getEntity(callee_entity_key).resolveType();
        //     }
        //     // Resolve if callee is the result of a property access.
        //     // Ex
        //     // type const T = struct {
        //     //     fn foo() void {}
        //     // };
        //     // fn main() void {
        //     //     T.foo();
        //     // }
        //     const callee_inst = self.getInstructionIndex(hir_inst.fn_call.callee);
        //     const callee_instruction = self.getInstruction(callee_inst);

        //     break :blk callee_instruction.type;
        // };

        // const callee_type: Sema.Type = self.builder.getType(callee_type_key) orelse {
        //     switch (callee_type_key.simple) {
        //         .builtin_fn_as => return try self.handleBuiltinCastCall(hir_inst_index),
        //         else => {
        //             std.debug.panic("unhandled callee type: {s}", .{@tagName(callee_type_key.simple)});
        //         },
        //     }
        // };

        // const args = self.entity.getHirList(hir_inst.fn_call.args_list);

        // const function: Sema.Type.Data.Function = switch (callee_type.data) {
        //     .function => |fn_type| fn_type,
        //     else => std.debug.panic("error: trying to call non function type", .{}),
        // };

        // // Resolve params
        // const param_types = self.builder.sema.lists.getSlice(function.params);
        // var args_list = self.builder.newList();

        // if (args.len != param_types.len) {
        //     std.debug.panic("error: function has {d} params but {d} args were provided", .{ param_types.len, args.len });
        // }

        // var all_args_resolved = true;

        // for (args, param_types) |arg_hir_index, param_type_key| {
        //     const param_type = Sema.Type.Key.decode(param_type_key);
        //     const arg_index = try self.getInstructionAsTypeByHirInst(arg_hir_index, param_type);
        //     const arg_inst = self.getInstruction(arg_index);
        //     if (!arg_inst.type.isEqual(param_type) and !param_type.isEqual(try self.builder.internTypeData(.{ .any = .{} }))) {
        //         std.debug.panic("error: argument type mismatch: {} != {}", .{ arg_inst.type, param_type });
        //     }

        //     // try args_typed_values.append(.{
        //     //     .type = arg_inst.type,
        //     //     .value = arg_inst.value,
        //     // });
        //     try args_list.append(arg_index);
        //     if (!self.isComptimeKnown(arg_index)) {
        //         all_args_resolved = false;
        //     }
        // }
        // if (callee_type.data.function.is_builtin) {
        //     return try self.pushInstruction(hir_inst_index, .{
        //         .op = .fn_call,
        //         .type = callee_type.data.function.ret,
        //         .value = Sema.Value.simple(.void),
        //         .data = .{ .builtin_call = .{
        //             .builtin = .comptime_log,
        //             .args_list = try args_list.commit(),
        //         } },
        //     });
        //     // return try self.handleBuiltinCall(hir_inst_index);
        // }
        // const callee_entity = self.builder.getEntity(function.entity);
        // const callee_value_key = try self.maybeResolveDependency(function.entity);

        // var value: Sema.Value.Key = Sema.Value.simple(.exec_time);
        // if (self.is_comptime and !callee_value_key.isEqualSimple(.exec_time) and !callee_value_key.isEqualSimple(.runtime) and all_args_resolved) {
        //     if (self.builder.getValue(callee_value_key)) |callee_value|
        //         if (callee_value.data.function.init) |init_inst| {
        //             var arena = std.heap.ArenaAllocator.init(self.builder.allocator);
        //             defer arena.deinit();
        //             var args_typed_values: std.ArrayList(Sema.TypedValue) = std.ArrayList(Sema.TypedValue).init(self.builder.allocator);
        //             defer args_typed_values.deinit();
        //             for (args_list.list.items) |arg_index| {
        //                 const arg_inst = self.getInstruction(arg_index);
        //                 self.markDead(arg_index);
        //                 try args_typed_values.append(.{
        //                     .type = arg_inst.type,
        //                     .value = arg_inst.value,
        //                 });
        //             }
        //             var exec_context = ExecContext.init(
        //                 arena.allocator(),
        //                 self,
        //                 args_typed_values.items,
        //                 callee_type_key,
        //                 callee_value_key,
        //                 init_inst,
        //             );
        //             value = try exec_context.exec();
        //         };
        // }
        // const args_list_range = try args_list.commit();

        // return try self.pushInstruction(hir_inst_index, .{
        //     .op = .fn_call,
        //     .type = function.ret,
        //     .value = value,
        //     .data = .{
        //         .fn_call = .{
        //             .callee_entity = callee_entity.key,
        //             .callee = callee_entity.data.function_declaration.declaration_index,
        //             .args_list = args_list_range,
        //         },
        //     },
        // });
    }
    pub fn handleBuiltinCall(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const callee_inst_index = self.getInstructionIndex(hir_inst.fn_call.callee);
        const callee_inst = self.getInstruction(callee_inst_index);
        // const callee_value: Sema.Value = self.builder.getValue(callee_inst.value) orelse {
        //     std.debug.panic("error: builtin call value is not a builtin global", .{});
        // };
        const builtin_global_key = self.builder.unwrapTypeValue(callee_inst.value);
        switch (self.builder.getComplexType(builtin_global_key).data.builtin_global) {
            .comptime_log => {
                // const writer = std.io.getStdErr().writer().any();
                // const hir_args_list = self.entity.getHirList(hir_inst.fn_call.args_list);
                var args_list = self.builder.newList();
                // for (hir_args_list) |arg_hir_index| {
                //     const arg_inst_index = try self.getInstructionAsTypeByHirInst(arg_hir_index, Sema.Type.simple(.any));
                //     try args_list.append(arg_inst_index);
                // }

                self.markDead(callee_inst_index);
                return try self.pushInstruction(hir_inst_index, .{
                    .op = .fn_call,
                    .type = Sema.Type.simple(.void),
                    .value = Sema.Value.simple(.void),
                    .data = .{ .builtin_call = .{
                        .builtin = .comptime_log,
                        .args_list = try args_list.commit(),
                    } },
                });
            },
            else => |builtin| {
                std.debug.panic("unimplemented builtin: @{s}", .{@tagName(builtin)});
            },
        }
    }
    pub fn handleBuiltinCastCall(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);

        const callee_inst_index = self.getInstructionIndex(hir_inst.fn_call.callee);
        const callee_inst = self.getInstruction(callee_inst_index);
        const lhs_inst_index = callee_inst.data.operand;
        const lhs_inst = self.getInstruction(lhs_inst_index);
        const args = self.entity.getHirList(hir_inst.fn_call.args_list);
        if (args.len != 1) {
            std.debug.panic("error: builtin_fn_as expects 1 type argument", .{});
        }
        const rhs_inst_index = self.getInstructionIndex(args[0]);
        const rhs_inst = self.getInstruction(rhs_inst_index);
        if (!rhs_inst.type.isEqualSimple(.type)) {
            std.debug.panic("Error: builtin_fn_as expects a type argument, received {}", .{rhs_inst.type});
        }

        const rhs_type = self.builder.unwrapTypeValue(rhs_inst.value);
        const lhs_type = lhs_inst.type;

        if (lhs_type.isEqual(rhs_type)) {
            std.debug.panic("error: unnecessary cast", .{});
        }
        const lhs_bits = self.builder.numberBits(lhs_type);
        const rhs_bits = self.builder.numberBits(rhs_type);

        const lhs_is_float = self.builder.isFloat(lhs_type);
        const rhs_is_float = self.builder.isFloat(rhs_type);

        self.markDead(callee_inst_index);
        self.markDeadIfComptimeKnown(lhs_inst_index);
        self.markDead(rhs_inst_index);
        // if (!lhs_inst.value.isEqualSimple(.exec_time))

        if (lhs_is_float != rhs_is_float) {
            // std.debug.panic("error: cannot cast {s} to {s}", .{ @tagName(lhs_type.simple), @tagName(rhs_type.simple) });
            if (lhs_is_float) {
                return try self.pushInstruction(hir_inst_index, .{
                    .op = .truncate_float_to_int,
                    .type = rhs_type,
                    .value = try self.maybeCoerceValue(lhs_inst.value, rhs_type),
                    .data = .{ .operand = lhs_inst_index },
                });
            } else {
                return try self.pushInstruction(hir_inst_index, .{
                    .op = .convert_int_to_float,
                    .type = rhs_type,
                    .value = try self.maybeCoerceValue(lhs_inst.value, rhs_type),
                    .data = .{ .operand = lhs_inst_index },
                });
            }
        }

        switch (lhs_type.simple) {
            .number => {
                return try self.pushMaybeCastInstruction(hir_inst_index, lhs_inst_index, rhs_inst_index);
            },
            .f32, .f64 => {
                // const lhs_bits = self.builder.numberBits(lhs_type);
                // const rhs_bits = self.builder.numberBits(rhs_type);
                if (rhs_bits > lhs_bits) {
                    return try self.pushInstruction(hir_inst_index, .{
                        .op = .float_promote,
                        .type = rhs_type,
                        .value = try self.maybeCoerceValue(lhs_inst.value, rhs_type),
                        .data = .{ .operand = lhs_inst_index },
                    });
                }

                return try self.pushInstruction(hir_inst_index, .{
                    .op = .float_demote,
                    .type = rhs_type,
                    .value = try self.maybeCoerceValue(lhs_inst.value, rhs_type),
                    .data = .{ .operand = lhs_inst_index },
                });
            },
            .u8, .u16, .u32, .u64, .usize => {
                // const lhs_bits = self.builder.numberBits(lhs_type);
                // const rhs_bits = self.builder.numberBits(rhs_type);
                const rhs_signed = self.builder.isSigned(rhs_type);
                if (rhs_signed) {
                    return try self.pushInstruction(hir_inst_index, .{
                        .op = .reinterpret,
                        .type = rhs_type,
                        .value = try self.maybeCoerceValue(lhs_inst.value, rhs_type),
                        .data = .{ .operand = lhs_inst_index },
                    });
                }
                if (rhs_bits > lhs_bits) {
                    return try self.pushInstruction(hir_inst_index, .{
                        .op = .int_extend,
                        .type = rhs_type,
                        .value = try self.maybeCoerceValue(lhs_inst.value, rhs_type),
                        .data = .{ .operand = lhs_inst_index },
                    });
                }

                return try self.pushInstruction(hir_inst_index, .{
                    .op = .int_wrap,
                    .type = rhs_type,
                    .value = try self.maybeCoerceValue(lhs_inst.value, rhs_type),
                    .data = .{ .operand = lhs_inst_index },
                });
            },

            .i8, .i16, .i32, .i64 => {
                // const lhs_bits = self.builder.numberBits(lhs_type);
                // const rhs_bits = self.builder.numberBits(rhs_type);
                const rhs_signed = self.builder.isSigned(rhs_type);
                if (!rhs_signed) {
                    return try self.pushInstruction(hir_inst_index, .{
                        .op = .reinterpret,
                        .type = rhs_type,
                        .value = try self.maybeCoerceValue(lhs_inst.value, rhs_type),
                        .data = .{ .operand = lhs_inst_index },
                    });
                }

                if (rhs_bits > lhs_bits) {
                    return try self.pushInstruction(hir_inst_index, .{
                        .op = .int_extend,
                        .type = rhs_type,
                        .value = try self.maybeCoerceValue(lhs_inst.value, rhs_type),
                        .data = .{ .operand = lhs_inst_index },
                    });
                }

                return try self.pushInstruction(hir_inst_index, .{
                    .op = .int_wrap,
                    .type = rhs_type,
                    .value = try self.maybeCoerceValue(lhs_inst.value, rhs_type),
                    .data = .{ .operand = lhs_inst_index },
                });
            },

            // .number => {},
            else => {
                std.debug.panic("error: cannot cast '{s}' to '{s}'", .{ @tagName(lhs_type.simple), @tagName(rhs_type.simple) });
            },
        }

        // const lhs_bits = self.builder.numberBits(lhs_type);
        // _ = lhs_bits; // autofix
        // const rhs_bits = self.builder.numberBits(rhs_type);
        // _ = rhs_bits; // autofix

        // const lhs_signed = self.builder.isSigned(lhs_type);
        // _ = lhs_signed; // autofix
        // const rhs_signed = self.builder.isSigned(rhs_type);
        // _ = rhs_signed; // autofix

        // std.debug.panic("unhandled callee type: {s}", .{@tagName(callee_instruction.type.simple)});
    }
    pub fn handleParamInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);

        const param_entity = self.builder.getEntityByHirInst(hir_inst.param.operand);
        return self.pushInstruction(hir_inst_index, .{
            .op = .param,
            .typed_value = .{
                .type = try param_entity.resolveType(),
                .value = Sema.Value.simple(.exec_time),
            },
            .data = .{ .param = .{ .index = param_entity.data.parameter_declaration.index } },
        });
    }
    pub fn handleAsInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const lhs_inst_index = self.getInstructionIndex(hir_inst.as.lhs);
        const rhs_inst_index = self.getInstructionIndex(hir_inst.as.rhs);
        const rhs_inst = self.getInstruction(rhs_inst_index);

        self.markDeadIfComptimeKnown(rhs_inst_index);
        self.markDeadIfComptimeKnown(lhs_inst_index);
        const rhs_type = self.builder.unwrapTypeValue(rhs_inst.typed_value.value);

        return try self.pushCastInstruction(hir_inst_index, lhs_inst_index, rhs_type);
    }

    pub fn handleArrayInitInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const type_inst_index = self.getInstructionIndex(hir_inst.array_init.type);
        const type_inst = self.getInstruction(type_inst_index);
        const type_key = self.builder.unwrapTypeValue(type_inst.typed_value.value);
        var items_list = self.builder.newList();

        for (self.entity.getHir().lists.getSlice(hir_inst.array_init.items_list)) |item_hir_index| {
            const item_inst_index = self.getInstructionIndex(item_hir_index);
            try items_list.append(item_inst_index);
        }

        self.markDead(type_inst_index);
        return try self.pushInstruction(hir_inst_index, .{
            .op = .array_init,
            .typed_value = .{
                .type = type_key,
                .value = try self.builder.internValueData(.{
                    .array_init = .{
                        .items_list = try items_list.commit(),
                        .type = type_key,
                    },
                }),
            },
            .data = .void,
        });
    }
    pub fn handleTypeInitInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const type_inst_index = self.getInstructionIndex(hir_inst.type_init.type);
        const type_inst = self.getInstruction(type_inst_index);
        const type_key = self.builder.unwrapTypeValue(type_inst.typed_value.value);
        var field_init_list = self.builder.newList();
        for (self.entity.getHir().lists.getSlice(hir_inst.type_init.field_init_list)) |field_init_hir_index| {
            const field_init_inst_index = self.getInstructionIndex(field_init_hir_index);

            try field_init_list.append(field_init_inst_index);
        }
        self.markDead(type_inst_index);

        return try self.pushInstruction(hir_inst_index, .{
            .op = .type_init,
            .typed_value = .{
                .type = type_key,
                .value = try self.builder.internValueData(.{
                    .type_init = .{
                        .type = type_key,
                        .field_init_list = try field_init_list.commit(),
                    },
                }),
            },
            .data = .void,
        });
    }
    pub fn handleFieldInitInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.entity.getHirInstruction(hir_inst_index);
        const field_name = try self.entity.internNode(hir_inst.field_init.name_node);
        const field_value_inst_index = self.getInstructionIndex(hir_inst.field_init.value);
        // const field_value_inst = self.getInstruction(field_value_inst_index);
        return try self.pushInstruction(hir_inst_index, .{
            .op = .field_init,
            // .type = field_value_inst.type,
            .typed_value = .{
                .type = Sema.Type.simple(.void),
                .value = try self.builder.internValueData(.{
                    .field_init = .{
                        .field_name = field_name,
                        .value_inst = field_value_inst_index,
                    },
                }),
            },
            .data = .void,
        });
    }
};
const ExecContext = struct {
    id: usize,
    args: []const Sema.TypedValue,
    builder: *Builder,
    callee_value: Sema.TypedValue,
    ret_value: ?Sema.Value.Key = null,
    values: std.AutoHashMapUnmanaged(Sema.Instruction.Index, Sema.TypedValue) = .{},
    allocator: std.mem.Allocator,
    instructions: []Sema.Instruction,
    root_block: Sema.Instruction.Index,
    depth: usize = 0,
    max_depth: usize = 100,
    cursor: usize = 0,
    active_block: Sema.Instruction.Index = 0,

    pub var ID: usize = 0;
    pub fn init(
        allocator: std.mem.Allocator,
        builder: *Builder,
        args: []const Sema.TypedValue,
        callee_value: Sema.TypedValue,
        // root_block: Sema.Instruction.Index,
    ) ExecContext {
        const root_block = builder.getComplexValue(callee_value.value).data.function.init orelse {
            std.debug.panic("error: callee value is not a function", .{});
        };
        const root_block_inst = builder.sema.instructions.items[root_block];
        const instructions = builder.sema.instructions.items[root_block .. root_block + root_block_inst.data.block.instructions_list.len];

        ID += 1;
        return .{
            .id = ID,
            .args = args,
            .builder = builder,
            .callee_value = callee_value,
            .allocator = allocator,
            .instructions = instructions,
            .root_block = root_block,
        };
    }

    pub fn exec(self: *ExecContext) Error!Sema.Value.Key {
        try self.execInstruction(self.cursor);

        return self.ret_value orelse Sema.Value.simple(.void);
    }

    pub fn incDepth(self: *ExecContext) void {
        self.depth += 1;
    }
    pub fn decDepth(self: *ExecContext) void {
        self.depth -= 1;
    }
    pub fn putValue(self: *ExecContext, inst_index: Sema.Instruction.Index, typed_value: Sema.TypedValue) !void {
        std.debug.print("%{d} putValue {}\n", .{ inst_index, self.builder.getFormattableTypedValue(typed_value) });
        try self.values.put(self.allocator, inst_index, typed_value);
    }
    pub fn getValue(self: *ExecContext, inst_index: Sema.Instruction.Index) Sema.TypedValue {
        return self.values.get(inst_index) orelse {
            std.debug.panic("error: value not resolved {}\n", .{inst_index});
        };
    }
    pub fn getInstruction(self: *ExecContext, inst_index: Sema.Instruction.Index) Sema.Instruction {
        return self.instructions[inst_index];
    }
    const ExecComputeContext = struct {
        exec_context: *ExecContext,
        pub fn getInstruction(self: @This(), index: Sema.Instruction.Index) *Sema.Instruction {
            return &self.exec_context.instructions[index];
        }
        pub fn getTypedValue(self: @This(), index: Sema.Instruction.Index) Sema.TypedValue {
            return self.exec_context.getValue(index);
        }
        pub fn setValue(self: @This(), index: Sema.Instruction.Index, value: Sema.TypedValue) !void {
            try self.exec_context.putValue(index, value);
        }
        pub fn goTo(self: @This(), index: Sema.Instruction.Index) !void {
            // self.exec_context.active_block = index;
            try self.exec_context.execInstruction(index);
        }
    };
    pub fn execInstruction(self: *ExecContext, inst_index: Sema.Instruction.Index) Error!void {
        if (self.depth > self.max_depth) {
            std.debug.panic("error: max depth reached {}\n", .{self.depth});
        }
        self.cursor = inst_index;

        const inst = self.getInstruction(inst_index);

        if (inst.liveness == 0 and inst.op != .@"if" and inst.op != .loop) {
            return;
        }
        std.debug.print("\n%{d} execInstruction .{s}\n", .{
            inst_index,
            @tagName(inst.op),
        });
        switch (inst.op) {
            .block => {
                // if (typed_value.value.isEqualSimple(.void) and !typed_value.isComptimeKnown()) {
                try self.execBlock(inst_index);
                //     return;
                // }
                // try self.putValue(inst_index, typed_value);
                return;
            },
            .ret => return try self.execReturn(inst_index),
            // .@"if" => return try self.execIf(inst_index),
            // .loop => return try self.execLoop(inst_index),
            .param_get => return try self.execParamGet(inst_index),
            .param => return try self.execParam(inst_index),
            // .br => return try self.execBr(inst_index),
            else => {},
        }
        const is_control_flow = switch (inst.op) {
            .@"if", .ret, .loop, .br => true,
            .store => true,
            else => false,
        };

        const typed_value = inst.typed_value;
        if (typed_value.isComptimeKnown() and !is_control_flow) {
            try self.putValue(inst_index, typed_value);
            return;
        }

        // var compute_context = self.builder.compContext(ExecComputeContext{
        //     .exec_context = self,
        // }, true);
        // const result = try compute_context.computeInstruction(inst_index);

        // try self.putValue(inst_index, result);
    }
    pub fn execBlock(self: *ExecContext, inst_index: Sema.Instruction.Index) Error!void {
        const parent_block = self.active_block;
        self.active_block = inst_index;
        const block_inst = self.getInstruction(inst_index);
        const instructions = self.builder.sema.lists.getSlice(block_inst.data.block.instructions_list);
        // const end = instructions[instructions.len - 1];
        for (instructions) |inst| {
            if (self.active_block != inst_index) return;
            // if (self.cursor)
            try self.execInstruction(inst);
        }
        self.active_block = parent_block;
    }
    pub fn execParam(self: *ExecContext, inst_index: Sema.Instruction.Index) Error!void {
        const inst = self.getInstruction(inst_index);
        const param_value = self.args[inst.data.param.index];
        try self.putValue(inst_index, param_value);
    }
    pub fn execParamGet(self: *ExecContext, inst_index: Sema.Instruction.Index) Error!void {
        const inst = self.getInstruction(inst_index);
        const param_value = self.getValue(inst.data.operand);
        try self.putValue(inst_index, param_value);
    }
    pub fn execCast(self: *ExecContext, inst_index: Sema.Instruction.Index) Error!void {
        const inst = self.getInstruction(inst_index);
        const value = self.getValue(inst.data.operand);
        try self.putValue(inst_index, value);
    }
    pub fn execConstant(self: *ExecContext, inst_index: Sema.Instruction.Index) Error!void {
        const inst = self.getInstruction(inst_index);
        try self.putValue(inst_index, .{ .type = inst.type, .value = inst.value });
    }
    pub fn execComparison(self: *ExecContext, inst_index: Sema.Instruction.Index) Error!void {
        const inst = self.getInstruction(inst_index);
        const lhs_value = self.getValue(inst.data.bin_op.lhs);
        const rhs_value = self.getValue(inst.data.bin_op.rhs);
        const result = try self.builder.doComparison(inst.op, lhs_value, rhs_value);

        try self.putValue(inst_index, result);
    }
    pub fn execArithmetic(self: *ExecContext, inst_index: Sema.Instruction.Index) Error!void {
        const inst = self.getInstruction(inst_index);
        const lhs_value = self.getValue(inst.data.bin_op.lhs);
        const rhs_value = self.getValue(inst.data.bin_op.rhs);
        switch (lhs_value.type.simple) {
            inline .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .usize => |tag| {
                const result = try self.builder.doArithmetic(
                    tag.getNativeType(),
                    inst.op,
                    lhs_value,
                    rhs_value,
                );
                try self.putValue(inst_index, result);
            },
            else => unreachable,
        }
    }
    pub fn execIf(self: *ExecContext, inst_index: Sema.Instruction.Index) Error!void {
        const inst = self.getInstruction(inst_index);
        const cond_value = self.getValue(inst.data.@"if".condition);

        self.incDepth();
        defer self.decDepth();

        const last_block_inst = inst.data.@"if".else_block orelse inst.data.@"if".then_block;
        const end = inst_index + self.getInstruction(last_block_inst).data.block.instructions_count;
        if (cond_value.value.isEqualSimple(.true)) {
            try self.execInstruction(inst.data.@"if".then_block);
            return;
        }
        if (cond_value.value.isEqualSimple(.false)) {
            if (inst.data.@"if".else_block) |else_block_inst_index| {
                try self.execInstruction(else_block_inst_index);
                return;
            }
        }
        self.cursor = end;
    }
    pub fn execLoop(self: *ExecContext, inst_index: Sema.Instruction.Index) Error!void {
        const inst = self.getInstruction(inst_index);
        const block_index = inst.data.loop.body_block;
        const end = inst_index + self.getInstruction(block_index).data.block.instructions_count;
        _ = end; // autofix
        self.cursor = block_index;
        try self.execInstruction(block_index);
        // while (self.cursor < end and self.ret_value == null) {
        //     try self.execInstruction(self.cursor);
        //     self.cursor += 1;
        // }
    }
    pub fn execReturn(self: *ExecContext, inst_index: Sema.Instruction.Index) Error!void {
        const inst = self.getInstruction(inst_index);

        const ret_value = if (inst.data.maybe_operand) |operand_inst_index| self.getValue(operand_inst_index).value else Sema.Value.simple(.void);
        try self.putValue(inst_index, .{ .type = inst.typed_value.type, .value = ret_value });
        self.ret_value = ret_value;
    }
    // pub fn execFunctionCall(self: *ExecContext, inst_index: Sema.Instruction.Index) Error!void {
    //     const inst = self.getInstruction(inst_index);
    //     switch (inst.data) {
    //         .builtin_call => |call| {
    //             switch (call.builtin) {
    //                 .comptime_log => {
    //                     const args = self.builder.sema.lists.getSlice(call.args_list);
    //                     const writer = std.io.getStdErr().writer().any();
    //                     for (args) |arg| {
    //                         const value = self.getValue(arg);
    //                         try self.builder.sema.formatTypedValue(writer, value, .{});
    //                     }

    //                     try self.putValue(inst_index, .{ .type = Sema.Type.simple(.void), .value = Sema.Value.simple(.void) });

    //                     return;
    //                 },
    //                 else => |builtin| std.debug.panic("unimplemented builtin {s}\n", .{@tagName(builtin)}),
    //             }
    //         },

    //         else => {},
    //     }

    //     const callee_entity = self.builder.getEntity(inst.data.fn_call.callee_entity);

    //     const callee_type_key = try callee_entity.resolveType();
    //     const callee_value_key = try callee_entity.resolveValue();
    //     const callee_type = self.builder.getType(callee_type_key) orelse std.debug.panic("error: callee type not resolved {}\n", .{callee_type_key});
    //     const callee_value = self.builder.getValue(callee_value_key) orelse std.debug.panic("error: callee value not resolved {}\n", .{callee_value_key});
    //     const callee_init = callee_value.data.function.init orelse std.debug.panic("error: callee value not resolved {}\n", .{callee_value_key});

    //     var args_list = std.ArrayList(Sema.TypedValue).init(self.allocator);
    //     defer args_list.deinit();

    //     for (self.builder.sema.lists.getSlice(inst.data.fn_call.args_list)) |arg_inst_index| {
    //         const arg_typed_value = self.getValue(arg_inst_index);
    //         try args_list.append(arg_typed_value);
    //     }

    //     self.incDepth();
    //     defer self.decDepth();
    //     var exec_context = ExecContext.init(
    //         self.allocator,
    //         self.scope,
    //         args_list.items,
    //         callee_type_key,
    //         callee_value_key,
    //         callee_init,
    //     );

    //     exec_context.depth = self.depth;
    //     const value = try exec_context.exec();
    //     try self.putValue(inst_index, .{ .type = callee_type.data.function.ret, .value = value });
    // }
};
