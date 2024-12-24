const std = @import("std");
const Tracer = @import("../Tracer2.zig");
const Sema = @import("./Sema.zig");
const Hir = @import("../Hir.zig");
const ErrorManager = @import("../ErrorManager.zig");
const Array = std.ArrayListUnmanaged;
const ArrayHashMap = std.AutoArrayHashMapUnmanaged;
const ChunkedArray = @import("../chunked_array.zig").ChunkedArray;
const Ast = @import("../Ast.zig");

inline fn formatHirIndex(hir: *Hir, index: Hir.Inst.Index) ![]const u8 {
    const hir_inst = getHirInst(hir, index);
    var buf: [256]u8 = undefined;
    const slice = try std.fmt.bufPrint(buf[0..], "hir = {d}:.{s}", .{ index, @tagName(hir_inst) });
    return slice;
}
fn getHirInst(hir: *Hir, index: Hir.Inst.Index) Hir.Inst {
    return hir.insts.items[index];
}
const Error = error{
    CircularDependency,
    SymbolNotFound,
    NoSpaceLeft,
    SymbolAlreadyExists,
    Overflow,
} || std.mem.Allocator.Error || std.fmt.ParseIntError || std.fmt.ParseFloatError;
pub const Builder = struct {
    // sema: *Sema,
    entities: ChunkedArray(Entity, 1024),
    strings: Sema.Strings,
    arena: std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,
    tracer: Tracer,
    errors_manager: *ErrorManager,
    hir: *Hir,
    symbols: std.AutoArrayHashMapUnmanaged(Sema.Strings.Range, Entity.Key) = .{},
    symbols_by_hir_inst: std.AutoArrayHashMapUnmanaged(Hir.Inst.Index, Entity.Key) = .{},
    values: ArrayHashMap(u64, Sema.Value) = .{},
    types: ArrayHashMap(u64, Sema.Type) = .{},
    lists: Sema.Lists,
    snapshot_scratch: std.ArrayListUnmanaged(u8) = .{},
    instructions: std.ArrayListUnmanaged(Sema.Instruction) = .{},
    declarations: std.ArrayListUnmanaged(Sema.Declaration) = .{},
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
    pub fn getAstNode(self: *Builder, node_index: Ast.Node.Index) Ast.Node {
        return self.hir.ast.getNode(node_index);
    }
    pub fn getNodeSlice(self: *Builder, node_index: Ast.Node.Index) []const u8 {
        return self.hir.ast.getNodeSlice(node_index);
    }
    pub fn getTokenSlice(self: *Builder, token: Ast.Token.Index) []const u8 {
        return self.hir.ast.getTokenSlice(token);
    }
    pub fn getState(self: *Builder) []const u8 {
        self.snapshot_scratch.clearRetainingCapacity();
        const writer = self.snapshot_scratch.writer(self.arena.allocator()).any();

        writer.print("Types: {d} items\n", .{self.types.count()}) catch {};
        writer.print("Values: {d} items\n", .{self.values.count()}) catch {};
        writer.print("Declarations: {d} items\n", .{self.declarations.items.len}) catch {};

        writer.print("\n", .{}) catch {};

        var entities_iter = self.entities.iterator();
        var symbols_iter = self.symbols.iterator();

        writer.print("Symbols: {d} items\n\n", .{self.symbols.count()}) catch {};
        while (symbols_iter.next()) |entry| {
            writer.print("- {s}: Ent({d})\n", .{ self.getSlice(entry.key_ptr.*), entry.value_ptr.* }) catch unreachable;
        }

        writer.print("\n", .{}) catch {};
        writer.print("Symbols by hir inst: {d} items\n\n", .{self.symbols_by_hir_inst.count()}) catch {};
        var symbols_by_hir_inst_iter = self.symbols_by_hir_inst.iterator();
        while (symbols_by_hir_inst_iter.next()) |entry| {
            writer.print("- {s} = Ent({d})\n", .{ formatHirIndex(self.hir, entry.key_ptr.*) catch unreachable, entry.value_ptr.* }) catch unreachable;
        }

        writer.print("\n", .{}) catch {};
        writer.print("Entities: {d} items\n\n", .{self.entities.len}) catch unreachable;
        while (entities_iter.next()) |entry| {
            writer.print("- {s} ", .{entry.formatKey() catch unreachable}) catch unreachable;

            writer.print("symbols = .{s}, ", .{@tagName(entry.symbols)}) catch unreachable;
            writer.print("type = .{s}, ", .{@tagName(entry.type)}) catch unreachable;
            writer.print("value = {s}\n", .{@tagName(entry.value)}) catch unreachable;
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
                return self.types.entries.items(.value)[complex];
            },
        }
    }
    pub fn getValue(self: *Builder, key: Sema.Value.Key) ?Sema.Value {
        switch (key) {
            .simple => {
                return null;
            },
            .complex => |complex| {
                return self.values.entries.items(.value)[complex];
            },
        }
    }

    pub fn reserveDeclaration(self: *Builder) !Sema.Declaration.Index {
        const trace = self.tracer.begin(
            @src(),
            .{ "reserveDeclaration", "Builder.reserveDeclaration()", .{} },
            .{},
        );
        const index = self.declarations.items.len;
        defer trace.end(.{
            .index = index,
        });
        try self.declarations.append(self.allocator, undefined);
        return index;
    }

    pub fn setDeclaration(self: *Builder, index: Sema.Declaration.Index, declaration: Sema.Declaration) void {
        self.declarations.items[index] = declaration;
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
    fn getEntityByHirInst(self: *Builder, hir_inst_index: Hir.Inst.Index) *Entity {
        const entity_key = self.symbols_by_hir_inst.get(hir_inst_index) orelse std.debug.panic("no entity for hir_inst: {d}", .{hir_inst_index});
        return self.getEntity(entity_key);
    }
    fn getEntity(self: *Builder, key: Entity.Key) *Entity {
        return self.entities.getPtr(key);
    }
    fn getEntityBySymbol(self: *Builder, name: []const u8) ?*Entity {
        const range = self.strings.getRange(name) orelse return null;
        const entity_key = self.symbols.get(range) orelse return null;
        return self.getEntity(entity_key);
    }
    fn getHirList(self: *Builder, range: Hir.InternedLists.Range) []Hir.Inst.Index {
        return self.hir.lists.getSlice(range);
    }
    // fn getNodeSlice(self: *Builder, node_index: Ast.Node.Index) []const u8 {
    //     return self.hir.ast.getNodeSlice(node_index);
    // }
    pub fn getSlice(self: *Builder, range: Sema.Strings.Range) []const u8 {
        return self.strings.getSlice(range);
    }
    fn internSlice(self: *Builder, slice: []const u8) Error!Sema.Strings.Range {
        return self.strings.internSlice(slice);
    }
    inline fn internMultipleSlices(self: *Builder, slices: anytype) Error!Sema.Strings.Range {
        var list = self.strings.new();
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
        return self.lists.new();
    }
    fn internNode(self: *Builder, node: Ast.Node.Index) Error!Sema.Strings.Range {
        return try self.internSlice(self.hir.ast.getNodeSlice(node));
    }

    pub fn unwrapTypeValue(self: *Builder, value_key: Sema.Value.Key) Sema.Type.Key {
        switch (value_key) {
            .complex => |complex| {
                const value = self.values.entries.items(.value)[complex];
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
            else => {
                return null;
            },
        }
    }
    const Hasher = struct {
        hasher: std.hash.Wyhash,
        pub fn new(value: anytype) Hasher {
            var hasher = Hasher{ .hasher = std.hash.Wyhash.init(0) };
            hasher.update(value);
            return hasher;
        }
        pub fn update(self: *Hasher, value: anytype) void {
            switch (@TypeOf(value)) {
                []const u8 => {
                    self.hasher.update(value);
                },
                else => {
                    self.hasher.update(std.mem.asBytes(&value));
                },
            }
        }
        pub fn hash(value: anytype) u64 {
            var hasher = Hasher.new(value);
            return hasher.final();
        }
        pub fn final(self: *Hasher) u64 {
            return self.hasher.final();
        }
    };
    fn getTypeKeyHash(self: *Builder, type_key: Sema.Type.Key) Error!u64 {
        switch (type_key) {
            .simple => |simple| {
                const hash = switch (simple) {
                    inline else => |simple_type| comptime std.hash.Wyhash.hash(0, @tagName(simple_type)),
                };
                return hash;
            },
            .complex => |complex| {
                const ty = self.types.entries.items(.value)[complex];
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
        if (self.types.getIndex(ty.hash)) |index| {
            const existing = Sema.Type.complex(index);

            trace.end(.{
                .type_key = existing,
                .is_new = false,
            });
            return existing;
        }
        const index = self.types.count();
        try self.types.put(self.allocator, ty.hash, ty);
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
            .function => |function| {
                var hasher = Hasher.new("function");
                for (self.lists.getSlice(function.params)) |param| {
                    const decoded = Sema.Type.Key.decode(param);
                    hasher.update(self.getTypeKeyHash(decoded));
                }
                const hash = hasher.final();
                // return key;
                return try self.internType(.{
                    .hash = hash,
                    .data = data,
                });
            },
            .array => |array| {
                var hasher = Hasher.new("array");
                hasher.update(self.getTypeKeyHash(array.child));
                hasher.update(array.size);
                return try self.internType(.{
                    .hash = hasher.final(),
                    .data = data,
                });
            },
            .pointer => |pointer| {
                var hasher = Hasher.new("pointer");
                hasher.update(self.getTypeKeyHash(pointer.child));
                return try self.internType(.{
                    .hash = hasher.final(),
                    .data = data,
                });
            },
            .@"struct" => |str| {
                var hasher = Hasher.new("struct");
                for (self.lists.getSlice(str.fields)) |field| {
                    hasher.update(self.getTypeKeyHash(Sema.Type.Key.decode(field)));
                }
                return try self.internType(.{
                    .hash = hasher.final(),
                    .data = data,
                });
            },
            .module => |module| {
                // Modules are always unique

                return try self.internType(.{
                    .hash = Hasher.hash(module.entity),
                    .data = data,
                });
            },

            // else => {},
        }
        _ = type; // autofix
        std.debug.panic("TODO: internType {s}", .{@tagName(data)});
    }
    fn getValueKeyHash(self: *Builder, value_key: Sema.Value.Key) Error!u64 {
        switch (value_key) {
            .simple => |simple| {
                const hash = switch (simple) {
                    inline else => |simple_type| comptime std.hash.Wyhash.hash(0, @tagName(simple_type)),
                };
                return hash;
            },
            .complex => |complex| {
                const value = self.values.entries.items(.value)[complex];
                return value.hash;
            },
        }
    }
    fn internValue(self: *Builder, value: Sema.Value) Error!Sema.Value.Key {
        const trace = self.tracer.begin(
            @src(),
            .{ "internValue", "Builder.internValue(.{s}, hash = {x})", .{ @tagName(value.data), value.hash } },
            .{
                .value = value,
            },
        );
        if (self.values.getIndex(value.hash)) |index| {
            const existing = Sema.Value.complex(index);

            trace.end(.{
                .value_key = existing,
                .is_new = false,
            });
            return existing;
        }
        const index = self.values.count();
        try self.values.put(self.allocator, value.hash, value);
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
                const type_hash = try self.getTypeKeyHash(function.type);
                hash.update(type_hash);
                if (function.init) |init| {
                    hash.update(init);
                }
                return try self.internValue(.{
                    .hash = hash.final(),
                    .data = data,
                });
            },
            .integer => |integer| {
                var hasher = Hasher.new("integer");
                hasher.update(integer);
                return try self.internValue(.{
                    .hash = hasher.final(),
                    .data = data,
                });
            },
            .float => |float| {
                var hasher = Hasher.new("float");
                hasher.update(float);
                return try self.internValue(.{
                    .hash = hasher.final(),
                    .data = data,
                });
            },
            .type => |type_key| {
                const type_hash = try self.getTypeKeyHash(type_key);
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
                var hasher = Hasher.new("global");
                const type_hash = try self.getTypeKeyHash(global.type);
                const value_hash = try self.getValueKeyHash(global.value);
                hasher.update(type_hash);
                hasher.update(value_hash);
                if (global.init) |init| {
                    hasher.update(init);
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
    pub inline fn build(allocator: std.mem.Allocator, hir: *Hir, errors_manager: *ErrorManager, options: BuildOptions) Error!Builder {
        _ = options; // autofix
        return Builder{
            .strings = Sema.Strings.init(allocator),
            .hir = hir,
            .errors_manager = errors_manager,
            .entities = ChunkedArray(Entity, 1024).init(allocator),
            // .sema = sema,
            .lists = Sema.Lists.init(allocator),
            .arena = std.heap.ArenaAllocator.init(allocator),
            .allocator = allocator,
            .tracer = Tracer.init(
                allocator,
                .sema,
                .{},
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
                    .number, .i8, .i16, .i32, .i64 => true,
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
                    else => unreachable,
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
    pub fn collectRoot(self: *Builder) !void {
        // const root_entity = try Entity.init(self, Hir.Inst.RootIndex);

        const root_entity_key = try self.makeEntity(.{
            .name = try self.internSlice("root"),
            .hir_inst_index = Hir.Inst.RootIndex,
            .data = .{ .module_declaration = .{} },
        });
        var root_entity = self.getEntity(root_entity_key);
        try root_entity.collectEntities();
    }
    pub fn deinit(self: *Builder) void {
        self.strings.deinit();
        self.lists.deinit();
        self.entities.deinit();
        self.arena.deinit();
        self.values.deinit(self.allocator);
        self.types.deinit(self.allocator);
        self.symbols.deinit(self.allocator);
        self.symbols_by_hir_inst.deinit(self.allocator);
        self.tracer.deinit();
        self.declarations.deinit(self.allocator);
        self.declarations_by_hir_inst.deinit(self.allocator);
        self.instructions.deinit(self.allocator);
        self.queue.deinit(self.allocator);
    }

    pub fn makeEntity(self: *Builder, input: Entity.EntityInput) !Entity.Key {
        const trace = self.tracer.begin(
            @src(),
            .{ "makeEntity", "makeEntity({s}, {s})", .{
                try formatHirIndex(self.hir, input.hir_inst_index),
                @tagName(input.data),
            } },
            .{
                .input = input,
            },
        );
        const key = self.entities.len;
        defer trace.end(.{ .key = key, .post_state = self.getState() });
        if (self.symbols_by_hir_inst.contains(input.hir_inst_index)) {
            std.debug.panic("Symbol already exists: {s}", .{
                try formatHirIndex(self.hir, input.hir_inst_index),
            });
        }
        const entity = try Entity.init(self, key, input);
        try self.entities.append(entity);
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

        return try entity.resolveDeclaration();
    }

    pub fn compileAll(self: *Builder) !void {
        const trace = self.tracer.begin(
            @src(),
            .{ "compileAll", "Builder.compileAll", .{} },
            .{},
        );
        defer trace.end(.{ .post_state = self.getState() });
        // try self.collectAll();
        try self.collectRoot();

        var i: usize = 0;
        var j: usize = 0;
        while (true) : (i += 1) {
            if (i >= self.entities.len) {
                // var symbols_iter = self.symbols.iterator();
                const symbols = self.symbols.entries.items(.value);
                while (j < symbols.len) : (j += 1) {
                    const entity = self.getEntity(symbols[j]);
                    _ = try entity.resolveDeclaration();
                }

                if (i >= self.entities.len) break;
            }
            var entity = self.entities.get(i);
            try entity.collectEntities();
        }
    }
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
            }) = .{},
            declarations: std.AutoArrayHashMapUnmanaged(Sema.Strings.Range, Entity.Key) = .{},
        },
        global_declaration: void,
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
        name: Sema.Strings.Range,
        hir_inst_index: Hir.Inst.Index,
        data: Data,
    };
    pub fn init(builder: *Builder, key: Key, input: EntityInput) !Entity {
        const hir_inst = getHirInst(builder.hir, input.hir_inst_index);

        _ = hir_inst; // autofix
        var ent = Entity{
            .name = if (input.parent) |parent| try builder.internMultipleSlices(&.{ builder.getEntity(parent).name, "::", input.name }) else input.name,
            .key = key,
            .hir_inst_index = input.hir_inst_index,
            .builder = builder,
            .data = input.data,
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
        switch (getHirInst(builder.hir, input.hir_inst_index)) {
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
        const slice = try std.fmt.bufPrint(buf[0..], "Ent({d}, .{s}, {s})", .{ self.key, @tagName(self.data), try formatHirIndex(self.builder.hir, self.hir_inst_index) });
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

        const hir_inst = getHirInst(self.builder.hir, self.hir_inst_index).struct_decl;
        const fields_list = self.builder.getHirList(hir_inst.fields_list);
        for (fields_list, 0..) |field_inst_index, i| {
            const field_inst = getHirInst(self.builder.hir, field_inst_index).struct_field;

            const name_slice_range = try self.builder.internNode(field_inst.name_node);
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
            });
        }

        const declarations_list = self.builder.getHirList(hir_inst.declarations_list);
        for (declarations_list) |declaration_inst_index| {
            switch (getHirInst(self.builder.hir, declaration_inst_index)) {
                .global_decl => |global_decl_inst| {
                    const name_slice_range = try self.builder.internNode(global_decl_inst.name_node);
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
                                    .declaration_index = try self.builder.reserveDeclaration(),
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
                                    .declaration_index = try self.builder.reserveDeclaration(),
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
                            .data = .global_declaration,
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
        const hir_inst = getHirInst(builder.hir, self.hir_inst_index).global_decl;
        const fn_inst = getHirInst(builder.hir, hir_inst.type orelse unreachable).fn_decl;
        const parameters_list = builder.getHirList(fn_inst.params_list);
        const data = &self.data.function_declaration;
        for (parameters_list) |parameter_inst_index| {
            const parameter_inst = getHirInst(builder.hir, parameter_inst_index).param_decl;
            const name_slice_range = try builder.internNode(parameter_inst.name_node);

            const parameter_trace = builder.tracer.begin(
                @src(),
                .{ "collectFunctionSymbols", "[COLLECT_PARAMETER='{s}']", .{
                    builder.getSlice(name_slice_range),
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
                .data = .{ .parameter_declaration = .{ .type = 0 } },
            });
            if (builder.getEntityKeyByHirInst(parameter_inst.type)) |parameter_type_entity_key| {
                builder.getEntity(parameter_entity_key).data.parameter_declaration.type = parameter_type_entity_key;
            } else {
                const parameter_type_entity_key = try builder.makeEntity(.{
                    .parent = parameter_entity_key,
                    .name = try builder.internSlice("%type"),
                    .hir_inst_index = parameter_inst.type,
                    .data = .{ .type = {} },
                });
                builder.getEntity(parameter_entity_key).data.parameter_declaration.type = parameter_type_entity_key;
                try data.parameters.put(builder.arena.allocator(), name_slice_range, parameter_type_entity_key);
            }

            defer parameter_trace.end(.{ .entity_key = parameter_entity_key });
            try data.parameters.put(
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

        const hir_inst = getHirInst(self.builder.hir, self.hir_inst_index).struct_field;
        // const name_slice_range = try self.builder.internNode(hir_inst.name_node);
        // const name_slice = self.builder.getSlice(name_slice_range);

        // const field_trace = self.builder.tracer.begin(
        //     @src(),
        //     .{ "collectFieldSymbols", "[COLLECT_FIELD='{s}']", .{
        //         name_slice,
        //     } },
        //     .{},
        // );
        // defer field_trace.end(.{});

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
                //noop, type of type is always 'type'..the actual type is the type values
                .module_declaration,
                .type,
                .global_type_declaration,
                => Sema.Type.simple(.type),
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

        return try self.builder.internTypeData(.{
            .function = .{
                .params = try params_list.commit(),
                .ret = self.builder.unwrapTypeValue(ret_type_value),
            },
        });
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
    pub fn resolveValue(self: *Entity) Error!Sema.Value.Key {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "resolveValue", "{s}.resolveValue", .{
                try self.formatKey(),
            } },
            .{},
        );
        defer trace.end(.{ .value = self.value, .post_state = self.builder.getState() });
        switch (self.value) {
            .idle => {
                _ = try self.resolveType();
            },
            .resolving => {
                return error.CircularDependency;
            },
            .resolved => {
                return self.value.resolved;
            },
        }
        self.value = .resolving;

        self.value = .{ .resolved = switch (self.data) {
            .type => try self.resolveTypeValue(),
            .function_declaration => try self.resolveFunctionValue(),
            .global_type_declaration => try self.resolveGlobalTypeValue(),
            .module_declaration => try self.resolveModuleValue(),
            else => std.debug.panic("unhandled value: {s}", .{@tagName(self.data)}),
        } };
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
        const hir_inst = getHirInst(self.builder.hir, self.hir_inst_index);

        switch (std.meta.activeTag(hir_inst)) {
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
        defer trace.end(.{});
        const hir_inst = getHirInst(self.builder.hir, self.hir_inst_index).global_decl;
        // const type_key = try self.resolveType();
        // const ty = self.builder.getType(type_key) orelse std.debug.panic("function type not resolved: {any}", .{type_key});

        var init_block: ?Sema.Instruction.Index = null;
        if (hir_inst.init) |init_inst| {
            var scope = Scope.init(self, self.builder.allocator);
            defer scope.deinit();
            var block = try scope.makeBlock(init_inst);
            // var param_iter = self.data.function_declaration.parameters.iterator();

            // while (param_iter.next()) |param_entry| {
            //     var param_entity = self.builder.getEntity(param_entry.value_ptr.*);
            //     const param_type = try param_entity.resolveType();
            //     _ = try block.pushInstruction(param_entity.hir_inst_index, .{
            //         .op = .param,
            //         .type = param_type,
            //         .value = Sema.Value.simple(.exec_time),
            //         .data = .void,
            //     });
            // }
            try block.handleBlockInstructionInner(init_inst);

            // init_block = try block.computeInstructionsBlock();
            const block_inst = try block.commit();

            init_block = block_inst;
        }

        return self.builder.internValueData(.{
            .function = .{
                .type = try self.resolveType(),
                .init = init_block,
            },
        });
    }
    pub fn resolveBlock(self: *Entity) Error!Block {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "resolveBlock", "{s}.resolveBlock", .{
                try self.formatKey(),
            } },
            .{},
        );
        defer trace.end(.{});
    }

    pub fn resolveGlobalTypeValue(self: *Entity) Error!Sema.Value.Key {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "resolveGlobalTypeValue", "{s}.resolveGlobalTypeValue", .{
                try self.formatKey(),
            } },
            .{},
        );
        defer trace.end(.{});
        const hir_inst = getHirInst(self.builder.hir, self.hir_inst_index).global_decl;

        const init_inst = hir_inst.init orelse std.debug.panic("global type declaration has no init", .{});
        // if (hir_inst.init) |init_inst| {
        var scope = Scope.init(self, self.builder.allocator);
        defer scope.deinit();
        var block = try scope.makeBlock(init_inst);
        try block.handleBlockInstructionInner(init_inst);
        const block_inst = try block.commit();

        const inst = self.builder.instructions.items[block_inst];
        return inst.value;
        // return self.builder.internValueData(.{
        //     .global = .{
        //         .type = inst.type,
        //         .value = inst.value,
        //         .init = block_inst,
        //     },
        // });
    }
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

        // const struct_decl_hir = getHirInst(self.builder.hir, self.hir_inst_index).struct_decl;

        var iter = self.data.module_declaration.fields.iterator();
        while (iter.next()) |field| {
            var field_entity = self.builder.getEntity(field.value_ptr.*.entity);
            const ty = try field_entity.resolveType();
            try fields_list.append(ty.encode());
        }
        const struct_ty = try self.builder.internTypeData(.{
            .@"struct" = .{
                .fields = try fields_list.commit(),
            },
        });
        const module_type = try self.builder.internTypeData(.{
            .module = .{
                .entity = self.key,
                .struct_type = struct_ty,
            },
        });

        return try self.builder.internValueData(.{
            .type = module_type,
        });
        // const  try self.builder.internValueData(.{
        //     .type = struct_ty,
        // });
        // const fields_list = self.builder.getHirList(struct_decl_hir.fields_list);
        // for (fields_list) |field_inst_index| {
        //     const field_hir = getHirInst(self.builder.hir, field_inst_index).struct_field;
        //     const field_entity = self.builder.getEntity(field_hir.name_node);
        //     _ = try field_entity.resolveValue();
        // }
        // std.debug.panic("TODO: resolveModuleValue {s}", .{@tagName(self.data)});
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

        // try self.collectEntities();
        // const declaration_index = self.builder.declarations.items.len;
        // try self.builder.declarations.append(self.builder.allocator, undefined);

        // const declaration = Sema.Declaration{
        //     .name = self.name,
        //     .is_export = self.is_export,
        //     .is_pub = self.is_pub,
        //     .type = try self.resolveType(),
        //     .value = try self.resolveValue(),
        // };
        // self.builder.declarations.items[declaration_index] = declaration;
        // try self.builder.declarations_by_hir_inst.put(
        //     self.builder.allocator,
        //     self.hir_inst_index,
        //     declaration_index,
        // );
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
            // .parameter_declaration => {
            //     // noop

            // },
            else => std.debug.panic("unhandled declaration: {s}", .{@tagName(self.data)}),
        }
    }
};

const Scope = struct {
    instructions: std.ArrayListUnmanaged(Sema.Instruction) = .{},
    // instructions: std.AutoArrayHashMapUnmanaged(Hir.Inst.Index, Sema.Instruction) = .{}, // allocator: std.mem.Allocator,
    instructions_by_hir_inst: std.AutoArrayHashMapUnmanaged(Hir.Inst.Index, Sema.Instruction.Index) = .{},
    arena: std.heap.ArenaAllocator,

    dependencies: std.AutoArrayHashMapUnmanaged(Entity.Key, void) = .{},

    entity: *Entity,
    builder: *Builder,
    trace: Tracer.EndTrace,
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

    pub fn makeBlock(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Block {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "makeBlock", "Scope.makeBlock({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{},
        );
        const instruction_index = try self.pushInstruction(hir_inst_index, .{
            .op = .block,
            .type = Sema.Type.simple(.void),
            .value = Sema.Value.simple(.void),
            .data = .{ .block = .{
                .instructions_count = 0,
            } },
        });
        defer trace.end(.{
            .instructions = self.instructions.items,
            .index = instruction_index,
        });
        return Block{
            .scope = self,
            .builder = self.builder,
            .root_hir_inst_index = hir_inst_index,
            .instruction_index = instruction_index,
        };
    }
    pub fn reserveInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const index = self.instructions.items.len;
        try self.instructions.append(self.arena.allocator(), undefined);
        try self.instructions_by_hir_inst.put(self.arena.allocator(), hir_inst_index, index);
        return index;
    }
    pub fn setInstruction(self: *Scope, index: Sema.Instruction.Index, instruction: Sema.Instruction) void {
        self.instructions.items[index] = instruction;
    }
    pub fn pushInstruction(self: *Scope, hir_inst_index: Hir.Inst.Index, instruction: Sema.Instruction) Error!Sema.Instruction.Index {
        const index = self.instructions.items.len;
        try self.instructions.append(self.arena.allocator(), instruction);
        try self.instructions_by_hir_inst.put(self.arena.allocator(), hir_inst_index, index);
        return index;
    }

    pub fn deinit(self: *Scope) void {
        self.trace.end(.{});

        self.arena.deinit();
    }
    // pub fn commit(self: *Scope) Error!Sema.Instruction.InstRange {
    //     defer self.deinit();
    //     const start = self.builder.instructions.items.len;
    //     const len = self.instructions.items.len;
    //     try self.builder.instructions.appendSlice(self.builder.allocator, self.instructions.items);

    //     return .{
    //         .start = start,
    //         .len = len,
    //     };
    // }

    pub fn pushDependency(self: *Scope, key: Entity.Key) !void {
        const entity = self.builder.getEntity(key);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "pushDependency", "Scope.pushDependency({s})", .{
                try entity.formatKey(),
            } },
            .{},
        );
        defer trace.end(.{});

        try self.dependencies.put(self.arena.allocator(), key, {});
        try self.builder.queueEntity(key);
    }
};

const Block = struct {
    scope: *Scope,
    builder: *Builder,

    root_hir_inst_index: Hir.Inst.Index,
    instructions: std.ArrayListUnmanaged(Sema.Instruction.Index) = .{},
    instruction_index: Sema.Instruction.Index,

    pub fn commit(self: *Block) Error!Sema.Instruction.Index {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "commit", "Block.commit", .{} },
            .{},
        );

        const inst = self.builder.instructions.items.len;
        const len = self.instructions.items.len + 1;
        self.getInstruction(self.instruction_index).data.block.instructions_count = len;
        try self.builder.instructions.appendSlice(self.builder.allocator, self.scope.instructions.items);
        defer trace.end(.{
            .start = 0,
            .len = len,
            .instructions = self.scope.instructions.items,
            .index = self.instruction_index,

            .result = self.getInstruction(self.instruction_index),
        });
        return inst;
    }
    pub fn reserveInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const index = try self.scope.reserveInstruction(hir_inst_index);
        self.builder.tracer.trace(
            @src(),
            .{ "newInstruction", "Block.reserveInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{},
        );

        try self.instructions.append(self.scope.arena.allocator(), index);
        return index;
    }
    pub fn setInstruction(self: *Block, index: Sema.Instruction.Index, instruction: Sema.Instruction) void {
        self.scope.instructions.items[index] = instruction;

        self.builder.tracer.trace(
            @src(),
            .{ "setInstruction", "Block.setInstruction(.{s}, index = {d})", .{
                @tagName(instruction.op),
                index,
            } },
            .{
                .index = index,
                .instructions = self.scope.instructions.items,
                .block_instructions = self.instructions.items,
            },
        );
    }
    pub fn pushInstruction(self: *Block, hir_inst_index: Hir.Inst.Index, instruction: Sema.Instruction) !Sema.Instruction.Index {
        const index = try self.scope.pushInstruction(hir_inst_index, instruction);
        self.builder.tracer.trace(
            @src(),
            .{ "newInstruction", "Block.pushInstruction(.{s}, hir = {d}, index = {d})", .{
                @tagName(instruction.op),
                hir_inst_index,
                index,
            } },
            .{
                .index = index,
                .instructions = self.scope.instructions.items,
                .block_instructions = self.instructions.items,
            },
        );
        try self.instructions.append(self.scope.arena.allocator(), index);
        return index;
    }

    pub fn getInstructionByHirIndex(self: *Block, hir_inst_index: Hir.Inst.Index) *Sema.Instruction {
        const index = self.scope.instructions_by_hir_inst.get(hir_inst_index).?;
        return &self.scope.instructions.items[index];
    }
    pub fn getInstruction(self: *Block, index: Sema.Instruction.Index) *Sema.Instruction {
        return &self.scope.instructions.items[index];
    }
    pub fn getInstructionIndex(self: *Block, hir_inst_index: Hir.Inst.Index) Sema.Instruction.Index {
        return self.scope.instructions_by_hir_inst.get(hir_inst_index) orelse {
            std.debug.panic("unreachable: no instruction index for hir_inst_index: {d}", .{hir_inst_index});
        };
    }

    pub fn computeInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);

        const trace = self.builder.tracer.begin(
            @src(),
            .{ "computeInstruction", "Block.computeInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .hir_inst_index = hir_inst_index,
                .hir_inst = hir_inst,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });

        return switch (hir_inst) {
            .global_get => self.handleGlobalGet(hir_inst_index),
            .comptime_number => self.handleConstantInstruction(hir_inst_index),
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
            .select_expr => self.handleSelectExprInstruction(hir_inst_index),
            .loop => self.handleLoopInstruction(hir_inst_index),
            .load => self.handleLoadInstruction(hir_inst_index),

            .param_set => self.handleParamSetInstruction(hir_inst_index),
            .br => self.handleBrInstruction(hir_inst_index),
            .ret => self.handleRetInstruction(hir_inst_index),
            .typeof => self.handleTypeOfInstruction(hir_inst_index),

            .struct_decl => self.handleStructDeclInstruction(hir_inst_index),
            .block, .inline_block => self.handleBlockInstruction(hir_inst_index),
            .get_property_pointer => self.handleGetPropertyPointerInstruction(hir_inst_index),
            .get_element_pointer => self.handleGetElementPointerInstruction(hir_inst_index),
            .fn_call => self.handleFnCallInstruction(hir_inst_index),
            .param_decl => self.handleParamDeclInstruction(hir_inst_index),
            else => std.debug.panic("unhandled hir_inst: {s}", .{@tagName(hir_inst)}),
        };
    }
    pub fn pushCastInstruction(
        self: *Block,
        hir_inst_index: Hir.Inst.Index,
        instruction_index: Sema.Instruction.Index,
        type_index: Sema.Type.Key,
    ) Error!Sema.Instruction.Index {
        const instruction = self.getInstruction(instruction_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "pushCastInstruction", "Block.pushCastInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });
        // return self.scope.instructions.put(self.scope.arena.allocator(), hir_inst_index, instruction);
        return self.pushInstruction(hir_inst_index, .{
            .op = .cast,
            .type = type_index,
            .value = blk: {
                const ty = switch (instruction.type) {
                    .simple => |simple| simple,
                    .complex => break :blk instruction.value,
                };
                const value = self.builder.getValue(instruction.value) orelse break :blk instruction.value;

                break :blk switch (value.data) {
                    .integer => |int| switch (ty) {
                        .f32, .f64 => try self.builder.internValueData(.{ .float = @floatFromInt(int) }),
                        else => instruction.value,
                    },
                    .float => |fl| switch (ty) {
                        .i8,
                        .i16,
                        .i32,
                        .i64,

                        .usize,
                        .u8,
                        .u16,
                        .u32,
                        .u64,
                        => try self.builder.internValueData(.{ .integer = @intFromFloat(fl) }),
                        else => instruction.value,
                    },
                    else => instruction.value,
                };
            },
            .data = .{ .operand = instruction_index },
        });
    }

    pub fn pushMaybeCastInstruction(
        self: *Block,
        hir_inst_index: Hir.Inst.Index,
        instruction_index: Sema.Instruction.Index,
        type_index: Sema.Type.Key,
    ) Error!Sema.Instruction.Index {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "pushMaybeCastInstruction", "Block.pushMaybeCastInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });
        const instruction = self.getInstruction(instruction_index);

        if (instruction.type.isEqual(type_index)) {
            return instruction_index;
        }
        if (type_index.isEqual(Sema.Type.simple(.number))) {
            return instruction_index;
        }

        if (instruction.type.isEqual(Sema.Type.simple(.number))) {
            return self.pushCastInstruction(hir_inst_index, instruction_index, type_index);
        }

        std.debug.panic("TODO: pushMaybeCastInstruction {s} to {s}", .{ @tagName(instruction.type), @tagName(type_index) });
    }
    pub fn getInstructionAsType(self: *Block, hir_inst_index: Hir.Inst.Index, type_index: Sema.Type.Key) Error!Sema.Instruction.Index {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "getInstructionAsType", "Block.getInstructionAsType({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });
        const instruction_index = self.getInstructionIndex(hir_inst_index);

        return self.pushMaybeCastInstruction(
            hir_inst_index,
            instruction_index,
            type_index,
        );
    }

    pub fn handleGlobalGet(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const global_entity = self.builder.getEntityByHirInst(hir_inst.global_get.operand);

        return self.pushGlobalGetInstruction(hir_inst_index, global_entity.key);
        // const global_type = try global_entity.resolveType();
        // // const global_value = try global_entity.resolveValue();
        // // const global_value = try global_entity.resolveValue();
        // // return self.pushInstruction(hir_inst_index, .{
        // //     .op = .global_get,
        // //     .type = global_value.type,
        // //     .value = global_value,
        // //     .data = .void,
        // // });
        // // std.debug.panic("TODO: handleGlobalGet {s}", .{@tagName(hir_inst)});
        // //   const hir_inst = getHirInst(self.builder.hir, hir_inst_index);

        // // const operand_inst_index = self.getInstructionIndex(hir_inst.param_get.operand);
        // // const operand_inst = self.getInstruction(operand_inst_index);
        // // const param_type = self.builder.getType(operand_inst.type) orelse {
        // // //     std.debug.panic("unreachable: should get a param type", .{});
        // // // };

        // switch (global_entity.data) {
        //     .function_declaration => |fn_decl| {
        //         // global_entity.data.function_declaration.declaration_index,
        //         return self.pushInstruction(hir_inst_index, .{
        //             .op = .global_get,
        //             .type = global_type,
        //             .value = Sema.Value.simple(.exec_time),
        //             .data = .{ .declaration = fn_decl.declaration_index },
        //         });
        //     },
        //     .global_type_declaration => |type_decl| {
        //         const global_key = try global_entity.resolveValue();
        //         // const global = self.builder.getValue(global_key) orelse {
        //         //     std.debug.panic("unreachable: should get a global value", .{});
        //         // };

        //         return self.pushInstruction(hir_inst_index, .{
        //             .op = .global_get,
        //             .type = global_type,
        //             .value = global_key,
        //             .data = .{ .declaration = type_decl.declaration_index },
        //         });
        //     },
        //     else => std.debug.panic("unhandled global_entity: {s}", .{@tagName(global_entity.data)}),
        // }
        // // const declaration = self.builder.declarations.items[declaration_index];

    }
    pub fn pushGlobalGetInstruction(self: *Block, hir_inst_index: Hir.Inst.Index, entity_key: Entity.Key) Error!Sema.Instruction.Index {
        const entity = self.builder.getEntity(entity_key);
        try self.scope.pushDependency(entity_key);
        const global_type = try entity.resolveType();
        switch (entity.data) {
            .function_declaration => |fn_decl| {
                // global_entity.data.function_declaration.declaration_index,
                return self.pushInstruction(hir_inst_index, .{
                    .op = .global_get,
                    .type = global_type,
                    .value = Sema.Value.simple(.exec_time),
                    .data = .{ .declaration = fn_decl.declaration_index },
                });
            },
            .global_type_declaration => |type_decl| {
                const global_key = try entity.resolveValue();
                // const global = self.builder.getValue(global_key) orelse {
                //     std.debug.panic("unreachable: should get a global value", .{});
                // };

                return self.pushInstruction(hir_inst_index, .{
                    .op = .global_get,
                    .type = global_type,
                    .value = global_key,
                    .data = .{ .declaration = type_decl.declaration_index },
                });
            },
            else => std.debug.panic("unhandled global_entity: {s}", .{@tagName(entity.data)}),
        }
    }
    pub fn handleConstantIntInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const value = hir_inst.constant_int.value;
        // const slice = self.builder.getNodeSlice(value);
        // const int = try std.fmt.parseInt(i64, slice, 10);
        return self.pushInstruction(hir_inst_index, .{
            .op = .constant,
            .type = Sema.Type.simple(.number),
            .value = try self.builder.internValueData(.{ .integer = value }),
            .data = .void,
        });
    }
    pub fn handleConstantInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleConstantNumber", "Block.handleConstantNumber({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .hir_inst_index = hir_inst_index,
                .hir_inst = hir_inst,
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });

        switch (hir_inst) {
            .comptime_number => |ast_node| {
                const slice = self.builder.getNodeSlice(ast_node.node);
                const is_float = std.mem.indexOf(u8, slice, ".") != null;
                const value: Sema.Value.Data = blk: {
                    if (is_float) {
                        break :blk .{
                            .float = try std.fmt.parseFloat(f64, slice),
                        };
                    } else {
                        break :blk .{ .integer = try std.fmt.parseInt(i64, slice, 10) };
                    }
                };

                const value_index = try self.builder.internValueData(value);
                return self.pushInstruction(hir_inst_index, .{
                    .op = .constant,
                    .type = Sema.Type.simple(.number),
                    .value = value_index,
                    .data = .void,
                });
            },
            else => std.debug.panic("unhandled hir_inst: {s}", .{@tagName(hir_inst)}),
        }
    }

    pub fn handleTypeLiteralInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleTypeLiteralInstruction", "Block.handleTypeLiteralInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .hir_inst_index = hir_inst_index,
                .hir_inst = hir_inst,
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });

        const type_value_index = switch (std.meta.activeTag(hir_inst)) {
            inline .ty_i8,
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
            .ty_boolean,
            => |tag| Sema.Type.simple(std.meta.stringToEnum(Sema.Type.Simple, (comptime @tagName(tag)[3..])) orelse {
                return std.debug.panic("not implemented: resolveTypeLiteralInstruction '{s}'", .{@tagName(tag)});
            }),
            .ty_array => ty: {
                const ty_array = hir_inst.ty_array;
                const type_inst_id = self.getInstructionIndex(ty_array.type);
                const type_inst = self.getInstruction(type_inst_id);
                const size_inst_id = self.getInstructionIndex(ty_array.size);
                const size_inst = self.getInstruction(size_inst_id);
                const size = size_inst.value;
                const size_value = self.builder.getValue(size) orelse std.debug.panic("Error: size_value is not a number", .{});
                const size_int = getNumberValueAs(u32, size_value);
                const type_value_index = type_inst.value;

                const type_index = try self.builder.internTypeData(.{ .array = .{
                    .child = self.builder.unwrapTypeValue(type_value_index),
                    .size = size_int,
                } });
                break :ty type_index;
            },

            else => unreachable,
        };

        return self.pushInstruction(
            hir_inst_index,
            .{
                .op = .type,
                .type = Sema.Type.simple(.type),
                .value = try self.builder.internValueData(.{ .type = type_value_index }),
                .data = .void,
            },
        );
    }
    pub fn handleAllocInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleAllocInstruction", "Block.handleAllocInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });

        const type_inst_index = self.getInstructionIndex(hir_inst.alloc.type);
        const type_inst = self.getInstruction(type_inst_index);

        const type_to_alloc = self.builder.unwrapTypeValue(type_inst.value);

        return self.pushInstruction(hir_inst_index, .{
            .op = .alloc,
            .type = try self.builder.internTypeData(.{
                .pointer = .{
                    .child = type_to_alloc,
                },
            }),
            .value = Sema.Value.simple(.exec_time),
            .data = .{ .alloc = .{
                .type_inst = type_inst_index,
                .mutable = hir_inst.alloc.mutable,
            } },
        });
    }
    pub fn handleStoreInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleStoreInstruction", "Block.handleStoreInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });

        const pointer_inst_index = self.getInstructionIndex(hir_inst.store.pointer);
        const pointer_inst = self.getInstruction(pointer_inst_index);
        const type_to_store = self.builder.unwrapPointerType(pointer_inst.type) orelse {
            std.debug.panic("expected type not found", .{});
        };

        const value_inst_index = try self.getInstructionAsType(
            hir_inst.store.value,
            type_to_store,
        );

        return self.pushInstruction(hir_inst_index, .{
            .op = .store,
            .type = Sema.Type.simple(.void),
            .value = Sema.Value.simple(.void),
            .data = .{ .operand_payload = .{
                .operand = pointer_inst_index,
                .payload = value_inst_index,
            } },
        });
    }
    pub fn handleParamGetInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleParamGetInstruction", "Block.handleParamGetInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });
        const operand_inst_index = self.getInstructionIndex(hir_inst.param_get.operand);
        const operand_inst = self.getInstruction(operand_inst_index);
        // const param_type = self.builder.getType(operand_inst.type) orelse {
        // //     std.debug.panic("unreachable: should get a param type", .{});
        // // };

        return self.pushInstruction(hir_inst_index, .{
            .op = .param_get,
            .type = operand_inst.type,
            .value = operand_inst.value,
            .data = .{ .operand = operand_inst_index },
        });
    }
    pub fn handleComparisonInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleComparisonInstruction", "Block.handleComparisonInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
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

        if (lhs_inst.type.isEqual(Sema.Type.simple(.number)) and !rhs_inst.type.isEqual(Sema.Type.simple(.number))) {
            lhs_index = try self.pushMaybeCastInstruction(hir_inst_index, lhs_index, rhs_inst.type);
        } else if (rhs_inst.type.isEqual(Sema.Type.simple(.number)) and !lhs_inst.type.isEqual(Sema.Type.simple(.number))) {
            rhs_index = try self.pushMaybeCastInstruction(hir_inst_index, rhs_index, lhs_inst.type);
        }

        return self.pushInstruction(hir_inst_index, .{
            .op = op,
            .type = Sema.Type.simple(.bool),
            .value = Sema.Value.simple(.exec_time),
            .data = .{ .bin_op = .{
                .lhs = lhs_index,
                .rhs = rhs_index,
            } },
        });
    }
    pub fn handleIfExprInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleIfExprInstruction", "Block.handleIfExprInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });
        // const then_is_inline = switch (getHirInst(self.builder.hir, hir_inst.if_expr.then_body)) {
        //     .inline_block => true,
        //     else => false,
        // };
        // const else_is_inline = if (hir_inst.if_expr.else_body) |else_body| switch (getHirInst(self.builder.hir, else_body)) {
        //     .inline_block => true,
        //     else => false,
        // } else false;
        // if (then_is_inline and else_is_inline) {
        //     const condition = self.getInstructionIndex(hir_inst.if_expr.cond);
        //     const then_block_index = try self.computeInstruction(hir_inst.if_expr.then_body);
        //     const else_block_index = try self.computeInstruction(hir_inst.if_expr.else_body.?);
        //     const then_block = self.getInstruction(then_block_index);
        //     const else_block = self.getInstruction(else_block_index);
        //     if (!then_block.type.isEqual(else_block.type)) {
        //         std.debug.panic("error: phi argument types are not equal", .{});
        //     }
        //     return try self.pushInstruction(hir_inst_index, .{
        //         .op = .select,
        //         .type = then_block.type,
        //         .value = Sema.Value.simple(.exec_time),
        //         .data = .{ .select = .{ .condition = condition, .then_block = then_block_index, .else_block = else_block_index } },
        //     });
        // }

        const index = try self.reserveInstruction(hir_inst_index);
        const condition = self.getInstructionIndex(hir_inst.if_expr.cond);

        const then_block_index = try self.computeInstruction(hir_inst.if_expr.then_body);
        const else_block_index = if (hir_inst.if_expr.else_body) |else_body| try self.computeInstruction(else_body) else null;

        self.setInstruction(index, .{
            .op = .@"if",
            .type = Sema.Type.simple(.void),
            .value = Sema.Value.simple(.void),
            .data = .{ .@"if" = .{
                .condition = condition,
                .then_block = then_block_index,
                .else_block = else_block_index,
            } },
        });

        return index;
    }

    pub fn handleSelectExprInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleSelectExprInstruction", "Block.handleSelectExprInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });

        const condition = self.getInstructionIndex(hir_inst.select_expr.cond);
        const then_block_index = try self.computeInstruction(hir_inst.select_expr.then_body);
        const else_block_index = try self.computeInstruction(hir_inst.select_expr.else_body.?);
        const then_block = self.getInstruction(then_block_index);
        const else_block = self.getInstruction(else_block_index);
        if (!then_block.type.isEqual(else_block.type)) {
            std.debug.panic("error: 'select' argument types are not equal", .{});
        }

        return self.pushInstruction(hir_inst_index, .{
            .op = .select,
            .type = then_block.type,
            .value = Sema.Value.simple(.exec_time),
            .data = .{ .select = .{ .condition = condition, .then_block = then_block_index, .else_block = else_block_index } },
        });
    }
    pub fn handleLoopInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleLoopInstruction", "Block.handleLoopInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });

        const index = try self.reserveInstruction(hir_inst_index);

        // var body_inst_range = Sema.Instruction.InstRange{
        //     .start = self.scope.instructions.items.len,
        //     .len = 0,
        // };
        // var body_block = try self.scope.makeBlock(hir_inst.loop.body);
        // try body_block.computeInstructionsBlock();
        // body_inst_range.len = self.scope.instructions.items.len - body_inst_range.start;

        self.setInstruction(index, .{
            .op = .loop,
            .type = Sema.Type.simple(.void),
            .value = Sema.Value.simple(.void),
            .data = .{
                .loop = .{
                    .body_block = try self.computeInstruction(hir_inst.loop.body),
                },
            },
        });

        return index;
    }

    pub fn handleLoadInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleLoadInstruction", "Block.handleLoadInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });

        const pointer_inst_index = self.getInstructionIndex(hir_inst.load.operand);
        const pointer_inst = self.getInstruction(pointer_inst_index);
        switch (pointer_inst.op) {
            .constant => {
                return try self.pushInstruction(hir_inst_index, .{
                    .op = pointer_inst.op,
                    .type = pointer_inst.type,
                    .value = pointer_inst.value,
                    .data = pointer_inst.data,
                });
            },
            else => {},
        }

        const type_to_load = self.builder.unwrapPointerType(pointer_inst.type) orelse {
            std.debug.panic("expected type not found", .{});
        };

        switch (pointer_inst.op) {
            .constant => {
                return try self.pushInstruction(hir_inst_index, .{
                    .op = pointer_inst.op,
                    .type = pointer_inst.type,
                    .value = pointer_inst.value,
                    .data = pointer_inst.data,
                });
            },
            else => {},
        }

        return self.pushInstruction(hir_inst_index, .{
            .op = .load,
            .type = type_to_load,
            .value = Sema.Value.simple(.exec_time),
            .data = .{ .operand = pointer_inst_index },
        });
    }

    pub fn handleArithmeticInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleArithmeticInstruction", "Block.handleArithmeticInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
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
        var ty = lhs_inst.type;

        if (lhs_inst.type.isEqualSimple(.number) and !rhs_inst.type.isEqualSimple(.number)) {
            lhs_index = try self.pushMaybeCastInstruction(hir_inst_index, lhs_index, rhs_inst.type);
            ty = rhs_inst.type;
        } else if (rhs_inst.type.isEqualSimple(.number) and !lhs_inst.type.isEqualSimple(.number)) {
            rhs_index = try self.pushMaybeCastInstruction(hir_inst_index, rhs_index, lhs_inst.type);
        }

        return self.pushMaybeFoldInstruction(hir_inst_index, .{
            .op = op,
            .type = ty,
            // .value = try self.builder.maybeFoldArithmetic(op, lhs_index, rhs_index),
            .value = Sema.Value.simple(.exec_time),
            .data = .{ .bin_op = .{
                .lhs = lhs_index,
                .rhs = rhs_index,
            } },
        });
    }

    pub fn handleParamSetInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleParamSetInstruction", "Block.handleParamSetInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });

        const lhs_index = self.getInstructionIndex(hir_inst.param_set.lhs);
        const rhs_index = self.getInstructionIndex(hir_inst.param_set.rhs);
        // const lhs_inst = self.getInstruction(lhs_index);
        // const rhs_inst = self.getInstruction(rhs_index);
        // const param_inst = self.getInstruction(param_index);
        // _ = param_inst; // autofix
        // const value_index = self.getInstructionIndex(hir_inst.param_set.rhs);
        // _ = value_inst; // autofix

        return try self.pushInstruction(hir_inst_index, .{
            .op = .param_set,
            .type = Sema.Type.simple(.void),
            .value = Sema.Value.simple(.void),
            .data = .{ .operand_payload = .{
                .operand = lhs_index,
                .payload = rhs_index,
            } },
        });
    }

    pub fn handleBrInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleBrInstruction", "Block.handleBrInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });

        const target_index = self.getInstructionIndex(hir_inst.br.target);
        const target_inst = self.getInstruction(target_index);
        if (hir_inst.br.operand) |operand| {
            const operand_index = self.getInstructionIndex(operand);
            const operand_inst = self.getInstruction(operand_index);
            target_inst.type = operand_inst.type;
            target_inst.value = operand_inst.value;
            // std.debug.panic("{}\n\n{d} = {any}\n\n{d} = {any}\n\n", .{
            //     hir_inst.br,
            //     operand_index,
            //     operand_inst,
            //     target_index,
            //     target_inst,
            // });
        }
        // target_inst.type =

        return self.pushInstruction(hir_inst_index, .{
            .op = .br,
            .type = Sema.Type.simple(.void),
            .value = Sema.Value.simple(.void),
            .data = .{
                .operand = target_index,
            },
        });
    }

    pub fn handleRetInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleRetInstruction", "Block.handleRetInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });

        return self.pushInstruction(hir_inst_index, .{
            .op = .ret,
            .type = Sema.Type.simple(.void),
            .value = Sema.Value.simple(.void),
            .data = .{
                .maybe_operand = if (hir_inst.ret.value) |operand| self.getInstructionIndex(operand) else null,
            },
        });
    }
    pub fn handleTypeOfInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleTypeOfInstruction", "Block.handleTypeOfInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const operand_index = self.getInstructionIndex(hir_inst.typeof.operand);
        const operand_inst = self.getInstruction(operand_index);
        return self.pushInstruction(hir_inst_index, .{
            .op = .typeof,
            .type = Sema.Type.simple(.type),
            .value = try self.builder.internValueData(.{ .type = operand_inst.type }),
            .data = .{ .operand = operand_index },
        });
    }

    pub fn handleStructDeclInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        _ = hir_inst; // autofix
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleStructDeclInstruction", "Block.handleStructDeclInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });

        // hir_inst.struct_decl.fields_list;
        const entity_key = try self.builder.makeEntity(.{
            .parent = self.scope.entity.key,
            .hir_inst_index = hir_inst_index,
            .name = try self.builder.internSlice("mod"),
            .data = .{
                .module_declaration = .{
                    // .name = try self.builder.internSlice("mod"),
                    // .exports = .{},
                },
            },

            // .name =

            // .kind = .struct,
        });
        var entity = self.builder.getEntity(entity_key);
        // const ty = try entity.resolveValue();

        // std.debug.panic("todo", .{});
        return self.pushInstruction(hir_inst_index, .{
            .op = .type,
            .type = Sema.Type.simple(.type),
            .value = try entity.resolveValue(),
            .data = .void,
        });
    }

    pub fn handleBlockInstructionInner(self: *Block, hir_inst_index: Hir.Inst.Index) Error!void {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const list_index = switch (hir_inst) {
            .block, .inline_block => |list_inst| list_inst.instructions_list,
            else => std.debug.panic("unhandled hir_inst: {s}", .{@tagName(hir_inst)}),
        };
        const list = self.builder.getHirList(list_index);
        for (list) |block_inst| {
            _ = try self.computeInstruction(block_inst);
        }
    }
    pub fn handleBlockInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleBlockInstruction", "Block.handleBlockInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });

        // var block =try  self.scope.makeBlock(hir_inst_index);
        const start = self.scope.instructions.items.len;
        const index = try self.reserveInstruction(hir_inst_index);
        self.setInstruction(index, .{
            .op = .block,
            .type = Sema.Type.simple(.void),
            .value = Sema.Value.simple(.void),
            .data = .{ .block = .{ .instructions_count = 0 } },
        });
        try self.handleBlockInstructionInner(hir_inst_index);
        // var body_inst_range = Sema.Instruction.InstRange{
        //     .start = self.scope.instructions.items.len,
        //     .len = 0,
        // };

        // const list_index = switch (hir_inst) {
        //     .block, .inline_block => |list_inst| list_inst.instructions_list,
        //     else => std.debug.panic("unhandled hir_inst: {s}", .{@tagName(hir_inst)}),
        // };
        // const list = self.builder.getHirList(list_index);
        // for (list) |block_inst| {
        //     _ = try self.computeInstruction(block_inst);
        // }
        self.getInstruction(index).data.block.instructions_count = self.scope.instructions.items.len - start;

        return index;
    }

    pub fn handleBuiltinPropertyAccessInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleBuiltinPropertyAccessInstruction", "Block.handleBuiltinPropertyAccessInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });
        const base_hir_index = hir_inst.get_property_pointer.base;
        const property_name_range = try self.builder.internNode(hir_inst.get_property_pointer.property_name_node);
        const property_name_slice = self.builder.getSlice(property_name_range);

        const base_index = self.getInstructionIndex(base_hir_index);
        const base_instruction = self.getInstruction(base_index);
        if (self.builder.getType(base_instruction.type)) |base_type| {
            std.debug.panic("todo {s}", .{@tagName(base_type.data)});
            // switch (base_type.data) {

            //     .array => |array_type| {
            //         _ = array_type; // autofix
            //     },
            //     else => {},
            // }
        }

        // var a: u16 = 0xabcd; // runtime-known
        // _ = &a;
        // const b: u8 = @intCast(a);
        // _ = b; // autofix

        switch (base_instruction.type.simple) {
            .usize,
            .i8,
            .i16,
            .i32,
            .i64,
            .u8,
            .u16,
            .u32,
            .u64,
            .f32,
            .f64,
            => {
                if (std.mem.eql(u8, property_name_slice, "as")) {
                    return try self.pushInstruction(hir_inst_index, .{
                        .op = .get_builtin_fn_as,
                        .type = Sema.Type.simple(.builtin_fn_as),
                        .value = Sema.Value.simple(.type_builtin_fn_as),
                        .data = .{ .operand = base_index },
                    });
                }
                std.debug.panic("todo {s}", .{property_name_slice});
            },
            else => |tag| std.debug.panic("unhandled base type: {s}", .{@tagName(tag)}),
        }
        std.debug.panic("unhandled base type", .{});
    }
    pub fn handleGetPropertyPointerInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleGetPropertyPointerInstruction", "Block.handleGetPropertyPointerInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });
        const is_builtin = hir_inst.get_property_pointer.is_builtin;
        if (is_builtin) {
            return try self.handleBuiltinPropertyAccessInstruction(hir_inst_index);
        }
        const base_hir_index = hir_inst.get_property_pointer.base;
        const property_name_range = try self.builder.internNode(hir_inst.get_property_pointer.property_name_node);
        const property_name_slice = self.builder.getSlice(property_name_range);

        if (self.builder.getEntityKeyByHirInst(base_hir_index)) |base_entity_key| {
            // Is referecing a global
            const base_entity = self.builder.getEntity(base_entity_key);
            const base_type = try base_entity.resolveType();
            if (base_type.isEqualSimple(.type)) {
                const value = try base_entity.resolveValue();
                const module_type_key = self.builder.unwrapTypeValue(value);
                const module_type = self.builder.getType(module_type_key) orelse {
                    std.debug.panic("unreachable: should get a base type", .{});
                };
                const module = self.builder.getEntity(module_type.data.module.entity);
                const declaration_entity_key = module.data.module_declaration.declarations.get(property_name_range) orelse {
                    std.debug.panic("unreachable: should get a declaration", .{});
                };
                return try self.pushGlobalGetInstruction(hir_inst_index, declaration_entity_key);
            }
        }

        const base_index = self.getInstructionIndex(base_hir_index);
        const base_instruction = self.getInstruction(base_index);
        const unwrapped_base_type_key = self.builder.unwrapPointerType(base_instruction.type) orelse {
            std.debug.panic("unreachable: should get a pointer type {}", .{(base_instruction)});
        };

        const base_type = self.builder.getType(unwrapped_base_type_key) orelse {
            std.debug.panic("unreachable: should get a base type", .{});
        };
        switch (base_type.data) {
            .array => |array_type| {
                if (!std.mem.eql(u8, property_name_slice, "len")) std.debug.panic("error: array property {s} doesn't exist", .{property_name_slice});

                return try self.pushInstruction(hir_inst_index, .{
                    .op = .constant,
                    .type = Sema.Type.simple(.usize),
                    .value = try self.builder.internValueData(.{ .integer = array_type.size }),
                    .data = .void,
                });
            },
            else => {},
        }
        const module = self.builder.getEntity(base_type.data.module.entity);
        if (module.data.module_declaration.fields.get(property_name_range)) |field| {
            const field_entity = self.builder.getEntity(field.entity);
            return self.pushInstruction(hir_inst_index, .{
                .op = .get_element_pointer,
                .type = try self.builder.internTypeData(.{ .pointer = .{ .child = try field_entity.resolveType() } }),
                .value = Sema.Value.simple(.exec_time),
                .data = .{ .get_element_pointer = .{
                    .base = base_index,
                    .index = field.index,
                } },
            });
        }
        if (module.data.module_declaration.declarations.get(property_name_range)) |declaration| {
            // const declaration_entity = self.builder.getEntity(declaration.entity);
            return try self.pushGlobalGetInstruction(hir_inst_index, declaration);
        }
        std.debug.panic("unreachable: should get a field or a declaration", .{});
    }
    pub fn handleGetElementPointerInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const base_index = self.getInstructionIndex(hir_inst.get_element_pointer.base);
        const base_instruction = self.getInstruction(base_index);

        const base_type_key = self.builder.unwrapPointerType(base_instruction.type) orelse {
            std.debug.panic("unreachable: should get a pointer type {}", .{(base_instruction)});
        };
        const base_type = self.builder.getType(base_type_key) orelse {
            std.debug.panic("unreachable: should get a base type", .{});
        };
        const element_type = base_type.data.array.child;
        const index_inst = self.getInstructionIndex(hir_inst.get_element_pointer.index);
        return self.pushInstruction(hir_inst_index, .{
            .op = .get_element_pointer,
            .type = try self.builder.internTypeData(.{ .pointer = .{ .child = element_type } }),
            .value = Sema.Value.simple(.exec_time),
            .data = .{ .get_element_pointer = .{
                .base = base_index,
                .index = index_inst,
            } },
        });
    }
    pub fn handleBuiltinCastCall(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);

        const callee_inst_index = self.getInstructionIndex(hir_inst.fn_call.callee);
        const callee_inst = self.getInstruction(callee_inst_index);
        const lhs_inst_index = callee_inst.data.operand;
        const lhs_inst = self.getInstruction(lhs_inst_index);
        const args = self.builder.getHirList(hir_inst.fn_call.args_list);
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

        if (lhs_is_float != rhs_is_float) {
            // std.debug.panic("error: cannot cast {s} to {s}", .{ @tagName(lhs_type.simple), @tagName(rhs_type.simple) });
            if (lhs_is_float) {
                return try self.pushInstruction(hir_inst_index, .{
                    .op = .cast_truncate,
                    .type = rhs_type,
                    .value = lhs_inst.value,
                    .data = .{ .operand = lhs_inst_index },
                });
            } else {
                return try self.pushInstruction(hir_inst_index, .{
                    .op = .cast_convert,
                    .type = rhs_type,
                    .value = lhs_inst.value,
                    .data = .{ .operand = lhs_inst_index },
                });
            }
        }

        switch (lhs_type.simple) {
            .f32, .f64 => {
                // const lhs_bits = self.builder.numberBits(lhs_type);
                // const rhs_bits = self.builder.numberBits(rhs_type);
                if (rhs_bits > lhs_bits) {
                    return try self.pushInstruction(hir_inst_index, .{
                        .op = .cast_promote,
                        .type = rhs_type,
                        .value = lhs_inst.value,
                        .data = .{ .operand = lhs_inst_index },
                    });
                }

                return try self.pushInstruction(hir_inst_index, .{
                    .op = .cast_demote,
                    .type = rhs_type,
                    .value = lhs_inst.value,
                    .data = .{ .operand = lhs_inst_index },
                });
            },
            .u8, .u16, .u32, .u64, .usize => {
                // const lhs_bits = self.builder.numberBits(lhs_type);
                // const rhs_bits = self.builder.numberBits(rhs_type);
                const rhs_signed = self.builder.isSigned(rhs_type);
                if (rhs_signed) {
                    return try self.pushInstruction(hir_inst_index, .{
                        .op = .cast_reinterpret,
                        .type = rhs_type,
                        .value = lhs_inst.value,
                        .data = .{ .operand = lhs_inst_index },
                    });
                }
                if (rhs_bits > lhs_bits) {
                    return try self.pushInstruction(hir_inst_index, .{
                        .op = .cast_extend,
                        .type = rhs_type,
                        .value = lhs_inst.value,
                        .data = .{ .operand = lhs_inst_index },
                    });
                }

                return try self.pushInstruction(hir_inst_index, .{
                    .op = .cast_wrap,
                    .type = rhs_type,
                    .value = lhs_inst.value,
                    .data = .{ .operand = lhs_inst_index },
                });
            },

            .i8, .i16, .i32, .i64 => {
                // const lhs_bits = self.builder.numberBits(lhs_type);
                // const rhs_bits = self.builder.numberBits(rhs_type);
                const rhs_signed = self.builder.isSigned(rhs_type);
                if (!rhs_signed) {
                    return try self.pushInstruction(hir_inst_index, .{
                        .op = .cast_reinterpret,
                        .type = rhs_type,
                        .value = lhs_inst.value,
                        .data = .{ .operand = lhs_inst_index },
                    });
                }

                if (rhs_bits > lhs_bits) {
                    return try self.pushInstruction(hir_inst_index, .{
                        .op = .cast_extend,
                        .type = rhs_type,
                        .value = lhs_inst.value,
                        .data = .{ .operand = lhs_inst_index },
                    });
                }

                return try self.pushInstruction(hir_inst_index, .{
                    .op = .cast_wrap,
                    .type = rhs_type,
                    .value = lhs_inst.value,
                    .data = .{ .operand = lhs_inst_index },
                });
            },

            // .number => {},
            else => {
                std.debug.panic("error: cannot cast {s} to {s}", .{ @tagName(lhs_type.simple), @tagName(rhs_type.simple) });
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
    pub fn handleFnCallInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const trace = self.builder.tracer.begin(
            @src(),
            .{ "handleFnCallInstruction", "Block.handleFnCallInstruction({s})", .{
                try formatHirIndex(self.builder.hir, hir_inst_index),
            } },
            .{
                .before = self.scope.instructions.items,
            },
        );
        defer trace.end(.{
            .instructions = self.scope.instructions.items,
        });

        const callee_inst = self.getInstructionIndex(hir_inst.fn_call.callee);
        const callee_instruction = self.getInstruction(callee_inst);

        // switch (callee_instruction.type) {
        //     .simple => |simple| switch (simple) {
        //         .builtin_fn_as => {
        //             const value = self.builder.getValue(callee_instruction.value) orelse unreachable;
        //             const type_value = self.builder.unwrapTypeValue(value);
        //             const type = self.builder.getType(type_value) orelse unreachable;
        //             return try self.pushInstruction(hir_inst_index, .{
        //                 .op = .get_builtin_fn_as,
        //                 .type = Sema.Type.simple(.builtin_fn_as),
        //                 .value = Sema.Value.simple(.type_builtin_fn_as),
        //                 .data = .{ .operand = callee_inst },
        //             });
        //         },
        //         else => {},
        //     },
        //     else => {},
        // }

        const callee_type = self.builder.getType(callee_instruction.type) orelse {
            switch (callee_instruction.type.simple) {
                .builtin_fn_as => {
                    return try self.handleBuiltinCastCall(hir_inst_index);
                    // if (args.len != 1) {
                    //     std.debug.panic("error: builtin_fn_as expects 1 type argument", .{});
                    // }

                    // const arg_inst = self.getInstruction(self.getInstructionIndex(args[0]));
                    // const arg_type = self.builder.unwrapTypeValue(arg_inst.value);
                    // const callee_type = callee_instruction.type;
                    // if (callee_type.isEqual(arg_type)) {
                    //     std.debug.panic("error: unnecessary cast", .{});
                    // }

                    // std.debug.panic("unhandled callee type: {s}", .{@tagName(callee_instruction.type.simple)});
                },
                else => {
                    std.debug.panic("unhandled callee type: {s}", .{@tagName(callee_instruction.type.simple)});
                },
            }
        };
        const args = self.builder.getHirList(hir_inst.fn_call.args_list);

        const function = switch (callee_type.data) {
            .function => |fn_type| fn_type,
            else => std.debug.panic("error: trying to call non function type", .{}),
        };
        const param_types = self.builder.lists.getSlice(function.params);
        // const arg_types = function.
        var args_list = self.builder.newList();
        if (args.len != param_types.len) {
            std.debug.panic("error: function has {d} params but {d} args were provided", .{ param_types.len, args.len });
        }
        for (args, param_types) |arg_hir_index, param_type_key| {
            const arg_index = self.getInstructionIndex(arg_hir_index);
            const param_type = Sema.Type.Key.decode(param_type_key);
            const arg_inst = self.getInstruction(arg_index);
            if (!arg_inst.type.isEqual(param_type)) {
                std.debug.panic("error: argument type mismatch", .{});
            }
            try args_list.append(arg_index);
        }
        return try self.pushInstruction(hir_inst_index, .{
            .op = .fn_call,
            .type = function.ret,
            .value = Sema.Value.simple(.exec_time),
            .data = .{ .fn_call = .{
                .callee = callee_inst,
                .args_list = try args_list.commit(),
            } },
        });
        // callee_instruction.value

        // _ = hir_inst; // autofix
        // return self.pushInstruction(hir_inst_index, .{
        //     .op = .fn_call,
        //     .type = Sema.Type.simple(.void),
        //     .value = Sema.Value.simple(.void),
        //     .data = .void,
        // });
    }
    pub fn handleParamDeclInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        // const hir_inst = getHirInst(self.builder.hir, hir_inst_index);
        const param_entity = self.builder.getEntityByHirInst(hir_inst_index);
        return self.pushInstruction(hir_inst_index, .{
            .op = .param,
            .type = try param_entity.resolveType(),
            .value = Sema.Value.simple(.exec_time),
            .data = .void,
        });
    }
    pub fn pushMaybeFoldInstruction(self: *Block, hir_inst_index: Hir.Inst.Index, data: Sema.Instruction) Error!Sema.Instruction.Index {
        switch (data.op) {
            .add, .sub, .mul, .div => return try self.pushMaybeFoldArithmetic(hir_inst_index, data),
            else => return try self.pushInstruction(hir_inst_index, data),
        }
    }
    pub fn pushMaybeFoldArithmetic(self: *Block, hir_inst_index: Hir.Inst.Index, data: Sema.Instruction) Error!Sema.Instruction.Index {
        const lhs_inst = self.getInstruction(data.data.bin_op.lhs);
        const rhs_inst = self.getInstruction(data.data.bin_op.rhs);
        if (lhs_inst.value.isEqualSimple(.exec_time) or rhs_inst.value.isEqualSimple(.exec_time)) {}
        return try self.pushInstruction(hir_inst_index, data);
    }
    pub fn maybeFoldComparison(self: *Block, op: Sema.Instruction.Op, lhs_index: Sema.Instruction.Index, rhs_index: Sema.Instruction.Index) Error!Sema.Instruction.Index {
        _ = op; // autofix
        const lhs_inst = self.getInstruction(lhs_index);
        const rhs_inst = self.getInstruction(rhs_index);

        if (lhs_inst.value.isEqualSimple(.exec_time) or rhs_inst.value.isEqualSimple(.exec_time)) {
            return Sema.Value.simple(.exec_time);
        }

        const lhs_value = self.builder.getValue(lhs_inst.value) orelse unreachable;
        _ = lhs_value; // autofix
        const rhs_value = self.builder.getValue(rhs_inst.value) orelse unreachable;
        _ = rhs_value; // autofix

        // const comparison_result = doComparison(op, lhs_value, rhs_value);
    }

    pub fn doComparison(comptime T: type, op: Sema.Instruction.Op, lhs_value: Sema.Value, rhs_value: Sema.Value) T {
        const lhs = getNumberValueAs(T, lhs_value);
        const rhs = getNumberValueAs(T, rhs_value);
        return switch (op) {
            .eq => lhs == rhs,
            .ne => lhs != rhs,
            else => unreachable,
        };
    }
};

pub fn getNumberValueAs(comptime T: type, value: Sema.Value) T {
    if (T == f64) {
        return switch (value.data) {
            .float => |f| f,
            .integer => |i| @floatFromInt(i),
            // .big_integer => |i| @floatFromInt(i),
            else => unreachable,
        };
    }
    return switch (value.data) {
        .integer => |i| @intCast(i),
        // .big_integer => |i| @intCast(i),
        .float => |f| @intFromFloat(f),
        else => unreachable,
    };
}
