const std = @import("std");
const Mir = @import("./Mir.zig");
const Logger = @import("./Logger.zig");
const host = @import("./host.zig");
const HashMap = std.AutoHashMapUnmanaged;
const ErrorManager = @import("./ErrorManager.zig");
const Ast = @import("./Ast.zig");
const HirBuilder = @import("./HirBuilder.zig");
const fmt = @import("./format_utils.zig");
const ChunkedArray = @import("./chunked_array.zig").ChunkedArray;
const ArrayHashMap = std.AutoArrayHashMapUnmanaged;
const tw = @import("./tw.zig");

const Array = std.ArrayListUnmanaged;
const Hir = @import("./Hir.zig");
const InternedSlice = @import("./InternedStrings.zig").InternedSlice;
const PackedList = @import("./PackedLists.zig");
const InstructionId = u32;
const WipArray = ChunkedArray(Wip, 4);
const assert = std.debug.assert;
const activeTag = std.meta.activeTag;
const POINTERS_TYPE = Mir.Type.Index.i32;
const Tracer = @import("./Tracer.zig");
const Color = @import("./Color.zig");
const TreeWriter = @import("./TreeWriter.zig");

pub const Builder = struct {
    wips: WipArray,
    wip_map: std.AutoHashMapUnmanaged(Mir.Type.Index, Wip.Index) = .{},
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    mir: *Mir,
    hir: *Hir,
    interned_types: std.AutoHashMapUnmanaged(u64, Mir.Type.Index) = .{},
    logger: Logger = Logger.init(host.getStdErrWriter(), "MirBuilder"),

    tracer: Tracer,
    fn getHirInst(self: *Builder, hir_index: Hir.Inst.Index) Hir.Inst {
        return self.hir.insts.items[hir_index];
    }
    // pub fn iterHirInsts(self: *Builder, list: Mir.ChildList) Hir.Lists.ListIter {
    //     return self.hir.lists.iterList(list);
    // }
    pub fn getListSlice(self: *Builder, list: Mir.List) []u32 {
        return self.hir.lists.getSlice(list);
    }
    pub fn getSlice(self: *Builder, interned_slice: InternedSlice) []const u8 {
        return self.mir.strings.getSlice(interned_slice);
    }
    fn assertHirInst(self: *Builder, hir_index: Hir.Inst.Index, tag: Hir.Inst.Tag) !void {
        const hir_inst = self.getHirInst(hir_index);
        if (std.meta.activeTag(hir_inst) != tag) {
            return self.tracer.panic("expected hir inst", .{ .expected = @tagName(tag), .actual = @tagName(std.meta.activeTag(hir_inst)) });
        }
    }
    pub fn newList(self: *Builder) Mir.WorkingList {
        return self.mir.interned_lists.new();
    }
    fn deinit(self: *Builder) void {
        self.arena.deinit();
        self.wips.deinit();
    }
    pub fn internSlice(self: *Builder, slice: []const u8) !InternedSlice {
        return try self.mir.strings.intern(slice);
    }
    pub fn internNode(self: *Builder, node: Ast.Node.Index) !InternedSlice {
        const token = self.hir.ast.getNodeStartToken(node);
        const slice = self.hir.ast.getTokenSlice(token);
        return try self.mir.strings.intern(slice);
    }
    pub inline fn getWip(self: *Builder, index: Wip.Index) *Wip {
        return self.wips.getPtr(index);
    }
    pub inline fn makeWip(self: *Builder, hir_index: Hir.Inst.Index, parent: Wip.Index, data: Wip.Data) !Wip.Index {
        const index = self.wips.len;
        try self.wips.append(try Wip.init(index, self, hir_index, parent, data));
        return index;
    }

    pub fn getWipByType(self: *Builder, type_index: Mir.Type.Index) *Wip {
        const index = self.wip_map.get(type_index) orelse {
            std.debug.panic("Builder.getWipByType wip not found for type_index: {d}", .{type_index});
        };
        return self.wips.getPtr(index);
    }

    pub inline fn makeAndGetWip(self: *Builder, hir_index: Hir.Inst.Index, parent: Wip.Index, data: Wip.Data) !*Wip {
        const index = try self.makeWip(hir_index, parent, data);
        return self.getWip(index);
    }
    pub fn makeEmptyWip(self: *Builder, hir_index: Hir.Inst.Index, parent: Wip.Index) !Wip.Index {
        return try self.makeWip(hir_index, parent, undefined);
    }
    pub fn makeType(self: *Builder, type_: Mir.Type) !Mir.Type.Index {
        const index = self.mir.types.items.len;
        self.tracer.logEvent("Builder.makeType", .{ .type = type_, .index = index });
        self.logger.log("makeType: {d} {s}", .{ index, @tagName(type_) }, null);
        try self.mir.types.append(self.mir.allocator, type_);

        return Mir.Type.Index.fromInt(@intCast(index));
    }
    pub fn pushInstruction(self: *Builder, instruction: Mir.Instruction) !Mir.Instruction.Index {
        const index: Mir.Instruction.Index = @intCast(self.mir.instructions.items.len);
        try self.mir.instructions.append(self.mir.allocator, instruction);
        self.tracer.logEvent("Builder.pushInstruction", .{ .instruction = instruction, .index = index });

        return index;
    }
    pub fn reserveInstructionIndex(self: *Builder) Mir.Instruction.Index {
        const index: Mir.Instruction.Index = @intCast(self.mir.instructions.items.len);
        return index;
    }
    pub fn setInstruction(self: *Builder, index: Mir.Instruction.Index, instruction: Mir.Instruction) void {
        self.mir.instructions.items[@intCast(index)] = instruction;
        self.tracer.logEvent("Builder.setInstruction", .{ .instruction = instruction, .index = index });
    }
    pub fn reserveTypeIndex(self: *Builder) !Mir.Type.Index {
        return try self.makeType(undefined);
    }
    pub fn setType(self: *Builder, index: Mir.Type.Index, type_: Mir.Type) void {
        self.mir.types.items[index.toInt().?] = type_;

        self.tracer.logEvent("Builder.setType", .{ .type = type_, .index = index });
    }
    pub fn getType(self: *Builder, index: Mir.Type.Index) ?*Mir.Type {
        if (index.toInt()) |int| {
            if (int >= self.mir.types.items.len) {
                self.tracer.panic("getType", .{ .index = index, .int = int });
            }
            return &self.mir.types.items[@intCast(int)];
        }
        return null;
    }
    pub fn pushType(self: *Builder, type_: Mir.Type) !Mir.Type.Index {
        const index = Mir.Type.Index.fromInt(@intCast(self.mir.types.items.len));
        // _ = try self.internType(type_);
        try self.mir.types.append(self.mir.allocator, type_);
        self.tracer.logEvent("Builder.pushType", .{ .type = type_, .index = index });
        return index;
    }
    pub fn computeTypeAlignment(self: *Builder, type_index: Mir.Type.Index) u32 {
        _ = self; // autofix
        _ = type_index; // autofix
        return 1;
    }
    const InternedResult = struct {
        index: Mir.Type.Index,
        hash: u64,
    };
    fn internTypeIndex(self: *Builder, index: Mir.Type.Index) !InternedResult {
        const event_id = self.tracer.beginEvent("Builder.internTypeIndex", .{ .index = index });
        defer self.tracer.endEvent(
            event_id,
            "Builder.internTypeIndex",
            .{ .index = index },
        );
        if (index.toInt()) |int| {
            const type_ = self.mir.types.items[@intCast(int)];
            switch (type_) {
                .@"struct" => {
                    var hasher = std.hash.Wyhash.init(0);
                    hasher.update("struct");
                    hasher.update(std.mem.asBytes(&int));

                    const hash = hasher.final();
                    const interned = try self.interned_types.getOrPut(self.arena.allocator(), hash);
                    if (interned.found_existing) {
                        return .{ .index = interned.value_ptr.*, .hash = hash };
                    }
                    const type_index = try self.pushType(type_);
                    interned.value_ptr.* = type_index;
                    return .{ .index = type_index, .hash = hash };
                },
                else => {
                    return try self.internTypeInner(type_);
                },
            }
        }
        return .{
            .index = index,
            .hash = @intFromEnum(index),
        };
    }
    fn internTypeInner(self: *Builder, ty: Mir.Type) std.mem.Allocator.Error!InternedResult {
        const event_id = self.tracer.beginEvent("Builder.internTypeInner", .{ .type = ty });
        defer self.tracer.endEvent(
            event_id,
            "Builder.internTypeInner",
            .{ .type = ty },
        );
        switch (ty) {
            .@"fn" => |fn_type| {
                var hasher = std.hash.Wyhash.init(0);
                hasher.update("fn");
                hasher.update(std.mem.asBytes(&fn_type.name));
                const params_list = self.mir.interned_lists.getSlice(fn_type.params_list);
                for (params_list) |param| {
                    const interned_param = try self.internTypeIndex(Mir.Type.Index.asTypeIndex(param));
                    hasher.update(std.mem.asBytes(&interned_param.hash));
                }
                const interned_return_type = try self.internTypeIndex(fn_type.return_type);
                hasher.update(std.mem.asBytes(&interned_return_type.hash));

                const hash = hasher.final();
                const interned = try self.interned_types.getOrPut(self.arena.allocator(), hash);
                if (interned.found_existing) {
                    // const index = try self.makeType(type_.fn_type);
                    // interned.value_ptr.* = index;
                    return .{
                        .index = interned.value_ptr.*,
                        .hash = hash,
                    };
                }
                const index = try self.pushType(ty);
                interned.value_ptr.* = index;
                return .{
                    .index = index,
                    .hash = hash,
                };
            },
            .param => |param| {
                var hasher = std.hash.Wyhash.init(0);
                hasher.update("param");
                hasher.update(std.mem.asBytes(&param.name));
                const interned_type = try self.internTypeIndex(param.type);
                hasher.update(std.mem.asBytes(&interned_type.hash));

                const hash = hasher.final();
                const interned = try self.interned_types.getOrPut(self.arena.allocator(), hash);
                if (interned.found_existing) {
                    return .{ .index = interned.value_ptr.*, .hash = hash };
                }
                const index = try self.pushType(ty);
                interned.value_ptr.* = index;
                return .{
                    .index = index,
                    .hash = hash,
                };
            },
            .optional => |optional| {
                var hasher = std.hash.Wyhash.init(0);
                hasher.update("optional");
                const interned_type = try self.internTypeIndex(optional.child);
                hasher.update(std.mem.asBytes(&interned_type.hash));
                const hash = hasher.final();

                const interned = try self.interned_types.getOrPut(self.arena.allocator(), hash);
                if (interned.found_existing) {
                    return .{ .index = interned.value_ptr.*, .hash = hash };
                }
                const index = try self.pushType(ty);

                interned.value_ptr.* = index;
                return .{
                    .index = index,
                    .hash = hash,
                };
            },
            .pointer => |pointer| {
                var hasher = std.hash.Wyhash.init(0);
                hasher.update("pointer");
                const interned_type = try self.internTypeIndex(pointer.child);
                hasher.update(std.mem.asBytes(&interned_type.hash));
                const hash = hasher.final();
                const interned = try self.interned_types.getOrPut(self.arena.allocator(), hash);
                if (interned.found_existing) {
                    return .{ .index = interned.value_ptr.*, .hash = hash };
                }
                const index = try self.pushType(ty);
                interned.value_ptr.* = index;
                return .{
                    .index = index,
                    .hash = hash,
                };
            },
            .array => |array| {
                var hasher = std.hash.Wyhash.init(0);
                hasher.update("array");
                const interned_type = try self.internTypeIndex(array.type);
                hasher.update(std.mem.asBytes(&interned_type.hash));
                hasher.update(std.mem.asBytes(&array.size));
                const hash = hasher.final();
                const interned = try self.interned_types.getOrPut(self.arena.allocator(), hash);
                if (interned.found_existing) {
                    return .{ .index = interned.value_ptr.*, .hash = hash };
                }
                const index = try self.pushType(ty);
                interned.value_ptr.* = index;
                return .{
                    .index = index,
                    .hash = hash,
                };
            },
            .@"struct" => |@"struct"| {
                var hasher = std.hash.Wyhash.init(0);
                hasher.update("struct");
                const fields_list = self.mir.interned_lists.getSlice(@"struct".fields);
                for (fields_list) |field_index| {
                    const interned_field = try self.internTypeIndex(Mir.Type.Index.asTypeIndex(field_index));
                    hasher.update(std.mem.asBytes(&interned_field.hash));
                }
                const hash = hasher.final();
                const interned = try self.interned_types.getOrPut(self.arena.allocator(), hash);
                if (interned.found_existing) {
                    return .{ .index = interned.value_ptr.*, .hash = hash };
                }
                const index = try self.pushType(ty);
                interned.value_ptr.* = index;
                return .{ .index = index, .hash = hash };
            },
            // .void => {
            //     return InternedResult{ .index = .void, .hash = 0 };
            // },
            // .module => |module| {
            //     var hasher = std.hash.Wyhash.init(0);
            //     hasher.update("module");
            //     const hash = hasher.final();
            //     module.

            //     const interned = try self.interned_types.getOrPut(self.arena.allocator(), hash);
            //     if (interned.found_existing) {
            //         return .{ .index = interned.value_ptr.*, .hash = hash };
            //     }
            // },
            else => {
                self.tracer.panic("unimplemented: internTypeInner", .{ .type = ty });
            },
        }
    }
    pub fn internType(self: *Builder, type_: Mir.Type) !Mir.Type.Index {
        const event_id = self.tracer.beginEvent("Builder.internType", .{ .type = type_ });
        defer self.tracer.endEvent(event_id, "Builder.internType", .{ .type = type_ });
        const interned = try self.internTypeInner(type_);
        return interned.index;
        // const index = self.interned_types.getOrPut(type_.hash()) catch |err| {
        //     return try self.makeType(type_);
        // };
        // return index.value_ptr.*;
    }

    pub fn reserveGlobalIndex(self: *Builder) !Mir.Global.Index {
        const index: Mir.Global.Index = @intCast(self.mir.globals.items.len);
        try self.mir.globals.append(self.mir.allocator, undefined);
        return index;
    }
    pub fn setGlobal(self: *Builder, index: Mir.Global.Index, global: Mir.Global) void {
        self.tracer.logEvent("Builder.setGlobal", .{ .index = index, .global = global });
        self.mir.globals.items[index] = global;
    }
    pub fn pushGlobal(self: *Builder, global: Mir.Global) !Mir.Global.Index {
        const index: Mir.Global.Index = @intCast(self.mir.globals.items.len);
        self.tracer.logEvent("Builder.pushGlobal", .{ .global = global, .index = index });
        try self.mir.globals.append(self.mir.allocator, global);
        return index;
    }
    pub fn getValue(self: *Builder, index: Mir.Value.Index) ?Mir.Value {
        if (index.toInt()) |int| {
            return self.mir.values.items[@intCast(int)];
        }
        return null;
    }
    pub fn pushValue(self: *Builder, value: Mir.Value) !Mir.Value.Index {
        const index = self.mir.values.items.len;
        self.tracer.logEvent("Builder.pushValue", .{ .value = value, .index = index });

        try self.mir.values.append(self.mir.allocator, value);
        return Mir.Value.Index.fromInt(@intCast(index));
    }
    pub inline fn formatType(self: *Builder, type_index: Mir.Type.Index) []const u8 {
        var buf = std.BoundedArray(u8, 1024).init(0) catch unreachable;
        const writer = buf.writer().any();
        Mir.formatTypeShort(.{
            .allocator = self.allocator,
            .builder = self,
            .instructions = self.mir.instructions.items,
            .lists = &self.mir.interned_lists,
            .writer = writer,
            .strings = &self.mir.strings,
            .types = self.mir.types.items,
            .values = self.mir.values.items,
        }, writer, type_index, .{}) catch unreachable;
        return buf.slice();
    }
    pub inline fn formatValue(self: *Builder, value: Mir.Value.Index) []const u8 {
        var buf = std.BoundedArray(u8, 1024).init(0) catch unreachable;
        const writer = buf.writer().any();
        Mir.formatValueShort(.{
            .allocator = self.allocator,
            .builder = self,
            .instructions = self.mir.instructions.items,
            .lists = &self.mir.interned_lists,
            .writer = writer,
            .strings = &self.mir.strings,
            .types = self.mir.types.items,
            .values = self.mir.values.items,
        }, writer, value, .{}) catch unreachable;
        return buf.slice();
    }
    fn build(self: *Builder) !void {
        const event_id = self.tracer.beginEvent("Builder.build", .{});
        defer self.tracer.endEvent(event_id, "Builder.build", .{});
        var root_wip = try self.makeAndGetWip(Hir.Inst.RootIndex, 0, .{
            .module = .{
                .name = try self.internSlice("root"),
                .fields = .{},
                .declarations = .{},
            },
        });
        try root_wip.collectSymbols();
        root_wip.type_index = try root_wip.resolveType();

        try root_wip.resolveInitializer();
        try self.getWip(0).dump(std.io.getStdErr().writer().any(), 0);
        try root_wip.commit();
        self.logger.log("Types ({d})", .{self.mir.types.items.len}, null);
    }
};

const WipInstructionIndex = Mir.Instruction.Index;
const Scope = struct {
    globals: ArrayHashMap(Hir.Inst.Index, Wip.Index) = .{},
    locals: ArrayHashMap(InternedSlice, WipInstructionIndex) = .{},
    pub fn deinit(self: *Scope, allocator: std.mem.Allocator) void {
        _ = self; // autofix
        _ = allocator; // autofix
    }
};

pub const Wip = struct {
    index: Wip.Index,
    hir_index: Hir.Inst.Index,
    type_index: Mir.Type.Index = .unknown,
    type_mode: TypeMode = .explicit,
    children: Array(Wip.Index) = .{},
    parent: Wip.Index,
    stage: Stage = .collected_symbols,
    scope: Scope = .{},
    data: Data,
    builder: *Builder,
    instructions: Array(Mir.Instruction) = .{},
    instructions_map: HashMap(Hir.Inst.Index, struct {
        owner: Wip.Index,
        instruction_index: InstructionId,
    }) = .{},
    event_id: u64,
    const TypeMode = enum {
        explicit,
        inferred,
    };

    pub const Index = usize;
    pub const Stage = enum(u8) {
        collected_symbols,
        resolving_type,
        type_resolved,
        resolving_instructions,
        resolved,
        committed,
    };
    pub const Kind = enum {
        fn_decl,
        block,
        module,
        global_declaration,
        global_type_declaration,
        param_declaration,
        field_declaration,
        type,
    };
    pub const Data = union(Kind) {
        fn_decl: struct {
            global: Mir.Global.Index,
            name: InternedSlice,
            params: Array(Wip.Index),
            return_type: Wip.Index,
            init_block: ?Wip.Index,
        },
        block: Block,

        module: struct {
            name: InternedSlice,
            declarations: Array(Wip.Index),
            fields: std.AutoArrayHashMapUnmanaged(InternedSlice, StructFieldWip),
        },
        global_declaration: struct {
            global: Mir.Global.Index,
            name: InternedSlice,
            type: Wip.Index,
            init_block: ?Wip.Index,
        },
        global_type_declaration: struct {
            global: Mir.Global.Index,
            name: InternedSlice,
            value_instruction: ?InstructionId = null,
            init_block: ?Wip.Index,
        },
        param_declaration: struct {
            name: InternedSlice,
            type: Wip.Index,
        },
        field_declaration: struct {
            name: InternedSlice,
            type: Wip.Index,
            init: ?Wip.Index,
        },
        type: void,
        const StructFieldWip = struct {
            index: u32,
            wip: Wip.Index,
        };
        pub const Block = struct {
            name: ?InternedSlice,
            owner: Wip.Index,
            value_instruction: ?InstructionId = null,
            break_type: ?Mir.Type.Index = null,
            instructions: Array(InstructionId) = .{},
            is_inline: bool = false,
        };
    };
    pub fn init(index: Wip.Index, builder: *Builder, hir_index: Hir.Inst.Index, parent: Wip.Index, data: Data) !Wip {
        var wip = Wip{
            .index = index,
            .builder = builder,
            .hir_index = hir_index,
            .parent = parent,
            .data = data,
            .type_mode = switch (std.meta.activeTag(data)) {
                .block => .inferred,
                else => .explicit,
            },
            .event_id = undefined,
        };
        wip.event_id = builder.tracer.beginAsync("Wip.init({s}) hir({d}) parent({d})", .{ wip.statusSlice(), hir_index, parent }, .{ .wip_index = index, .wip_type = std.meta.activeTag(data), .hir_index = hir_index, .parent = parent, .data = data });
        return wip;
    }

    pub fn getNameSlice(self: *Wip) []const u8 {
        if (self.getName()) |name| {
            return self.builder.getSlice(name);
        }
        return "";
    }
    pub fn getName(self: *Wip) ?InternedSlice {
        return switch (self.data) {
            .fn_decl => |fn_decl| fn_decl.name,
            .field_declaration => |field_decl| field_decl.name,
            .global_declaration => |global_decl| global_decl.name,
            .global_type_declaration => |global_type_decl| global_type_decl.name,
            .param_declaration => |param_decl| param_decl.name,
            .type => null,
            else => null,
        };
    }
    pub fn deinit(self: *Wip) void {
        self.builder.tracer.endFlow(self.event_id);
        self.scope.deinit(self.builder.allocator);
    }
    pub fn getBlockData(self: *Wip) *Data.Block {
        return &self.builder.getWip(self.index).data.block;
    }
    // pub inline fn update(self: *Wip) void {
    //     const pointer = self.builder.wips.getPtr(self.index);
    //     self.builder.logger.log("UPDATE: {s} {x}", .{ self.format(), @intFromPtr(pointer) }, null);

    //     pointer.* = self.*;
    // }
    pub inline fn statusSlice(self: *Wip) []const u8 {
        var buf = std.BoundedArray(u8, 1024).init(0) catch unreachable;
        const writer = buf.writer().any();
        writer.print("{d}:.{s}:.{s}", .{ self.index, @tagName(std.meta.activeTag(self.data)), @tagName(self.stage) }) catch unreachable;
        return buf.slice();
    }
    pub fn getGlobal(self: *Wip, inst_index: Hir.Inst.Index) ?Wip.Index {
        const event_id = self.builder.tracer.begin("Wip({s}).getGlobal(hir({d}))", .{ self.statusSlice(), inst_index }, .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .inst_index = inst_index });
        defer self.builder.tracer.end(event_id);
        if (self.scope.globals.get(inst_index)) |global| {
            self.builder.tracer.printEvent("Found global {d} for hir({d})", .{ global, inst_index }, .{
                .hir_inst = inst_index,
                .wip_index = global,
                .wip_data = self.builder.getWip(global).data,
            });
            return global;
        }
        if (self.index == 0 and self.parent == 0) return null;
        const parent = self.builder.getWip(self.parent);
        return parent.getGlobal(inst_index);
    }
    pub fn getInstruction(self: *Wip, id: InstructionId) !*Mir.Instruction {
        switch (self.data) {
            .block => |*block| {
                const owner_wip = self.builder.getWip(block.owner);
                return &owner_wip.instructions.items[@intCast(id)];
            },
            else => {
                return &self.instructions.items[@intCast(id)];
            },
        }
        self.builder.tracer.logEvent("instruction not found", .{ .id = id, .wip = self.index });
        return error.InstructionNotFound;
    }
    pub fn getInstructionId(self: *Wip, hir_inst_index: Hir.Inst.Index) !InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.getInstructionId", .{ .hir_inst_index = hir_inst_index });
        defer self.builder.tracer.endEvent(event_id, "Wip.getInstructionId", .{ .hir_inst_index = hir_inst_index });
        const owner_wip = self.builder.getWip(self.data.block.owner);
        const id = owner_wip.instructions_map.get(hir_inst_index) orelse {
            self.builder.tracer.panic("instruction not found", .{hir_inst_index});
        };
        return id.instruction_index;
    }

    pub fn getInstructionByHirInst(self: *Wip, hir_inst: Hir.Inst.Index) !*Mir.Instruction {
        const event_id = self.builder.tracer.beginEvent("Wip.getInstructionByHirInst", .{ .hir_inst = hir_inst });
        defer self.builder.tracer.endEvent(event_id, "Wip.getInstructionByHirInst", .{ .hir_inst = hir_inst });
        const id = try self.getInstructionId(hir_inst);
        return self.getInstruction(id);
    }
    pub fn getInstructionIdAsType(self: *Wip, hir_inst: Hir.Inst.Index, expected_type: Mir.Type.Index) !InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.getInstructionIdAsType", .{ .hir_inst = hir_inst, .expected_type = expected_type });
        defer self.builder.tracer.endEvent(event_id, "Wip.getInstructionIdAsType", .{ .hir_inst = hir_inst, .expected_type = expected_type });
        self.builder.logger.log("getInstructionIdAsType: hir({d}) -> expected_type: {d}", .{ hir_inst, expected_type }, null);
        const id = try self.getInstructionId(hir_inst);
        return try self.tryCastInstruction(hir_inst, id, expected_type);
        // const instruction = try self.getInstruction(id);
        // if (instruction.type != expected_type) {
        // }
        // return id;
    }
    pub fn putGlobal(self: *Wip, inst_index: Hir.Inst.Index, index: Wip.Index) !void {
        self.builder.tracer.printEvent("Wip.putGlobal(hir({d}) -> wip({d})", .{ inst_index, index }, .{ .hir_inst = inst_index, .wip_index = index });
        const result = try self.scope.globals.getOrPut(self.builder.arena.allocator(), inst_index);
        if (!result.found_existing) {
            result.value_ptr.* = index;
            return;
        }
        return error.GlobalAlreadyExists;
    }

    pub fn reserveInstruction(self: *Wip, hir_inst: Hir.Inst.Index) !InstructionId {
        switch (self.data) {
            .block => |*block| {
                const allocator = self.builder.arena.allocator();
                const owner_wip = self.builder.getWip(block.owner);
                const instruction_index: InstructionId = @intCast(owner_wip.instructions.items.len);

                // self.builder.logger.log(
                //     "[PUSH_INSTRUCTION] hir({d}) -> %{d} '{s}' on BLOCK #{d} to OWNER #{d}",
                //     .{ hir_inst, instruction_index, @tagName(instruction.op), self.index, block.owner },
                //     null,
                // );
                try owner_wip.instructions.append(allocator, undefined);
                try owner_wip.instructions_map.put(allocator, hir_inst, .{
                    .owner = self.index,
                    .instruction_index = instruction_index,
                });
                try self.builder.getWip(self.index).data.block.instructions.append(allocator, instruction_index);
                const color = fmt.pickColor(instruction_index);
                self.builder.logger.log("[MAP INST]: %hir({d}) -> %{d}", .{ hir_inst, instruction_index }, color);
                // if (block.is_inline) {
                //     block.value = instruction.value;
                //     block.break_type = instruction.type;
                // }
                // self.dumpInstruction(std.io.getStdErr().writer().any(), instruction_index, self.builder.logger.ind) catch {};
                // try block.instructions.append(allocator, instruction_index);

                return @intCast(instruction_index);
            },
            else => @panic("only block has instructions"),
        }
    }
    pub fn setInstruction(self: *Wip, id: InstructionId, instruction: Mir.Instruction) void {
        const owner_wip = self.builder.getWip(self.data.block.owner);

        owner_wip.instructions.items[@intCast(id)] = instruction;

        const color = fmt.pickColor(id);
        self.builder.tracer.logEvent("Wip.setInstruction", .{ .id = id, .instruction = instruction });
        self.builder.logger.log("[SET_INSTRUCTION] %{d} '{s}' on BLOCK #{d}, OWNER #{d}", .{
            id,
            @tagName(instruction.op),
            self.index,
            self.data.block.owner,
        }, color);
        self.dumpInstruction(
            std.io.getStdErr().writer().any(),
            id,
        ) catch {};
    }
    pub fn pushInstruction(self: *Wip, hir_inst: Hir.Inst.Index, instruction: Mir.Instruction) !InstructionId {
        const id = try self.reserveInstruction(hir_inst);
        self.setInstruction(id, instruction);
        switch (self.data) {
            .block => |*block| {
                if (block.is_inline) {
                    block.value_instruction = id;
                    self.builder.tracer.printEvent("set instruction as value {d}", .{id}, .{ .instruction_id = id });
                    block.break_type = instruction.type;
                }
            },
            else => {},
        }
        return id;
        // switch (self.data) {
        //     .block => |*block| {
        //         const allocator = self.builder.arena.allocator();
        //         const owner_wip = self.builder.getWip(block.owner);
        //         const instruction_index: InstructionId = @intCast(owner_wip.instructions.items.len);

        //         self.builder.logger.log(
        //             "[PUSH_INSTRUCTION] hir({d}) -> %{d} '{s}' on BLOCK #{d} to OWNER #{d}",
        //             .{ hir_inst, instruction_index, @tagName(instruction.op), self.index, block.owner },
        //             null,
        //         );
        //         try owner_wip.instructions.append(allocator, instruction);
        //         try owner_wip.instructions_map.put(allocator, hir_inst, .{
        //             .owner = self.index,
        //             .instruction_index = instruction_index,
        //         });
        //         try self.builder.getWip(self.index).data.block.instructions.append(allocator, instruction_index);
        //         if (block.is_inline) {
        //             block.value = instruction.value;
        //             block.break_type = instruction.type;
        //         }
        //         self.dumpInstruction(std.io.getStdErr().writer().any(), instruction_index, self.builder.logger.ind) catch {};
        //         // try block.instructions.append(allocator, instruction_index);

        //         return @intCast(instruction_index);
        //     },
        //     else => @panic("only block has instructions"),
        // }
    }
    pub fn collectSymbols(self: *Wip) Error!void {
        const event_id = self.builder.tracer.begin("Wip({s}).collectSymbols", .{self.statusSlice()}, .{});
        defer self.builder.tracer.end(event_id);
        switch (self.data) {
            .module => try self.collectModuleSymbols(),
            .field_declaration => try self.collectFieldDeclarationSymbols(),
            else => {},
        }
    }
    fn collectFieldDeclarationSymbols(self: *Wip) !void {
        const hir_inst = self.builder.getHirInst(self.hir_index);
        const type_hir_inst = hir_inst.struct_field.ty orelse std.debug.panic("field {s} has no type", .{self.getNameSlice()});
        const type_wip_index: Wip.Index = try self.builder.makeWip(type_hir_inst, self.index, .{
            .type = {},
        });
        const name = try self.builder.internNode(hir_inst.struct_field.name_node);
        _ = name; // autofix
        const init_wip = if (hir_inst.struct_field.init) |init_hir_index| try self.builder.makeWip(init_hir_index, self.index, .{
            .block = .{
                .name = null,
                .owner = self.index,
                .instructions = .{},
                .break_type = null,
                .is_inline = switch (self.builder.getHirInst(init_hir_index)) {
                    .inline_block => true,
                    else => false,
                },
            },
        }) else null;
        self.data.field_declaration.init = init_wip;
        self.data.field_declaration.type = type_wip_index;
    }
    fn collectModuleSymbols(self: *Wip) !void {
        const event_id = self.builder.tracer.begin("Wip({d}:{s}).collectModuleSymbols", .{ self.index, @tagName(std.meta.activeTag(self.data)) }, .{});
        defer self.builder.tracer.end(event_id);
        // const self_index = self.index;
        const builder = self.builder;

        // try builder.wips.ensureUnusedCapacity(builder.arena.allocator(), 1024);
        // const self = builder.getWip(self_index);

        const hir_inst = self.builder.getHirInst(self.hir_index);
        const module_decl = hir_inst.struct_decl;

        const allocator = builder.arena.allocator();

        const hir_fields_list = builder.getListSlice(module_decl.fields_list);

        for (hir_fields_list) |field_hir_index| {
            const field_hir_inst = builder.getHirInst(field_hir_index);
            const field_name = try builder.internNode(field_hir_inst.struct_field.name_node);
            const field_name_slice = builder.getSlice(field_name);
            const collect_field_event_id = self.builder.tracer.begin("[COLLECT_FIELD_SYMBOL='{s}']", .{field_name_slice}, .{
                .field_hir_index = field_hir_index,
                .field_name = field_name,
            });
            defer self.builder.tracer.end(collect_field_event_id);
            var field_wip = try builder.makeAndGetWip(field_hir_index, self.index, .{
                .field_declaration = .{
                    .name = field_name,
                    .type = 0,
                    .init = null,
                },
            });
            try field_wip.collectSymbols();
            try self.data.module.fields.put(allocator, field_name, .{
                .wip = field_wip.index,
                .index = @intCast(self.data.module.fields.count()),
            });

            // const ty_hir_inst = field_hir_inst.struct_field.ty orelse std.debug.panic("field {s} has no type", .{field_name_slice});
            // const type_wip_index: Wip.Index = try builder.makeWip(ty_hir_inst, field_wip_index, .{
            //     .type = {},
            // });
            // const init_wip = if (field_hir_inst.struct_field.init) |init_hir_index| try builder.makeWip(init_hir_index, field_wip_index, .{
            //     .block = .{
            //         .name = null,
            //         .owner = self.index,
            //         .instructions = .{},
            //         .break_type = null,
            //         .is_inline = switch (self.builder.getHirInst(init_hir_index)) {
            //             .inline_block => true,
            //             else => false,
            //         },
            //     },
            // }) else null;
            // self.builder.getWip(field_wip_index).data.field_declaration.init = init_wip;
            // self.builder.getWip(field_wip_index).data.field_declaration.type = type_wip_index;
            // try self.data.module.fields.put(allocator, field_name, .{
            //     .wip = field_wip_index,
            //     .index = @intCast(self.data.module.fields.count()),
            // });
        }

        const hir_declarations_list = builder.getListSlice(module_decl.declarations_list);

        for (hir_declarations_list) |decl_hir_index| {
            const decl_hir_inst = builder.getHirInst(decl_hir_index);
            const global_decl = decl_hir_inst.global_decl;
            const name = try builder.internNode(global_decl.name_node);
            const name_slice = builder.getSlice(name);
            const collect_declaration_event_id = self.builder.tracer.begin("[COLLECT_DECLARATION_SYMBOL='{s}']", .{name_slice}, .{
                .decl_hir_index = decl_hir_index,
                .name = name,
            });
            defer self.builder.tracer.end(collect_declaration_event_id);
            if (global_decl.is_type) {
                const global_declaration_wip_index: Wip.Index = try builder.makeWip(decl_hir_index, self.index, .{
                    .global_type_declaration = .{
                        .global = try builder.reserveGlobalIndex(),
                        .name = name,
                        .init_block = null,
                    },
                });

                builder.getWip(global_declaration_wip_index).stage = .type_resolved;
                builder.getWip(global_declaration_wip_index).type_index = .type;

                // const type_wip_index: Wip.Index = try builder.makeWip(type_hir_index, global_declaration_wip_index, .{
                //     .type = {},
                // });
                // builder.getWip(global_declaration_wip_index).data.global_declaration.type = type_wip_index;
                if (global_decl.init) |init_hir_index| {
                    builder.getWip(global_declaration_wip_index).data.global_type_declaration.init_block = try builder.makeWip(init_hir_index, global_declaration_wip_index, .{
                        .block = .{
                            .name = null,
                            .owner = global_declaration_wip_index,
                            .instructions = .{},
                            .break_type = null,
                            .is_inline = switch (self.builder.getHirInst(init_hir_index)) {
                                .inline_block => true,
                                else => false,
                            },
                        },
                    });
                }
                try self.putGlobal(decl_hir_index, global_declaration_wip_index);
                try self.data.module.declarations.append(allocator, global_declaration_wip_index);
                continue;
            }

            if (global_decl.type) |type_hir_index| {
                const type_hir_inst = builder.getHirInst(type_hir_index);

                switch (type_hir_inst) {
                    .fn_decl => {
                        const fn_decl_wip_index = try builder.makeWip(decl_hir_index, self.index, .{
                            .fn_decl = .{
                                .global = try builder.reserveGlobalIndex(),
                                .name = name,
                                .params = .{},
                                .return_type = 0,
                                .init_block = null,
                            },
                        });

                        const return_type_wip_index = try builder.makeWip(type_hir_inst.fn_decl.return_type, fn_decl_wip_index, .{
                            .type = {},
                        });
                        builder.getWip(fn_decl_wip_index).data.fn_decl.return_type = return_type_wip_index;
                        if (global_decl.init) |init_hir_index| {
                            builder.getWip(fn_decl_wip_index).data.fn_decl.init_block = try builder.makeWip(init_hir_index, fn_decl_wip_index, .{
                                .block = .{
                                    .name = null,
                                    .owner = fn_decl_wip_index,
                                    .instructions = .{},
                                    .break_type = null,
                                },
                            });
                        }

                        try self.data.module.declarations.append(allocator, fn_decl_wip_index);
                        try self.putGlobal(decl_hir_index, fn_decl_wip_index);

                        const params_list = builder.getListSlice(type_hir_inst.fn_decl.params_list);
                        for (params_list) |param_hir_index| {
                            const param_hir_inst = builder.getHirInst(param_hir_index);
                            const param_name = try builder.internNode(param_hir_inst.param_decl.name_node);

                            const param_wip_index = try builder.makeWip(param_hir_index, fn_decl_wip_index, .{
                                .param_declaration = .{
                                    .name = param_name,
                                    .type = 0,
                                },
                            });
                            // param_wip.stage = .collected_symbols;
                            builder.getWip(param_wip_index).data.param_declaration.type = try builder.makeWip(
                                param_hir_inst.param_decl.ty,
                                param_wip_index,
                                .{
                                    .type = {},
                                },
                            );
                            // try self.putLocal(param_name, param_wip_index);
                            try builder.getWip(fn_decl_wip_index).data.fn_decl.params.append(builder.arena.allocator(), param_wip_index);
                        }
                    },
                    else => {
                        const global_declaration_wip_index: Wip.Index = try builder.makeWip(decl_hir_index, self.index, .{
                            .global_declaration = .{
                                .global = try builder.reserveGlobalIndex(),
                                .name = name,
                                .type = 0,
                                .init_block = null,
                            },
                        });
                        const type_wip_index: Wip.Index = try builder.makeWip(type_hir_index, global_declaration_wip_index, .{
                            .type = {},
                        });
                        builder.getWip(global_declaration_wip_index).data.global_declaration.type = type_wip_index;
                        if (global_decl.init) |init_hir_index| {
                            builder.getWip(global_declaration_wip_index).data.global_declaration.init_block = try builder.makeWip(init_hir_index, global_declaration_wip_index, .{
                                .block = .{
                                    .name = null,
                                    .owner = global_declaration_wip_index,
                                    .instructions = .{},
                                    .break_type = null,
                                    .is_inline = switch (self.builder.getHirInst(init_hir_index)) {
                                        .inline_block => true,
                                        else => false,
                                    },
                                },
                            });
                        }
                        try self.putGlobal(decl_hir_index, global_declaration_wip_index);
                        try self.data.module.declarations.append(allocator, global_declaration_wip_index);
                    },
                }
            }
        }
    }

    pub const Error = error{
        Unimplemented,
        CircularTypeReference,
        InstructionNotFound,
        GlobalAlreadyExists,
        TypeNotFound,
        TypeMismatch,
    } || std.fmt.ParseFloatError || std.fmt.ParseIntError || std.mem.Allocator.Error;
    pub fn resolveType(self: *Wip) Error!Mir.Type.Index {
        const event_id = self.builder.tracer.begin("Wip({s}).resolveType", .{self.statusSlice()}, .{});
        defer self.builder.tracer.end(event_id);
        self.builder.logger.open("resolveType: {s}", .{self.format()});
        defer self.builder.logger.close();

        switch (self.stage) {
            .collected_symbols => {
                if (self.type_mode == .inferred) {
                    try self.resolveInitializer();
                }
            },
            .resolving_type => {
                return error.CircularTypeReference;
            },

            else => {
                self.builder.logger.log("resolved previously: {s}", .{self.format()}, null);
                return self.type_index;
            },
        }
        // self.builder.tracer.stepFlow(event_id, "Wip({d}:{s}).resolveType", .{ self.index, @tagName(std.meta.activeTag(self.data)) }, .{
        //     .wip = self.index,
        //     .wip_type = std.meta.activeTag(self.data),
        // });
        defer self.builder.logger.log("resolved: {s}", .{self.format()}, null);
        self.type_index = switch (self.data) {
            .module => try self.resolveModuleType(),
            .fn_decl => try self.resolveFnDeclType(),
            .type => try self.resolveSimpleType(),
            .param_declaration => try self.resolveParamType(),
            .global_declaration => try self.resolveGlobalDeclarationType(),
            .field_declaration => try self.resolveFieldDeclarationType(),

            .global_type_declaration,
            .block,
            => {
                std.debug.panic("unimplemented resolveType: {s}", .{@tagName(std.meta.activeTag(self.data))});
            },
            // else => {
            //     return self.builder.logger.panic("unimplemented resolveType: {s}", .{@tagName(std.meta.activeTag(self.data))});
            // },
        };
        self.stage = .type_resolved;
        return self.type_index;
    }

    const SortByAlignmentContext = struct {
        keys: []InternedSlice,
        types: []Mir.Type.Index,
        builder: *Builder,

        pub fn lessThan(ctx: @This(), a_index: usize, b_index: usize) bool {
            return ctx.computeAlignment(a_index) > ctx.computeAlignment(b_index);
        }
        pub fn computeAlignment(ctx: @This(), index: usize) u32 {
            const type_index: Mir.Type.Index = ctx.types[index];
            if (ctx.builder.getType(type_index)) |ty| {
                _ = ty; // autofix
                @panic("TODO");
            }
            switch (type_index) {
                .i8, .u8 => return 1,

                .i16, .u16 => return 2,

                .f32,
                .i32,
                .u32,
                => return 4,

                .i64, .f64, .u64 => return 8,

                .usize => return 4, // TODO: this is platform dependent, we are assuming 32 bits here.

                .i128,
                .u128,
                .i256,
                .u256,
                => @panic("TODO"),
                else => @panic("TODO"),
            }
        }
    };
    fn resolveModuleType(self: *Wip) !Mir.Type.Index {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveModuleType", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveModuleType", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        const type_index = try self.builder.reserveTypeIndex();
        const builder = self.builder;
        const module = self.data.module;

        try builder.wip_map.put(builder.arena.allocator(), type_index, self.index);

        // trace({
        var fields_map = std.AutoArrayHashMapUnmanaged(InternedSlice, Mir.Type.Index){};
        var fields_list = self.builder.newList();

        // var field_types = self.builder.newList();
        var field_iter = module.fields.iterator();
        while (field_iter.next()) |entry| {
            const field_name = entry.key_ptr.*;
            const field_name_slice = self.builder.getSlice(field_name);
            const resolving_field_event_id = builder.tracer.begin("[RESOLVING_FIELD_TYPE='{s}']", .{field_name_slice}, .{
                .wip = self.index,
                .wip_type = std.meta.activeTag(self.data),
            });
            defer builder.tracer.end(resolving_field_event_id);
            const field_wip = builder.getWip(entry.value_ptr.wip);
            const field_type_index = try field_wip.resolveType();
            try fields_map.put(builder.arena.allocator(), field_name, field_type_index);
            // try fields_list.append(field_type_index.asInt());
            // const field_type_wip = builder.getWip(entry.value_ptr.type);
            // const field_type_index = try field_type_wip.resolveType();
            // try fields_map.put(builder.arena.allocator(), entry.key_ptr.*, field_type_index);
            // if (entry.value_ptr.init) |init_wip_index| {
            //     const init_wip = builder.getWip(init_wip_index);
            //     const init_type_index = try init_wip.resolveType();
            //     _ = init_type_index; // autofix
            // }
        }

        fields_map.sort(SortByAlignmentContext{
            .builder = builder,
            .keys = fields_map.keys(),
            .types = fields_map.values(),
        });

        var sorted_fields_iter = fields_map.iterator();
        var index: u32 = 0;

        while (sorted_fields_iter.next()) |entry| {
            const field_name = entry.key_ptr.*;

            const field_type_index = entry.value_ptr.*;
            try fields_list.append(field_type_index.asInt());
            const field_from_wip = module.fields.getPtr(field_name) orelse unreachable;
            // field_from_wip.type = field_type_index;
            field_from_wip.index = index;
            index += 1;
        }
        // });
        var declarations_list = self.builder.newList();

        for (module.declarations.items) |declaration| {
            const declaration_wip = builder.getWip(declaration);
            const resolving_declaration_event_id = builder.tracer.begin("[RESOLVING_DECLARATION_TYPE='{s}']", .{declaration_wip.getNameSlice()}, .{
                .wip = self.index,
                .wip_type = std.meta.activeTag(self.data),
            });
            defer builder.tracer.end(resolving_declaration_event_id);
            _ = try declaration_wip.resolveType();
            try declarations_list.append(declaration_wip.type_index.asInt());
        }

        // std.mem.sort(u32, declaration_types.list.items, self, sortByAlignment);
        // const declaration_types_list = try declaration_types.commit();

        builder.setType(type_index, .{
            .@"struct" = .{
                .fields = try fields_list.commit(),
                .declarations = try declarations_list.commit(),
                .alignment = 1,
            },
        });

        return type_index;
    }
    fn resolveFnDeclType(self: *Wip) !Mir.Type.Index {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveFnDeclType", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.end(event_id);
        const builder = self.builder;
        const fn_decl = self.data.fn_decl;
        const return_type_wip = builder.getWip(fn_decl.return_type);
        const return_type_index = try return_type_wip.resolveType();
        var params_types = builder.newList();
        for (fn_decl.params.items) |param| {
            var param_wip = builder.getWip(param);

            const param_type_index = try param_wip.resolveType();
            try params_types.append(param_type_index.asInt());
        }
        // const type_index = try self.builder.reserveTypeIndex();
        const type_index = try builder.internType(.{
            .@"fn" = .{
                .name = fn_decl.name,
                .params_list = try params_types.commit(),
                .return_type = return_type_index,
            },
        });
        return type_index;

        // return self.builder.logger.panic("not implemented: resolveFnDeclType", .{});
    }
    fn resolveSimpleType(self: *Wip) !Mir.Type.Index {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveSimpleType", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveSimpleType", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        const type_inst = self.builder.getHirInst(self.hir_index);

        const type_index = switch (std.meta.activeTag(type_inst)) {
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
            .ty_f32,
            .ty_f64,
            .ty_void,
            => |tag| {
                const tag_name = comptime @tagName(tag)[3..];
                return std.meta.stringToEnum(Mir.Type.Index, tag_name) orelse {
                    return self.builder.tracer.panic("not implemented: resolveSimpleType", .{ .type = tag_name });
                };
                // return try self.builder.makeType(.{ .type_number = .{ .tag = tag } });
            },
            else => {
                std.debug.panic("not implemented: resolveSimpleType {s}", .{@tagName(std.meta.activeTag(type_inst))});
            },
        };
        self.type_index = type_index;
        return type_index;
    }
    fn resolveParamType(self: *Wip) !Mir.Type.Index {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveParamType", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveParamType", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        const param = self.data.param_declaration;
        const type_index = try self.builder.makeType(.{
            .param = .{
                .name = param.name,
                .type = try self.builder.getWip(param.type).resolveType(),
            },
        });

        self.type_index = type_index;
        // param type has no initializer so it's resolved immediately
        // TODO: maybe when supporting default values OR types been the result of a function call, we would need to resolve instructions first for those cases
        return type_index;
    }

    fn resolveGlobalDeclarationType(self: *Wip) !Mir.Type.Index {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveGlobalDeclarationType", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveGlobalDeclarationType", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        self.stage = .resolving_type;
        const global_decl = self.data.global_declaration;
        const type_index = try self.builder.getWip(global_decl.type).resolveType();

        // const type_index = try self.builder.makeType(.{
        //     .global = .{
        //         .name = global_decl.name,
        //         .type = type_type_index,
        //         .init = null, // Resolve this later when resolving instructions

        //     },
        // });
        self.type_index = type_index;
        self.stage = .type_resolved;
        return type_index;
    }
    fn resolveFieldDeclarationType(self: *Wip) !Mir.Type.Index {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveFieldDeclarationType", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.end(event_id);
        const field_decl = self.data.field_declaration;
        const type_index = try self.builder.getWip(field_decl.type).resolveType();
        if (field_decl.init) |init_wip_index| {
            _ = init_wip_index; // autofix
            // const init_wip = self.builder.getWip(init_wip_index);
            // const init_type_index = try init_wip.resolveType();
        }

        return type_index;
    }
    fn dumpType(self: *Wip, writer: std.io.AnyWriter, index: Mir.Type.Index, depth: usize) !void {
        const event_id = self.builder.tracer.beginEvent("Wip.dumpType", .{ .index = index, .depth = depth });
        defer self.builder.tracer.endEvent(event_id, "Wip.dumpType", .{ .index = index, .depth = depth });
        if (self.builder.getType(index)) |ty| {
            // try fmt.writeIndent(writer, depth, .{});
            switch (ty.*) {
                .param => |data| {
                    try writer.print("{d} param('{s}', ", .{ index.toInt().?, self.builder.getSlice(data.name) });
                    try self.dumpType(writer, data.type, depth + 1);
                    try writer.writeAll(")");
                },
                .@"fn" => {
                    var iter = self.builder.mir.lists.iterList(ty.@"fn".params);
                    try writer.writeAll("fn(");
                    var first = true;
                    while (iter.next()) |param_id| {
                        if (!first) {
                            try writer.writeAll(", ");
                        }
                        first = false;
                        // const param = self.builder.getType(Mir.Type.Index.asTypeIndex(param_id)) orelse unreachable;
                        const param_index = Mir.Type.Index.asTypeIndex(param_id);
                        const param_type = self.builder.getType(param_index) orelse unreachable;

                        // self.builder.logger.log("param: \n{any} \n{any}\n", .{ param, param.param.type }, null);
                        try self.dumpType(writer, param_type.param.type, depth + 1);
                    }
                    try writer.writeAll("):");
                    try self.dumpType(writer, ty.@"fn".return_type, depth + 1);
                },
                .array => |data| {
                    try writer.print("[{d}]", .{data.size});
                    try self.dumpType(writer, data.type, depth + 1);
                },
                else => |data| {
                    _ = data; // autofix
                    try writer.print("type_{s}({d})", .{ @tagName(ty.*), index.toInt().? });
                },
            }
        } else {
            const tag_name = @tagName(index);
            if (std.mem.startsWith(u8, tag_name, "type_")) {
                try writer.print("{s}", .{tag_name[5..]});
            } else {
                try writer.print("{s}", .{tag_name});
            }
        }
    }
    pub fn resolveInitializer(self: *Wip) !void {
        const event_id = self.builder.tracer.begin("Wip({d}:{s}).resolveInitializer", .{ self.index, @tagName(std.meta.activeTag(self.data)) }, .{});
        defer self.builder.tracer.end(event_id);
        switch (self.stage) {
            .collected_symbols => {
                if (self.type_mode == .explicit) {
                    _ = try self.resolveType();
                }
            },
            .resolving_type,
            .resolving_instructions,
            => {
                return error.CircularTypeReference;
            },
            .committed, .resolved => return,
            .type_resolved => {},
        }

        self.stage = .resolving_instructions;
        switch (self.data) {
            .module => try self.resolveModuleInitializer(),
            .fn_decl => try self.resolveFnDeclInitializer(),
            .block => try self.resolveBlockInitializer(),
            .global_declaration => try self.resolveGlobalDeclarationInitializer(),
            .global_type_declaration => try self.resolveGlobalTypeDeclarationInitializer(),
            .field_declaration => try self.resolveFieldDeclarationInitializer(),
            else => {
                return std.debug.panic("unimplemented: resolveInitializer {s}", .{@tagName(std.meta.activeTag(self.data))});
            },
        }
        self.stage = .resolved;
    }
    fn resolveModuleInitializer(self: *Wip) Error!void {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveModuleInitializer", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveModuleInitializer", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        self.stage = .resolving_instructions;
        const builder = self.builder;
        const module = self.data.module;
        var fields_iter = module.fields.iterator();

        while (fields_iter.next()) |field| {
            const field_wip = builder.getWip(field.value_ptr.wip);
            const field_name_slice = field_wip.getNameSlice();
            const resolve_field_initializer_event_id = builder.tracer.begin("[RESOLVE_FIELD_INITIALIZER='{s}']", .{field_name_slice}, .{
                .wip = self.index,
                .wip_type = std.meta.activeTag(self.data),
            });
            defer builder.tracer.end(resolve_field_initializer_event_id);

            try field_wip.resolveInitializer();
        }

        for (module.declarations.items) |field| {
            const declaration_wip = builder.getWip(field);
            const resolve_decl_initializer_event_id = builder.tracer.begin("[RESOLVE_DECL_INITIALIZER='{s}']", .{declaration_wip.getNameSlice()}, .{
                .wip = self.index,
                .wip_type = std.meta.activeTag(self.data),
            });
            defer builder.tracer.end(resolve_decl_initializer_event_id);
            try declaration_wip.resolveInitializer();
        }
        self.stage = .resolved;
    }
    fn resolveFnDeclInitializer(self: *Wip) Error!void {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveFnDeclInitializer", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveFnDeclInitializer", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        self.stage = .resolving_instructions;
        defer self.stage = .resolved;
        const init_block_wip_index = self.data.fn_decl.init_block orelse {
            self.stage = .resolved;
            return;
        };
        const init_block_wip = self.builder.getWip(init_block_wip_index);
        try init_block_wip.resolveInitializer();
    }
    fn resolveBlockInitializer(self: *Wip) Error!void {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveBlockInitializer", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveBlockInitializer", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        self.stage = .resolving_instructions;

        //  Pushing Parameters as instructions
        const parent = self.builder.getWip(self.parent);
        switch (parent.data) {
            .fn_decl => |*fn_decl| {
                for (fn_decl.params.items) |param_wip_index| {
                    const param_wip = self.builder.getWip(param_wip_index);
                    _ = try self.resolveParamInstruction(
                        param_wip.hir_index,
                        param_wip.data.param_declaration.name,
                        try param_wip.resolveType(),
                    );
                }
            },
            .field_declaration => |field_decl| {
                const field_wip = self.builder.getWip(field_decl.type);
                const ty = try field_wip.resolveType();
                _ = try self.pushInstruction(field_wip.hir_index, .{
                    .op = .type,
                    .type = .type,
                    .value = try self.getTypeAsValue(ty),
                    .data = .{
                        .type = ty,
                    },
                });
            },
            .block => {},
            .global_declaration => {},
            .global_type_declaration => {},

            .module => {},
            .param_declaration => {
                self.builder.logger.todo("error message for blocks at the param_declaration level (maybe in the future?)", .{});
            },
            .type => {
                // I guess this should never happen?
                self.builder.logger.todo("error message for blocks at the type level", .{});
            },
        }

        try self.resolveBlockInstructions();

        self.builder.logger.log("resolved block #{d} with #{d} instructions", .{
            self.index,
            self.data.block.instructions.items.len,
        }, null);
        self.stage = .resolved;
    }
    pub fn resolveGlobalDeclarationInitializer(self: *Wip) Error!void {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveGlobalDeclarationInitializer", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveGlobalDeclarationInitializer", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        self.stage = .resolving_instructions;
        defer self.stage = .resolved;
        const init_block_wip_index = self.data.global_declaration.init_block orelse {
            self.stage = .resolved;
            return;
        };
        const init_block_wip = self.builder.getWip(init_block_wip_index);
        try init_block_wip.resolveInitializer();
    }
    pub fn resolveGlobalTypeDeclarationInitializer(self: *Wip) Error!void {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveGlobalTypeDeclarationInitializer", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveGlobalTypeDeclarationInitializer", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        self.stage = .resolving_instructions;
        defer self.stage = .resolved;
        const init_block_wip_index = self.data.global_type_declaration.init_block orelse {
            std.debug.panic("type declarations should have an init block", .{});
            return;
        };

        const init_block_wip = self.builder.getWip(init_block_wip_index);
        try init_block_wip.resolveInitializer();

        // std.debug.panic("value_instruction: {any}", .{init_block_wip.data.block});
        const value_instruction = init_block_wip.data.block.value_instruction orelse unreachable;
        const value_inst = try init_block_wip.getInstruction(value_instruction);
        const value = value_inst.getValue();
        const value_str = self.builder.formatValue(value);

        self.data.global_type_declaration.value_instruction = value_instruction;
        self.builder.tracer.logEvent("set global_type_declaration.value_instruction", .{
            .wip = self.index,
            .value_instruction = value_inst,
            .value_instruction_index = value_instruction,
            .value_str = value_str,
        });

        try self.putGlobal(self.hir_index, self.index);

        switch (value_inst.data) {
            .struct_wip => {
                const struct_wip = self.builder.getWip(value_inst.data.struct_wip.wip);
                try struct_wip.resolveInitializer();
            },
            else => {},
        }
    }
    pub fn resolveFieldDeclarationInitializer(self: *Wip) Error!void {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveFieldDeclarationInitializer", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveFieldDeclarationInitializer", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        const init_block_wip_index = self.data.field_declaration.init orelse return;
        const init_block_wip = self.builder.getWip(init_block_wip_index);
        try init_block_wip.resolveInitializer();
    }

    pub fn resolveBlockInstructions(self: *Wip) Error!void {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveBlockInstructions", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveBlockInstructions", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        const block_inst = self.builder.getHirInst(self.hir_index);
        const block_hir_inst = switch (block_inst) {
            .block, .inline_block => |hir_inst| hir_inst,
            else => {
                std.debug.panic("unimplemented: resolveBlockInstructions {s}", .{@tagName(std.meta.activeTag(block_inst))});
            },
        };
        const instructions_list = self.builder.getListSlice(block_hir_inst.instructions_list);
        for (instructions_list) |hir_inst| {
            _ = try self.resolveInstruction(hir_inst);
        }

        // switch (block_inst) {
        //     .block => {},
        //     .inline_block => {
        //         self.pushInstruction(hir_inst: Hir.Inst.Index, instruction: Mir.Instruction)
        //     },
        //     else => unreachable,
        // }
    }
    pub fn resolveParamInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, name: InternedSlice, type_index: Mir.Type.Index) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveParamInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .name = name, .type_index = type_index });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveParamInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .name = name, .type_index = type_index });
        return try self.pushInstruction(hir_inst_index, .{
            .op = .param,
            .type = type_index,
            .value = .runtime,
            .data = .{
                .scoped = .{
                    .name = name,
                    .index = 0, // TODO: resolve indexes later
                    .mutable = false,
                },
            },
        });
    }
    pub fn resolveInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index) Error!Mir.Instruction.Index {
        const hir_inst = self.builder.getHirInst(hir_inst_index);
        const event_id = self.builder.tracer.begin(
            "Wip.resolveInstruction(hir({d}):.{s})",
            .{ hir_inst_index, @tagName(std.meta.activeTag(hir_inst)) },
            .{
                .wip = self.index,
                .wip_type = std.meta.activeTag(self.data),
                .hir_inst_index = hir_inst_index,
                .hir_inst = hir_inst,
            },
        );
        defer self.builder.tracer.end(event_id);
        switch (hir_inst) {
            .local => |local| return try self.resolveLocalInstruction(hir_inst_index, local),
            .comptime_number => return try self.resolveConstantInstruction(hir_inst_index, hir_inst.comptime_number.node),

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
            .ty_boolean,
            .ty_array,
            => return try self.resolveTypeLiteralInstruction(hir_inst_index),
            // .ty_array => |ty_array| return try self.resolveArrayTypeInstruction(hir_inst_index, ty_array),
            .gt, .ge, .lt, .le, .eq, .ne => |bin_op| return try self.resolveComparisonInstruction(hir_inst_index, bin_op),
            .as => |bin_op| return try self.resolveAsInstruction(hir_inst_index, bin_op),
            .param_get => |node| return try self.resolveParamGetInstruction(hir_inst_index, node),
            .param_set => |node| return try self.resolveParamSetInstruction(hir_inst_index, node),
            .add, .sub, .mul, .div => |bin_op| return try self.resolveArithmeticInstruction(hir_inst_index, bin_op),
            .ret => |node| return try self.resolveReturnInstruction(hir_inst_index, node),
            .local_set => |bin_op| return try self.resolveLocalSetInstruction(hir_inst_index, bin_op),
            .local_get => |node| return try self.resolveLocalGetInstruction(hir_inst_index, node),
            .typeof => |node| return try self.resolveTypeOfInstruction(hir_inst_index, node),
            .if_expr => |node| return try self.resolveIfExpressionInstruction(hir_inst_index, node),
            .loop => |node| return try self.resolveLoopExpressionInstruction(hir_inst_index, node),
            .br => |node| return try self.resolveBreakInstruction(hir_inst_index, node),
            .global_get => |node| return try self.resolveGlobalGetInstruction(hir_inst_index, node),
            .global_set => |node| return try self.resolveGlobalSetInstruction(hir_inst_index, node),
            .fn_call => |node| return try self.resolveFnCallInstruction(hir_inst_index, node),
            .alloc => |node| return try self.resolveAllocInstruction(hir_inst_index, node),
            .get_element_pointer => |node| return try self.resolveGetElementPointerInstruction(hir_inst_index, node),
            .get_element_value => |node| return try self.resolveGetElementValueInstruction(hir_inst_index, node),
            .store => |node| return try self.resolveStoreInstruction(hir_inst_index, node),
            .constant_int => |node| return try self.resolveConstantIntInstruction(hir_inst_index, node),
            .get_property_pointer => |node| return try self.resolveGetPropertyPointerInstruction(hir_inst_index, node),
            .get_property_value => |node| return try self.resolveGetPropertyValueInstruction(hir_inst_index, node),
            .load => |node| return try self.resolveLoadInstruction(hir_inst_index, node),
            .ty_pointer => |node| return try self.resolveTyPointerInstruction(hir_inst_index, node),
            .struct_decl => |node| return try self.resolveStructDeclarationInstruction(hir_inst_index, node),
            else => {
                return std.debug.panic("not implemented: resolveInstruction {s}", .{@tagName(std.meta.activeTag(hir_inst))});
            },
        }
    }
    pub fn resolveConstantIntInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.Constant) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveConstantIntInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveConstantIntInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        const value_index = try self.builder.pushValue(.{ .integer = node.value });
        return try self.pushInstruction(hir_inst_index, .{
            .op = .constant,
            .type = POINTERS_TYPE,
            .value = value_index,
            .data = .{
                .value = value_index,
            },
        });
    }
    pub fn markDead(self: *Wip, inst_index: InstructionId) !void {
        self.builder.tracer.logEvent("Wip.markDead", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .inst_index = inst_index });
        const inst = try self.getInstruction(inst_index);
        self.builder.logger.log("marking dead: {d}", .{inst_index}, null);
        inst.liveness = 0;
    }

    pub fn resolveLocalInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, local: Hir.Inst.Local) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveLocalInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .local = local });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveLocalInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .local = local });
        const type_inst_id = try self.getInstructionId(local.type);
        const type_inst = try self.getInstruction(type_inst_id);

        // const value_inst = try self.getInstructionByHirInst(local.init);
        try self.markDead(type_inst_id);
        // value_inst.liveness = 0;
        return try self.pushInstruction(hir_inst_index, .{
            .op = .local,
            // .type = type_inst.getValue().toType(),
            .type = self.getValueAsType(type_inst.getValue()),
            .value = if (local.mutable) .runtime else .undefined,
            .data = .{
                .scoped = .{
                    .name = try self.builder.internNode(local.name_node),
                    .index = 0, // TODO: resolve indexes later
                    .mutable = local.mutable,
                },
            },
        });
    }

    pub fn resolveConstantInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node_index: Ast.Node.Index) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveConstantInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node_index = node_index });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveConstantInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node_index = node_index });
        const ast = self.builder.hir.ast;
        const node = ast.getNodeData(node_index);
        const node_tag = ast.getNodeTag(node_index);
        // const value = std.fmt.parseFloat(f64, slice);
        switch (node.*) {
            .number_literal => |token| {
                const slice = ast.getTokenSlice(token.token);
                const is_float = std.mem.indexOf(u8, slice, ".") != null;
                const value: Mir.Value = blk: {
                    if (is_float) {
                        break :blk .{
                            .float = try std.fmt.parseFloat(f64, slice),
                        };
                    } else {
                        const n = try std.fmt.parseInt(i128, slice, 10);
                        if (n <= std.math.maxInt(i64)) {
                            break :blk .{ .integer = @intCast(n) };
                        } else {
                            break :blk .{ .big_integer = n };
                        }
                    }
                };

                const value_index = try self.builder.pushValue(value);
                return try self.pushInstruction(hir_inst_index, .{
                    .op = .constant,
                    .type = .number,
                    .value = value_index,
                    .data = .{
                        .value = value_index,
                    },
                });
            },
            else => {
                return self.builder.tracer.panic("not implemented: resolveConstantInstruction", .{ .tag = @tagName(node_tag) });
            },
        }
    }
    pub fn resolveTypeLiteralInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveTypeLiteralInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveTypeLiteralInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index });
        const hir_inst = self.builder.getHirInst(hir_inst_index);
        const ty = switch (std.meta.activeTag(hir_inst)) {
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
            => |tag| std.meta.stringToEnum(Mir.Type.Index, (comptime @tagName(tag)[3..])) orelse {
                return self.builder.tracer.panic("not implemented: resolveTypeLiteralInstruction", .{ .type = @tagName(tag) });
            },
            .ty_array => ty: {
                const ty_array = hir_inst.ty_array;
                const type_inst_id = try self.getInstructionId(ty_array.type);
                const type_inst = try self.getInstruction(type_inst_id);
                const size_inst_id = try self.getInstructionId(ty_array.size);
                const size_inst = try self.getInstruction(size_inst_id);
                const size = size_inst.getValue();
                const size_value = self.builder.getValue(size) orelse self.builder.logger.todo("Error: size_value is not a number", .{});
                const size_int = getNumberValueAs(u32, size_value);
                const type_value_index = type_inst.getValue();
                // std.debug.panic("type_inst {d} {}\n", .{ ty_array.type, size_value });

                const type_value = self.builder.getValue(type_value_index);
                try self.markDead(type_inst_id);
                try self.markDead(size_inst_id);

                const type_index = try self.builder.internType(.{ .array = .{
                    .type = if (type_value) |v| v.type else type_value_index.toType(),
                    .size = size_int,
                } });
                std.debug.print("type: {?}\n", .{type_value_index});
                break :ty type_index;
                // break :ty try self.builder.pushValue(.{ .type = type_index });
            },
            // .ty_f64 => .type_f64,
            // .ty_f64 => .type_f64,
            // .ty_f32 => .type_f32,
            // .ty_i64 => .type_i64,
            // .ty_i32 => .type_i32,
            // .ty_boolean => .type_boolean,
            else => unreachable,
        };

        const type_value = try self.getTypeAsValue(ty);
        return try self.pushInstruction(hir_inst_index, .{
            .op = .type,
            .type = .type,
            .value = type_value,
            .data = .{
                .type = ty,
            },
            .liveness = 0,
        });
    }
    pub fn resolveArrayTypeInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, ty_array: Hir.Inst.TyArray) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveArrayTypeInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .ty_array = ty_array });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveArrayTypeInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .ty_array = ty_array });
        // return try self.pushInstruction(hir_inst_index, .{
        //     .op = .type,
        //     .type = ty_array.type,
        //     .value = .runtime,
        //     .data = .void,
        // });

    }
    pub fn resolveAsInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, bin_op: Hir.Inst.BinaryOp) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveAsInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .bin_op = bin_op });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveAsInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .bin_op = bin_op });
        const instruction = try self.getInstructionId(bin_op.lhs);
        const type_instruction_id = try self.getInstructionId(bin_op.rhs);
        const type_instruction = try self.getInstruction(type_instruction_id);
        try self.markDead(type_instruction_id);
        return try self.castInstruction(
            hir_inst_index,
            instruction,
            self.getValueAsType(type_instruction.getValue()),
            // type_instruction.value.toType(),
        );
    }
    pub fn resolveParamGetInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, param_get: Hir.Inst.UnaryOp) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveParamGetInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .param_get = param_get });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveParamGetInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .param_get = param_get });
        const instruction_id = try self.getInstructionId(param_get.operand);
        const instruction = try self.getInstruction(instruction_id);
        const param_type_index = instruction.type;
        const param_type: *Mir.Type = self.builder.getType(param_type_index) orelse unreachable;
        return try self.pushInstruction(hir_inst_index, .{
            .op = .param_get,
            .type = param_type.param.type,
            .value = instruction.value,
            .data = .{ .instruction = instruction_id },
        });
    }
    pub fn resolveComparisonInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, bin_op: Hir.Inst.BinaryOp) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveComparisonInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .bin_op = bin_op });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveComparisonInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .bin_op = bin_op });
        const hir_inst = self.builder.getHirInst(hir_inst_index);
        var lhs_id = try self.getInstructionId(bin_op.lhs);
        var rhs_id = try self.getInstructionId(bin_op.rhs);

        const lhs_instruction = try self.getInstruction(lhs_id);
        const rhs_instruction = try self.getInstruction(rhs_id);

        if (lhs_instruction.type == .number and rhs_instruction.type != .number) {
            lhs_id = try self.tryCastInstruction(hir_inst_index, lhs_id, rhs_instruction.type);
        } else if (rhs_instruction.type == .number and lhs_instruction.type != .number) {
            rhs_id = try self.tryCastInstruction(hir_inst_index, rhs_id, lhs_instruction.type);
        }

        const op: Mir.Instruction.Op = switch (hir_inst) {
            .gt => .gt,
            .ge => .ge,
            .lt => .lt,
            .le => .le,
            .eq => .eq,
            .ne => .ne,
            else => unreachable,
        };
        return try self.pushInstruction(hir_inst_index, .{
            .op = op,
            .type = .boolean,
            .value = try self.tryResolveComparison(op, lhs_id, rhs_id),
            .data = .{
                .bin_op = .{
                    .lhs = lhs_id, // TODO: try cast
                    .rhs = rhs_id,
                },
            },
        });
    }
    pub fn tryResolveComparison(self: *Wip, op: Mir.Instruction.Op, lhs_id: InstructionId, rhs_id: InstructionId) Error!Mir.Value.Index {
        const event_id = self.builder.tracer.beginEvent("Wip.tryResolveComparison", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .op = op, .lhs_id = lhs_id, .rhs_id = rhs_id });
        defer self.builder.tracer.endEvent(event_id, "Wip.tryResolveComparison", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .op = op, .lhs_id = lhs_id, .rhs_id = rhs_id });
        const lhs_instruction = try self.getInstruction(lhs_id);
        const rhs_instruction = try self.getInstruction(rhs_id);
        const lhs_value_index = lhs_instruction.getValue();
        const rhs_value_index = rhs_instruction.getValue();
        if (lhs_value_index == .runtime or rhs_value_index == .runtime) {
            return .runtime;
        }
        const lhs_value = self.builder.getValue(lhs_value_index) orelse return .runtime;
        const rhs_value = self.builder.getValue(rhs_value_index) orelse return .runtime;
        const type_index = getConcreteTypeFromPair(lhs_instruction, rhs_instruction);
        if (!type_index.isNumeric()) {
            self.builder.logger.todo("error message for non-numeric comparison", .{});
            return .runtime;
        }
        const comparison_result = doComparison(op, lhs_value, rhs_value);
        try self.markDead(lhs_id);
        try self.markDead(rhs_id);
        if (comparison_result) {
            return .true;
        } else {
            return .false;
        }
    }
    fn doComparison(op: Mir.Instruction.Op, lhs: Mir.Value, rhs: Mir.Value) bool {
        // const lhs_number, const rhs_number = switch (lhs) {
        //     .float => .{ getNumberValueAs(f64, lhs), getNumberValueAs(f64, rhs) },
        //     .integer => .{ getNumberValueAs(i64, lhs), getNumberValueAs(i64, rhs) },
        //     .big_integer => .{ getNumberValueAs(i128, lhs), getNumberValueAs(i128, rhs) },
        //     else => unreachable,
        // };
        switch (lhs) {
            .float => {
                const lhs_number = getNumberValueAs(f64, lhs);
                const rhs_number = getNumberValueAs(f64, rhs);
                switch (op) {
                    .gt => return lhs_number > rhs_number,
                    .ge => return lhs_number >= rhs_number,
                    .lt => return lhs_number < rhs_number,
                    .le => return lhs_number <= rhs_number,
                    .eq => return lhs_number == rhs_number,
                    .ne => return lhs_number != rhs_number,
                    else => unreachable,
                }
            },
            .integer => {
                const lhs_number = getNumberValueAs(i64, lhs);
                const rhs_number = getNumberValueAs(i64, rhs);
                switch (op) {
                    .gt => return lhs_number > rhs_number,
                    .ge => return lhs_number >= rhs_number,
                    .lt => return lhs_number < rhs_number,
                    .le => return lhs_number <= rhs_number,
                    .eq => return lhs_number == rhs_number,
                    .ne => return lhs_number != rhs_number,
                    else => unreachable,
                }
            },
            .big_integer => {
                const lhs_number = getNumberValueAs(i128, lhs);
                const rhs_number = getNumberValueAs(i128, rhs);
                switch (op) {
                    .gt => return lhs_number > rhs_number,
                    .ge => return lhs_number >= rhs_number,
                    .lt => return lhs_number < rhs_number,
                    .le => return lhs_number <= rhs_number,
                    .eq => return lhs_number == rhs_number,
                    .ne => return lhs_number != rhs_number,
                    else => unreachable,
                }
            },
            else => unreachable,
        }
    }
    pub fn getConcreteTypeFromPair(lhs: *Mir.Instruction, rhs: *Mir.Instruction) Mir.Type.Index {
        if (lhs.type == rhs.type) {
            return lhs.type;
        }
        if (lhs.type == .number) {
            return rhs.type;
        }
        return lhs.type;
    }
    pub fn resolveArithmeticInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, bin_op: Hir.Inst.BinaryOp) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveArithmeticInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .bin_op = bin_op });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveArithmeticInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .bin_op = bin_op });
        var lhs_id = try self.getInstructionId(bin_op.lhs);
        var rhs_id = try self.getInstructionId(bin_op.rhs);

        const lhs_instruction = try self.getInstruction(lhs_id);
        const rhs_instruction = try self.getInstruction(rhs_id);

        var ty = lhs_instruction.type;

        if (lhs_instruction.type == .number and rhs_instruction.type != .number) {
            lhs_id = try self.tryCastInstruction(hir_inst_index, lhs_id, rhs_instruction.type);
            ty = rhs_instruction.type;
        } else if (rhs_instruction.type == .number and lhs_instruction.type != .number) {
            rhs_id = try self.tryCastInstruction(hir_inst_index, rhs_id, lhs_instruction.type);
        }

        const op: Mir.Instruction.Op = switch (std.meta.activeTag(self.builder.getHirInst(hir_inst_index))) {
            .add => .add,
            .sub => .sub,
            .mul => .mul,
            .div => .div,
            else => {
                return self.builder.tracer.panic("not implemented: resolveArithmeticInstruction", .{ .tag = @tagName(std.meta.activeTag(self.builder.getHirInst(hir_inst_index))) });
            },
        };
        return try self.pushInstruction(hir_inst_index, .{
            .op = op,
            .type = ty,
            .value = try self.tryResolveArithmetic(op, lhs_id, rhs_id, ty),
            .data = .{
                .bin_op = .{
                    .lhs = lhs_id,
                    .rhs = rhs_id,
                },
            },
        });
    }
    pub fn resolveReturnInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.Return) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveReturnInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveReturnInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        if (node.value == null) {
            return try self.pushInstruction(hir_inst_index, .{
                .op = .ret,
                .type = .void,
                .value = .void,
                .data = .void,
            });
        }
        const instruction_id = try self.getInstructionId(node.value.?);
        const instruction = try self.getInstruction(instruction_id);

        return try self.pushInstruction(hir_inst_index, .{
            .op = .ret,
            .type = instruction.type,
            .value = instruction.getValue(),
            .data = .{
                .instruction = instruction_id,
            },
        });
    }

    pub fn tryCastInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, id: InstructionId, type_index: Mir.Type.Index) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.tryCastInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .id = id, .type_index = type_index });
        defer self.builder.tracer.endEvent(event_id, "Wip.tryCastInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .id = id, .type_index = type_index });
        const instruction = try self.getInstruction(id);
        // if (@intFromEnum(type_index) >= Mir.Type.INDEX_START) {
        //     // TODO: check more complex types equality
        //     return id;
        // }

        if (instruction.type == type_index) {
            return id;
        }
        if (type_index == .number) {
            return id;
        }

        switch (instruction.type) {
            .number => {
                switch (type_index) {
                    .i8, .i16, .i32, .i64, .i128, .i256, .u8, .u16, .u32, .u64, .u128, .u256, .usize, .f32, .f64 => {
                        return try self.castInstruction(hir_inst_index, id, type_index);
                    },
                    else => {},
                }
            },

            else => {},
        }

        std.debug.print("\n\nnot implemented: tryCastInstruction\ninstruction_type: {s}\ntarget_type: {s}\n\n\n", .{ self.builder.formatType(instruction.type), self.builder.formatType(type_index) });
        self.builder.tracer.panic("not implemented: tryCastInstruction", .{ .instruction_type = instruction.type, .target_type = type_index });
    }

    pub fn castInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, id: InstructionId, type_index: Mir.Type.Index) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.castInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .id = id, .type_index = type_index });
        defer self.builder.tracer.endEvent(event_id, "Wip.castInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .id = id, .type_index = type_index });
        const inst = try self.getInstruction(id);
        if (inst.value != .runtime) {
            try self.markDead(id);
        }
        // TODO: check for ilegal casts
        return try self.pushInstruction(hir_inst_index, .{
            .op = .as,
            .type = type_index,
            .value = inst.value,
            .data = .{ .cast = .{
                .instruction = id,
                .type = type_index,
            } },
        });
    }

    pub fn tryResolveArithmetic(self: *Wip, op: Mir.Instruction.Op, lhs_id: InstructionId, rhs_id: InstructionId, type_index: Mir.Type.Index) Error!Mir.Value.Index {
        const event_id = self.builder.tracer.beginEvent("Wip.tryResolveArithmetic", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .op = op, .lhs_id = lhs_id, .rhs_id = rhs_id, .type_index = type_index });
        defer self.builder.tracer.endEvent(event_id, "Wip.tryResolveArithmetic", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .op = op, .lhs_id = lhs_id, .rhs_id = rhs_id, .type_index = type_index });
        const lhs_inst = try self.getInstruction(lhs_id);
        const rhs_inst = try self.getInstruction(rhs_id);
        const lhs_value_index = lhs_inst.getValue();
        const rhs_value_index = rhs_inst.getValue();
        if (lhs_value_index == .runtime or rhs_value_index == .runtime) {
            return .runtime;
        }
        const lhs_value = self.builder.getValue(lhs_value_index) orelse return .runtime;
        const rhs_value = self.builder.getValue(rhs_value_index) orelse return .runtime;

        if (isOneNumberOfType(.float, lhs_value, rhs_value) or type_index == .f64 or type_index == .f32 or (type_index == .number and op == .div)) {
            try self.markDead(lhs_id);
            try self.markDead(rhs_id);
            return try self.builder.pushValue(.{ .float = doComptimeMath(f64, op, lhs_value, rhs_value) });
        } else if (isOneNumberOfType(.big_integer, lhs_value, rhs_value) or type_index == .i128) {
            try self.markDead(lhs_id);
            try self.markDead(rhs_id);
            return try self.builder.pushValue(.{
                .big_integer = doComptimeMath(i128, op, lhs_value, rhs_value),
            });
        } else {
            try self.markDead(lhs_id);
            try self.markDead(rhs_id);
            return try self.builder.pushValue(.{
                .integer = doComptimeMath(i64, op, lhs_value, rhs_value),
            });
        }
    }
    pub fn doComptimeMath(T: type, op: Mir.Instruction.Op, lhs_value: Mir.Value, rhs_value: Mir.Value) T {
        const lhs = getNumberValueAs(T, lhs_value);
        const rhs = getNumberValueAs(T, rhs_value);
        switch (T) {
            f64 => switch (op) {
                .add => return lhs + rhs,
                .sub => return lhs - rhs,
                .mul => return lhs * rhs,
                .div => return lhs / rhs,
                else => unreachable,
            },
            i128, i64 => switch (op) {
                .add => return lhs + rhs,
                .sub => return lhs - rhs,
                .mul => return lhs * rhs,
                .div => return @divTrunc(lhs, rhs),
                else => unreachable,
            },
            else => unreachable,
        }
    }

    pub fn isOneNumberOfType(comptime tag: std.meta.Tag(Mir.Value), a: Mir.Value, b: Mir.Value) bool {
        return std.meta.activeTag(a) == tag or std.meta.activeTag(b) == tag;
    }
    pub fn isPairOf(comptime a_tag: std.meta.Tag(Mir.Value), comptime b_tag: std.meta.Tag(Mir.Value), a: Mir.Value, b: Mir.Value) bool {
        const a_active_tag = std.meta.activeTag(a);
        const b_active_tag = std.meta.activeTag(b);
        return (a_active_tag == a_tag and b_active_tag == b_tag) or (a_active_tag == b_tag and b_active_tag == a_tag);
    }
    pub fn getNumberValueAs(comptime T: type, value: Mir.Value) T {
        if (T == f64) {
            return switch (value) {
                .float => |f| f,
                .integer => |i| @floatFromInt(i),
                .big_integer => |i| @floatFromInt(i),
                else => unreachable,
            };
        }
        return switch (value) {
            .integer => |i| @intCast(i),
            .big_integer => |i| @intCast(i),
            .float => |f| @intFromFloat(f),
            else => unreachable,
        };
    }

    pub fn resolveLocalSetInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, bin_op: Hir.Inst.BinaryOp) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveLocalSetInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .bin_op = bin_op });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveLocalSetInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .bin_op = bin_op });
        const local_id = try self.getInstructionId(bin_op.lhs);
        const local_instruction = try self.getInstruction(local_id);
        const value_id = try self.getInstructionIdAsType(bin_op.rhs, local_instruction.type);
        const value_instruction = try self.getInstruction(value_id);
        // var liveness: u32 = 1;
        const value = value_instruction.getValue();
        if (value != .runtime and local_instruction.getValue() != .runtime) {
            local_instruction.value = value;
        }

        return try self.pushInstruction(hir_inst_index, .{
            .op = .local_set,
            .type = .void,
            .value = .void,
            .data = .{ .bin_op = .{
                .lhs = local_id,
                .rhs = value_id,
            } },
        });
    }
    pub fn resolveParamSetInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, bin_op: Hir.Inst.BinaryOp) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveParamSetInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .bin_op = bin_op });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveParamSetInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .bin_op = bin_op });
        const local_id = try self.getInstructionId(bin_op.lhs);
        const value_id = try self.getInstructionId(bin_op.rhs);
        const local_instruction = try self.getInstruction(local_id);
        const value_instruction = try self.getInstruction(value_id);
        // var liveness: u32 = 1;
        const value = value_instruction.getValue();
        if (value != .runtime and local_instruction.getValue() != .runtime) {
            local_instruction.value = value;
        }

        return try self.pushInstruction(hir_inst_index, .{
            .op = .param_set,
            .type = value_instruction.type,
            .value = value,
            .data = .{ .bin_op = .{
                .lhs = local_id,
                .rhs = value_id,
            } },
        });
    }
    pub fn resolveGlobalSetInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, bin_op: Hir.Inst.BinaryOp) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveGlobalSetInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .bin_op = bin_op });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveGlobalSetInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .bin_op = bin_op });
        const global_wip_index = self.getGlobal(bin_op.lhs) orelse self.builder.logger.todo("error for global not found: {d}", .{bin_op.lhs});

        const global_wip = self.builder.getWip(global_wip_index);
        const value_id = try self.getInstructionId(bin_op.rhs);
        // const global_instruction = try self.getInstruction(global_id);
        const value_instruction = try self.getInstruction(value_id);
        // var liveness: u32 = 1;
        const value = value_instruction.getValue();
        // if (value != .runtime and global_wip.getValue() != .runtime) {
        //     global_wip.value = value;
        // }

        return try self.pushInstruction(hir_inst_index, .{
            .op = .global_set,
            .type = value_instruction.type,
            .value = value,
            .data = .{ .global_set = .{
                .global = switch (global_wip.data) {
                    .fn_decl => |fn_decl| fn_decl.global,
                    .global_declaration => |global_declaration| global_declaration.global,
                    else => unreachable,
                },
                .value = value_id,
            } },
            .liveness = 1,
        });
    }
    pub fn resolveLocalGetInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.UnaryOp) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveLocalGetInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveLocalGetInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        const instruction_id = try self.getInstructionId(node.operand);
        const instruction = try self.getInstruction(instruction_id);
        return try self.pushInstruction(hir_inst_index, .{
            .op = .local_get,
            .type = instruction.type,
            .value = instruction.getValue(),
            .data = .{ .instruction = instruction_id },
        });
    }
    pub fn resolveLoadInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.UnaryOp) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveLoadInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveLoadInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        const instruction_id = try self.getInstructionId(node.operand);
        const instruction = try self.getInstruction(instruction_id);
        switch (instruction.op) {
            .constant => {
                try self.markDead(instruction_id);
                return try self.pushInstruction(hir_inst_index, .{
                    .op = instruction.op,
                    .type = instruction.type,
                    .value = instruction.value,
                    .data = instruction.data,
                });
            },
            else => {},
        }
        return try self.pushInstruction(hir_inst_index, .{
            .op = .load,
            .type = try self.unwrapPointerType(instruction.type),
            .value = instruction.getValue(),
            .data = .{ .instruction = instruction_id },
        });
    }
    pub fn resolveTyPointerInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.UnaryOp) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveTyPointerInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveTyPointerInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        const instruction_id = try self.getInstructionId(node.operand);
        const instruction = try self.getInstruction(instruction_id);
        const ty = switch (instruction.data) {
            .type => instruction.data.type,
            .global_get_wip => blk: {
                const global_wip = self.builder.getWip(instruction.data.global_get_wip.wip);

                switch (global_wip.data) {
                    .global_type_declaration => {},
                    else => unreachable,
                }

                const value_instruction_index = global_wip.data.global_type_declaration.value_instruction orelse unreachable;
                const value_instruction = try global_wip.getInstruction(value_instruction_index);
                switch (value_instruction.data) {
                    .type => |type_index| {
                        break :blk type_index;
                    },
                    .struct_wip => {
                        const struct_wip = self.builder.getWip(value_instruction.data.struct_wip.wip);
                        break :blk struct_wip.type_index;
                    },
                    else => unreachable,
                }

                // break :blk value_instruction.value;
            },
            else => unreachable,
        };
        const pointer_type = try self.builder.internType(.{ .pointer = .{ .child = ty } });
        const type_value = try self.getTypeAsValue(pointer_type);
        return try self.pushInstruction(hir_inst_index, .{
            .op = .type,
            .type = .type,
            .value = type_value,
            .data = .{ .type = pointer_type },
            .liveness = 0,
        });
    }
    pub fn resolveStructDeclarationInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.StructDecl) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveStructDeclarationInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveStructDeclarationInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        const struct_wip = try self.builder.makeAndGetWip(hir_inst_index, self.index, .{
            .module = .{
                .name = try self.builder.internSlice("root"),
                .declarations = .{},
                .fields = .{},
            },
        });
        try struct_wip.collectSymbols();
        struct_wip.type_index = try struct_wip.resolveType();

        // try struct_wip.resolveInitializer();
        return try self.pushInstruction(hir_inst_index, .{
            .op = .struct_decl,
            .type = struct_wip.type_index,
            .value = try self.getTypeAsValue(struct_wip.type_index),
            .data = .{ .struct_wip = .{
                .wip = struct_wip.index,
            } },
        });
    }
    pub fn resolveGlobalGetInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.UnaryOp) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveGlobalGetInstruction", .{
            .wip = self.index,
            .wip_type = std.meta.activeTag(self.data),
            .hir_inst_index = hir_inst_index,
            .node = node,
        });
        defer self.builder.tracer.end(event_id);
        const global_wip_id = self.getGlobal(node.operand) orelse self.builder.logger.todo("error for global not found: {d}", .{node.operand});
        const global_wip = self.builder.getWip(global_wip_id);
        const global_type = try global_wip.resolveType();
        // std.debug.panic("..global_get: {}", .{global_type});
        switch (global_wip.data) {
            .global_declaration => {
                return try self.pushInstruction(hir_inst_index, .{
                    .op = .global_get,
                    .type = global_type,
                    .value = .runtime,
                    .data = .{ .global_get_wip = .{
                        .wip = global_wip.index,
                    } },
                });
            },
            .global_type_declaration => {
                _ = try global_wip.resolveInitializer();
                const value_instruction = try global_wip.getInstruction(
                    global_wip.data.global_type_declaration.value_instruction orelse unreachable,
                );
                const value = value_instruction.getValue();
                return try self.pushInstruction(hir_inst_index, .{
                    .op = .global_get,
                    .type = global_type,
                    .value = value,
                    .data = .{ .global_get_wip = .{
                        .wip = global_wip.index,
                    } },
                });
            },
            .fn_decl => {
                const value = try self.builder.pushValue(.{ .@"fn" = .{
                    .type = global_type,
                } });
                return try self.pushInstruction(hir_inst_index, .{
                    .op = .global_get,
                    .type = global_type,
                    .value = value,
                    .data = .{ .global_get_wip = .{
                        .wip = global_wip.index,
                    } },
                });
            },
            else => unreachable,
        }
        // if (self.builder.getType(global_type)) |ty| {
        //     switch (ty.*) {
        //         .@"fn" => {
        //             const value = try self.builder.pushValue(.{ .@"fn" = .{
        //                 .type = global_type,
        //             } });
        //             return try self.pushInstruction(hir_inst_index, .{
        //                 .op = .global_get,
        //                 .type = global_type,
        //                 .value = value,
        //                 .data = .{ .global_get = .{
        //                     .global = global_wip.data.fn_decl.global,
        //                 } },
        //             });
        //         },
        //         .global => {},
        //         else => {},
        //     }
        // }

        // return try self.pushInstruction(hir_inst_index, .{
        //     .op = .global_get,
        //     .type = global_type,
        //     .value = .runtime, // TODO: resolve comptime global values
        //     .data = .{ .void = {} },
        // });
    }
    pub fn getTypeAsValue(self: *Wip, type_index: Mir.Type.Index) !Mir.Value.Index {
        const event_id = self.builder.tracer.beginEvent("Wip.getTypeAsValue", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .type_index = type_index });
        defer self.builder.tracer.endEvent(event_id, "Wip.getTypeAsValue", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .type_index = type_index });
        if (self.builder.getType(type_index)) |_| {
            return self.builder.pushValue(.{
                .type = type_index,
            });
        }
        return type_index.toValueIndex();
    }

    pub fn getValueAsType(self: *Wip, value: Mir.Value.Index) Mir.Type.Index {
        const event_id = self.builder.tracer.beginEvent("Wip.getValueAsType", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .value = value });
        defer self.builder.tracer.endEvent(event_id, "Wip.getValueAsType", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .value = value });
        if (self.builder.getValue(value)) |v| {
            return v.type;
        }
        return value.toType();
    }
    pub fn resolveTypeOfInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.UnaryOp) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveTypeOfInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveTypeOfInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        const instruction_id = try self.getInstructionId(node.operand);
        const instruction = try self.getInstruction(instruction_id);
        return try self.pushInstruction(hir_inst_index, .{
            .op = .typeof,
            .type = .type,
            .value = try self.getTypeAsValue(instruction.type),
            // .value = .runtime,
            .data = .{ .instruction = instruction_id },
        });
    }
    pub fn resolveBreakInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.UnaryOp) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveBreakInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveBreakInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        const operand_id = try self.getInstructionId(node.operand);
        const operand = try self.getInstruction(operand_id);
        const value = operand.getValue();
        _ = value; // autofix
        // if (value != .runtime) {
        //     operand.liveness = 0;
        // }

        return try self.pushInstruction(hir_inst_index, .{
            .op = .br,
            .type = .void,
            .value = .void,
            .data = .{ .br = .{
                .instruction = operand_id,
                .value = .void,
            } },
        });
    }

    pub fn resolveIfExpressionInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.IfExpr) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveIfExpressionInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveIfExpressionInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        const owner = self.data.block.owner;

        const condition_id = try self.getInstructionId(node.cond);

        // defer if_expr_wip.update();
        var then_branch = try self.builder.makeAndGetWip(node.then_body, self.index, .{
            .block = .{
                .owner = owner,
                .name = null,
                .break_type = null,
            },
        });

        try then_branch.resolveInitializer();
        var else_branch: ?*Wip = null;
        if (node.else_body) |else_body| {
            else_branch = try self.builder.makeAndGetWip(else_body, self.index, .{
                .block = .{
                    .owner = owner,
                    .name = null,
                    .break_type = null,
                },
            });
            try else_branch.?.resolveInitializer();
        }

        return try self.pushInstruction(hir_inst_index, .{
            .op = .branch,
            .type = .void,
            .value = .void,
            .data = .{ .branch_wip = .{
                .condition = condition_id,
                .then_wip = then_branch.index,
                .else_wip = if (else_branch) |else_branch_wip| else_branch_wip.index else null,
            } },
        });
    }
    pub fn resolveLoopExpressionInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.Loop) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveLoopExpressionInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveLoopExpressionInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        const owner = self.data.block.owner;

        // const loop_wip = try self.builder.makeAndGetWip(hir_inst_index, self.index, .{
        //     .loop_expression = .{
        //         .body = undefined,
        //     },
        // });

        // loop_wip.stage = .resolving_instructions;

        const loop_instruction_id = try self.reserveInstruction(hir_inst_index);
        var body_wip = try self.builder.makeAndGetWip(node.body, self.index, .{
            .block = .{
                .owner = owner,
                .name = null,
                .break_type = null,
            },
        });
        try body_wip.resolveInitializer();
        self.setInstruction(loop_instruction_id, .{
            .op = .loop,
            .type = .void,
            .value = .void,
            .data = .{ .loop_wip = .{
                .wip = body_wip.index,
            } },
        });

        return loop_instruction_id;
    }

    pub fn resolveFnCallInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.FnCall) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveFnCallInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveFnCallInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        // const callee_id = try self.getInstructionId(node.callee);
        const callee_id = self.getGlobal(node.callee) orelse self.builder.logger.todo("error for global not found: {d}", .{node.callee});
        const callee_wip = self.builder.getWip(callee_id);
        // const callee_instruction = try self.getInstruction(callee_id);
        var list = self.builder.newList();
        const callee_type_index = try callee_wip.resolveType();
        const callee_type = self.builder.getType(callee_type_index) orelse unreachable;
        const args_list = self.builder.getListSlice(node.args_list);
        for (args_list) |arg_id| {
            const arg_instruction_id = try self.getInstructionId(arg_id);
            try list.append(arg_instruction_id);
        }

        const return_type = switch (callee_type.*) {
            .@"fn" => |fn_type| fn_type.return_type,
            else => self.builder.logger.todo("not a function: {any}", .{callee_type}),
        };
        // try self.markDead(callee_id);
        // const callee_type = callee_instruction.type;
        // TODO: check if callable and if params match
        return try self.pushInstruction(hir_inst_index, .{
            .op = .call,
            .type = return_type,
            .value = .runtime,
            .data = .{
                .call = .{
                    .callee = callee_wip.data.fn_decl.global,
                    .args_list = try list.commit(),
                },
            },
        });
    }
    pub fn resolveAllocInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.Alloc) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveAllocInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveAllocInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        // const size_id = try self.getInstructionId(node.size);
        // const size_instruction = try self.getInstruction(size_id);
        // const size = size_instruction.getValue();
        const type_value_id = try self.getInstructionId(node.type);
        const type_value = try self.getInstruction(type_value_id);

        const alloc_type = if (self.builder.getValue(type_value.value)) |value| value.type else type_value.value.toType();
        const is_array = if (self.builder.getType(alloc_type)) |ty| std.meta.activeTag(ty.*) == .array else false;
        _ = is_array; // autofix
        try self.markDead(type_value_id);
        return try self.pushInstruction(hir_inst_index, .{
            .op = .alloc,
            .type = try self.builder.internType(.{
                .pointer = .{ .child = alloc_type },
            }),
            // .type = switch (is_array) {
            //     true => alloc_type,
            //     else => try self.builder.pushType(.{ .pointer = .{ .child = alloc_type } }),
            // },
            // .type = alloc_type,
            .value = .runtime,
            .data = .{ .alloc = .{
                .type = alloc_type,
                .mutable = node.mutable,
            } },
        });
    }
    pub fn resolveGetElementPointerInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.GetElement) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveGetElementPointerInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveGetElementPointerInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        const pointer_id = try self.getInstructionId(node.pointer);
        const pointer_instruction = try self.getInstruction(pointer_id);

        const index_id = try self.getInstructionIdAsType(
            node.index,
            POINTERS_TYPE,
        );

        const array_type_index = try self.unwrapPointerType(pointer_instruction.type); // orelse self.builder.logger.todo("error for pointer type not found: {d}", .{pointer_instruction.type});
        const array_type = self.builder.getType(array_type_index) orelse self.builder.logger.todo("error for index type not found: {d}", .{array_type_index});
        // const array_type = self.builder.getType(array_pointer_type.pointer.child) orelse self.builder.logger.todo("error for index type not found: {d}", .{array_pointer_type.pointer.child});

        return try self.pushInstruction(hir_inst_index, .{
            .op = .get_element_pointer,
            .type = try self.builder.internType(.{ .pointer = .{
                .child = array_type.array.type,
            } }),
            .value = .runtime,
            .data = .{ .get_element_pointer = .{
                .pointer = pointer_id,
                .index = index_id,
            } },
        });
    }
    pub fn resolveGetElementValueInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.GetElement) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveGetElementValueInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveGetElementValueInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        const pointer_id = try self.getInstructionId(node.pointer);
        const index_id = try self.getInstructionIdAsType(
            node.index,
            POINTERS_TYPE,
        );

        const base_pointer_instruction = try self.getInstruction(pointer_id);
        const load_value = base_pointer_instruction.getValue();
        const array_type = self.builder.getType(base_pointer_instruction.type) orelse self.builder.logger.todo("error for pointer type not found: {d}", .{base_pointer_instruction.type});
        const pointer_inst = try self.pushInstruction(hir_inst_index, .{
            .op = .get_element_pointer,
            .type = try self.builder.internType(.{ .pointer = .{ .child = array_type.array.type } }),
            .value = .runtime,
            .data = .{ .get_element_pointer = .{
                .pointer = pointer_id,
                .index = index_id,
            } },
        });
        return try self.pushInstruction(hir_inst_index, .{
            .op = .load,
            .type = array_type.array.type,
            .value = load_value,
            .data = .{ .instruction = pointer_inst },
        });
    }
    pub fn unwrapPointerInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, pointer_id: InstructionId) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.unwrapPointerInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .pointer_id = pointer_id });
        defer self.builder.tracer.endEvent(event_id, "Wip.unwrapPointerInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .pointer_id = pointer_id });
        const pointer_instruction = try self.getInstruction(pointer_id);
        const pointer_type = try self.unwrapPointerType(pointer_instruction.type);
        // const pointer_type_value = try self.getTypeAsValue(pointer_type);
        const pointer_value = pointer_instruction.getValue();
        return try self.pushInstruction(hir_inst_index, .{
            .op = .load,
            .type = pointer_type,
            .value = pointer_value,
            .data = .{ .instruction = pointer_id },
        });
    }
    pub fn resolveGetPropertyPointerInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.GetProperty) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveGetPropertyPointerInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveGetPropertyPointerInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        const base_id = try self.getInstructionId(node.base);
        const base_instruction = try self.getInstruction(base_id);
        const property_name = self.builder.hir.ast.getNodeSlice(node.property_name_node);
        const interned_property_name = try self.builder.internNode(node.property_name_node);
        const base_type_index = try self.unwrapPointerType(base_instruction.type);
        const base_type = self.builder.getType(base_type_index) orelse self.builder.logger.todo("error for base type not found: {d}", .{base_type_index});
        if (std.mem.eql(u8, property_name, "len")) {
            const value = try self.builder.pushValue(.{ .integer = @intCast(base_type.array.size) });
            return try self.pushInstruction(hir_inst_index, .{
                .op = .constant,
                .type = POINTERS_TYPE,
                .value = value,
                .data = .{ .value = value },
            });
        }
        const base_wip: *Wip = self.builder.getWipByType(base_type_index);
        switch (base_wip.data) {
            .module => |module_data| {
                const field = module_data.fields.get(interned_property_name) orelse self.builder.logger.todo("error for field not found: {s}", .{property_name});
                const field_wip = self.builder.getWip(field.wip);
                const field_type_wip = self.builder.getWip(field_wip.data.field_declaration.type);

                return try self.pushInstruction(hir_inst_index, .{
                    .op = .get_element_pointer,
                    .type = try self.builder.internType(.{ .pointer = .{ .child = try field_type_wip.resolveType() } }),
                    .value = .runtime,
                    .data = .{
                        .get_element_pointer = .{
                            .pointer = base_id,
                            .index = field.index,
                        },
                    },
                });
            },
            else => {
                self.builder.logger.todo("not a struct: {any}", .{base_type});
            },
        }
        // const field_index = self.getGlobal(property_name) orelse self.builder.logger.todo("error for field not found: {s}", .{property_name});
        // _ = property_name; // autofix

    }

    pub fn resolveGetPropertyValueInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.GetProperty) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveGetPropertyValueInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveGetPropertyValueInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        const base_id = try self.getInstructionId(node.base);
        const base_instruction = try self.getInstruction(base_id);
        const property_name = self.builder.hir.ast.getNodeSlice(node.property_name_node);
        // const property_pointer_id = try self.pushInstruction(hir_inst_index, .{
        //     .op = .get_property_pointer,
        //     .type = base_instruction.type,
        //     .value = base_instruction.getValue(),
        //     .data = .{ .get_property_pointer = .{
        //         .base = base_id,
        //         .property_name = property_name,
        //     } },
        // });
        const base_type = self.builder.getType(base_instruction.type) orelse self.builder.logger.todo("error for base type not found: {d}", .{base_instruction.type});
        switch (base_type.*) {
            .array => |array| {
                if (std.mem.eql(u8, property_name, "len")) {
                    const value = try self.builder.pushValue(.{ .integer = @intCast(array.size) });
                    return try self.pushInstruction(hir_inst_index, .{
                        .op = .constant,
                        .type = POINTERS_TYPE,
                        .value = value,
                        .data = .{ .value = value },
                    });
                }
            },
            else => {},
        }
        // const arr: [3]u8 = undefined;
        // @compileLog(arr);
        self.builder.logger.todo("TODO: resolve get property value instruction {d} {s}", .{ base_id, property_name });
        // return try self.pushInstruction(hir_inst_index, .{
        //     .op = .get_property_value,
        //     .type = base_instruction.type,
        //     .value = base_instruction.getValue(),
        //     .data = .{ .get_property_value = .{
        //         .pointer = property_pointer_id,
        //     } },
        // });
    }

    pub fn getArrayElementType(self: *Wip, type_index: Mir.Type.Index) !Mir.Type.Index {
        self.builder.tracer.logEvent("Wip.getArrayElementType", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .type_index = type_index });
        const type_value = self.builder.getType(type_index) orelse return error.TypeNotFound;
        switch (type_value.*) {
            .pointer => |pointer| {
                return self.getArrayElementType(pointer.child);
            },
            .array => |array| {
                return array.type;
            },
            else => {
                return error.TypeMismatch;
            },
        }
    }
    pub fn unwrapPointerType(self: *Wip, type_index: Mir.Type.Index) !Mir.Type.Index {
        self.builder.tracer.logEvent("Wip.unwrapPointerType", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .type_index = type_index });
        const type_value = self.builder.getType(type_index) orelse return error.TypeNotFound;
        switch (type_value.*) {
            .pointer => |pointer| {
                return pointer.child;
            },
            else => {
                return error.TypeMismatch;
            },
        }
    }
    pub fn maybeUnwrapPointerType(self: *Wip, type_index: Mir.Type.Index) !Mir.Type.Index {
        self.builder.tracer.logEvent("Wip.maybeUnwrapPointerType", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .type_index = type_index });
        const type_value = self.builder.getType(type_index) orelse return error.TypeNotFound;
        switch (type_value.*) {
            .pointer => |pointer| {
                return pointer.child;
            },
            else => {
                return type_index;
            },
        }
    }

    pub fn resolveStoreInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.Store) Error!InstructionId {
        const event_id = self.builder.tracer.beginEvent("Wip.resolveStoreInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        defer self.builder.tracer.endEvent(event_id, "Wip.resolveStoreInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .hir_inst_index = hir_inst_index, .node = node });
        const pointer_id = try self.getInstructionId(node.pointer);
        const pointer_instruction = try self.getInstruction(pointer_id);
        const expected_type = self.unwrapPointerType(pointer_instruction.type) catch {
            self.builder.tracer.panic("expected type not found", .{
                .type = pointer_instruction.type,
            });
        };
        std.debug.print("expected_type:{d} {d} {s}, {s}\n", .{ hir_inst_index, node.value, self.builder.formatType(pointer_instruction.type), self.builder.formatType(expected_type) });
        const value_id = try self.getInstructionIdAsType(node.value, expected_type);
        return try self.pushInstruction(hir_inst_index, .{
            .op = .store,
            .type = .void,
            .value = .void,
            .data = .{ .store = .{
                .pointer = pointer_id,
                .value = value_id,
            } },
        });
    }

    pub fn commit(self: *Wip) Error!void {
        const event_id = self.builder.tracer.begin("Wip({s}).commit", .{self.statusSlice()}, .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.end(event_id);
        self.builder.logger.log("commit: {s}", .{self.format()}, null);
        switch (self.data) {
            .module => try self.commitModule(),
            .fn_decl => try self.commitFnDecl(),
            .param_declaration => try self.commitParamDecl(),
            .type => try self.commitType(),
            .block => try self.commitBlock(),
            .global_declaration => try self.commitGlobalDeclaration(),
            .global_type_declaration => try self.commitGlobalTypeDeclaration(),
            .field_declaration => try self.commitFieldDeclaration(),
            // else => {
            //     self.builder.logger.panic("Not implemented: {s}", .{@tagName(self.data)});
            // },

        }
        self.stage = .committed;
        self.builder.tracer.endAsync(self.event_id);
    }

    pub fn commitModule(self: *Wip) Error!void {
        const event_id = self.builder.tracer.beginEvent("Wip.commitModule", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.endEvent(event_id, "Wip.commitModule", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        self.builder.logger.log("commit module: {s}", .{self.format()}, null);
        assert(self.stage == .resolved);
        assert(activeTag(self.data) == .module);

        const module_type = self.builder.getType(self.type_index) orelse unreachable;
        assert(activeTag(module_type.*) == .@"struct");
        var fields_iterator = self.data.module.fields.iterator();
        while (fields_iterator.next()) |field| {
            const wip = self.builder.getWip(field.value_ptr.wip);
            const commit_event_id = self.builder.tracer.begin(
                "[COMMIT_FIELD='{s}']",
                .{wip.getNameSlice()},
                .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .field = field },
            );
            defer self.builder.tracer.end(commit_event_id);
            try wip.commit();
        }

        for (self.data.module.declarations.items) |declaration| {
            const wip = self.builder.getWip(declaration);
            const commit_event_id = self.builder.tracer.begin(
                "[COMMIT_DECLARATION='{s}']",
                .{wip.getNameSlice()},
                .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .declaration = declaration },
            );
            defer self.builder.tracer.end(commit_event_id);
            try wip.commit();
        }

        // for (module_type.module.declarations.items) |declaration| {
        //     const wip = self.builder.getWip(declaration);
        //     try wip.commit();
        // }
    }
    pub fn commitFnDecl(self: *Wip) Error!void {
        const event_id = self.builder.tracer.beginEvent("Wip.commitFnDecl", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.endEvent(event_id, "Wip.commitFnDecl", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        self.builder.logger.log("commit fn_decl: {s} type_index = {d}", .{ self.format(), self.type_index }, null);
        assert(self.stage == .resolved);
        assert(activeTag(self.data) == .fn_decl);

        for (self.data.fn_decl.params.items) |param| {
            var wip = self.builder.getWip(param);
            try wip.commit();
        }

        var return_type_wip = self.builder.getWip(self.data.fn_decl.return_type);
        try return_type_wip.commit();

        var init_instructions: ?Mir.Instruction.List = null;
        // assert(activeTag(fn_type.*) == .@"fn");
        if (self.data.fn_decl.init_block) |init_block| {
            const init_block_wip = self.builder.getWip(init_block);
            const fn_type = self.builder.getType(self.type_index) orelse unreachable;
            init_instructions = try InstructionSet.generate(init_block_wip);
            const ty: Mir.Type.Fn = fn_type.@"fn";
            try self.setType(.{ .@"fn" = .{
                .name = ty.name,
                .params_list = ty.params_list,
                .return_type = ty.return_type,
            } });
        }

        self.builder.setGlobal(self.data.fn_decl.global, .{
            .name = self.data.fn_decl.name,
            .type = self.type_index,
            .value = .runtime,
            .init = init_instructions,
        });
    }

    pub fn commitBlock(self: *Wip) Error!void {
        const event_id = self.builder.tracer.beginEvent("Wip.commitBlock", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.endEvent(event_id, "Wip.commitBlock", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        self.builder.logger.log("commit block: {s}", .{self.format()}, null);
        assert(self.stage == .resolved);
        assert(activeTag(self.data) == .block);
        @panic("TODO");
        // const instructions = try InstructionSet.generate(self);

        // for (self.data.block.instructions.items) |instruction_id| {
        //     const instruction = try self.getInstruction(instruction_id);
        //     switch (instruction.data) {
        //         .bin_op => |bin_op| {
        //             _ = bin_op; // autofix
        //         },
        //         else => {
        //             self.builder.logger.log("skipping instruction: {s}", .{@tagName(instruction.op)}, null);
        //         },
        //     }
        // }
    }
    pub fn commitInstruction(self: *Wip, instruction_id: InstructionId) Mir.Instruction.Index!void {
        const event_id = self.builder.tracer.beginEvent("Wip.commitInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .instruction_id = instruction_id });
        defer self.builder.tracer.endEvent(event_id, "Wip.commitInstruction", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .instruction_id = instruction_id });
        // self.builder.logger.log("commit instruction: {s}", .{self.format()}, null);
    }
    pub fn commitParamDecl(self: *Wip) Error!void {
        const event_id = self.builder.tracer.beginEvent("Wip.commitParamDecl", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.endEvent(event_id, "Wip.commitParamDecl", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        self.builder.logger.log("commit param_decl: {s}", .{self.format()}, null);
        assert(@intFromEnum(self.stage) >= @intFromEnum(Stage.type_resolved));
        var type_wip = self.builder.getWip(self.data.param_declaration.type);
        try type_wip.commit();
    }
    pub fn commitType(self: *Wip) Error!void {
        const event_id = self.builder.tracer.beginEvent("Wip.commitType", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.endEvent(event_id, "Wip.commitType", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        self.builder.logger.log("commit type: {s}", .{self.format()}, null);
        assert(@intFromEnum(self.stage) >= @intFromEnum(Stage.type_resolved));
    }
    pub fn setType(self: *Wip, type_: Mir.Type) !void {
        const event_id = self.builder.tracer.beginEvent("Wip.setType", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .type_ = type_ });
        defer self.builder.tracer.endEvent(event_id, "Wip.setType", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data), .type_ = type_ });
        if (self.type_index.toInt()) |int| {
            self.builder.mir.types.items[int] = type_;
            return;
        }
        self.type_index = try self.builder.makeType(type_);
    }
    pub fn commitGlobalDeclaration(self: *Wip) Error!void {
        const event_id = self.builder.tracer.beginEvent("Wip.commitGlobalDeclaration", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.endEvent(event_id, "Wip.commitGlobalDeclaration", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        self.builder.logger.log("commit global_declaration: {s}", .{self.format()}, null);
        assert(self.stage == .resolved);
        if (self.data.global_declaration.init_block) |init_block| {
            const init_block_wip = self.builder.getWip(init_block);
            // const init_instructions = try InstructionSet.generate(init_block_wip);
            const value = blk: {
                const value_instruction = init_block_wip.data.block.value_instruction orelse break :blk .void;
                const value_inst = try self.getInstruction(value_instruction);
                break :blk value_inst.getValue();
            };

            self.builder.setGlobal(self.data.global_declaration.global, .{
                .name = self.data.global_declaration.name,
                .type = self.type_index,
                .value = value,
                .init = try InstructionSet.generate(init_block_wip),
            });
            return;
        }
        self.builder.setGlobal(self.data.global_declaration.global, .{
            .name = self.data.global_declaration.name,
            .type = self.type_index,
            .value = .undefined,
            .init = null,
        });
    }
    pub fn commitGlobalTypeDeclaration(self: *Wip) Error!void {
        const event_id = self.builder.tracer.beginEvent("Wip.commitGlobalTypeDeclaration", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.endEvent(event_id, "Wip.commitGlobalTypeDeclaration", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        self.builder.logger.log("commit global_type_declaration: {s}", .{self.format()}, null);
        assert(self.stage == .resolved);
        if (self.data.global_type_declaration.init_block) |init_block| {
            const init_block_wip = self.builder.getWip(init_block);
            // const init_instructions = try InstructionSet.generate(init_block_wip);
            const value = blk: {
                const value_instruction = init_block_wip.data.block.value_instruction orelse break :blk .void;
                const value_inst = try self.getInstruction(value_instruction);
                break :blk value_inst.getValue();
            };

            self.builder.setGlobal(self.data.global_type_declaration.global, .{
                .name = self.data.global_type_declaration.name,
                .type = self.type_index,
                .value = value,
                .init = try InstructionSet.generate(init_block_wip),
            });
            return;
        }
        self.builder.setGlobal(self.data.global_type_declaration.global, .{
            .name = self.data.global_type_declaration.name,
            .type = self.type_index,
            .value = .undefined,
            .init = null,
        });
    }
    pub fn commitFieldDeclaration(self: *Wip) Error!void {
        const event_id = self.builder.tracer.beginEvent("Wip.commitFieldDeclaration", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        defer self.builder.tracer.endEvent(event_id, "Wip.commitFieldDeclaration", .{ .wip = self.index, .wip_type = std.meta.activeTag(self.data) });
        self.builder.logger.log("commit field_declaration: {s}", .{self.format()}, null);

        // std.debug.panic("self.stage {s}", .{@tagName(self.stage)});

        assert(self.stage == .resolved);
        assert(activeTag(self.data) == .field_declaration);
        // @panic("TODO");
    }
    pub fn dump(self: *Wip, writer: std.io.AnyWriter, index: Wip.Index) !void {
        try writer.print("Wips({d}):\n", .{self.builder.wips.len});
        var iter = self.builder.wips.iterator();
        while (iter.next()) |wip| {
            try fmt.writeIndent(writer, 1, .{});
            try writer.print("{s}", .{wip.format()});
            switch (wip.data) {
                .block => try writer.print(" owner = {d}", .{wip.data.block.owner}),
                else => {},
            }
            try writer.writeAll("\n");
        }

        var tree_writer = TreeWriter.init(writer);
        try Mir.formatWip(.{
            .builder = self.builder,
            .writer = writer,
            .strings = &self.builder.mir.strings,
            .types = self.builder.mir.types.items,
            .values = self.builder.mir.values.items,
            .lists = &self.builder.mir.interned_lists,
            .instructions = self.instructions.items,
            .allocator = self.builder.arena.allocator(),
        }, writer, index, &tree_writer, .{});
        // try self.dumpInner(writer, index, 0);
    }

    pub fn dumpInner(self: *Wip, writer: std.io.AnyWriter, index: Wip.Index, depth: usize) std.io.AnyWriter.Error!void {
        const wip = self.builder.getWip(index);
        const tag = @tagName(std.meta.activeTag(wip.data));
        try fmt.writeIndent(writer, depth, .{});
        const name_slice = wip.getNameSlice();
        if (name_slice.len > 0) {
            try writer.print("#{d} {s} \"{s}\" [{s}] hir({d}):\n", .{ wip.index, tag, name_slice, @tagName(wip.stage), wip.hir_index });
        } else {
            try writer.print("#{d} {s} [{s}] hir({d}):\n", .{ wip.index, tag, @tagName(wip.stage), wip.hir_index });
        }

        switch (wip.data) {
            .module => {
                try fmt.writeIndent(writer, depth + 1, .{});
                if (wip.scope.globals.count() > 0) {
                    try writer.writeAll("symbols: [\n");
                    var iter_symbols = wip.scope.globals.iterator();
                    while (iter_symbols.next()) |entry| {
                        try fmt.writeIndent(writer, depth + 2, .{});
                        try writer.print("\"{d}\" => #{d}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
                    }
                    try fmt.writeIndent(writer, depth + 1, .{});
                    try writer.writeAll("]\n");
                } else {
                    try writer.writeAll("no symbols\n");
                }
                for (wip.data.module.declarations.items) |field| {
                    try self.dumpInner(writer, field, depth + 1);
                }
            },
            .fn_decl => {
                for (wip.data.fn_decl.params.items) |param| {
                    try self.dumpInner(writer, param, depth + 1);
                }
                try fmt.writeIndent(writer, depth + 1, .{});
                try writer.writeAll("return_type: {\n");
                try self.dumpInner(writer, wip.data.fn_decl.return_type, depth + 2);
                try fmt.writeIndent(writer, depth + 1, .{});
                try writer.writeAll("}\n");
                if (wip.data.fn_decl.init_block) |init_wip_index| {
                    try fmt.writeIndent(writer, depth + 1, .{});
                    try writer.writeAll("init: {\n");
                    try self.dumpInner(writer, init_wip_index, depth + 2);
                    try fmt.writeIndent(writer, depth + 1, .{});
                    try writer.writeAll("}\n");
                }
            },
            .global_declaration => {
                try fmt.writeIndent(writer, depth + 1, .{});
                try writer.writeAll("type: {\n");
                try self.dumpInner(writer, wip.data.global_declaration.type, depth + 2);
                try fmt.writeIndent(writer, depth + 1, .{});
                try writer.writeAll("}\n");
                try fmt.writeIndent(writer, depth + 1, .{});
                try writer.writeAll("\n");

                if (wip.data.global_declaration.init_block) |init_block_index| {
                    try fmt.writeIndent(writer, depth + 1, .{});
                    try writer.writeAll("init: {\n");
                    try self.dumpInner(writer, init_block_index, depth + 2);
                    try fmt.writeIndent(writer, depth + 1, .{});
                    try writer.writeAll("}\n");
                }
            },
            .global_type_declaration => {
                try fmt.writeIndent(writer, depth + 1, .{});
                if (wip.data.global_type_declaration.init_block) |init_block_index| {
                    try fmt.writeIndent(writer, depth + 1, .{});
                    try writer.writeAll("init: {\n");
                    try self.dumpInner(writer, init_block_index, depth + 2);
                    try fmt.writeIndent(writer, depth + 1, .{});
                    try writer.writeAll("}\n");
                }
            },
            .param_declaration => {
                try fmt.writeIndent(writer, depth + 1, .{});
                try writer.writeAll("type: {\n");
                try self.dumpInner(writer, wip.data.param_declaration.type, depth + 2);
                try fmt.writeIndent(writer, depth + 1, .{});
                try writer.writeAll("}\n");
            },
            .block => {
                // try fmt.writeIndent(writer, depth + 1, .{});
                // try writer.print("instructions({d}):\n", .{wip.data.block.instructions.items.len});
                for (wip.data.block.instructions.items) |instruction| {
                    try wip.dumpInstruction(
                        writer,
                        instruction,
                    );
                }
            },

            // .loop_expression => {
            //     try fmt.writeIndent(writer, depth, .{});
            //     try writer.writeAll("body:\n");
            //     try self.dumpInner(writer, wip.data.loop_expression.body, depth + 1);
            // },
            // .branch => {
            //     try fmt.writeIndent(writer, depth, .{});

            //     try writer.writeAll("condition: %{d}\n", .{instru condition});
            //     try fmt.writeIndent(writer, depth, .{});
            //     try writer.writeAll("then:\n");
            //     try self.dumpInner(writer, wip.data.branch.then_instructions_list, depth + 1);
            //     if (wip.data.branch.else_instructions_list) |else_instructions_list| {
            //         try fmt.writeIndent(writer, depth, .{});
            //         try writer.writeAll("else:\n");
            //         try self.dumpInner(writer, else_instructions_list, depth + 1);
            //     }
            // },

            else => {},
        }
    }

    fn dumpInstruction(
        self: *Wip,
        writer: std.io.AnyWriter,
        id: InstructionId,
    ) !void {
        const owner_wip = self.builder.getWip(self.data.block.owner);

        var tree_writer = TreeWriter.init(writer);
        try Mir.formatInst(.{
            .allocator = self.builder.arena.allocator(),
            .instructions = owner_wip.instructions.items,
            .types = self.builder.mir.types.items,
            .values = self.builder.mir.values.items,
            .lists = &self.builder.mir.interned_lists,
            .builder = self.builder,
            .writer = writer,
            .strings = &self.builder.mir.strings,
        }, id, &tree_writer, .{});
        // const instruction = try self.getInstruction(id);
        // const data: Mir.Instruction.Data = instruction.data;
        // // if (std.meta.activeTag(data) == .wip and instruction.op != .global_get) {
        // //     var wip = self.builder.getWip(data.wip);
        // //     try wip.dumpInner(writer, data.wip, depth + 1);
        // //     return;
        // // }
        // if (instruction.liveness == 0) {
        //     try tw.gray_500.csiOpen(writer);
        // }
        // defer {
        //     if (instruction.liveness == 0) {
        //         tw.gray_500.csiClose(writer) catch unreachable;
        //     }
        // }

        // try fmt.writeIndent(writer, depth, .{});
        // switch (instruction.data) {
        //     .loop => |loop| {
        //         try writer.print("[{d}]loop{s}\n", .{ id, if (instruction.liveness == 0) "!" else "" });
        //         try self.dumpInner(writer, loop.instructions_list, depth + 1);
        //         return;
        //     },
        //     .branch => |branch| {
        //         try writer.print("[{d}]if{s} (#{d}) then:\n", .{ id, if (instruction.liveness == 0) "!" else "", branch.condition });
        //         // try fmt.writeIndent(writer, depth, .{});

        //         // try writer.writeAll("then:\n");
        //         try self.dumpInner(writer, branch.then_instructions_list, depth + 1);
        //         if (branch.else_instructions_list) |else_instructions_list| {
        //             try fmt.writeIndent(writer, depth, .{});
        //             try writer.writeAll("else:\n");

        //             try self.dumpInner(writer, else_instructions_list, depth + 1);
        //         }
        //         return;
        //     },
        //     else => {},
        // }
        // // try fmt.formatInstruction(self.builder, writer, .{}, instruction, depth);
        // const tag_name = @tagName(instruction.data);

        // try writer.print("{s: <4}", .{tag_name[0..@min(tag_name.len, 4)]});
        // try writer.print("[{d: >3}]", .{id});
        // try writer.print("{s: <3}", .{if (instruction.liveness == 0) "!" else ""});
        // try writer.print("{s}", .{@tagName(instruction.op)});

        // switch (data) {
        //     .scoped => |scoped| {
        //         // try self.dumpType(writer, instruction.type, depth + 1);
        //         try writer.print("('{s}', index = {d})", .{ self.builder.getSlice(scoped.name), scoped.index });
        //     },
        //     .bin_op => |bin_op| {
        //         try writer.print("(%{d}, %{d})", .{ bin_op.lhs, bin_op.rhs });
        //     },
        //     .instruction => |instruction_index| {
        //         try writer.print("(#{d})", .{instruction_index});
        //     },

        //     // .branch => |branch| {
        //     //     try writer.print("(#{d}) then:\n", .{branch.condition});
        //     //     // try fmt.writeIndent(writer, depth, .{});

        //     //     // try writer.writeAll("then:\n");
        //     //     try self.dumpInner(writer, branch.then_instructions_list, depth + 1);
        //     //     if (branch.else_instructions_list) |else_instructions_list| {
        //     //         try fmt.writeIndent(writer, depth, .{});
        //     //         try writer.writeAll("else:\n");

        //     //         try self.dumpInner(writer, else_instructions_list, depth + 1);
        //     //     }
        //     //     return;
        //     // },
        //     .type => |type_index| {
        //         try writer.print(" ", .{});
        //         try self.dumpType(writer, type_index, depth + 1);
        //     },
        //     .call => |call| {
        //         var iter = self.builder.mir.lists.iterList(call.args_list);
        //         try writer.print("(#{?d}) with (", .{call.callee.toInt()});
        //         var first = true;
        //         while (iter.next()) |arg_id| {
        //             if (!first) {
        //                 try writer.writeAll(", ");
        //             }
        //             first = false;
        //             try writer.print("#{d}", .{arg_id});
        //         }
        //         try writer.writeAll(")");
        //     },
        //     .global_set => |global_set| {
        //         try writer.print("(#{d}, #{d})", .{ global_set.global, global_set.value });
        //     },
        //     .void => {},
        //     .store => |store| {
        //         try writer.print("(#{d}, #{d})", .{ store.pointer, store.value });
        //     },
        //     .get_element_pointer => |get_element_pointer| {
        //         try writer.print("(#{d}, #{d})", .{ get_element_pointer.pointer, get_element_pointer.index });
        //     },
        //     // .type => |type_index| {
        //     //     try self.dumpType(writer, type_index, depth + 1);
        //     // },

        //     else => {
        //         try writer.writeAll("(TODO)");
        //     },
        // }

        // try writer.writeAll(": ");

        // try self.dumpType(writer, instruction.type, depth + 1);
        // try writer.writeAll(" -> ");

        // if (self.builder.getValue(instruction.value)) |value| {
        //     try writer.print("[{}]", .{value});
        // } else {
        //     try writer.print("[{s}]", .{@tagName(instruction.value)});
        // }
        // try tw.gray_500.csiClose(writer);
        // try writer.writeAll("\n");
    }
    pub inline fn format(self: *Wip) []const u8 {
        const color = fmt.pickColor(self.index);
        var buf: [1024]u8 = undefined;
        var buf_writer = std.io.fixedBufferStream(&buf);
        const writer = buf_writer.writer();
        // try writer.print("#{d} {s}
        const name = switch (self.data) {
            .block => if (self.data.block.is_inline) "inline_block" else "block",

            else => @tagName(self.data),
        };
        color.print(
            writer,
            "#{d} {s} [{s}] {x}",
            .{ self.index, name, @tagName(self.stage), @intFromPtr(self) },
            .{},
        ) catch unreachable;
        switch (self.data) {
            .block => {
                color.print(
                    writer,
                    " instructions({d})",
                    .{self.data.block.instructions.items.len},
                    .{},
                ) catch unreachable;
            },
            else => {},
        }
        // writer.print("#{d} {s} [{s}] {x}", .{ self.index, @tagName(self.data), @tagName(self.stage), pointer }) catch unreachable;
        return buf_writer.getWritten();
    }
};

const InstructionSet = struct {
    block_wip: *Wip,
    builder: *Builder,
    instructions: Mir.WorkingList,
    map: std.AutoHashMapUnmanaged(InstructionId, Mir.Instruction.Index),
    local_count: u32 = 0,

    pub fn push(self: *InstructionSet, instruction_id: InstructionId, instruction: Mir.Instruction) !Mir.Instruction.Index {
        const event_id = self.builder.tracer.beginEvent("InstructionSet.push", .{ .wip = self.block_wip.index, .instruction_id = instruction_id });
        defer self.builder.tracer.endEvent(event_id, "InstructionSet.push", .{ .wip = self.block_wip.index, .instruction_id = instruction_id });
        const liveness: u32 = switch (instruction.op) {
            .call => 1,
            .branch => 1,
            .loop => 1,
            .br => 1,
            .ret => 1,
            .store => 1,
            else => 0,
        };
        const index = try self.builder.pushInstruction(.{
            .op = instruction.op,
            .type = instruction.type,
            .value = instruction.value,
            .data = instruction.data,
            .liveness = liveness,
        });
        try self.instructions.append(index);
        self.builder.logger.log("{d} -> {d}: {s}", .{ instruction_id, index, @tagName(instruction.op) }, null);
        try self.map.put(self.builder.arena.allocator(), instruction_id, index);
        return index;
    }

    pub fn getIndex(self: *InstructionSet, instruction_id: InstructionId) Mir.Instruction.Index {
        return self.map.get(instruction_id) orelse {
            self.builder.tracer.panic("instruction_id not found", .{ .instruction_id = instruction_id });
        };
    }
    pub fn getIndexAndMarkLive(self: *InstructionSet, instruction_id: InstructionId) Mir.Instruction.Index {
        const event_id = self.builder.tracer.beginEvent("InstructionSet.getIndexAndMarkLive", .{ .wip = self.block_wip.index, .instruction_id = instruction_id });
        defer self.builder.tracer.endEvent(event_id, "InstructionSet.getIndexAndMarkLive", .{ .wip = self.block_wip.index, .instruction_id = instruction_id });
        const index = self.getIndex(instruction_id);
        var instruction = &self.builder.mir.instructions.items[index];
        instruction.liveness += 1;
        return index;
    }
    pub fn maybePush(self: *InstructionSet, instruction_id: InstructionId) !void {
        const event_id = self.builder.tracer.beginEvent("InstructionSet.maybePush", .{ .wip = self.block_wip.index, .instruction_id = instruction_id });
        defer self.builder.tracer.endEvent(event_id, "InstructionSet.maybePush", .{ .wip = self.block_wip.index, .instruction_id = instruction_id });
        const instruction = try self.block_wip.getInstruction(instruction_id);
        self.builder.logger.log("maybePush: {d} {s} {s}", .{ instruction_id, @tagName(instruction.op), @tagName(activeTag(instruction.data)) }, null);
        // if (instruction.liveness == 0) {
        //     return;
        // }

        switch (instruction.data) {
            .scoped => |scoped| {
                _ = try self.push(instruction_id, .{
                    .op = instruction.op,
                    .type = instruction.type,
                    .value = instruction.value,
                    .data = .{
                        .scoped = .{
                            .index = self.local_count,
                            .name = scoped.name,
                            .mutable = scoped.mutable,
                        },
                    },
                });
                self.local_count += 1;
            },
            .bin_op => |bin_op| {
                const lhs = self.getIndexAndMarkLive(bin_op.lhs);
                const rhs = self.getIndexAndMarkLive(bin_op.rhs);
                _ = try self.push(instruction_id, .{
                    .op = instruction.op,
                    .type = instruction.type,
                    .value = instruction.value,
                    .data = .{
                        .bin_op = .{
                            .lhs = lhs,
                            .rhs = rhs,
                        },
                    },
                });
            },
            .global_set => |global_set| {
                _ = global_set; // autofix
                // const global_wip = self.builder.getWip(global_set.global);
                // const value_id = self.getIndex(global_set.value);
                _ = try self.push(instruction_id, instruction.*);
            },
            .global_get => |global_get| {
                _ = global_get; // autofix
                _ = try self.push(instruction_id, instruction.*);
            },
            .instruction => |operand_id| {
                _ = try self.push(instruction_id, .{
                    .op = instruction.op,
                    .type = instruction.type,
                    .value = instruction.value,
                    .data = .{
                        .instruction = self.getIndexAndMarkLive(operand_id),
                    },
                });
            },

            .call => |call| {
                var args_list = self.builder.getListSlice(call.args_list);
                // var i: usize = 0;
                for (args_list, 0..args_list.len) |arg_id, i| {
                    args_list[i] = self.getIndex(arg_id);
                }
                _ = try self.push(instruction_id, .{
                    .op = instruction.op,
                    .type = instruction.type,
                    .value = instruction.value,
                    .data = .{
                        .call = .{
                            .callee = call.callee,
                            .args_list = call.args_list,
                        },
                    },
                });
            },
            .void => {
                switch (instruction.op) {
                    .load => {
                        _ = try self.push(instruction_id, .{
                            .op = .constant,
                            .type = instruction.type,
                            .value = instruction.value,
                            .data = .{ .void = {} },
                        });
                    },
                    else => {
                        _ = try self.push(instruction_id, .{
                            .op = instruction.op,
                            .type = instruction.type,
                            .value = instruction.value,
                            .data = .{ .void = {} },
                        });
                    },
                }
            },
            .branch_wip => |branch_wip| {
                _ = try self.push(instruction_id, .{
                    .op = instruction.op,
                    .type = instruction.type,
                    .value = instruction.value,
                    .data = .{ .branch = .{
                        .condition = self.getIndexAndMarkLive(branch_wip.condition),
                        .then_instructions_list = try self.genInstructionsFromChildWip(branch_wip.then_wip),
                        .else_instructions_list = if (branch_wip.else_wip) |else_wip| try self.genInstructionsFromChildWip(else_wip) else null,
                    } },
                });
            },
            .get_element_pointer => {
                const get_element_pointer = instruction.data.get_element_pointer;
                _ = try self.push(instruction_id, .{
                    .op = instruction.op,
                    .type = instruction.type,
                    .value = instruction.value,
                    .data = .{
                        .get_element_pointer = .{
                            .pointer = self.getIndexAndMarkLive(get_element_pointer.pointer),
                            .index = get_element_pointer.index,
                        },
                    },
                });
            },
            .struct_wip => |struct_wip| {
                const wip = self.builder.getWip(struct_wip.wip);
                try wip.commit();
                _ = try self.push(instruction_id, .{
                    .op = .struct_decl,
                    .type = instruction.type,
                    .value = instruction.value,
                    .data = .{ .void = {} },
                });
            },
            .store => {
                const store = instruction.data.store;
                _ = try self.push(instruction_id, .{
                    .op = instruction.op,
                    .type = instruction.type,
                    .value = instruction.value,
                    .data = .{ .store = .{
                        .pointer = self.getIndexAndMarkLive(store.pointer),
                        .value = self.getIndexAndMarkLive(store.value),
                    } },
                });
            },
            .loop_wip => |loop_wip| {
                // Push furst because inner instructions may reference it
                const loop_instruction_id = try self.push(instruction_id, instruction.*);
                // then update it
                self.builder.setInstruction(loop_instruction_id, .{
                    .op = instruction.op,
                    .type = instruction.type,
                    .value = instruction.value,
                    .data = .{
                        .loop = .{
                            .instructions_list = try self.genInstructionsFromChildWip(loop_wip.wip),
                        },
                    },
                });
            },
            .type => |type_index| {
                _ = type_index; // autofix
                _ = try self.push(instruction_id, instruction.*);
            },
            .value => |value| {
                _ = value; // autofix
                _ = try self.push(instruction_id, instruction.*);
            },
            .cast => |cast| {
                if (instruction.value != .runtime) {
                    _ = try self.push(instruction_id, .{
                        .op = .constant,
                        .type = instruction.type,
                        .value = instruction.value,
                        .data = .{ .value = instruction.value },
                    });
                    return;
                }
                _ = try self.push(instruction_id, .{
                    .op = instruction.op,
                    .type = instruction.type,
                    .value = instruction.value,
                    .data = .{ .cast = .{
                        .instruction = self.getIndexAndMarkLive(cast.instruction),
                        .type = cast.type,
                    } },
                });
            },
            .br => {
                _ = try self.push(instruction_id, .{
                    .op = instruction.op,
                    .type = instruction.type,
                    .value = instruction.value,
                    .data = .{ .br = .{
                        .instruction = self.getIndexAndMarkLive(instruction.data.br.instruction),
                        .value = instruction.data.br.value,
                    } },
                });
            },
            .global_get_wip => {
                const global_get_wip = instruction.data.global_get_wip;
                const global_wip = self.builder.getWip(global_get_wip.wip);
                _ = try self.push(instruction_id, .{
                    .op = instruction.op,
                    .type = instruction.type,
                    .value = instruction.value,
                    .data = .{ .global_get = .{
                        .global = global_wip.data.global_type_declaration.global,
                    } },
                });
            },

            else => {
                if (instruction.op == .alloc) {
                    _ = try self.push(instruction_id, instruction.*);
                    return;
                }

                std.debug.panic("instruction.data not supported: {s}", .{@tagName(instruction.data)});
            },
        }
    }

    pub fn genInstructions(self: *InstructionSet) Wip.Error!Mir.Instruction.List {
        const event_id = self.builder.tracer.beginEvent("InstructionSet.genInstructions", .{ .wip = self.block_wip.index });
        defer self.builder.tracer.endEvent(event_id, "InstructionSet.genInstructions", .{ .wip = self.block_wip.index });
        // const current = self.block_wip;
        // const instructions = self.instructions;
        // defer self.block_wip = current;
        // defer self.instructions = instructions;

        // const wip = self.builder.getWip(wip_id);
        // self.block_wip = wip;
        // self.instructions = wip.builder.newList();
        const block = self.block_wip.data.block;

        for (block.instructions.items) |instruction_id| {
            try self.maybePush(instruction_id);
        }
        const count = self.instructions.list.items.len;
        const instructions_list = try self.instructions.commit();
        const color = fmt.pickColor(self.block_wip.index);
        self.builder.logger.log("commited {d} instructions to list {d}, on block {d} owner {d}", .{ count, instructions_list.start, self.block_wip.index, self.block_wip.data.block.owner }, color);
        return instructions_list;
    }
    pub fn genInstructionsFromChildWip(self: *InstructionSet, wip_id: Wip.Index) Wip.Error!Mir.Instruction.List {
        const event_id = self.builder.tracer.beginEvent("InstructionSet.genInstructionsFromChildWip", .{ .wip = self.block_wip.index, .wip_id = wip_id });
        defer self.builder.tracer.endEvent(event_id, "InstructionSet.genInstructionsFromChildWip", .{ .wip = self.block_wip.index, .wip_id = wip_id });
        const current = self.block_wip;
        const instructions = self.instructions;
        defer self.block_wip = current;
        defer self.instructions = instructions;

        const wip = self.builder.getWip(wip_id);
        self.block_wip = wip;
        self.instructions = wip.builder.newList();
        return try self.genInstructions();
    }
    // pub fn generateBlockInstruction(block_wip: *Wip) Wip.Error!Mir.Instruction.Index {
    //     var set = InstructionSet{
    //         .builder = block_wip.builder,
    //         .block_wip = block_wip,
    //         .instructions = block_wip.builder.newList(),
    //         .map = .{},
    //     };
    //     const block_inst = block_wip.builder.reserveInstructionIndex();
    //     block_wip.builder.setInstruction(block_inst, .{
    //         .op = .block,
    //         .type = .void,
    //         .value = .void,
    //         .data = .{
    //             .block = .{
    //                 .instructions_list = try set.genInstructions(),
    //                 // .name = block_wip.data.block.name,
    //             },
    //         },
    //     });
    //     return block_inst;
    // }
    pub fn generate(block_wip: *Wip) Wip.Error!Mir.Instruction.List {
        const event_id = block_wip.builder.tracer.beginEvent("InstructionSet.generate", .{ .wip = block_wip.index });
        defer block_wip.builder.tracer.endEvent(event_id, "InstructionSet.generate", .{ .wip = block_wip.index });
        var set = InstructionSet{
            .builder = block_wip.builder,
            .block_wip = block_wip,
            .instructions = block_wip.builder.newList(),
            .map = .{},
        };

        return try set.genInstructions();
    }
};
pub const Options = struct {
    trace_dir: ?[]const u8 = null,
    trace_name: ?[]const u8 = null,
    unique_trace_name: bool = true,
};
pub fn build(allocator: std.mem.Allocator, hir: *Hir, errors: *ErrorManager, options: Options) !Mir {
    _ = errors; // autofix
    var mir = try Mir.init(allocator);
    var builder = Builder{
        .wips = WipArray.init(allocator),
        .allocator = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
        .mir = &mir,
        .hir = hir,
        .tracer = try Tracer.init(
            options.trace_dir orelse "./.tmp/trace",
            options.trace_name orelse "mir-builder",
            options.unique_trace_name,
        ),
    };
    defer builder.deinit();
    try builder.build();
    return mir;
}
test "MirBuilder2" {
    const test_allocator = std.testing.allocator;
    const file = try std.fs.cwd().openFile("./playground.zig", .{});
    defer file.close();
    const source = try file.readToEndAlloc(test_allocator, 1024 * 1024);
    defer test_allocator.free(source);
    std.debug.print("source:\n\n{s}\n", .{source});

    var errors = try ErrorManager.init(test_allocator);
    defer errors.deinit();
    var ast = try Ast.parse(test_allocator, &errors, source, .{});
    defer ast.deinit();
    std.debug.print("AST:\n", .{});
    try ast.format(std.io.getStdErr().writer().any(), 0, .{});

    var hir = try Hir.build(test_allocator, &ast, &errors, .{});
    defer hir.deinit();
    std.debug.print("Hir:\n{}\n", .{hir});

    var mir = try build(test_allocator, &hir, &errors, .{});
    defer mir.deinit();

    std.debug.print("Mir:\n{}\n", .{mir});
}
