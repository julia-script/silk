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

const Builder = struct {
    wips: WipArray,
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    mir: *Mir,
    hir: *Hir,
    logger: Logger = Logger.init(host.getStdErrWriter(), "MirBuilder"),

    fn getHirInst(self: *Builder, hir_index: Hir.Inst.Index) Hir.Inst {
        return self.hir.insts.items[hir_index];
    }
    pub fn iterHirInsts(self: *Builder, list: Hir.Lists.Index) Hir.Lists.ListIter {
        return self.hir.lists.iterList(list);
    }
    pub fn getSlice(self: *Builder, interned_slice: InternedSlice) []const u8 {
        return self.mir.strings.getSlice(interned_slice);
    }
    fn assertHirInst(self: *Builder, hir_index: Hir.Inst.Index, tag: Hir.Inst.Tag) !void {
        const hir_inst = self.getHirInst(hir_index);
        if (std.meta.activeTag(hir_inst) != tag) {
            return self.logger.panic("expected hir inst tag {s}, got {s}", .{ @tagName(tag), @tagName(std.meta.activeTag(hir_inst)) });
        }
    }
    pub fn newList(self: *Builder) Mir.ChildList {
        return self.mir.lists.new(self.mir.allocator);
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
    // pub fn setWip(self: *Builder, index: Wip.Index, wip: Wip) void {
    //     self.wips.items[index] = wip;
    // // }
    // pub fn updateWipData(self: *Builder, index: Wip.Index, data: Wip.Data) void {
    //     self.wips.items[index].data = data;
    // }

    pub inline fn makeAndGetWip(self: *Builder, hir_index: Hir.Inst.Index, parent: Wip.Index, data: Wip.Data) !*Wip {
        const index = try self.makeWip(hir_index, parent, data);
        return self.getWip(index);
    }
    pub fn makeEmptyWip(self: *Builder, hir_index: Hir.Inst.Index, parent: Wip.Index) !Wip.Index {
        return try self.makeWip(hir_index, parent, undefined);
    }
    pub fn makeType(self: *Builder, type_: Mir.Type) !Mir.Type.Index {
        const index = self.mir.types.items.len;
        self.logger.log("makeType: {d} {s}", .{ index, @tagName(type_) }, null);
        try self.mir.types.append(self.mir.allocator, type_);

        return Mir.Type.Index.fromInt(@intCast(index));
    }
    pub fn reserveTypeIndex(self: *Builder) !Mir.Type.Index {
        return try self.makeType(undefined);
    }
    pub fn setType(self: *Builder, index: Mir.Type.Index, type_: Mir.Type) void {
        self.mir.types.items[index.toInt().?] = type_;
    }
    pub fn getType(self: *Builder, index: Mir.Type.Index) ?Mir.Type {
        if (index.toInt()) |int| {
            return self.mir.types.items[@intCast(int)];
        }
        return null;
    }
    pub fn getValue(self: *Builder, index: Mir.Value.Index) ?Mir.Value {
        if (index.toInt()) |int| {
            return self.mir.values.items[@intCast(int)];
        }
        return null;
    }
    pub fn pushValue(self: *Builder, value: Mir.Value) !Mir.Value.Index {
        const index = self.mir.values.items.len;
        try self.mir.values.append(self.mir.allocator, value);
        return Mir.Value.Index.fromInt(@intCast(index));
    }

    fn build(self: *Builder) !void {
        // const root_index = try self.makeEmptyWip(Hir.Inst.RootIndex, 0);
        var root_wip = try self.makeAndGetWip(Hir.Inst.RootIndex, 0, .{
            .module = .{
                .name = try self.internSlice("root"),
                .declarations = .{},
            },
        });
        try root_wip.collectSymbols();
        root_wip.type_index = try root_wip.resolveType();

        try root_wip.resolveInitializer();
        try root_wip.commit();
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

const Wip = struct {
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
        param_declaration,
        if_expression,
        loop_expression,
        type,
    };
    pub const Data = union(Kind) {
        fn_decl: struct {
            name: InternedSlice,
            params: Array(Wip.Index),
            return_type: Wip.Index,
            init_block: ?Wip.Index,
        },
        block: Block,
        module: struct {
            name: InternedSlice,
            declarations: Array(Wip.Index),
        },
        global_declaration: struct {
            name: InternedSlice,
            type: Wip.Index,
            init_block: ?Wip.Index,
        },
        param_declaration: struct {
            name: InternedSlice,
            type: Wip.Index,
        },
        if_expression: struct {
            condition: InstructionId,
            then_branch: Wip.Index,
            else_branch: ?Wip.Index,
        },
        loop_expression: struct {
            body: Wip.Index,
        },
        type: void,
        pub const Block = struct {
            name: ?InternedSlice,
            owner: Wip.Index,
            break_type: ?Wip.Index,
            instructions: Array(InstructionId) = .{},
        };
    };
    pub fn init(index: Wip.Index, builder: *Builder, hir_index: Hir.Inst.Index, parent: Wip.Index, data: Data) !Wip {
        return Wip{
            .index = index,
            .builder = builder,
            .hir_index = hir_index,
            .parent = parent,
            .data = data,
            .type_mode = switch (std.meta.activeTag(data)) {
                .if_expression, .loop_expression, .block => .inferred,
                else => .explicit,
            },
        };
    }
    pub fn getBlockData(self: *Wip) *Data.Block {
        return &self.builder.getWip(self.index).data.block;
    }
    pub inline fn update(self: *Wip) void {
        const pointer = self.builder.wips.getPtr(self.index);
        self.builder.logger.log("UPDATE: {s} {x}", .{ self.format(), @intFromPtr(pointer) }, null);

        pointer.* = self.*;
    }
    pub fn getGlobal(self: *Wip, inst_index: Hir.Inst.Index) ?Wip.Index {
        if (self.scope.globals.get(inst_index)) |global| return global;
        if (self.index == 0 and self.parent == 0) return null;
        const parent = self.builder.getWip(self.parent);
        return parent.getGlobal(inst_index);
    }
    pub fn deinit(self: *Wip) void {
        self.scope.deinit(self.builder.allocator);
    }
    pub fn getInstruction(self: *Wip, id: InstructionId) !*Mir.Instruction {
        switch (self.data) {
            .block => |*block| {
                const owner_wip = self.builder.getWip(block.owner);
                return &owner_wip.instructions.items[@intCast(id)];
            },
            else => {},
        }
        return error.InstructionNotFound;
    }
    pub fn getInstructionId(self: *Wip, hir_inst_index: Hir.Inst.Index) !InstructionId {
        const owner_wip = self.builder.getWip(self.data.block.owner);
        const id = owner_wip.instructions_map.get(hir_inst_index) orelse {
            const hir_inst = self.builder.getHirInst(hir_inst_index);
            self.builder.logger.log("getInstructionId: #{d} -> not found from owner #{d}", .{ hir_inst_index, self.parent }, null);
            self.builder.logger.panic("instruction not found: %{d} {s}", .{ hir_inst_index, @tagName(std.meta.activeTag(hir_inst)) });
        };
        return id.instruction_index;
    }
    pub fn getInstructionByHirInst(self: *Wip, hir_inst: Hir.Inst.Index) !*Mir.Instruction {
        const id = try self.getInstructionId(hir_inst);
        return self.getInstruction(id);
    }
    pub fn putGlobal(self: *Wip, inst_index: Hir.Inst.Index, index: Wip.Index) !void {
        const result = try self.scope.globals.getOrPut(self.builder.arena.allocator(), inst_index);
        if (!result.found_existing) {
            result.value_ptr.* = index;
            return;
        }
        return error.GlobalAlreadyExists;
        // self.builder.logger.todo("Error message: global {s} already exists", .{self.builder.getSlice(inst_index)});
    }

    pub fn pushInstruction(self: *Wip, hir_inst: Hir.Inst.Index, instruction: Mir.Instruction) !InstructionId {
        switch (self.data) {
            .block => |*block| {
                const allocator = self.builder.arena.allocator();
                const owner_wip = self.builder.getWip(block.owner);
                const instruction_index: InstructionId = @intCast(owner_wip.instructions.items.len);

                self.builder.logger.log(
                    "[PUSH_INSTRUCTION] hir({d}) -> %{d} '{s}' on BLOCK #{d} to OWNER #{d}",
                    .{ hir_inst, instruction_index, @tagName(instruction.op), self.index, block.owner },
                    null,
                );
                try owner_wip.instructions.append(allocator, instruction);
                try owner_wip.instructions_map.put(allocator, hir_inst, .{
                    .owner = self.index,
                    .instruction_index = instruction_index,
                });
                try self.builder.getWip(self.index).data.block.instructions.append(allocator, instruction_index);
                // try block.instructions.append(allocator, instruction_index);

                return @intCast(instruction_index);
            },
            else => @panic("only block has instructions"),
        }
    }
    pub fn collectSymbols(self: *Wip) !void {
        self.builder.logger.open("collectSymbols: {s}", .{self.format()});
        defer self.builder.logger.close();
        switch (self.data) {
            .module => try self.collectModuleSymbols(),
            else => {},
        }
    }
    fn collectModuleSymbols(self: *Wip) !void {
        // const self_index = self.index;
        const builder = self.builder;

        // try builder.wips.ensureUnusedCapacity(builder.arena.allocator(), 1024);
        // const self = builder.getWip(self_index);

        const hir_inst = self.builder.getHirInst(self.hir_index);
        const module_decl = hir_inst.mod_decl;

        const allocator = builder.arena.allocator();
        var iter = builder.iterHirInsts(module_decl.declarations_list);

        while (iter.next()) |decl_hir_index| {
            const decl_hir_inst = builder.getHirInst(decl_hir_index);
            const global_decl = decl_hir_inst.global_decl;
            const name = try builder.internNode(global_decl.name_node);

            if (global_decl.type) |type_hir_index| {
                const type_hir_inst = builder.getHirInst(type_hir_index);

                switch (type_hir_inst) {
                    .fn_decl => {
                        const fn_decl_wip_index = try builder.makeWip(decl_hir_index, self.index, .{
                            .fn_decl = .{
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

                        var params_iter = builder.iterHirInsts(type_hir_inst.fn_decl.params_list);
                        while (params_iter.next()) |param_hir_index| {
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
                                },
                            });
                        }
                        // builder.getWip(global_declaration_wip_index).data.global_declaration.ty =
                        try self.putGlobal(decl_hir_index, global_declaration_wip_index);
                        try self.data.module.declarations.append(allocator, global_declaration_wip_index);
                        // try self.builder.logger.panic("not implemented: #{d} {s}", .{ type_hir_index, @tagName(std.meta.activeTag(type_hir_inst)) });
                    },
                }
            }
        }
    }
    const Error = error{
        Unimplemented,
        CircularTypeReference,
        InstructionNotFound,
        GlobalAlreadyExists,
    } || std.fmt.ParseFloatError || std.fmt.ParseIntError || std.mem.Allocator.Error;
    pub fn resolveType(self: *Wip) Error!Mir.Type.Index {
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
        defer self.builder.logger.log("resolved: {s}", .{self.format()}, null);
        self.type_index = switch (self.data) {
            .module => try self.resolveModuleType(),
            .fn_decl => try self.resolveFnDeclType(),
            .type => try self.resolveSimpleType(),
            .param_declaration => try self.resolveParamType(),
            .global_declaration => try self.resolveGlobalDeclarationType(),

            .block,
            .if_expression,
            .loop_expression,
            => unreachable,
            // else => {
            //     return self.builder.logger.panic("unimplemented resolveType: {s}", .{@tagName(std.meta.activeTag(self.data))});
            // },
        };
        self.stage = .type_resolved;
        return self.type_index;
    }
    fn resolveModuleType(self: *Wip) !Mir.Type.Index {
        const type_index = try self.builder.reserveTypeIndex();
        const builder = self.builder;
        const module = self.data.module;
        var field_types = self.builder.newList();

        for (module.declarations.items) |field| {
            const field_wip = builder.getWip(field);
            const field_type_index = try field_wip.resolveType();
            const decl_type = try builder.makeType(.{ .global = .{
                .name = switch (field_wip.data) {
                    .global_declaration => field_wip.data.global_declaration.name,
                    .fn_decl => field_wip.data.fn_decl.name,
                    else => {
                        unreachable;
                    },
                },
                .type = field_type_index,
                .init = null,
            } });
            try field_types.append(decl_type.asInt());
        }

        builder.setType(type_index, .{ .module = .{
            .decls = try field_types.commit(),
        } });

        return type_index;
    }
    fn resolveFnDeclType(self: *Wip) !Mir.Type.Index {
        const type_index = try self.builder.reserveTypeIndex();
        const builder = self.builder;
        self.stage = .resolving_type;
        const fn_decl = self.data.fn_decl;
        const return_type_wip = builder.getWip(fn_decl.return_type);
        const return_type_index = try return_type_wip.resolveType();
        var params_types = builder.newList();
        for (fn_decl.params.items) |param| {
            var param_wip = builder.getWip(param);

            const param_type_index = try param_wip.resolveType();
            try params_types.append(param_type_index.asInt());
            self.builder.logger.log(".. {s}", .{param_wip.format()}, null);
        }
        builder.setType(type_index, .{ .@"fn" = .{
            .name = fn_decl.name,
            .params = try params_types.commit(),
            .return_type = return_type_index,
        } });
        self.stage = .type_resolved;
        return type_index;

        // return self.builder.logger.panic("not implemented: resolveFnDeclType", .{});
    }
    fn resolveSimpleType(self: *Wip) !Mir.Type.Index {
        const type_inst = self.builder.getHirInst(self.hir_index);
        self.stage = .resolving_type;
        const type_index = switch (type_inst) {
            .ty_i32 => Mir.Type.Index.i32,
            .ty_i64 => Mir.Type.Index.i64,
            .ty_f32 => Mir.Type.Index.f32,
            .ty_f64 => Mir.Type.Index.f64,
            else => {
                self.builder.logger.panic("not implemented: resolveSimpleType: {s}", .{@tagName(std.meta.activeTag(type_inst))});
            },
        };
        self.type_index = type_index;
        self.stage = .type_resolved;
        return type_index;
    }
    fn resolveParamType(self: *Wip) !Mir.Type.Index {
        const param = self.data.param_declaration;
        self.stage = .resolving_type;
        const type_index = try self.builder.makeType(.{
            .param = .{
                .name = param.name,
                .type = try self.builder.getWip(param.type).resolveType(),
            },
        });

        self.type_index = type_index;
        // param type has no initializer so it's resolved immediately
        // TODO: maybe when supporting default values OR types been the result of a function call, we would need to resolve instructions first for those cases
        self.stage = .resolved;
        return type_index;
    }

    fn resolveGlobalDeclarationType(self: *Wip) !Mir.Type.Index {
        self.stage = .resolving_type;
        const global_decl = self.data.global_declaration;
        const type_type_index = try self.builder.getWip(global_decl.type).resolveType();
        const type_index = try self.builder.makeType(.{
            .global = .{
                .name = global_decl.name,
                .type = type_type_index,
                .init = null, // Resolve this later when resolving instructions
                // .init = blk: {
                //     const init_wip = global_decl.init_block orelse break :blk null;
                //     const init_type_index = try self.builder.getWip(init_wip).resolveType();
                //     break :blk init_type_index;
                // },
            },
        });
        self.type_index = type_index;
        self.stage = .type_resolved;
        return type_index;
    }
    fn dumpType(self: *Wip, writer: std.io.AnyWriter, index: Mir.Type.Index, depth: usize) !void {
        if (self.builder.getType(index)) |ty| {
            // try fmt.writeIndent(writer, depth, .{});
            switch (ty) {
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
                else => |data| {
                    _ = data; // autofix
                    try writer.print("type_{s}({d})", .{ @tagName(ty), index.toInt().? });
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
        self.builder.logger.open("resolveInitializer: {s}", .{self.format()});
        defer self.builder.logger.close();
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
        switch (self.data) {
            .module => try self.resolveModuleInitializer(),
            .fn_decl => try self.resolveFnDeclInitializer(),
            .block => try self.resolveBlockInitializer(),
            else => {
                return self.builder.logger.panic("unimplemented: resolveInitializer: {s}", .{@tagName(std.meta.activeTag(self.data))});
            },
        }
    }
    fn resolveModuleInitializer(self: *Wip) Error!void {
        self.stage = .resolving_instructions;
        const builder = self.builder;
        const module = self.data.module;
        for (module.declarations.items) |field| {
            const field_wip = builder.getWip(field);
            try field_wip.resolveInitializer();
        }
        self.stage = .resolved;
    }
    fn resolveFnDeclInitializer(self: *Wip) Error!void {
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
        // self.builder.logger.open("resolveBlockInitializer: #{d}", .{self.index});
        // defer self.builder.logger.close();
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
            .loop_expression => {},
            .if_expression => {},
            .block => {},

            .module => {
                self.builder.logger.todo("error message for blocks at the module level", .{});
            },
            .global_declaration => {
                self.builder.logger.panic("Unimplemented: resolveBlockInitializer: global_declaration", .{});
            },
            .param_declaration => {
                self.builder.logger.todo("error message for blocks at the param_declaration level (maybe in the future?)", .{});
            },
            .type => {
                // I guess this should never happen?
                self.builder.logger.todo("error message for blocks at the type level", .{});
            },

            // else => {
            //     self.builder.logger.panic("error message for blocks at the wrong wip level: {s}", .{@tagName(std.meta.activeTag(parent.data))});
            // },
        }

        try self.resolveBlockInstructions();
        // const block_hir_inst = switch (self.builder.getHirInst(self.hir_index)) {
        //     .block, .inline_block => |hir_inst| hir_inst,
        //     else => {
        //         self.builder.logger.panic("error message for blocks at the wrong hir level: {s}", .{@tagName(std.meta.activeTag(self.builder.getHirInst(self.hir_index)))});
        //     },
        // };
        // var iter_instructions = self.builder.iterHirInsts(block_hir_inst.instructions_list);
        // while (iter_instructions.next()) |hir_inst| {
        //     _ = try self.resolveInstruction(hir_inst);
        // }
        self.builder.logger.log("resolved block #{d} with #{d} instructions", .{
            self.index,
            self.data.block.instructions.items.len,
        }, null);
        self.stage = .resolved;
    }
    pub fn resolveBlockInstructions(self: *Wip) Error!void {
        self.builder.logger.open("resolveBlockInstructions: {s}", .{self.format()});
        defer self.builder.logger.close();
        const block_hir_inst = switch (self.builder.getHirInst(self.hir_index)) {
            .block, .inline_block => |hir_inst| hir_inst,
            else => unreachable,
        };
        var iter_instructions = self.builder.iterHirInsts(block_hir_inst.instructions_list);
        while (iter_instructions.next()) |hir_inst| {
            _ = try self.resolveInstruction(hir_inst);
        }
    }
    pub fn resolveParamInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, name: InternedSlice, type_index: Mir.Type.Index) Error!InstructionId {
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
        switch (hir_inst) {
            .local => |local| return try self.resolveLocalInstruction(hir_inst_index, local),
            .comptime_number => return try self.resolveConstantInstruction(hir_inst_index, hir_inst.comptime_number),
            .ty_f64, .ty_f32, .ty_i64, .ty_i32, .ty_boolean => return try self.resolveTypeLiteralInstruction(hir_inst_index),
            .gt, .ge, .lt, .le, .eq, .ne => |bin_op| return try self.resolveComparisonInstruction(hir_inst_index, bin_op),
            .as => |bin_op| return try self.resolveAsInstruction(hir_inst_index, bin_op),
            .param_get => |node| return try self.resolveParamGetInstruction(hir_inst_index, node),
            .add, .sub, .mul, .div => |bin_op| return try self.resolveArithmeticInstruction(hir_inst_index, bin_op),
            .ret => |node| return try self.resolveReturnInstruction(hir_inst_index, node),
            .local_set => |bin_op| return try self.resolveLocalSetInstruction(hir_inst_index, bin_op),
            .local_get => |node| return try self.resolveLocalGetInstruction(hir_inst_index, node),
            .typeof => |node| return try self.resolveTypeOfInstruction(hir_inst_index, node),
            .if_expr => |node| return try self.resolveIfExpressionInstruction(hir_inst_index, node),
            .loop => |node| return try self.resolveLoopExpressionInstruction(hir_inst_index, node),
            .br => |node| return try self.resolveBreakInstruction(hir_inst_index, node),
            .global_get => |node| return try self.resolveGlobalGetInstruction(hir_inst_index, node),
            .fn_call => |node| return try self.resolveFnCallInstruction(hir_inst_index, node),
            else => {
                return self.builder.logger.panic("not implemented: resolveInstruction: {s}", .{@tagName(std.meta.activeTag(hir_inst))});
            },
        }
    }

    pub fn resolveLocalInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, local: Hir.Inst.Local) Error!InstructionId {
        const type_inst = try self.getInstructionByHirInst(local.type);
        // const value_inst = try self.getInstructionByHirInst(local.init);
        type_inst.liveness = 0;
        // value_inst.liveness = 0;
        return try self.pushInstruction(hir_inst_index, .{
            .op = .local,
            .type = type_inst.getValue().toType(),
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

                return try self.pushInstruction(hir_inst_index, .{
                    .op = .constant,
                    .type = .number,
                    .value = try self.builder.pushValue(value),
                    .data = .void,
                });
            },
            else => {
                return self.builder.logger.panic("not implemented: resolveConstantInstruction: {s}", .{@tagName(node_tag)});
            },
        }
        std.debug.panic("not implemented: resolveConstantInstruction", .{});
    }
    pub fn resolveTypeLiteralInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index) Error!InstructionId {
        const hir_inst = self.builder.getHirInst(hir_inst_index);
        return try self.pushInstruction(hir_inst_index, .{
            .op = .constant,
            .type = .type,
            .value = switch (hir_inst) {
                .ty_f64 => .type_f64,
                .ty_f32 => .type_f32,
                .ty_i64 => .type_i64,
                .ty_i32 => .type_i32,
                .ty_boolean => .type_boolean,
                else => unreachable,
            },

            .data = .void,
        });
    }
    pub fn resolveAsInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, bin_op: Hir.Inst.BinaryOp) Error!InstructionId {
        const instruction = try self.getInstructionId(bin_op.lhs);
        const type_instruction = try self.getInstructionByHirInst(bin_op.rhs);
        return try self.castInstruction(hir_inst_index, instruction, type_instruction.value.toType());
    }
    pub fn resolveParamGetInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, param_get: Hir.Inst.UnaryOp) Error!InstructionId {
        const instruction_id = try self.getInstructionId(param_get.operand);
        const instruction = try self.getInstruction(instruction_id);
        const param_type_index = instruction.type;
        const param_type: Mir.Type = self.builder.getType(param_type_index) orelse unreachable;
        return try self.pushInstruction(hir_inst_index, .{
            .op = .param_get,
            .type = param_type.param.type,
            .value = instruction.value,
            .data = .{ .instruction = instruction_id },
        });
    }
    pub fn resolveComparisonInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, bin_op: Hir.Inst.BinaryOp) Error!InstructionId {
        const hir_inst = self.builder.getHirInst(hir_inst_index);
        const lhs_id = try self.getInstructionId(bin_op.lhs);
        const rhs_id = try self.getInstructionId(bin_op.rhs);
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
        lhs_instruction.liveness = 0;
        rhs_instruction.liveness = 0;
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
                return self.builder.logger.panic("not implemented: resolveArithmeticInstruction: {s}", .{@tagName(std.meta.activeTag(self.builder.getHirInst(hir_inst_index)))});
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
    pub fn resolveReturnInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.UnaryOp) Error!InstructionId {
        const instruction_id = try self.getInstructionId(node.operand);
        const instruction = try self.getInstruction(instruction_id);

        return try self.pushInstruction(hir_inst_index, .{
            .op = .ret,
            .type = instruction.type,
            .value = instruction.getValue(),
            .data = .{ .instruction = instruction_id },
        });
    }

    pub fn tryCastInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, id: InstructionId, type_index: Mir.Type.Index) Error!InstructionId {
        const instruction = try self.getInstruction(id);
        if (instruction.type == type_index) {
            return id;
        }
        if (type_index == .number) {
            return id;
        }

        switch (instruction.type) {
            .number => {
                switch (type_index) {
                    .i64, .i32, .f64, .f32 => {
                        return try self.castInstruction(hir_inst_index, id, type_index);
                    },
                    else => {},
                }
            },
            else => {},
        }
        self.builder.logger.panic("not implemented: tryCastInstruction: instruction type: {any} to {any}", .{ instruction.type, type_index });
    }
    pub fn castInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, id: InstructionId, type_index: Mir.Type.Index) Error!InstructionId {
        const inst = try self.getInstruction(id);
        inst.liveness = 0;
        // TODO: check for ilegal casts
        return try self.pushInstruction(hir_inst_index, .{
            .op = inst.op,
            .type = type_index,
            .value = inst.value,
            .data = inst.data,
        });
    }

    pub fn tryResolveArithmetic(self: *Wip, op: Mir.Instruction.Op, lhs_id: InstructionId, rhs_id: InstructionId, type_index: Mir.Type.Index) Error!Mir.Value.Index {
        const lhs_inst = try self.getInstruction(lhs_id);
        const rhs_inst = try self.getInstruction(rhs_id);
        const lhs_value_index = lhs_inst.getValue();
        const rhs_value_index = rhs_inst.getValue();
        if (lhs_value_index == .runtime or rhs_value_index == .runtime) {
            return .runtime;
        }
        const lhs_value = self.builder.getValue(lhs_value_index) orelse return .runtime;
        const rhs_value = self.builder.getValue(rhs_value_index) orelse return .runtime;

        if (isOneNumberOfType(.float, lhs_value, rhs_value) or type_index == .f64 or type_index == .f32) {
            lhs_inst.liveness = 0;
            rhs_inst.liveness = 0;
            return try self.builder.pushValue(.{ .float = doComptimeMath(f64, op, lhs_value, rhs_value) });
        } else if (isOneNumberOfType(.big_integer, lhs_value, rhs_value) or type_index == .i128) {
            lhs_inst.liveness = 0;
            rhs_inst.liveness = 0;
            return try self.builder.pushValue(.{
                .big_integer = doComptimeMath(i128, op, lhs_value, rhs_value),
            });
        } else {
            lhs_inst.liveness = 0;
            rhs_inst.liveness = 0;
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
        return std.meta.activeTag(a) == tag and std.meta.activeTag(b) == tag;
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
            .op = .local_set,
            .type = value_instruction.type,
            .value = value,
            .data = .{ .bin_op = .{
                .lhs = local_id,
                .rhs = value_id,
            } },
            .liveness = 1,
        });
    }
    pub fn resolveLocalGetInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.UnaryOp) Error!InstructionId {
        const instruction_id = try self.getInstructionId(node.operand);
        const instruction = try self.getInstruction(instruction_id);
        return try self.pushInstruction(hir_inst_index, .{
            .op = .local_get,
            .type = instruction.type,
            .value = instruction.getValue(),
            .data = .{ .instruction = instruction_id },
        });
    }
    pub fn resolveGlobalGetInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.UnaryOp) Error!InstructionId {
        const global_wip_id = self.getGlobal(node.operand) orelse self.builder.logger.todo("error for global not found: {d}", .{node.operand});
        const global_wip = self.builder.getWip(global_wip_id);
        const global_type = try global_wip.resolveType();
        self.builder.logger.log("..global_get: {}", .{global_type}, null);
        if (self.builder.getType(global_type)) |ty| {
            switch (ty) {
                .@"fn" => {
                    const value = try self.builder.pushValue(.{ .@"fn" = .{
                        .type = global_type,
                    } });
                    return try self.pushInstruction(hir_inst_index, .{
                        .op = .global_get,
                        .type = global_type,
                        .value = value,
                        .data = .{ .wip = global_wip_id },
                    });
                },
                else => {},
            }
        }
        return try self.pushInstruction(hir_inst_index, .{
            .op = .global_get,
            .type = global_type,
            .value = .runtime, // TODO: resolve comptime global values
            .data = .{ .void = {} },
        });
    }
    pub fn resolveTypeOfInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.UnaryOp) Error!InstructionId {
        const instruction_id = try self.getInstructionId(node.operand);
        const instruction = try self.getInstruction(instruction_id);
        return try self.pushInstruction(hir_inst_index, .{
            .op = .type,
            .type = .type,
            .value = instruction.type.toValueIndex(),
            // .value = .runtime,
            .data = .{ .void = {} },
        });
    }
    pub fn resolveBreakInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.UnaryOp) Error!InstructionId {
        const operand_id = try self.getInstructionId(node.operand);
        // const operand = try self.getInstruction(operand_id);

        return try self.pushInstruction(hir_inst_index, .{
            .op = .br,
            .type = .void,
            .value = .void,
            .data = .{ .instruction = operand_id },
        });
    }

    pub fn resolveIfExpressionInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.IfExpr) Error!InstructionId {
        const owner = self.data.block.owner;

        const condition_id = try self.getInstructionId(node.cond);
        var if_expr_wip = try self.builder.makeAndGetWip(hir_inst_index, self.index, .{
            .if_expression = .{
                .condition = condition_id,
                .then_branch = undefined,
                .else_branch = null,
            },
        });
        // defer if_expr_wip.update();
        var then_branch = try self.builder.makeAndGetWip(node.then_body, if_expr_wip.index, .{
            .block = .{
                .owner = owner,
                .name = null,
                .break_type = null,
            },
        });
        // defer then_branch.update();

        if_expr_wip.stage = .resolving_instructions;
        defer if_expr_wip.stage = .resolved;
        // then_branch.stage = .resolving_instructions;
        if_expr_wip.data.if_expression.then_branch = then_branch.index;

        var else_branch: ?*Wip = null;
        if (node.else_body) |else_body| {
            else_branch = try self.builder.makeAndGetWip(else_body, if_expr_wip.index, .{
                .block = .{
                    .owner = owner,
                    .name = null,
                    .break_type = null,
                },
            });
            if_expr_wip.data.if_expression.else_branch = else_branch.?.index;
            else_branch.?.stage = .resolving_instructions;
        }

        const if_expr_instruction_id = try self.pushInstruction(hir_inst_index, .{
            .op = .if_expr,
            .type = .unknown,
            .value = .runtime,
            .data = .{ .wip = if_expr_wip.index },
        });
        try then_branch.resolveInitializer();
        if (else_branch) |else_branch_wip| {
            try else_branch_wip.resolveInitializer();
        }

        return if_expr_instruction_id;
    }
    pub fn resolveLoopExpressionInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.Loop) Error!InstructionId {
        const owner = self.data.block.owner;

        const loop_wip = try self.builder.makeAndGetWip(hir_inst_index, self.index, .{
            .loop_expression = .{
                .body = undefined,
            },
        });

        loop_wip.stage = .resolving_instructions;
        const body_wip = try self.builder.makeAndGetWip(node.body, loop_wip.index, .{
            .block = .{
                .owner = owner,
                .name = null,
                .break_type = null,
            },
        });
        loop_wip.data.loop_expression.body = body_wip.index;
        const loop_instruction_id = try self.pushInstruction(hir_inst_index, .{
            .op = .loop,
            .type = .void,
            .value = .void,
            .data = .{ .wip = loop_wip.index },
        });
        try body_wip.resolveBlockInitializer();

        return loop_instruction_id;
    }

    pub fn resolveFnCallInstruction(self: *Wip, hir_inst_index: Hir.Inst.Index, node: Hir.Inst.FnCall) Error!InstructionId {
        const callee_id = try self.getInstructionId(node.callee);
        const callee_instruction = try self.getInstruction(callee_id);
        var list = self.builder.newList();
        const callee_type = self.builder.getType(callee_instruction.type) orelse unreachable;
        var iter = self.builder.iterHirInsts(node.args_list);
        while (iter.next()) |arg_id| {
            const arg_instruction_id = try self.getInstructionId(arg_id);
            try list.append(arg_instruction_id);
        }

        const return_type = switch (callee_type) {
            .@"fn" => |fn_type| fn_type.return_type,
            else => self.builder.logger.todo("not a function: {any}", .{callee_type}),
        };
        callee_instruction.liveness = 0;
        // const callee_type = callee_instruction.type;
        // TODO: check if callable and if params match
        return try self.pushInstruction(hir_inst_index, .{
            .op = .call,
            .type = return_type,
            .value = .runtime,
            .data = .{
                .call = .{
                    .callee = callee_instruction.type,
                    .args_list = try list.commit(),
                },
            },
        });
    }

    pub fn commit(self: *Wip) Error!void {
        self.builder.logger.log("commit: {s}", .{self.format()}, null);
        switch (self.data) {
            .module => try self.commitModule(),
            else => {
                self.builder.logger.panic("Not implemented: {s}", .{@tagName(self.data)});
            },
        }
    }

    pub fn commitModule(self: *Wip) Error!void {
        self.builder.logger.log("commit module: {s}", .{self.format()}, null);
        assert(self.stage == .resolved);
        assert(activeTag(self.data) == .module);
        defer self.stage = .committed;

        const module_type = self.builder.getType(self.type_index) orelse unreachable;
        assert(activeTag(module_type) == .module);

        // for (module_type.module.declarations.items) |declaration| {
        //     const wip = self.builder.getWip(declaration);
        //     try wip.commit();
        // }
    }
    pub fn dump(self: *Wip, writer: std.io.AnyWriter, index: Wip.Index) !void {
        try writer.print("Wips({d}):\n", .{self.builder.wips.len});
        var iter = self.builder.wips.iterator();
        while (iter.next()) |wip| {
            try fmt.writeIndent(writer, 1, .{});
            try writer.print("{s}\n", .{wip.format()});
        }

        try self.dumpInner(writer, index, 0);
    }

    pub fn dumpInner(self: *Wip, writer: std.io.AnyWriter, index: Wip.Index, depth: usize) std.io.AnyWriter.Error!void {
        const wip = self.builder.getWip(index);
        const tag = @tagName(std.meta.activeTag(wip.data));
        const name = switch (wip.data) {
            .module => wip.data.module.name,
            .fn_decl => wip.data.fn_decl.name,
            .global_declaration => wip.data.global_declaration.name,
            .param_declaration => wip.data.param_declaration.name,
            else => null,
        };
        try fmt.writeIndent(writer, depth, .{});
        if (name) |n| {
            try writer.print("#{d} {s} \"{s}\" [{s}] hir({d}):\n", .{ wip.index, tag, self.builder.getSlice(n), @tagName(wip.stage), wip.hir_index });
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
                    try wip.dumpInstruction(writer, instruction, depth + 1);
                }
            },
            .if_expression => {
                try fmt.writeIndent(writer, depth, .{});
                try writer.print("condition: %{d}\n", .{wip.data.if_expression.condition});
                try fmt.writeIndent(writer, depth, .{});
                try writer.writeAll("then:\n");
                try self.dumpInner(writer, wip.data.if_expression.then_branch, depth + 1);
                if (wip.data.if_expression.else_branch) |else_branch| {
                    try fmt.writeIndent(writer, depth, .{});
                    try writer.writeAll("else:\n");
                    try self.dumpInner(writer, else_branch, depth + 1);
                }
            },
            .loop_expression => {
                try fmt.writeIndent(writer, depth, .{});
                try writer.writeAll("body:\n");
                try self.dumpInner(writer, wip.data.loop_expression.body, depth + 1);
            },
            else => {},
        }
    }

    fn dumpInstruction(self: *Wip, writer: std.io.AnyWriter, id: InstructionId, depth: usize) !void {
        const instruction = try self.getInstruction(id);
        const data: Mir.Instruction.Data = instruction.data;
        if (std.meta.activeTag(data) == .wip and instruction.op != .global_get) {
            var wip = self.builder.getWip(data.wip);
            try wip.dumpInner(writer, data.wip, depth + 1);
            return;
        }

        try fmt.writeIndent(writer, depth, .{});
        // try fmt.formatInstruction(self.builder, writer, .{}, instruction, depth);
        const tag_name = @tagName(instruction.data);
        if (instruction.liveness == 0) {
            try tw.gray_500.csiOpen(writer);
        }
        try writer.print("{s: <4}", .{tag_name[0..@min(tag_name.len, 4)]});
        try writer.print("[{d: >3}]", .{id});
        try writer.print("{s: <3}", .{if (instruction.liveness == 0) "!" else ""});
        try writer.print("{s}", .{@tagName(instruction.op)});

        switch (data) {
            .scoped => |scoped| {
                // try self.dumpType(writer, instruction.type, depth + 1);
                try writer.print("('{s}', index = {d})", .{ self.builder.getSlice(scoped.name), scoped.index });
            },
            .bin_op => |bin_op| {
                try writer.print("(%{d}, %{d})", .{ bin_op.lhs, bin_op.rhs });
            },
            .instruction => |instruction_index| {
                try writer.print("(#{d})", .{instruction_index});
            },
            // .value => |value| {
            //     _ = value; // autofix
            //     // try self.dumpValue(writer, value, depth + 1);

            // },
            .type => |type_index| {
                try self.dumpType(writer, type_index, depth + 1);
            },
            .void => {},
            .call => |call| {
                var iter = self.builder.mir.lists.iterList(call.args_list);
                try writer.print("(#{?d}) with (", .{call.callee.toInt()});
                var first = true;
                while (iter.next()) |arg_id| {
                    if (!first) {
                        try writer.writeAll(", ");
                    }
                    first = false;
                    try writer.print("#{d}", .{arg_id});
                }
                try writer.writeAll(")");
            },
            // .wip => |wip_index| {
            //     var wip = self.builder.getWip(wip_index);
            //     try wip.dumpInner(writer, wip_index, depth + 1);
            // },
            else => {
                try writer.writeAll("(TODO)");
            },
        }
        try writer.writeAll(": ");

        try self.dumpType(writer, instruction.type, depth + 1);
        try writer.writeAll(" -> ");

        if (self.builder.getValue(instruction.value)) |value| {
            try writer.print("[{}]", .{value});
        } else {
            try writer.print("[{s}]", .{@tagName(instruction.value)});
        }
        try tw.gray_500.csiClose(writer);
        try writer.writeAll("|\n");
    }
    pub inline fn format(self: *Wip) []const u8 {
        const color = fmt.pickColor(self.index);
        var buf: [1024]u8 = undefined;
        var buf_writer = std.io.fixedBufferStream(&buf);
        const writer = buf_writer.writer();
        // try writer.print("#{d} {s}
        color.print(
            writer,
            "#{d} {s} [{s}] {x}",
            .{ self.index, @tagName(self.data), @tagName(self.stage), @intFromPtr(self) },
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

pub fn build(allocator: std.mem.Allocator, hir: *Hir) !Mir {
    var mir = try Mir.init(allocator);
    var builder = Builder{
        .wips = WipArray.init(allocator),
        .allocator = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
        .mir = &mir,
        .hir = hir,
    };
    // try builder.wips.ensureUnusedCapacity(builder.arena.allocator(), 1024);
    defer builder.deinit();
    try builder.build();
    try builder.getWip(0).dump(std.io.getStdErr().writer().any(), 0);
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
    var ast = try Ast.parse(test_allocator, &errors, source);
    defer ast.deinit();
    std.debug.print("AST:\n", .{});
    try ast.format(std.io.getStdErr().writer().any(), 0, .{});

    var hir = try HirBuilder.gen(test_allocator, &ast, &errors);
    defer hir.deinit();
    std.debug.print("Hir:\n{}\n", .{hir});

    var mir = try build(test_allocator, &hir);
    defer mir.deinit();
}
