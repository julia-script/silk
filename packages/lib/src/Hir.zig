const std = @import("std");
const Array = std.ArrayListUnmanaged;
const PackedLists = @import("PackedLists.zig").new;
const ErrorManager = @import("ErrorManager.zig");
const Allocator = std.mem.Allocator;
const Ast = @import("Ast.zig");

const shared = @import("shared.zig");
const InternedStrings = @import("InternedStrings.zig");
const InternedSlice = InternedStrings.InternedSlice;
const assert = std.debug.assert;
const Self = @This();
// pub const IrLists = PackedLists(u32, std.math.maxInt(u32));
pub const Lists = PackedLists(u32, std.math.maxInt(u32));

// All arrays of indexes, just aliases to make usage intention clearer
// pub const TypeLists = IrLists;
// pub const ValueLists = IrLists;
// pub const DefLists = IrLists;

ast: *Ast,
allocator: std.mem.Allocator,
lists: Lists = .{},
insts: Array(Inst) = .{},

// inst: Array(Inst) = .{},
// types: Array(Type) = .{},
// values: Array(Value) = .{},
// defs: Array(Def) = .{},

pub fn init(allocator: Allocator, ast: *Ast) !Self {
    return .{
        .allocator = allocator,
        .ast = ast,
    };
}
pub fn deinit(self: *Self) void {
    self.lists.deinit(self.allocator);
    self.insts.deinit(self.allocator);
}
const Formater = struct {
    writer: std.io.AnyWriter,
    level: usize,
    value: *Self,
    pub fn indent(self: *@This()) !void {
        // std.debug.print("{}|", .{self.level});
        try self.writer.writeByteNTimes(' ', self.level * 2);
    }
    pub fn printNode(self: *@This(), node: Ast.Node.Index) !void {
        const token = self.value.ast.getNodeStartToken(node);
        try self.writer.print("{s}", .{self.value.ast.getTokenSlice(token)});
    }
    pub fn openBrace(self: *@This()) !void {
        try self.writer.writeAll("{");
    }
    pub fn closeBrace(self: *@This()) !void {
        try self.writer.writeAll("}");
    }
    pub fn breakLine(self: *@This()) !void {
        try self.writer.writeAll("\n");
    }
};
pub fn formatInstruction(value: *Self, writer: std.io.AnyWriter, options: std.fmt.FormatOptions, inst_index: Inst.Index, indent: usize) !void {
    const inst: Inst = value.insts.items[inst_index];
    var fmt = Formater{
        .writer = writer,
        .level = indent,
        .value = value,
    };
    // std.debug.print("indent: {}\n", .{indent});
    switch (inst) {
        .mod_decl => |mod_decl| {
            try fmt.indent();
            try writer.writeAll("module {\n");
            var declarations_iter = value.lists.iterList(mod_decl.declarations_list);
            while (declarations_iter.next()) |declaration_index| {
                try formatInstruction(value, writer, options, declaration_index, indent + 1);
            }
            try fmt.indent();
            try writer.writeAll("}\n");
        },
        .local => |local| {
            try fmt.indent();
            try writer.print("%{d} = local %{} '", .{ inst_index, local.type });

            try fmt.printNode(local.name_node);
            try writer.print("'\n", .{});
        },
        .local_set => |local_set| {
            try fmt.indent();
            try writer.print("%{d} = local(%{d}).set %{d}\n", .{ inst_index, local_set.lhs, local_set.rhs });
        },
        .typeof => |typeof| {
            try fmt.indent();
            try writer.print("%{d} = typeof %{d}\n", .{ inst_index, typeof.operand });
        },
        .fn_decl => |fn_decl| {
            try fmt.indent();
            try writer.writeAll("fn (");

            // fmt.level += 1;
            // try fmt.printNode(fn_decl.name_node);
            // try fmt.openBrace();
            try writer.writeAll(" ");
            try fmt.breakLine();

            var params_iter = value.lists.iterList(fn_decl.params_list);

            while (params_iter.next()) |param_index| {
                try formatInstruction(value, writer, options, param_index, fmt.level + 1);
            }

            try fmt.indent();
            try writer.writeAll(")\n");
            try fmt.indent();

            try writer.writeAll("ret:\n");
            try formatInstruction(value, writer, options, fn_decl.return_type, fmt.level + 1);
        },
        .constant_int => |constant_int| {
            try fmt.indent();
            try writer.print("%{d} = const {d}\n", .{ inst_index, constant_int.value });
            // try fmt.printNode(const_int.name_node);
            // try fmt.breakLine();
            // try formatInstruction(value, writer, options, const_int, indent + 1);
        },
        .param_decl => |param_decl| {
            try fmt.indent();
            try writer.print("%{d} = param @", .{inst_index});
            try fmt.printNode(param_decl.name_node);
            try fmt.breakLine();
            try formatInstruction(value, writer, options, param_decl.ty, indent + 1);
        },
        .inline_block, .block => |block| {
            try fmt.indent();
            try writer.print("%{d} {s} {{\n", .{ inst_index, @tagName(inst) });
            var instructions_iter = value.lists.iterList(block.instructions_list);
            while (instructions_iter.next()) |instruction_index| {
                try formatInstruction(value, writer, options, instruction_index, indent + 1);
            }
            try fmt.indent();
            try writer.writeAll("}\n");
        },
        .comptime_number => |comptime_number| {
            try fmt.indent();
            const token = value.ast.getNodeStartToken(comptime_number);
            try writer.print("%{d} = comp.number {s}\n", .{ inst_index, value.ast.getTokenSlice(token) });
        },
        .add,
        .sub,
        .mul,
        .div,

        .gt,
        .lt,
        .ge,
        .le,
        .eq,
        .ne,
        .as,
        => |bin| {
            try fmt.indent();
            try writer.print("%{d} = {s} %{d} %{d}\n", .{ inst_index, @tagName(inst), bin.lhs, bin.rhs });
        },

        .decl_ref => |decl_ref| {
            try fmt.indent();
            try writer.print("%{d} = ref @", .{inst_index});
            try fmt.printNode(decl_ref);
            try fmt.breakLine();
        },
        .ret, .param_get, .local_get, .global_get => |ret| {
            try fmt.indent();
            try writer.print("%{d} = {s} %{d}\n", .{ inst_index, @tagName(inst), ret.operand });
        },
        .if_expr => |if_expr| {
            try fmt.indent();
            try writer.print("if %{d} ", .{if_expr.cond});
            try fmt.openBrace();
            try fmt.breakLine();
            try formatInstruction(value, writer, options, if_expr.then_body, indent + 1);
            if (if_expr.else_body) |else_body| {
                try fmt.indent();
                try writer.writeAll("} else {\n");
                try formatInstruction(value, writer, options, else_body, indent + 1);
            }
            try fmt.indent();
            try writer.writeAll("}");
            try fmt.breakLine();
        },
        .global_decl => |global_decl| {
            try fmt.indent();

            try writer.print("%{d} = define global @", .{inst_index});
            try fmt.printNode(global_decl.name_node);
            try writer.writeAll(" {\n");
            if (global_decl.type) |type_index| {
                try formatInstruction(value, writer, options, type_index, indent + 1);
            }
            // if (global_decl.init) |init_index| {
            // std.debug.print("init: {} {}\n", .{ inst_index, init_index });
            if (global_decl.init) |init_index| {
                try formatInstruction(value, writer, options, init_index, indent + 1);
            }
            // } else {}
            try fmt.indent();
            try writer.writeAll("}\n");
        },
        .undefined_value => {
            try fmt.indent();
            try writer.print("%{d} = undefined\n", .{inst_index});
        },
        // .ty_number => |ty_number| {
        //     _ = ty_number; // autofix
        //     try fmt.indent();
        //     try writer.print("%{d} = type(number)\n", .{
        //         inst_index,
        //     });
        // },
        // .ty_i32 => |ty_i32| {
        //     _ = ty_i32; // autofix
        //     try fmt.indent();
        //     try writer.print("%{d} = type(i32)\n", .{
        //         inst_index,
        //     });
        // },
        // .ty_i64 => |ty_i64| {
        //     _ = ty_i64; // autofix
        //     try fmt.indent();
        //     try writer.print("%{d} = type(i64)\n", .{
        //         inst_index,
        //     });
        // },
        // .ty_f32 => |ty_f32| {
        //     _ = ty_f32; // autofix
        //     try fmt.indent();
        //     try writer.print("%{d} = type(f32)\n", .{
        //         inst_index,
        //     });
        // },
        // .ty_f64 => |ty_f64| {
        //     _ = ty_f64; // autofix
        //     try fmt.indent();
        //     try writer.print("%{d} = type(f64)\n", .{
        //         inst_index,
        //     });
        // },
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

        .ty_f32,
        .ty_f64,
        .ty_void,
        => {
            try fmt.indent();
            try writer.print("%{d} = {s}\n", .{
                inst_index,
                @tagName(inst),
            });
        },
        .loop => |loop| {
            try fmt.indent();
            try writer.print("%{d} = loop\n", .{inst_index});
            try formatInstruction(value, writer, options, loop.body, indent + 1);
        },
        .br => |br| {
            try fmt.indent();
            try writer.print("%{d} = break %{d}\n", .{ inst_index, br.operand });
        },
        .assign => |assign| {
            try fmt.indent();
            try writer.print("%{d} = assign %{d} = %{d}\n", .{ inst_index, assign.lhs, assign.rhs });
        },
        .debug_var => |debug_var| {
            try fmt.indent();
            try writer.print("%{d} = debug_var '", .{inst_index});
            try fmt.printNode(debug_var.name_node);
            try writer.print("' %{d}\n", .{debug_var.instruction});
        },
        .fn_call => |fn_call| {
            try fmt.indent();
            try writer.print("%{d} = call %{d} ", .{ inst_index, fn_call.callee });
            if (fn_call.args_list != 0) {
                // try fmt.indent();
                try writer.writeAll("with {\n");
                var args_iter = value.lists.iterList(fn_call.args_list);
                fmt.level += 1;
                while (args_iter.next()) |arg_index| {
                    try fmt.indent();
                    try writer.print("%{d}\n", .{arg_index});
                    // try formatInstruction(value, writer, options, arg_index, indent + 1);
                }
                fmt.level -= 1;
                try fmt.indent();
                try writer.writeAll("}\n");
            }
        },
        .ty_array => |ty_array| {
            try fmt.indent();
            try writer.print("%{d} = type array<%{d}, size=%{d}> \n", .{ inst_index, ty_array.type, ty_array.size });
        },
        .alloc => |alloc| {
            try fmt.indent();
            try writer.print("%{d} = alloc %{d}\n", .{ inst_index, alloc.type });
        },
        .store => |store| {
            try fmt.indent();
            try writer.print("%{d} = store %{d} = %{d}\n", .{ inst_index, store.pointer, store.value });
        },
        .get_element_pointer => |get_element_pointer| {
            try fmt.indent();
            try writer.print("%{d} = get_element_pointer(%{d}, index=%{d})\n", .{ inst_index, get_element_pointer.pointer, get_element_pointer.index });
        },
        .get_element_value => |get_element_value| {
            try fmt.indent();
            try writer.print("%{d} = get_element_value(%{d}, index=%{d})\n", .{ inst_index, get_element_value.pointer, get_element_value.index });
        },
        .get_property_pointer, .get_property_value => |get_property_value| {
            try fmt.indent();
            try writer.print("%{d} = {s}(%{d}, property_name='%{s}')\n", .{ inst_index, @tagName(inst), get_property_value.base, get_property_value.property_name });
        },
        .load => |load| {
            try fmt.indent();
            try writer.print("%{d} = load %{d}\n", .{ inst_index, load.operand });
        },
        .ty_pointer => |ty_pointer| {
            try fmt.indent();
            try writer.print("%{d} = type pointer<%{d}>\n", .{ inst_index, ty_pointer.operand });
        },

        else => {
            try fmt.indent();
            try writer.print("{s}\n", .{@tagName(inst)});
        },
    }
}
pub fn format(value: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: std.io.AnyWriter) !void {
    _ = fmt; // autofix

    try formatInstruction(
        @constCast(&value),
        writer,
        options,
        Inst.RootIndex,
        0,
    );
}
pub const Value = struct {
    // tag: Tag,
    // data: Data,
    data: union(enum) {
        comptime_number: InternedSlice,
    },
    pub const Index = enum(u32) {
        true,
        false,
        null,

        _,
        pub const INDEX_START: u32 = std.meta.tags(@This().Index).len;

        pub fn asInt(self: Index) u32 {
            return @intCast(@intFromEnum(self));
        }

        pub fn asTypeIndex(self: u32) @This().Index {
            return @enumFromInt(self);
        }

        pub fn toTypeIndex(index: u32) Index {
            return @enumFromInt(
                @as(u32, @intCast(index + INDEX_START)),
            );
        }

        pub fn toInt(self: Index) ?u32 {
            const index = @intFromEnum(self);
            if (index < INDEX_START) return null;
            return @intCast(index - INDEX_START);
        }
    };
};

// std.zig.Zir.Header

pub const Inst = union(enum) {
    fn_decl: FnDecl,
    fn_call: FnCall,
    mod_decl: Module,
    param_decl: Param,
    param_get: UnaryOp,
    global_get: UnaryOp,
    global_decl: GlobalDecl,
    local_get: UnaryOp,
    load: UnaryOp,
    block: Block,
    inline_block: Block,
    local: Local,
    local_set: BinaryOp,
    param_set: BinaryOp,
    global_set: BinaryOp,
    decl_ref: Ast.Node.Index,
    loop: Loop,
    assign: BinaryOp,
    alloc: Alloc,
    store: Store,
    get_element_pointer: GetElement,
    get_element_value: GetElement,
    get_property_pointer: GetProperty,
    get_property_value: GetProperty,
    constant_int: Constant,

    comptime_number: Ast.Node.Index,
    ty_number: Ast.Node.Index,
    ty_boolean: Ast.Node.Index,
    undefined_value: ?Ast.Node.Index,

    as: BinaryOp,
    add: BinaryOp,
    sub: BinaryOp,
    mul: BinaryOp,
    div: BinaryOp,

    gt: BinaryOp,
    ge: BinaryOp,
    lt: BinaryOp,
    le: BinaryOp,
    eq: BinaryOp,
    ne: BinaryOp,

    typeof: UnaryOp,
    sizeof: UnaryOp,
    ret: UnaryOp,
    if_expr: IfExpr,
    br: UnaryOp,

    ty_i8: Ast.Node.Index,
    ty_i16: Ast.Node.Index,
    ty_i32: Ast.Node.Index,
    ty_i64: Ast.Node.Index,
    ty_i128: Ast.Node.Index,
    ty_i256: Ast.Node.Index,

    ty_u8: Ast.Node.Index,
    ty_u16: Ast.Node.Index,
    ty_u32: Ast.Node.Index,
    ty_u64: Ast.Node.Index,
    ty_u128: Ast.Node.Index,
    ty_u256: Ast.Node.Index,
    ty_usize: Ast.Node.Index,
    ty_array: TyArray,

    ty_f32: Ast.Node.Index,
    ty_f64: Ast.Node.Index,
    ty_void: Ast.Node.Index,
    ty_pointer: UnaryOp,

    debug_var: DebugVar,

    pub const Tag: type = std.meta.Tag(@This());
    pub const Alloc = struct {
        mutable: bool,
        type: Inst.Index,
    };
    pub const Constant = struct {
        value: i64,
    };
    pub const TyArray = struct {
        type: Inst.Index,
        size: Inst.Index,
    };
    pub const Store = struct {
        pointer: Inst.Index,
        value: Inst.Index,
    };
    pub const GetElement = struct {
        pointer: Inst.Index,
        index: Inst.Index,
    };
    pub const GetProperty = struct {
        base: Inst.Index,
        property_name: []const u8,
    };
    pub const Local = struct {
        name_node: Ast.Node.Index,
        mutable: bool,
        type: Inst.Index,
        // init: Inst.Index,
    };
    pub const FnCall = struct {
        callee: Index,
        args_list: List,
    };
    pub const Index = u32;
    pub const RootIndex: Index = 0;
    pub const Enum = std.meta.Tag(@This());
    pub const List = usize;
    pub const IfExpr = struct {
        cond: Index,
        then_body: Index,
        else_body: ?Index,
    };
    pub const Loop = struct {
        body: Index,
    };
    pub const UnaryOp = struct {
        operand: Index,
    };
    pub const BinaryOp = struct {
        lhs: Index,
        rhs: Index,
    };
    pub const FnDecl = struct {
        // name: InternedSlice,
        name_node: Ast.Node.Index,
        return_type: Inst.Index,
        params_list: List,

        // @"extern": bool,
        // visibility: shared.Visibility,
        // exported: bool,
        // mutable: bool,
    };
    pub const DebugVar = struct {
        name_node: Ast.Node.Index,
        instruction: Index,
    };
    pub const LocalDecl = struct {
        name_node: Ast.Node.Index,
        ty_node: ?Ast.Node.Index,
        init: ?Inst.Index,
        mutable: bool,
    };
    pub const Block = struct {
        name_node: ?Ast.Node.Index,
        instructions_list: List,
    };
    pub const GlobalDecl = struct {
        name_node: Ast.Node.Index,
        @"extern": bool,
        is_fn: bool,
        visibility: shared.Visibility,
        type: ?Inst.Index,
        exported: bool,
        mutable: bool,
        init: ?Inst.Index,
    };
    pub const Module = struct {
        name_node: ?Ast.Node.Index,
        declarations_list: List,
    };
    pub const Param = struct {
        name_node: Ast.Node.Index,
        // ty_node: Ast.Node.Index,
        ty: Inst.Index,
    };
};

pub const Definition = union(enum) {
    fn_definition: FnDefinition,

    pub const Index = u32;
    pub const List = usize;

    const FnDefinition = struct {
        name: InternedSlice,
        visibility: Visibility,
        exported: bool,
        mutable: bool,
        params_list: List,
        return_type: Inst.Index,
        init: ?Inst.Index = null,
    };
    pub const Visibility = enum {
        @"pub",
        private,
    };
};
