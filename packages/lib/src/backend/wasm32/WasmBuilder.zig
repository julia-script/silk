const std = @import("std");
const BoundedArray = std.BoundedArray;

fn writeVarUInt32(writer: std.io.AnyWriter, value: u32) !void {
    var val = value;
    while (true) {
        var byte: u8 = @intCast(val & 0x7F);
        val = val >> 7;
        if (val == 0) {
            try writer.writeByte(byte);
            break;
        } else {
            byte |= 0x80;
            try writer.writeByte(byte);
        }
    }
}
fn writeAllVarUInt32(writer: std.io.AnyWriter, values: anytype) !void {
    inline for (values) |value| {
        try writeVarUInt32(writer, @intCast(value));
    }
}
pub const Type = enum(u8) {
    function = 0x60,
    empty = 0x40,
    i32 = 0x7F,
    i64 = 0x7E,
    f32 = 0x7D,
    f64 = 0x7C,
    pub fn toBytes(self: Type, writer: std.io.AnyWriter) !void {
        try writeVarUInt32(writer, @intCast(@intFromEnum(self)));
    }
};

pub const Instruction = union(enum) {
    formatting: Formatting,

    @"return": void,
    @"local.get": u32,
    @"local.set": u32,
    @"local.tee": u32,
    @"global.get": u32,
    @"global.set": u32,
    end: void,
    call: u32,
    drop: void,

    // Arithmetic Operations
    @"i32.add": void,
    @"i64.add": void,
    @"f32.add": void,
    @"f64.add": void,

    @"i32.sub": void,
    @"i64.sub": void,
    @"f32.sub": void,
    @"f64.sub": void,

    @"i32.mul": void,
    @"i64.mul": void,
    @"f32.mul": void,
    @"f64.mul": void,

    @"i32.div_s": void,
    @"i64.div_s": void,
    @"i32.div_u": void,
    @"i64.div_u": void,
    @"f32.div": void,
    @"f64.div": void,

    // Comparison Operations
    @"i32.eq": void,
    @"i32.ne": void,
    @"i32.lt_s": void,
    @"i32.lt_u": void,
    @"i32.gt_s": void,
    @"i32.gt_u": void,
    @"i32.le_s": void,
    @"i32.le_u": void,
    @"i32.ge_s": void,
    @"i32.ge_u": void,

    @"i64.eq": void,
    @"i64.ne": void,
    @"i64.lt_s": void,
    @"i64.lt_u": void,
    @"i64.le_s": void,
    @"i64.le_u": void,
    @"i64.gt_s": void,
    @"i64.gt_u": void,
    @"i64.ge_s": void,
    @"i64.ge_u": void,

    @"f32.eq": void,
    @"f32.ne": void,
    @"f32.lt": void,
    @"f32.le": void,
    @"f32.gt": void,
    @"f32.ge": void,

    @"f64.eq": void,
    @"f64.ne": void,
    @"f64.lt": void,
    @"f64.le": void,
    @"f64.gt": void,
    @"f64.ge": void,

    @"i32.const": i32,
    @"i64.const": i64,
    @"f32.const": f32,
    @"f64.const": f64,

    @"if": Type,
    @"else": void,
    br: u32,

    loop: Type,

    @"i32.store": MemOp,
    @"i64.store": MemOp,
    @"f32.store": MemOp,
    @"f64.store": MemOp,
    @"i32.store8": MemOp,
    @"i32.store16": MemOp,
    @"i64.store8": MemOp,
    @"i64.store16": MemOp,
    @"i64.store32": MemOp,

    @"i32.load": MemOp,
    @"i64.load": MemOp,
    @"f32.load": MemOp,
    @"f64.load": MemOp,
    @"i32.load8_s": MemOp,
    @"i32.load8_u": MemOp,
    @"i32.load16_s": MemOp,
    @"i32.load16_u": MemOp,
    @"i64.load8_s": MemOp,
    @"i64.load8_u": MemOp,
    @"i64.load16_s": MemOp,
    @"i64.load16_u": MemOp,
    @"i64.load32_s": MemOp,
    @"i64.load32_u": MemOp,

    pub const Formatting = union(enum) {
        linebreak: void,
        comment: []const u8,
        inline_comment: []const u8,
    };

    // Memory instructions

    pub const BinOp = enum { add, sub, mul, div, mod };

    pub const MemOp = struct {
        memory: u32,
        offset: u8 = 0,
        alignment: ?u8 = null,
    };
    pub const END: Instruction = .{ .end = {} };
    pub fn writeInto(self: Instruction, section: *Section) !void {
        switch (self) {
            .formatting => {},

            .@"return" => {
                try section.write(0x0f);
            },
            .@"local.get" => |index| {
                try section.writeByte(0x20);
                try section.write(index);
            },
            .@"local.set" => |index| {
                try section.writeByte(0x21);
                try section.write(index);
            },
            .@"local.tee" => |index| {
                try section.writeByte(0x22);
                try section.write(index);
            },
            .@"global.get" => |index| {
                try section.writeByte(0x23);
                try section.write(index);
            },
            .@"global.set" => |index| {
                try section.writeByte(0x24);
                try section.write(index);
            },
            .call => |callee| {
                try section.writeByte(0x10);
                try section.write(callee);
            },
            .drop => try section.writeByte(0x1A),

            // Arithmetic Operations
            .@"i32.add" => try section.writeByte(0x6A), // i32.add
            .@"i64.add" => try section.writeByte(0x7C), // i64.add
            .@"f32.add" => try section.writeByte(0x92), // f32.add
            .@"f64.add" => try section.writeByte(0xA0), // f64.add

            .@"i32.sub" => try section.writeByte(0x6B), // i32.sub
            .@"i64.sub" => try section.writeByte(0x7D), // i64.sub
            .@"f32.sub" => try section.writeByte(0x93), // f32.sub
            .@"f64.sub" => try section.writeByte(0xA1), // f64.sub

            .@"i32.mul" => try section.writeByte(0x6C), // i32.mul
            .@"i64.mul" => try section.writeByte(0x7E), // i64.mul
            .@"f32.mul" => try section.writeByte(0x94), // f32.mul
            .@"f64.mul" => try section.writeByte(0xA2), // f64.mul

            .@"i32.div_s" => try section.writeByte(0x6D), // i32.div_s
            .@"i64.div_s" => try section.writeByte(0x7F), // i64.div_s
            .@"i32.div_u" => try section.writeByte(0x6E), // i32.div_u
            .@"i64.div_u" => try section.writeByte(0x80), // i64.div_u
            .@"f32.div" => try section.writeByte(0x95), // f32.div
            .@"f64.div" => try section.writeByte(0xA3),

            .@"i32.eq" => try section.writeByte(0x46),
            .@"i64.eq" => try section.writeByte(0x51),
            .@"i32.ne" => try section.writeByte(0x47),
            .@"i64.ne" => try section.writeByte(0x52),
            .@"i32.lt_s" => try section.writeByte(0x48),
            .@"i64.lt_s" => try section.writeByte(0x53),
            .@"i32.lt_u" => try section.writeByte(0x49),
            .@"i64.lt_u" => try section.writeByte(0x54),

            .@"i32.le_s" => try section.writeByte(0x4c),
            .@"i64.le_s" => try section.writeByte(0x57),
            .@"i32.le_u" => try section.writeByte(0x4d),
            .@"i64.le_u" => try section.writeByte(0x58),

            .@"i32.gt_s" => try section.writeByte(0x4a),
            .@"i64.gt_s" => try section.writeByte(0x55),
            .@"i32.gt_u" => try section.writeByte(0x4b),
            .@"i64.gt_u" => try section.writeByte(0x56),

            .@"i32.ge_s" => try section.writeByte(0x4e),
            .@"i64.ge_s" => try section.writeByte(0x59),
            .@"i32.ge_u" => try section.writeByte(0x4f),
            .@"i64.ge_u" => try section.writeByte(0x5a),

            .@"f32.eq" => try section.writeByte(0x5b),
            .@"f64.eq" => try section.writeByte(0x61),
            .@"f32.ne" => try section.writeByte(0x5c),
            .@"f64.ne" => try section.writeByte(0x62),
            .@"f32.lt" => try section.writeByte(0x5d),
            .@"f64.lt" => try section.writeByte(0x63),
            .@"f32.le" => try section.writeByte(0x5f),
            .@"f64.le" => try section.writeByte(0x65),
            .@"f32.gt" => try section.writeByte(0x5e),
            .@"f64.gt" => try section.writeByte(0x64),
            .@"f32.ge" => try section.writeByte(0x60),
            .@"f64.ge" => try section.writeByte(0x66),

            .@"i32.const" => |value| {
                try section.writeByte(0x41);
                try section.write(value);
            },
            .@"i64.const" => |value| {
                try section.writeByte(0x42);
                try section.write(value);
            },

            .@"f32.const" => |value| {
                try section.writeByte(0x43);
                try section.write(value);
            },
            .@"f64.const" => |value| {
                try section.writeByte(0x44);
                try section.write(value);
            },
            .@"if" => |ty| {
                try section.writeByte(0x04);
                try section.write(ty);
            },
            .@"else" => try section.writeByte(0x05),
            .end => try section.writeByte(0x0b),
            .loop => |ty| {
                try section.writeByte(0x03);
                try section.write(ty);
            },
            .br => |index| {
                try section.writeByte(0x0c);
                try section.write(index);
            },

            .@"i32.store" => |mem| {
                try section.writeByte(0x36);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 4);
            },
            .@"i64.store" => |mem| {
                try section.writeByte(0x37);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 8);
            },
            .@"f32.store" => |mem| {
                try section.writeByte(0x38);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 4);
            },
            .@"f64.store" => |mem| {
                try section.writeByte(0x39);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 8);
            },
            .@"i32.store8" => |mem| {
                try section.writeByte(0x3a);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 1);
            },
            .@"i32.store16" => |mem| {
                try section.writeByte(0x3b);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 2);
            },
            .@"i64.store8" => |mem| {
                try section.writeByte(0x3c);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 1);
            },
            .@"i64.store16" => |mem| {
                try section.writeByte(0x3d);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 2);
            },
            .@"i64.store32" => |mem| {
                try section.writeByte(0x3e);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 4);
            },

            .@"i32.load" => |mem| {
                try section.writeByte(0x28);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 4);
            },
            .@"i64.load" => |mem| {
                try section.writeByte(0x29);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 8);
            },
            .@"f32.load" => |mem| {
                try section.writeByte(0x2a);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 4);
            },
            .@"f64.load" => |mem| {
                try section.writeByte(0x2b);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 8);
            },
            .@"i32.load8_s" => |mem| {
                try section.writeByte(0x2c);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 1);
            },
            .@"i32.load8_u" => |mem| {
                try section.writeByte(0x2d);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 1);
            },
            .@"i32.load16_s" => |mem| {
                try section.writeByte(0x2e);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 2);
            },
            .@"i32.load16_u" => |mem| {
                try section.writeByte(0x2f);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 2);
            },
            .@"i64.load8_s" => |mem| {
                try section.writeByte(0x30);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 1);
            },
            .@"i64.load8_u" => |mem| {
                try section.writeByte(0x31);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 1);
            },
            .@"i64.load16_s" => |mem| {
                try section.writeByte(0x32);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 2);
            },
            .@"i64.load16_u" => |mem| {
                try section.writeByte(0x33);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 2);
            },
            .@"i64.load32_s" => |mem| {
                try section.writeByte(0x34);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 4);
            },
            .@"i64.load32_u" => |mem| {
                try section.writeByte(0x35);
                try section.writeByte(mem.offset);
                try section.writeByte(mem.alignment orelse 4);
            },
        }
    }
    pub fn toWat(self: Instruction, writer: *WatWriter) !void {
        switch (self) {
            // .@"local.get" => |index| try writer.print("local.get {d}", .{index}),
            // .@"local.set" => |index| try writer.print("local.set {d}", .{index}),
            // .@"local.tee" => |index| try writer.print("local.tee {d}", .{index}),
            // .@"global.get" => |index| try writer.print("global.get {d}", .{index}),
            // .@"global.set" => |index| try writer.print("global.set {d}", .{index}),

            // .@"i32.const" => |value| try writer.print("i32.const {d}", .{value}),
            // .@"i64.const" => |value| try writer.print("i64.const {d}", .{value}),
            // .@"f32.const" => |value| try writer.print("f32.const {d}", .{value}),
            // .@"f64.const" => |value| try writer.print("f64.const {d}", .{value}),
            .@"if" => unreachable,
            .@"else" => unreachable,
            .end => unreachable,
            .loop => unreachable,
            // .br => |index| try writer.print("br {d}", .{index}),

            // .@"return" => try writer.write("return"),
            // .i32_add => try writer.write("i32.add"),
            // .i64_add => try writer.write("i64.add"),
            // .f32_add => try writer.write("f32.add"),
            // .f64_add => try writer.write("f64.add"),
            // .i32_sub => try writer.write("i32.sub"),
            // .i64_sub => try writer.write("i64.sub"),
            // .f32_sub => try writer.write("f32.sub"),
            // .f64_sub => try writer.write("f64.sub"),
            // .i32_mul => try writer.write("i32.mul"),
            // .i64_mul => try writer.write("i64.mul"),
            // .f32_mul => try writer.write("f32.mul"),
            // .f64_mul => try writer.write("f64.mul"),
            // .i32_div_s => try writer.write("i32.div_s"),
            // .i64_div_s => try writer.write("i64.div_s"),
            // .i32_div_u => try writer.write("i32.div_u"),
            // .i64_div_u => try writer.write("i64.div_u"),
            // .f32_div => try writer.write("f32.div"),
            // .f64_div => try writer.write("f64.div"),
            // .i32_eq => try writer.write("i32.eq"),
            // .i32_ne => try writer.write("i32.ne"),
            // .i32_lt_s => try writer.write("i32.lt_s"),
            // .i32_lt_u => try writer.write("i32.lt_u"),
            // .i32_gt_s => try writer.write("i32.gt_s"),
            // .i32_gt_u => try writer.write("i32.gt_u"),
            // .i64_eq => try writer.write("i64.eq"),
            // .i64_ne => try writer.write("i64.ne"),
            // .i64_lt_s => try writer.write("i64.lt_s"),
            // .i64_lt_u => try writer.write("i64.lt_u"),
            // .i64_gt_s => try writer.write("i64.gt_s"),
            // .i64_gt_u => try writer.write("i64.gt_u"),

            // .f32_eq => try writer.write("f32.eq"),
            // .f32_ne => try writer.write("f32.ne"),
            // .f32_lt => try writer.write("f32.lt"),
            // .f32_gt => try writer.write("f32.gt"),

            // .f64_eq => try writer.write("f64.eq"),

            // .i32_store => try writer.write("i32.store"),
            // .i64_store => try writer.write("i64.store"),
            // .f32_store => try writer.write("f32.store"),
            // .f64_store => try writer.write("f64.store"),
            // .i32_store8 => try writer.write("i32.store8"),
            // .i32_store16 => try writer.write("i32.store16"),
            // .i64_store8 => try writer.write("i64.store8"),
            // .i64_store16 => try writer.write("i64.store16"),
            // .i64_store32 => try writer.write("i64.store32"),
            inline else => |payload| {
                const T = @TypeOf(payload);
                switch (T) {
                    void => {
                        try writer.write(@tagName(self));
                        return;
                    },
                    u32 => {
                        try writer.print("{s} {d}", .{ @tagName(self), payload });
                        return;
                    },
                    else => {
                        try writer.print("UNIMPLEMENTED: {s}", .{@tagName(self)});
                    },
                }
            },
        }
    }
};
const Local = struct {
    name: []const u8,
    type: Type,
    pub fn toBytes(self: Local, writer: std.io.AnyWriter) !void {
        try writeVarUInt32(writer, self.count);
        try self.type.toBytes(writer);
    }
};
pub const Function = struct {
    index: usize,
    name: []const u8 = "",
    // params: Array(Local),
    param_count: usize = 0,
    results: Array(Type),
    prologue: Array(Instruction),

    instructions: Array(Instruction),
    module: *Module,
    locals: Array(Local),
    @"export": bool = false,

    pub fn init(module: *Module, index: usize) Function {
        return .{
            .index = index,
            .results = Array(Type).init(module.arena.allocator()),
            .prologue = Array(Instruction).init(module.arena.allocator()),
            .instructions = Array(Instruction).init(module.arena.allocator()),
            .locals = Array(Local).init(module.arena.allocator()),
            .module = module,
        };
    }
    pub fn getParams(self: Function) []const Local {
        return self.locals.items[0..self.param_count];
    }
    pub fn getLocals(self: Function) []const Local {
        return self.locals.items[self.param_count..];
    }
    pub fn pushParam(self: *Function, name: []const u8, param_type: Type) !u32 {
        const index = try self.pushLocal(name, param_type);
        std.debug.print("pushParam: {any}\n", .{self.locals.items});
        self.param_count += 1;
        return index;
    }

    pub fn pushInstruction(self: *Function, instruction: Instruction) !void {
        std.debug.print("pushInstruction: {}\n", .{(instruction)});
        try self.instructions.append(instruction);
    }
    pub fn pushPrologue(self: *Function, instruction: Instruction) !void {
        try self.prologue.append(instruction);
    }

    pub fn pushFormatting(self: *Function, formatting: Instruction.Formatting) !void {
        switch (formatting) {
            .comment => |comment| {
                try self.instructions.append(.{ .formatting = .{ .comment = try self.module.arena.allocator().dupe(u8, comment) } });
            },
            .inline_comment => |comment| {
                try self.instructions.append(.{ .formatting = .{ .inline_comment = try self.module.arena.allocator().dupe(u8, comment) } });
            },
            else => {
                try self.instructions.append(.{ .formatting = formatting });
            },
        }
    }
    pub fn pushPrologueFormatting(self: *Function, formatting: Instruction.Formatting) !void {
        switch (formatting) {
            .comment => |comment| {
                try self.prologue.append(.{ .formatting = .{ .comment = try self.module.arena.allocator().dupe(u8, comment) } });
            },
            .inline_comment => |comment| {
                try self.prologue.append(.{ .formatting = .{ .inline_comment = try self.module.arena.allocator().dupe(u8, comment) } });
            },
            else => {
                try self.prologue.append(.{ .formatting = formatting });
            },
        }
    }
    pub fn pushResult(self: *Function, result: Type) !void {
        try self.results.append(result);
    }
    pub fn pushLocal(self: *Function, name: []const u8, value_type: Type) !u32 {
        const local_count: u32 = @intCast(self.locals.items.len);
        // const prev = self.locals.items.len;
        // defer self.local_count += 1;
        // if (self.locals.items.len > 0) {
        //     const last = self.locals.items[prev - 1];
        //     if (last.type == value_type) {
        //         self.locals.items[prev - 1].count += 1;
        //         return self.local_count;
        //     }
        // }

        // const local = Local{ .name = name, .type = value_type };
        try self.locals.append(Local{ .name = name, .type = value_type });
        return local_count;
    }
    pub fn getFunctionType(self: Function) FunctionType {
        return .{ .params = self.getParams(), .results = self.results.items };
    }
};
const FunctionType = struct {
    params: []const Local,
    results: []Type,
    pub fn toBytes(self: FunctionType, writer: std.io.AnyWriter) !void {
        try writeVarUInt32(writer, 0x60);
        try writeVarUInt32(writer, @intCast(self.params.len));
        for (self.params) |param| {
            try param.toBytes(writer);
        }
        try writeVarUInt32(writer, @intCast(self.results.len));
        for (self.results) |result| {
            try result.toBytes(writer);
        }
    }
    pub fn hash(self: FunctionType) u64 {
        var hasher = std.hash.Wyhash.init(0);
        hasher.update("function_type");
        hasher.update("params");
        for (self.params) |param| {
            hasher.update(std.mem.asBytes(&param));
        }
        hasher.update("results");
        for (self.results) |result| {
            hasher.update(std.mem.asBytes(&result));
        }
        return hasher.final();
    }
};

pub const Global = struct {
    index: usize,
    name: []const u8 = "",
    type: Type,
    mutable: bool = false,
    module: *Module,
    instructions: Array(Instruction),
    pub fn init(module: *Module, index: usize, name: []const u8, ty: Type) Global {
        return .{
            .index = index,
            .name = name,
            .type = ty,
            .instructions = Array(Instruction).init(module.arena.allocator()),
            .module = module,
        };
    }
    pub fn pushInstruction(self: *Global, instruction: Instruction) !void {
        std.debug.print("pushInstruction: {}\n", .{(instruction)});
        try self.instructions.append(instruction);
    }
    pub fn commit(self: *Global) !void {
        self.module.globals.items[self.index] = self.*;
    }
};
const Memory = struct {
    initial: u32,
    maximum: ?u32 = null,
    name: []const u8,
    is_exported: bool = true,
};

const Array = std.ArrayList;
pub const Module = struct {
    const FunctionTypeEntry = struct {
        usize,
        FunctionType,
    };
    function_types: std.AutoArrayHashMapUnmanaged(u64, FunctionTypeEntry) = .{},
    functions: std.ArrayListUnmanaged(Function) = .{},
    globals: std.ArrayListUnmanaged(Global) = .{},
    memories: std.ArrayListUnmanaged(Memory) = .{},
    allocator: std.mem.Allocator,
    arena: std.heap.ArenaAllocator,
    scratch: std.ArrayListUnmanaged(u8) = .{},
    start: ?u32 = null,
    pub fn init(allocator: std.mem.Allocator) Module {
        return Module{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .allocator = allocator,
        };
    }
    pub fn deinit(self: *Module) void {
        self.arena.deinit();
    }
    pub fn putFunction(self: *Module, function: *Function) !u32 {
        const func_type = function.getFunctionType();
        const hash = func_type.hash();
        if (self.function_types.get(hash) == null) {
            try self.function_types.put(self.arena.allocator(), hash, .{ self.function_types.count(), func_type });
        }
        self.functions.items[function.index] = function.*;
        return @intCast(self.functions.items.len - 1);
    }
    pub fn makeFunction(self: *Module) !Function {
        const index = self.functions.items.len;
        try self.functions.append(self.arena.allocator(), undefined);
        return Function.init(self, index);
    }
    pub fn makeGlobal(self: *Module, name: []const u8, ty: Type) !Global {
        const index = self.globals.items.len;
        try self.globals.append(self.arena.allocator(), undefined);
        return Global.init(self, index, name, ty);
    }
    pub fn makeMemory(self: *Module, name: []const u8, initial: u32, maximum: ?u32) !u32 {
        const index = self.memories.items.len;
        const allocator = self.arena.allocator();

        try self.memories.append(allocator, .{
            .name = try allocator.dupe(u8, name),
            .initial = initial,
            .maximum = maximum,
        });
        return @intCast(index);
    }
    pub fn toBytes(self: *Module, writer: std.io.AnyWriter) !void {
        // Magic number
        try writeAllVarUInt32(writer, &.{ 0x00, 0x61, 0x73, 0x6d });

        // Version
        try writeAllVarUInt32(writer, &.{ 0x01, 0x00, 0x00, 0x00 });

        // Write the Type Section
        // 01                 ; Section ID for Type Section
        // 07                 ; Section size (7 bytes)
        //
        // 01                 ; Type count (1 type)
        // for each type:
        // 60                 ; Function type indicator
        // 02                 ; Parameter count (2)
        // 7F 7F              ; Parameter types (i32, i32)
        // 01                 ; Result count (1)
        // 7F                 ; Result type (i32)

        const types_count = self.function_types.count();
        if (types_count > 0) {
            var types_section = Section.init(self.allocator, self, .type);
            defer types_section.deinit();

            try types_section.write(types_count);

            for (self.function_types.values()) |entry| {
                const func_type: FunctionType = entry[1];
                try types_section.write(Type.function);

                try types_section.write(func_type.params.len);
                // try types_section.write(func_type.params);
                for (func_type.params) |param| {
                    try types_section.write(param.type);
                }

                try types_section.write(func_type.results.len);
                try types_section.write(func_type.results);
            }

            try types_section.toBytes(writer);
        }

        // Write the Function Section
        // Example:
        // 03                 ; Section ID for Function Section
        // 02                 ; Section size (2 bytes)
        //
        // 01                 ; Function count (1 function)
        // 00                 ; Type index (0) for the function

        const functions_count = self.functions.items.len;
        if (functions_count > 0) {
            var functions_section = Section.init(self.allocator, self, .function);
            defer functions_section.deinit();

            try functions_section.write(functions_count);

            for (self.functions.items) |function| {
                const index = self.function_types.get(function.getFunctionType().hash()) orelse {
                    @panic("Function type not found");
                };
                try functions_section.write(index[0]);
            }

            try functions_section.toBytes(writer);
        }

        // Memory section
        // 05                 ; Section ID for Memory Section
        // 01                 ; Section size (1 bytes)
        // 01                 ; Memory count (1 memory)

        // 0x01               ; Flag (0x01 = has maximum, 0x00 = no maximum)
        // 0x00               ; Initial size (0)
        // 0x00               ; Maximum size (0) // only if if has maximum

        const memories_count = self.memories.items.len;
        if (memories_count > 0) {
            var memories_section = Section.init(self.allocator, self, .memory);
            defer memories_section.deinit();
            try memories_section.write(memories_count);

            for (self.memories.items) |memory| {
                if (memory.maximum) |max| {
                    try memories_section.writeByte(0x01);
                    try memories_section.write(memory.initial);
                    try memories_section.write(max);
                } else {
                    try memories_section.writeByte(0x00);
                    try memories_section.write(memory.initial);
                }
            }
            try memories_section.toBytes(writer);
        }

        // Global section
        // Global section
        // 06                 ; Section ID for Global Section
        // 01                 ; Section size (1 bytes)
        // 01                 ; Global count (1 global)

        // 0x7F               ; Global type (i32)
        // 01                 ; Mutable (true)
        // 0x41 0x2A          ; i32.const 42 (0x41 is the opcode for i32.const, 0x2A = 42 in LEB128)
        // 0x0B               ; end of init_expr

        const globals_count = self.globals.items.len;
        if (globals_count > 0) {
            var globals_section = Section.init(self.allocator, self, .global);
            defer globals_section.deinit();
            try globals_section.write(globals_count);

            for (self.globals.items) |global| {
                // try global.type.toBytes(globals_section.writer());
                try globals_section.write(global.type);

                try globals_section.write(global.mutable);
                // Instructions
                for (global.instructions.items) |instruction| {
                    try instruction.writeInto(&globals_section);
                }
                try Instruction.END.writeInto(&globals_section);
            }
            try globals_section.toBytes(writer);
        }

        // Export section
        // 07                 ; Section ID for Export Section
        // 01                 ; Section size (1 bytes)
        // 01                 ; Export count (1 export)
        // 00                 ; Function index (0)
        var export_count: usize = 0;
        for (self.functions.items) |function| {
            if (function.@"export") {
                export_count += 1;
            }
        }
        if (export_count > 0) {
            var export_section = Section.init(self.allocator, self, .@"export");
            defer export_section.deinit();
            try export_section.write(export_count);

            for (self.functions.items, 0..) |function, i| {
                if (function.@"export") {
                    try export_section.write(function.name.len);
                    try export_section.writeDirect(function.name);
                    try export_section.write(0);
                    try export_section.write(i);
                }
            }
            try export_section.toBytes(writer);
        }

        // Start section
        // 08                 ; Section ID for Start Section
        // 01                 ; Section size (1 bytes)
        // 00                 ; Function index (0)
        if (self.start) |index| {
            var start_section = Section.init(self.allocator, self, .start);
            defer start_section.deinit();

            try start_section.write(index);
            try start_section.toBytes(writer);
        }

        // Code section
        // 0A                 ; Section ID for Code Section
        // 09                 ; Section size (9 bytes)

        // 01                 ; Function body count (1 function)
        // 07                 ; Function body size (7 bytes)
        // 00                 ; Local count (0)
        // 20 00              ; local.get 0
        // 20 01              ; local.get 1
        // 6A                 ; i32.add
        // 0B                 ; end
        // Write the Code Section
        const body_count = self.functions.items.len;
        if (body_count > 0) {
            var code_section = Section.init(self.allocator, self, .code);
            defer code_section.deinit();

            try code_section.write(body_count);

            // Function body count
            var function_body_section = Section.init(self.allocator, self, .unidentified);

            defer function_body_section.deinit();
            for (self.functions.items) |function| {
                function_body_section.clearRetainingCapacity();

                // Locals
                try function_body_section.write(function.locals.items.len);

                var prev_type: ?Type = null;
                var local_count: u8 = 0;
                for (function.locals.items) |local| {
                    if (local.type != prev_type or local_count == 255) {
                        try function_body_section.write(1);
                        try function_body_section.write(local.type);
                        local_count = 1;
                    } else {
                        local_count += 1;
                        function_body_section.bytes.items[function_body_section.bytes.items.len - 2] = local_count;
                    }
                    prev_type = local.type;
                }

                // Instructions
                for (function.prologue.items) |instruction| {
                    try instruction.writeInto(&function_body_section);
                }
                for (function.instructions.items) |instruction| {
                    try instruction.writeInto(&function_body_section);
                }
                try Instruction.END.writeInto(&function_body_section);

                try code_section.write(&function_body_section);
            }

            try code_section.toBytes(writer);
        }
    }

    fn writeWatInstruction(self: *Module, wat_writer: *WatWriter, instruction: Instruction, locals: []const Local) !void {
        switch (instruction) {
            .formatting => |formatting| {
                switch (formatting) {
                    .comment, .inline_comment => |comment| {
                        try wat_writer.write(";; ");
                        try wat_writer.write(comment);
                        // try wat_writer.breakLine();
                    },
                    .linebreak => {
                        try wat_writer.breakLine();
                    },
                }
            },
            .@"if" => {
                try wat_writer.write("if");
                // try wat_writer.breakLine();
                wat_writer.indent += 1;
            },
            .end => {
                wat_writer.indent -= 1;
                try wat_writer.write("end");
                // try wat_writer.breakLine();
            },
            .loop => {
                try wat_writer.write("loop");
                // try wat_writer.breakLine();
                wat_writer.indent += 1;
            },
            // .@"i32.store",
            // .@"i64.store",
            // .@"f32.store",
            // .@"f64.store",
            // .@"i32.store8",
            // .@"i32.store16",
            // .@"i64.store8",
            // .@"i64.store16",
            // .@"i64.store32",
            // .@"i32.load",
            // .@"i64.load",
            // .@"f32.load",
            // .@"f64.load",
            // .@"i32.load8_s",
            // .@"i32.load8_u",
            // .@"i32.load16_s",
            // .@"i32.load16_u",
            // .@"i64.load8_s",
            // .@"i64.load8_u",
            // .@"i64.load16_s",
            // .@"i64.load16_u",
            // .@"i64.load32_s",
            // .@"i64.load32_u",
            // => |mem_op| {
            //     _ = mem_op; // autofix
            //     const tag = @tagName(instruction);
            //     try wat_writer.write(tag);
            // },
            .@"local.get", .@"local.set", .@"local.tee" => |local_index| {
                const name = locals[local_index].name;
                try wat_writer.print("{s} ${s}", .{ @tagName(instruction), name });
            },
            .@"global.get", .@"global.set" => |global_index| {
                const global = self.globals.items[global_index];
                try wat_writer.print("{s} ${s}", .{ @tagName(instruction), global.name });
            },
            inline else => |_payload| {
                const T = @TypeOf(_payload);
                switch (T) {
                    u32, i32, i64, f32, f64 => {
                        try wat_writer.print("{s} {d}", .{ @tagName(instruction), _payload });
                    },
                    Instruction.MemOp => {
                        const mem_op: Instruction.MemOp = _payload;
                        const mem = self.memories.items[mem_op.memory];
                        try wat_writer.print("{s} (memory ${s})", .{ @tagName(instruction), mem.name });
                    },
                    else => {
                        try instruction.toWat(wat_writer);
                    },
                }
                // try wat_writer.breakLine();
            },
        }
    }
    pub fn toWat(self: *Module, writer: std.io.AnyWriter) !void {
        var wat_writer = WatWriter{ .writer = writer };
        try wat_writer.open("module");
        // Memory section
        for (self.memories.items) |memory| {
            try wat_writer.writeIndent();
            //   (memory $memory (export "memory") 1 2)
            if (memory.is_exported) {
                try wat_writer.print("(memory ${s} (export \"{s}\") {d}", .{ memory.name, memory.name, memory.initial });
            } else {
                try wat_writer.print("(memory ${s} {d})", .{ memory.name, memory.initial });
            }
            if (memory.maximum) |max| {
                try wat_writer.print(" ${d}", .{max});
            }
            try wat_writer.write(")");
            try wat_writer.breakLine();
        }

        // Type section
        for (self.function_types.values(), 0..) |entry, i| {
            const entry_fn: FunctionType = entry[1];
            try wat_writer.writeIndent();
            try wat_writer.print("(type (;{d};) ", .{i});
            try wat_writer.write("(func");
            if (entry_fn.params.len > 0 or entry_fn.results.len > 0) {
                for (entry_fn.params) |param| {
                    try wat_writer.print(" (param {s})", .{@tagName(param.type)});
                }
                for (entry_fn.results) |result| {
                    try wat_writer.print(" (result {s})", .{@tagName(result)});
                }
            } else {
                try wat_writer.write(")");
            }
            try wat_writer.write(")\n");
        }

        // Memory section
        // for (self.memories.items) |memory| {
        //     try wat_writer.writeIndent();
        //     try wat_writer.print("(export \"{s}\" (memory ${s})", .{ memory.name, memory.name });
        //     try wat_writer.print(" {d}", .{memory.initial});
        //     if (memory.maximum) |max| {
        //         try wat_writer.print(" {d})", .{max});
        //     } else {
        //         try wat_writer.write(")\n");
        //     }
        // }

        // Function section
        // for (self.functions.items, 0..) |function, i| {
        for (self.functions.items) |function| {
            const index = self.function_types.get(function.getFunctionType().hash()) orelse {
                @panic("Function type not found");
            };
            try wat_writer.writeIndent();
            try wat_writer.print("(func ${s}", .{function.name});
            if (function.@"export") {
                try wat_writer.print(" (export \"{s}\")", .{function.name});
            }
            // try wat_writer.write(function.name);
            try wat_writer.print(" (type {d})", .{index[0]});
            // std.debug.print("params: {any}\n", .{function.getParams()});
            for (function.getParams()) |param| {
                try wat_writer.print(" (param ${s} {s})", .{ param.name, @tagName(param.type) });
            }
            for (function.results.items) |result| {
                try wat_writer.print(" (result {s})", .{@tagName(result)});
            }
            wat_writer.indent += 1;
            try wat_writer.breakLine();
            const locals = function.getLocals();
            if (locals.len > 0) {
                for (locals) |local| {
                    try wat_writer.writeIndent();
                    try wat_writer.print("(local ${s} {s})\n", .{ local.name, @tagName(local.type) });
                }
            }

            // for (function.prolo)

            inline for (&.{ function.prologue.items, function.instructions.items }) |instructions| {
                for (instructions, 0..) |instruction, i| {
                    try wat_writer.writeIndent();

                    try self.writeWatInstruction(&wat_writer, instruction, function.locals.items);
                    if (i + 1 < instructions.len) {
                        const next_i = instructions[i + 1];
                        switch (next_i) {
                            .formatting => |formatting| {
                                if (std.meta.activeTag(formatting) == .inline_comment) {
                                    continue;
                                }
                            },
                            else => {},
                        }
                    }
                    try wat_writer.breakLine();
                }
            }
        }

        wat_writer.indent -= 1;
        try wat_writer.writeIndent();
        try wat_writer.write(")\n");

        // Global section
        for (self.globals.items) |global| {
            try wat_writer.writeIndent();
            try wat_writer.print("(global ${s} ", .{global.name});
            if (global.mutable) {
                try wat_writer.print("(mut {s})", .{@tagName(global.type)});
            } else {
                try wat_writer.print("{s}", .{@tagName(global.type)});
            }
            std.debug.assert(global.instructions.items.len <= 1);
            try wat_writer.write(" (");
            for (global.instructions.items) |instruction| {
                try self.writeWatInstruction(&wat_writer, instruction, &.{});
            }
            try wat_writer.closeInline();
            try wat_writer.write(")\n");
        }

        // wat_writer.indent -= 1;
        // try wat_writer.writeIndent();
        // try wat_writer.write(")\n");

        try wat_writer.close(); //module
        try wat_writer.breakLine();
    }
    pub fn dumpBytes(self: *Module) !void {
        const buffer_capacity = 1024;
        var buf = try std.BoundedArray(u8, buffer_capacity).init(0);
        try self.toBytes(buf.writer().any());
        for (buf.slice(), 0..) |byte, i| {
            std.debug.print("{x:02} ", .{byte});
            if ((i + 1) % 4 == 0) {
                std.debug.print("\n", .{});
            }
        }
        std.debug.print("\n", .{});
    }
};

const WatWriter = struct {
    writer: std.io.AnyWriter,
    indent: usize = 0,
    pub fn writeIndent(self: *WatWriter) !void {
        for (0..self.indent) |_| {
            try self.writer.writeAll("  ");
        }
    }
    pub fn inlineComment(self: *WatWriter, comment: []const u8) !void {
        try self.writer.writeAll("(;");
        try self.writer.writeAll(comment);
        try self.writer.writeAll(";)");
    }
    pub fn openInline(self: *WatWriter, expr: []const u8) !void {
        try self.writer.writeAll("(");
        try self.writer.writeAll(expr);
        try self.writer.writeAll("");
    }
    pub fn closeInline(self: *WatWriter) !void {
        try self.writer.writeAll(")");
    }
    pub fn breakLine(self: *WatWriter) !void {
        try self.writer.writeAll("\n");
    }
    pub fn open(self: *WatWriter, expr: []const u8) !void {
        try self.writeIndent();
        self.indent += 1;
        try self.writer.writeAll("(");
        try self.writer.writeAll(expr);
        try self.writer.writeAll("\n");
    }
    pub fn close(self: *WatWriter) !void {
        self.indent -= 1;
        try self.writeIndent();
        try self.writer.writeAll(")");
    }
    pub fn write(self: *WatWriter, str: []const u8) !void {
        try self.writer.writeAll(str);
    }
    pub fn print(self: *WatWriter, comptime format: []const u8, args: anytype) !void {
        try self.writer.print(format, args);
    }
};
// fn writeVarUInt32(writer: std.io.AnyWriter, value: u32) !void {
//     var val = value;
//     while (true) {
//         var byte: u8 = @intCast(val & 0x7F);
//         val = val >> 7;
//         if (val != 0) {
//             byte |= 0x80;
//         }
//         try writer.writeByte(byte);
//     }
// }
pub const Section = struct {
    bytes: std.ArrayListUnmanaged(u8) = .{},
    kind: Kind,
    module: *Module,
    allocator: std.mem.Allocator,
    pub const Kind = enum(u8) {
        custom = 0x00,
        type = 0x01,
        import = 0x02,
        function = 0x03,
        table = 0x04,
        memory = 0x05,
        global = 0x06,
        @"export" = 0x07,
        start = 0x08,
        element = 0x09,
        code = 0x0a,
        data = 0x0b,
        data_count = 0x0c,

        unidentified = 0xFF,
    };
    pub fn init(allocator: std.mem.Allocator, module: *Module, kind: Kind) Section {
        return .{
            .module = module,
            .kind = kind,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Section) void {
        self.bytes.deinit(self.allocator);
    }

    pub fn writeUint32(self: *Section, value: u32) !usize {
        var val = value;
        const start_len = self.bytes.items.len;
        while (true) {
            var byte: u8 = @intCast(val & 0x7F);
            val = val >> 7;
            if (val == 0) {
                try self.bytes.append(self.allocator, byte);
                break;
            } else {
                byte |= 0x80;
                try self.bytes.append(self.allocator, byte);
            }
        }
        return self.bytes.items.len - start_len;
    }

    pub inline fn writeEnum(self: *Section, value: anytype) !usize {
        return try self.writeUint32(@intFromEnum(value));
    }

    pub inline fn writeSlice(self: *Section, T: type, value: []const T) !void {
        for (value) |item| {
            try self.write(item);
        }
    }
    pub fn writeByte(self: *Section, value: u8) !void {
        try self.bytes.append(self.allocator, value);
    }
    pub fn writeDirect(self: *Section, value: []const u8) !void {
        try self.bytes.appendSlice(self.allocator, value);
    }
    pub fn write(self: *Section, value: anytype) !void {
        const T = @TypeOf(value);
        const writer = self.bytes.writer(self.allocator);
        switch (T) {
            *Section => {
                try value.toBytes(self.bytes.writer(self.allocator).any());
            },
            i32, i64 => {
                try std.leb.writeIleb128(writer, value);
            },
            f32, f64 => {
                try self.write(std.mem.asBytes(&value));
            },
            u32, usize => {
                try std.leb.writeUleb128(writer, value);
            },

            comptime_int, u8 => {
                try self.writeByte(value);
            },
            bool => {
                try self.writeByte(if (value) 0x01 else 0x00);
            },

            else => {
                switch (@typeInfo(T)) {
                    .@"enum" => {
                        try self.writeByte(@intFromEnum(value));
                    },
                    .pointer => {
                        for (value) |item| {
                            try self.write(item);
                        }
                    },
                    else => {
                        std.debug.panic("Unsupported type {any}", .{T});
                    },
                }
            },
        }
    }

    pub fn dump(self: *Section) void {
        std.debug.print("Section '{s}'\n", .{@tagName(self.kind)});
        for (self.bytes.items, 0..) |byte, i| {
            std.debug.print("{x:02} ", .{byte});
            if ((i + 1) % 4 == 0) {
                std.debug.print("\n", .{});
            }
        }
        std.debug.print("\n", .{});
    }
    pub fn clearRetainingCapacity(self: *Section) void {
        self.bytes.clearRetainingCapacity();
    }
    pub fn toBytes(self: *Section, writer: std.io.AnyWriter) !void {
        if (self.kind != .unidentified) {
            try writeVarUInt32(writer, @intFromEnum(self.kind));
        }
        try writeVarUInt32(writer, @intCast(self.bytes.items.len));
        try writer.writeAll(self.bytes.items);
    }
};
test "wasm" {
    // const magic_bytes = std.mem.toBytes(MAGIC);
    // std.debug.print("MAGIC: {x}\n", .{magic_bytes});
    // expect(MAGIC == [_]u8{ 0x00, 0x61, 0x73, 0x6d });

    const buffer_capacity = 1024;
    var buf = try std.BoundedArray(u8, buffer_capacity).init(0);

    var module = try Module.init(std.testing.allocator);
    defer module.deinit();
    var func_a = module.makeFunction();
    try func_a.pushParam(.i32);
    try func_a.pushParam(.i32);
    try func_a.pushResult(.i32);

    try func_a.pushLocal(.i32);
    try func_a.pushLocal(.i32);

    try func_a.pushInstruction(.{ .local_get = 0 });
    try func_a.pushInstruction(.{ .local_get = 1 });
    try func_a.pushInstruction(.{ .i32_add = {} });
    const func_a_index = try module.pushFunction(func_a);
    _ = func_a_index; // autofix

    try module.toBytes(buf.writer().any());

    std.debug.print("=========== bytes ==========\n", .{});
    // std.debug.print("{x}\n", .{buf.slice()});
    for (buf.slice(), 0..) |byte, i| {
        std.debug.print("{x:02} ", .{byte});
        if ((i + 1) % 4 == 0) {
            std.debug.print("\n", .{});
        }
    }
    std.debug.print("\n=========== bytes ==========\n", .{});
    var output = try std.fs.cwd().createFile("writer.wasm", .{});
    defer output.close();
    try output.writeAll(buf.slice());
    var section = Section.init(std.testing.allocator, &module, .custom);
    defer section.deinit();

    try module.toWat(std.io.getStdErr().writer().any());
    var sec = Section.init(std.testing.allocator, &module, .custom);
    defer sec.deinit();

    // try add.writeInto(&sec);
}
