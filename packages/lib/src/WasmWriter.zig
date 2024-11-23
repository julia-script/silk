pub const MAGIC = [_]u8{ 0x00, 0x61, 0x73, 0x6d };
pub const VERSION = [_]u8{ 0x01, 0x00, 0x00, 0x00 };
const std = @import("std");

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
    for (values) |value| {
        try writeVarUInt32(writer, value);
    }
}

const Module = struct {
    functions: BoundedArray(Function, 1024),
    type_section: TypeSection,
    function_section: FunctionSection,
    // function_section: FunctionSection,
    // code_section: CodeSection,

    pub fn toBytes(self: *Module, writer: std.io.AnyWriter) !void {
        // Write magic number
        try writer.writeAll(MAGIC[0..]);

        // Write version
        try writer.writeAll(VERSION[0..]);

        // Write sections
        if (self.type_section.types.len > 0) {
            try self.type_section.toBytes(writer);
        }
        if (self.function_section.functions.len > 0) {
            try self.function_section.toBytes(writer);
        }
        // try self.code_section.toBytes(writer);
    }

    pub fn init() Module {
        return .{ .type_section = TypeSection.init(), .function_section = FunctionSection.init() };
    }
};
fn ByteArray(comptime capacity: usize) type {
    return std.BoundedArray(u8, capacity);
}
const BoundedArray = std.BoundedArray;
pub const Type = enum(u8) {
    empty = 0x40,

    i32 = 0x7F,
    i64 = 0x7E,
    f32 = 0x7D,
    f64 = 0x7C,
};
const TypeSection = struct {
    types: BoundedArray(FuncType, 1024) = .{},

    pub const FuncType = struct {
        params: BoundedArray(Type, 1024),
        results: BoundedArray(Type, 1024),
        pub fn init() FuncType {
            return .{ .params = .{}, .results = .{} };
        }
        pub fn pushParam(self: *FuncType, param: Type) !void {
            try self.params.append(param);
        }
        pub fn pushResult(self: *FuncType, result: Type) !void {
            try self.results.append(result);
        }
    };

    pub fn init() TypeSection {
        return .{ .types = .{} };
    }
    pub fn makeFuncType(self: *TypeSection) !*FuncType {
        const func_type = FuncType.init();
        try self.types.append(func_type);
        const pointer = &self.types.buffer[self.types.len - 1];
        pointer.index = @intCast(self.types.len - 1);
        return pointer;
    }

    // pub fn pushType(self: *TypeSection, typ: FuncType) !void {
    //     // std.debug.print("pushType: {any}\n", .{typ.results.slice()});
    //     typ.index = self.types.len;
    //     try self.types.append(typ);
    //     std.debug.print("types: {any}\n", .{self.types.slice()});
    // }

    pub fn toBytes(self: *TypeSection, writer: std.io.AnyWriter) !void {
        // Write section code for Type Section
        try writer.writeByte(0x01);
        var content = try ByteArray(1024).init(0);
        const content_writer = content.writer().any();

        // Number of types
        try writeVarUInt32(content_writer, @intCast(self.types.len));

        // Write each type
        std.debug.print("types: {any}\n", .{self.types.slice()});
        for (self.types.slice()) |typ| {
            // Function type indicator
            try content_writer.writeByte(0x60);

            // Parameter types
            try writeVarUInt32(content_writer, @intCast(typ.params.len));
            for (typ.params.slice()) |param| {
                try writeVarUInt32(content_writer, @intFromEnum(param));
            }

            // Result types
            try writeVarUInt32(content_writer, @intCast(typ.results.len));
            for (typ.results.slice()) |result| {
                try writeVarUInt32(content_writer, @intFromEnum(result));
            }
        }

        // Write section size
        try writeVarUInt32(writer, @intCast(content.len));

        // Write section content
        try writer.writeAll(content.slice());
    }
};

// const FunctionSection = struct {
//     type_indices: []u32, // Indices into the Type Section
//     pub fn toBytes(self: FunctionSection, writer: std.io.AnyWriter) []const u8 {
//     }
// }

const FunctionSection = struct {
    functions: BoundedArray(u32, 1024),

    pub fn init() FunctionSection {
        return .{ .functions = .{} };
    }
    pub fn toBytes(self: FunctionSection, writer: std.io.AnyWriter) !void {
        // Write section code for Function Section
        try writer.writeByte(0x03);

        // Calculate section content size
        var content = try ByteArray(1024).init(0);
        const content_writer = content.writer().any();

        // Number of functions
        try writeVarUInt32(content_writer, @intCast(self.functions.len));

        // Write each function's type index
        for (self.functions.slice()) |func| {
            try writeVarUInt32(content_writer, func);
        }

        // Write section size
        try writeVarUInt32(writer, @intCast(content.len));

        // Write section content
        try writer.writeAll(content.slice());
    }
    pub fn pushFunc(self: *FunctionSection, function_type: *TypeSection.FuncType) !void {
        try self.functions.append(function_type.index);
    }
};
pub fn wasm_to_wat(allocator: std.mem.Allocator) []const u8 {
    const Child = std.process.Child;
    var proc = Child.init(&[_]u8{"wat2wasm"}, .{ .allocator = allocator });
    proc.stderr_behavior = .Pipe;
    proc.stdout_behavior = .Pipe;

    try proc.spawn();

    var reader = proc.stdout.?.reader();
    while (reader.readByte()) |value| {
        std.debug.print("{c}", .{value});
    } else |err| {
        if (err == error.ReadError) {
            std.debug.print("read stdout failed\n", .{});
            // break;
            @panic("read stdout failed");
        }
    }
    const term = try proc.wait();
    _ = term; // autofix
    return "";
}

const Instruction = union(enum) {
    local_get: u32,
    local_set: u32,
    end: void,
    i32_add: void,
    // Memory instructions

    pub const BinOp = enum { add, sub, mul, div, mod };

    pub fn toBytes(self: Instruction, writer: std.io.AnyWriter) !void {
        switch (self) {
            .local_get => |index| try writeAllVarUInt32(writer, .{ 0x20, index }),
            .local_set => |index| try writeAllVarUInt32(writer, .{ 0x21, index }),
            .i32_add => try writeVarUInt32(writer, 0x6a),
            .end => try writeVarUInt32(writer, 0x0b),
        }
    }
};
const Function = struct {
    params: BoundedArray(Type, 1024),
    results: BoundedArray(Type, 1024),
    instructions: BoundedArray(u8, 1024),
    signature_hasher: std.hash.Wyhash,
    pub fn init() Function {
        return .{
            .params = .{},
            .results = .{},
            .instructions = .{},
            .signature_hasher = std.hash.Wyhash.init(std.hash.Wyhash.hash(0, "function")),
        };
    }
    pub fn pushParam(self: *Function, param: Type) !void {
        try self.params.append(param);
        self.signature_hasher.update("param");
        self.signature_hasher.update(std.mem.asBytes(&param));
    }
    pub fn pushInstruction(self: *Function, instruction: Instruction) !void {
        try self.instructions.append(instruction);
        // self.hasher.update("instruction");
        // try instruction.toBytes(self.instructions.writer().any());
    }
    pub fn pushResult(self: *Function, result: Type) !void {
        try self.results.append(result);
        self.signature_hasher.update("result");
        self.signature_hasher.update(std.mem.asBytes(&result));
    }
    pub fn getSignatureHash(self: Function) u64 {
        return self.signature_hasher.final();
    }
};

test "wasm" {
    // const magic_bytes = std.mem.toBytes(MAGIC);
    // std.debug.print("MAGIC: {x}\n", .{magic_bytes});
    // expect(MAGIC == [_]u8{ 0x00, 0x61, 0x73, 0x6d });

    const buffer_capacity = 1024;
    var buf = try std.BoundedArray(u8, buffer_capacity).init(0);

    var module = Module.init();
    // var type_section = &module.type_section;
    var func_type = try module.type_section.makeFuncType();
    try func_type.pushParam(.i32);
    try func_type.pushParam(.f64);
    try func_type.pushResult(.i32);
    try func_type.pushResult(.f64);
    // try type_section.pushType(func_type);
    // module.type_section = type_section;
    try module.function_section.pushFunc(func_type);

    try module.toBytes(buf.writer().any());
    var output = try std.fs.cwd().createFile("writer.wasm", .{});
    defer output.close();
    try output.writeAll(buf.slice());
    std.debug.print("=========== bytes ==========\n", .{});
    std.debug.print("{x}\n", .{buf.slice()});
    std.debug.print("\n=========== bytes ==========\n", .{});
}
