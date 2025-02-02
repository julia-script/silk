const std = @import("std");

const c = @cImport({
    @cInclude("cranelift.h");
});

pub const FunctionBuilderContext = struct {
    c: *c.struct_FunctionBuilderContext,

    pub fn init() FunctionBuilderContext {
        return .{
            .c = c.FunctionBuilderContext_new() orelse @panic("Failed to create FunctionBuilderContext"),
        };
    }
    pub fn deinit(self: *FunctionBuilderContext) void {
        c.FunctionBuilderContext_free(self.c);
    }
};

pub const Function = struct {
    c: *c.struct_Function,

    pub fn init() Function {
        return .{
            .c = c.Function_new() orelse @panic("Failed to create Function"),
        };
    }
    pub const UserFuncName = struct {
        namespace: u32,
        index: u32,
    };
    pub fn withNameSignature(name: UserFuncName, signature: *Signature) Function {
        return .{
            .c = c.Function_with_name_signature(name.namespace, name.index, signature.c) orelse @panic("Failed to create Function"),
        };
    }
    pub fn deinit(self: *Function) void {
        c.Function_free(self.c);
    }
    pub fn dump(self: *Function) void {
        c.Function_dump(self.c);
    }
    pub fn verify(self: *Function) void {
        c.Function_verify(self.c);
    }
};
fn Id(
    name: []const u8,
    T: type,
) type {
    return struct {
        id: T,
        pub const NAME = name;
        pub fn new(id: T) @This() {
            return .{ .id = id };
        }
    };
}

const Block = Id("Block", u32);
const Variable = Id("Variable", u32);
const Value = Id("Value", u32);
const FuncId = Id("FuncId", u32);
pub const FunctionBuilder = struct {
    c: *c.struct_FunctionBuilder,

    pub fn init(func: *Function, ctx: *FunctionBuilderContext) FunctionBuilder {
        return .{
            .c = c.FunctionBuilder_new(func.c, ctx.c) orelse @panic("Failed to create FunctionBuilder"),
        };
    }
    pub fn deinit(self: *FunctionBuilder) void {
        c.FunctionBuilder_free(self.c);
    }
    pub fn createBlock(self: *FunctionBuilder) Block {
        return .{ .id = c.FunctionBuilder_create_block(self.c) };
    }
    pub fn declareVar(self: *FunctionBuilder, variable: Variable, ty: Type) void {
        c.FunctionBuilder_declare_var(self.c, variable.id, ty.asC());
    }
    pub fn switchToBlock(self: *FunctionBuilder, block: Block) void {
        c.FunctionBuilder_switch_to_block(self.c, block.id);
    }
    pub fn appendBlockParamsForFunctionParams(self: *FunctionBuilder, block: Block) void {
        c.FunctionBuilder_append_block_params_for_function_params(self.c, block.id);
    }
    pub fn sealBlock(self: *FunctionBuilder, block: Block) void {
        c.FunctionBuilder_seal_block(self.c, block.id);
    }
    pub fn sealAllBlocks(self: *FunctionBuilder) void {
        c.FunctionBuilder_seal_all_blocks(self.c);
    }
    pub fn finalize(self: *FunctionBuilder) void {
        c.FunctionBuilder_finalize(self.c);
    }
    pub fn defVar(self: *FunctionBuilder, variable: Variable, value: Value) void {
        c.FunctionBuilder_def_var(self.c, variable.id, value.id);
    }
    pub fn useVar(self: *FunctionBuilder, variable: Variable) Value {
        return Value.new(c.FunctionBuilder_use_var(self.c, variable.id));
    }
    pub fn blockParam(self: *FunctionBuilder, block: Block, index: u32) Value {
        return Value.new(c.FunctionBuilder_block_param(self.c, block.id, index));
    }

    // Instructions
    pub fn insIconst(self: *FunctionBuilder, narrow_int: Type, value: i64) Value {
        return Value.new(c.FunctionBuilder_ins_iconst(self.c, narrow_int.asC(), value));
    }
    pub fn insIadd(self: *FunctionBuilder, a: Value, b: Value) Value {
        return Value.new(c.FunctionBuilder_ins_iadd(self.c, a.id, b.id));
    }
    pub fn insIsub(self: *FunctionBuilder, a: Value, b: Value) Value {
        return Value.new(c.FunctionBuilder_ins_isub(self.c, a.id, b.id));
    }
    pub fn insReturn(self: *FunctionBuilder, values: []const Value) void {
        c.FunctionBuilder_ins_return(self.c, valueSliceToC(values), @intCast(values.len));
    }
    pub fn insImul(self: *FunctionBuilder, a: Value, b: Value) Value {
        return Value.new(c.FunctionBuilder_ins_imul(self.c, a.id, b.id));
    }
    fn valueSliceToC(values: []const Value) *const c.struct_Value {
        return @alignCast(@ptrCast(values.ptr));
    }

    pub fn insJump(self: *FunctionBuilder, block: Block, values: []const Value) void {
        std.debug.print("insJump {any}\n", .{std.mem.sliceAsBytes(values)});

        c.FunctionBuilder_ins_jump(
            self.c,
            block.id,
            valueSliceToC(values),
            @intCast(values.len),
        );
    }
    pub fn insBrif(
        self: *FunctionBuilder,
        condition: Value,
        block_then: Block,
        block_then_args: []const Value,
        block_else: Block,
        block_else_args: []const Value,
    ) void {
        c.FunctionBuilder_ins_brif(
            self.c,
            condition.id,
            block_then.id,
            valueSliceToC(block_then_args),
            @intCast(block_then_args.len),
            block_else.id,
            valueSliceToC(block_else_args),
            @intCast(block_else_args.len),
        );
    }
};
const SettingsBuilder = struct {
    c: *c.struct_Builder,
    pub fn new() SettingsBuilder {
        return .{
            .c = c.SettingsBuilder_new() orelse @panic("Failed to create SettingsBuilder"),
        };
    }
    // pub fn deinit(self: *SettingsBuilder) void {
    //     c.SettingsBuilder_free(self.c);
    // }
    pub fn set(self: *SettingsBuilder, name: [:0]const u8, value: [:0]const u8) void {
        c.SettingsBuilder_set(self.c, name.ptr, value.ptr);
    }
};
pub const Flags = struct {
    c: *c.struct_Flags,
    pub fn new(settings_builder: *SettingsBuilder) Flags {
        return .{
            .c = c.Flags_new(settings_builder.c) orelse @panic("Failed to create Flags"),
        };
    }
};

const CallConv = enum(c.enum_CCallConv) {
    Fast,
    Cold,
    Tail,
    SystemV,
    WindowsFastcall,
    AppleAarch64,
    Probestack,
    Winch,
    pub fn asC(self: CallConv) c.enum_CCallConv {
        return @intFromEnum(self);
    }
};

pub const Signature = struct {
    c: *c.struct_Signature,
    pub fn init(call_conv: CallConv) Signature {
        return .{
            .c = c.Signature_new(call_conv.asC()) orelse @panic("Failed to create Signature"),
        };
    }
    pub fn deinit(self: *Signature) void {
        c.Signature_free(self.c);
    }
    pub fn addParam(self: *Signature, param: Type) void {
        c.Signature_add_param(self.c, param.asC());
    }
    pub fn addReturn(self: *Signature, ret: Type) void {
        c.Signature_add_return(self.c, ret.asC());
    }
};

const Type = enum(c.enum_CType) {
    /// An integer type with 8 bits.
    /// WARNING: arithmetic on 8bit integers is incomplete
    I8,

    /// An integer type with 16 bits.
    /// WARNING: arithmetic on 16bit integers is incomplete
    I16,

    /// An integer type with 32 bits.
    I32,

    /// An integer type with 64 bits.
    I64,

    /// An integer type with 128 bits.
    I128,

    /// A 16-bit floating point type represented in the IEEE 754-2008
    /// *binary16* interchange format. This corresponds to the :c:type:`_Float16`
    /// type in most C implementations.
    /// WARNING: f16 support is a work-in-progress and is incomplete
    F16,

    /// A 32-bit floating point type represented in the IEEE 754-2008
    /// *binary32* interchange format. This corresponds to the :c:type:`float`
    /// type in most C implementations.
    F32,

    /// A 64-bit floating point type represented in the IEEE 754-2008
    /// *binary64* interchange format. This corresponds to the :c:type:`double`
    /// type in most C implementations.
    F64,

    /// A 128-bit floating point type represented in the IEEE 754-2008
    /// *binary128* interchange format. This corresponds to the :c:type:`_Float128`
    /// type in most C implementations.
    /// WARNING: f128 support is a work-in-progress and is incomplete
    F128,

    /// A SIMD vector with 2 lanes containing a `i8` each.
    I8X2,

    /// A dynamically-scaled SIMD vector with a minimum of 2 lanes containing `i8` bits each.
    I8X2XN,

    /// A SIMD vector with 4 lanes containing a `i8` each.
    I8X4,

    /// A SIMD vector with 2 lanes containing a `i16` each.
    I16X2,

    /// A SIMD vector with 2 lanes containing a `f16` each.
    F16X2,

    /// A dynamically-scaled SIMD vector with a minimum of 4 lanes containing `i8` bits each.
    I8X4XN,

    /// A dynamically-scaled SIMD vector with a minimum of 2 lanes containing `i16` bits each.
    I16X2XN,

    /// A dynamically-scaled SIMD vector with a minimum of 2 lanes containing `f16` bits each.
    F16X2XN,

    /// A SIMD vector with 8 lanes containing a `i8` each.
    I8X8,

    /// A SIMD vector with 4 lanes containing a `i16` each.
    I16X4,

    /// A SIMD vector with 2 lanes containing a `i32` each.
    I32X2,

    /// A SIMD vector with 4 lanes containing a `f16` each.
    F16X4,

    /// A SIMD vector with 2 lanes containing a `f32` each.
    F32X2,

    /// A dynamically-scaled SIMD vector with a minimum of 8 lanes containing `i8` bits each.
    I8X8XN,

    /// A dynamically-scaled SIMD vector with a minimum of 4 lanes containing `i16` bits each.
    I16X4XN,

    /// A dynamically-scaled SIMD vector with a minimum of 2 lanes containing `i32` bits each.
    I32X2XN,

    /// A dynamically-scaled SIMD vector with a minimum of 4 lanes containing `f16` bits each.
    F16X4XN,

    /// A dynamically-scaled SIMD vector with a minimum of 2 lanes containing `f32` bits each.
    F32X2XN,

    /// A SIMD vector with 16 lanes containing a `i8` each.
    I8X16,

    /// A SIMD vector with 8 lanes containing a `i16` each.
    I16X8,

    /// A SIMD vector with 4 lanes containing a `i32` each.
    I32X4,

    /// A SIMD vector with 2 lanes containing a `i64` each.
    I64X2,

    /// A SIMD vector with 8 lanes containing a `f16` each.
    F16X8,

    /// A SIMD vector with 4 lanes containing a `f32` each.
    F32X4,

    /// A SIMD vector with 2 lanes containing a `f64` each.
    F64X2,

    /// A dynamically-scaled SIMD vector with a minimum of 16 lanes containing `i8` bits each.
    I8X16XN,

    /// A dynamically-scaled SIMD vector with a minimum of 8 lanes containing `i16` bits each.
    I16X8XN,

    /// A dynamically-scaled SIMD vector with a minimum of 4 lanes containing `i32` bits each.
    I32X4XN,

    /// A dynamically-scaled SIMD vector with a minimum of 2 lanes containing `i64` bits each.
    I64X2XN,

    /// A dynamically-scaled SIMD vector with a minimum of 8 lanes containing `f16` bits each.
    F16X8XN,

    /// A dynamically-scaled SIMD vector with a minimum of 4 lanes containing `f32` bits each.
    F32X4XN,

    /// A dynamically-scaled SIMD vector with a minimum of 2 lanes containing `f64` bits each.
    F64X2XN,

    /// A SIMD vector with 32 lanes containing a `i8` each.
    I8X32,

    /// A SIMD vector with 16 lanes containing a `i16` each.
    I16X16,

    /// A SIMD vector with 8 lanes containing a `i32` each.
    I32X8,

    /// A SIMD vector with 4 lanes containing a `i64` each.
    I64X4,

    /// A SIMD vector with 2 lanes containing a `i128` each.
    I128X2,

    /// A SIMD vector with 16 lanes containing a `f16` each.
    F16X16,

    /// A SIMD vector with 8 lanes containing a `f32` each.
    F32X8,

    /// A SIMD vector with 4 lanes containing a `f64` each.
    F64X4,

    /// A SIMD vector with 2 lanes containing a `f128` each.
    F128X2,

    /// A dynamically-scaled SIMD vector with a minimum of 32 lanes containing `i8` bits each.
    I8X32XN,

    /// A dynamically-scaled SIMD vector with a minimum of 16 lanes containing `i16` bits each.
    I16X16XN,

    /// A dynamically-scaled SIMD vector with a minimum of 8 lanes containing `i32` bits each.
    I32X8XN,

    /// A dynamically-scaled SIMD vector with a minimum of 4 lanes containing `i64` bits each.
    I64X4XN,

    /// A dynamically-scaled SIMD vector with a minimum of 2 lanes containing `i128` bits each.
    I128X2XN,

    /// A dynamically-scaled SIMD vector with a minimum of 16 lanes containing `f16` bits each.
    F16X16XN,

    /// A dynamically-scaled SIMD vector with a minimum of 8 lanes containing `f32` bits each.
    F32X8XN,

    /// A dynamically-scaled SIMD vector with a minimum of 4 lanes containing `f64` bits each.
    F64X4XN,

    /// A dynamically-scaled SIMD vector with a minimum of 2 lanes containing `f128` bits each.
    F128X2XN,

    /// A SIMD vector with 64 lanes containing a `i8` each.
    I8X64,

    /// A SIMD vector with 32 lanes containing a `i16` each.
    I16X32,

    /// A SIMD vector with 16 lanes containing a `i32` each.
    I32X16,

    /// A SIMD vector with 8 lanes containing a `i64` each.
    I64X8,

    /// A SIMD vector with 4 lanes containing a `i128` each.
    I128X4,

    /// A SIMD vector with 32 lanes containing a `f16` each.
    F16X32,

    /// A SIMD vector with 16 lanes containing a `f32` each.
    F32X16,

    /// A SIMD vector with 8 lanes containing a `f64` each.
    F64X8,

    /// A SIMD vector with 4 lanes containing a `f128` each.
    F128X4,

    /// A dynamically-scaled SIMD vector with a minimum of 64 lanes containing `i8` bits each.
    I8X64XN,

    /// A dynamically-scaled SIMD vector with a minimum of 32 lanes containing `i16` bits each.
    I16X32XN,

    /// A dynamically-scaled SIMD vector with a minimum of 16 lanes containing `i32` bits each.
    I32X16XN,

    /// A dynamically-scaled SIMD vector with a minimum of 8 lanes containing `i64` bits each.
    I64X8XN,

    /// A dynamically-scaled SIMD vector with a minimum of 4 lanes containing `i128` bits each.
    I128X4XN,

    /// A dynamically-scaled SIMD vector with a minimum of 32 lanes containing `f16` bits each.
    F16X32XN,

    /// A dynamically-scaled SIMD vector with a minimum of 16 lanes containing `f32` bits each.
    F32X16XN,

    /// A dynamically-scaled SIMD vector with a minimum of 8 lanes containing `f64` bits each.
    F64X8XN,

    /// A dynamically-scaled SIMD vector with a minimum of 4 lanes containing `f128` bits each.
    F128X4XN,
    pub fn asC(self: Type) c.CType {
        return @intFromEnum(self);
    }
};

const IsaBuilder = struct {
    c: *c.struct_IsaBuilder,
    pub fn new() IsaBuilder {
        return .{
            .c = c.IsaBuilder_native_builder() orelse @panic("Failed to create IsaBuilder"),
        };
    }
    pub fn finish(self: *IsaBuilder, flags: *Flags) TargetIsa {
        return .{ .c = c.IsaBuilder_finish(self.c, flags.c) orelse @panic("Failed to finish IsaBuilder") };
    }
};

const TargetIsa = struct {
    c: *c.struct_TargetIsaHandle,
};

const ObjectBuilder = struct {
    c: *c.struct_ObjectBuilder,
    pub fn new(isa: *TargetIsa, name: [:0]const u8) ObjectBuilder {
        return .{
            .c = c.ObjectBuilder_new(isa.c, name.ptr) orelse @panic("Failed to create ObjectBuilder"),
        };
    }
};

const ObjectModule = struct {
    c: *c.struct_ObjectModule,
    pub fn new(object_builder: *ObjectBuilder) ObjectModule {
        return .{
            .c = c.ObjectModule_new(object_builder.c) orelse @panic("Failed to create ObjectModule"),
        };
    }
    pub fn declareFunction(self: *ObjectModule, name: [:0]const u8, signature: *Signature) FuncId {
        return FuncId.new(c.ObjectModule_declare_function(self.c, name.ptr, signature.c));
    }
    pub fn defineFunction(self: *ObjectModule, func_id: FuncId, codegen_ctx: *CodegenContext) void {
        c.ObjectModule_define_function(self.c, func_id.id, codegen_ctx.c);
    }
    pub fn finish(self: *ObjectModule) ObjectProduct {
        return .{ .c = c.ObjectModule_finish(self.c) orelse @panic("Failed to finish ObjectModule") };
    }
};
const ObjectProduct = struct {
    c: *c.struct_ObjectProduct,
    pub fn emit(self: *ObjectProduct, allocator: std.mem.Allocator) !std.ArrayList(u8) {
        const c_bytes = c.ObjectProduct_emit(self.c);
        const len = c.byte_buffer_len(c_bytes);
        const data = c.byte_buffer_data(c_bytes);
        var array_list = std.ArrayList(u8).init(allocator);
        try array_list.appendSlice(data[0..len]);

        c.free_byte_buffer(c_bytes);
        return array_list;
    }
};

const CodegenContext = struct {
    c: *c.struct_Context,
    pub fn forFunction(func: *Function) CodegenContext {
        return .{
            .c = c.codegen_Context_for_function(func.c) orelse @panic("Failed to create CodegenContext"),
        };
    }
};

test "cranelift" {
    const decls = @typeInfo(c).@"struct".decls;
    for (decls) |decl| {
        std.debug.print("{s}\n", .{decl.name});
    }
    var settings = SettingsBuilder.new();

    settings.set("opt_level", "speed");
    var flags = Flags.new(&settings);
    var isa_builder = IsaBuilder.new();

    var isa = isa_builder.finish(&flags);
    var sig = Signature.init(CallConv.SystemV);
    // defer sig.deinit();
    sig.addParam(Type.I32);
    sig.addReturn(Type.I32);

    var object_builder = ObjectBuilder.new(&isa, "main");
    var object_module = ObjectModule.new(&object_builder);

    var ctx = FunctionBuilderContext.init();
    defer ctx.deinit();

    const function_id = object_module.declareFunction("main", &sig);
    var func = Function.withNameSignature(.{ .namespace = 0, .index = 0 }, &sig);
    // defer func.deinit();

    var builder = FunctionBuilder.init(&func, &ctx);
    // defer builder.deinit();

    const block0 = builder.createBlock();
    const block1 = builder.createBlock();
    const block2 = builder.createBlock();
    const block3 = builder.createBlock();

    const x = Variable.new(0);
    const y = Variable.new(1);
    const z = Variable.new(2);

    builder.declareVar(x, Type.I32);
    builder.declareVar(y, Type.I32);
    builder.declareVar(z, Type.I32);
    builder.appendBlockParamsForFunctionParams(block0);

    builder.switchToBlock(block0);
    builder.sealBlock(block0);

    {
        const tmp = builder.blockParam(block0, 0);
        builder.defVar(x, tmp);
    }
    {
        const tmp = builder.insIconst(Type.I32, 2);
        builder.defVar(y, tmp);
    }
    {
        const arg1 = builder.useVar(x);
        const arg2 = builder.useVar(y);
        const tmp = builder.insIadd(arg1, arg2);
        builder.defVar(z, tmp);
    }

    builder.insJump(block1, &.{});
    builder.switchToBlock(block1);
    {
        const arg1 = builder.useVar(y);
        const arg2 = builder.useVar(z);
        const tmp = builder.insIsub(arg1, arg2);
        builder.defVar(z, tmp);
    }
    {
        const arg = builder.useVar(y);
        builder.insBrif(arg, block3, &.{}, block2, &.{});
    }

    builder.switchToBlock(block2);
    // builder.sealBlock(block2);
    {
        const arg1 = builder.useVar(z);
        const arg2 = builder.useVar(x);
        const tmp = builder.insIsub(arg1, arg2);
        builder.defVar(z, tmp);
    }
    {
        const arg = builder.useVar(y);
        builder.insReturn(&.{arg});
    }

    builder.switchToBlock(block3);
    builder.sealBlock(block3);

    {
        const arg1 = builder.useVar(y);
        const arg2 = builder.useVar(x);
        const tmp = builder.insIsub(arg1, arg2);
        builder.defVar(y, tmp);
    }
    builder.insJump(block1, &.{});
    builder.sealBlock(block1);

    builder.sealAllBlocks();

    func.dump();
    func.verify();
    builder.finalize();

    var codegen_ctx = CodegenContext.forFunction(&func);
    object_module.defineFunction(function_id, &codegen_ctx);
    var product = object_module.finish();

    const bytes = try product.emit(std.testing.allocator);
    defer bytes.deinit();

    std.debug.print("{any}\n", .{bytes.items});

    try std.fs.cwd().writeFile(.{
        .sub_path = "main.o",
        .data = bytes.items,
    });
}
