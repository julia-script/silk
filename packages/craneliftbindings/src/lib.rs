use cranelift::codegen;
use cranelift::codegen::ir::types::*;
use cranelift::codegen::ir::Function;
use cranelift::codegen::ir::UserFuncName;
use cranelift::codegen::verify_function;
use cranelift::codegen::CodegenError;
use cranelift::module::FuncId;
use cranelift::module::Linkage;
use cranelift::module::Module;
use cranelift::object::ObjectBuilder;
use cranelift::object::ObjectModule;
use cranelift::object::ObjectProduct;
use cranelift::prelude::isa;
use cranelift::prelude::isa::CallConv;
use cranelift::prelude::isa::TargetIsa;
use cranelift::prelude::settings;
use cranelift::prelude::settings::Builder;
use cranelift::prelude::settings::Flags;
use cranelift::prelude::AbiParam;
use cranelift::prelude::Block;
use cranelift::prelude::Configurable;
use cranelift::prelude::EntityRef;
use cranelift::prelude::FunctionBuilder;
use cranelift::prelude::FunctionBuilderContext;
use cranelift::prelude::InstBuilder;
use cranelift::prelude::Signature;
use cranelift::prelude::Value;
use cranelift::prelude::Variable;
use std::ffi::c_char;
use std::ffi::CStr;
use std::sync::Arc;

#[repr(C)]
pub enum CCallConv {
    Fast,
    Cold,
    Tail,
    SystemV,
    WindowsFastcall,
    AppleAarch64,
    Probestack,
    Winch,
}
impl From<CCallConv> for CallConv {
    fn from(conv: CCallConv) -> Self {
        match conv {
            CCallConv::Fast => CallConv::Fast,
            CCallConv::Cold => CallConv::Cold,
            CCallConv::Tail => CallConv::Tail,
            CCallConv::SystemV => CallConv::SystemV,
            CCallConv::WindowsFastcall => CallConv::WindowsFastcall,
            CCallConv::AppleAarch64 => CallConv::AppleAarch64,
            CCallConv::Probestack => CallConv::Probestack,
            CCallConv::Winch => CallConv::Winch,
        }
    }
}

#[repr(C)]
pub enum CType {
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
}
impl Into<Type> for CType {
    fn into(self) -> Type {
        return match self {
            CType::I8 => I8,
            CType::I16 => I16,
            CType::I32 => I32,
            CType::I64 => I64,
            CType::I128 => I128,
            CType::F16 => F16,
            CType::F32 => F32,
            CType::F64 => F64,
            CType::F128 => F128,
            CType::I8X8 => I8X8,
            CType::I16X4 => I16X4,
            CType::I32X2 => I32X2,
            CType::F32X2 => F32X2,
            CType::I8X16 => I8X16,
            CType::I16X8 => I16X8,
            CType::I32X4 => I32X4,
            CType::I64X2 => I64X2,
            CType::F32X4 => F32X4,
            CType::F64X2 => F64X2,
            CType::I8X2 => I8X2,
            CType::I8X2XN => I8X2XN,
            CType::I8X4 => I8X4,
            CType::I16X2 => I16X2,
            CType::F16X2 => F16X2,
            CType::I8X4XN => I8X4XN,
            CType::I16X2XN => I16X2XN,
            CType::F16X2XN => F16X2XN,
            CType::F16X4 => F16X4,
            CType::I8X8XN => I8X8XN,
            CType::I16X4XN => I16X4XN,
            CType::I32X2XN => I32X2XN,
            CType::F16X4XN => F16X4XN,
            CType::F32X2XN => F32X2XN,
            CType::F16X8 => F16X8,
            CType::I8X16XN => I8X16XN,
            CType::I16X8XN => I16X8XN,
            CType::I32X4XN => I32X4XN,
            CType::I64X2XN => I64X2XN,
            CType::F16X8XN => F16X8XN,
            CType::F32X4XN => F32X4XN,
            CType::F64X2XN => F64X2XN,
            CType::I8X32 => I8X32,
            CType::I16X16 => I16X16,
            CType::I32X8 => I32X8,
            CType::I64X4 => I64X4,
            CType::I128X2 => I128X2,
            CType::F16X16 => F16X16,
            CType::F32X8 => F32X8,
            CType::F64X4 => F64X4,
            CType::F128X2 => F128X2,
            CType::I8X32XN => I8X32XN,
            CType::I16X16XN => I16X16XN,
            CType::I32X8XN => I32X8XN,
            CType::I64X4XN => I64X4XN,
            CType::I128X2XN => I128X2XN,
            CType::F16X16XN => F16X16XN,
            CType::F32X8XN => F32X8XN,
            CType::F64X4XN => F64X4XN,
            CType::F128X2XN => F128X2XN,
            CType::I8X64 => I8X64,
            CType::I16X32 => I16X32,
            CType::I32X16 => I32X16,
            CType::I64X8 => I64X8,
            CType::I128X4 => I128X4,
            CType::F16X32 => F16X32,
            CType::F32X16 => F32X16,
            CType::F64X8 => F64X8,
            CType::F128X4 => F128X4,
            CType::I8X64XN => I8X64XN,
            CType::I16X32XN => I16X32XN,
            CType::I32X16XN => I32X16XN,
            CType::I64X8XN => I64X8XN,
            CType::I128X4XN => I128X4XN,
            CType::F16X32XN => F16X32XN,
            CType::F32X16XN => F32X16XN,
            CType::F64X8XN => F64X8XN,
            CType::F128X4XN => F128X4XN,
        };
    }
}

// type SettingsBuilder = settings::Builder;

#[no_mangle]
pub extern "C" fn SettingsBuilder_free(builder: *mut settings::Builder) {
    if !builder.is_null() {
        unsafe { drop(Box::from_raw(builder)) };
    }
}
#[no_mangle]
pub extern "C" fn SettingsBuilder_new() -> *mut settings::Builder {
    return Box::into_raw(Box::new(settings::builder()));
}

#[no_mangle]
pub extern "C" fn SettingsBuilder_set(
    builder: *mut Builder,
    name: *const c_char,
    value: *const c_char,
) {
    assert!(!builder.is_null());
    assert!(!builder.is_null());

    // Borrow the builder as a mutable reference
    let builder = unsafe { &mut *builder };

    // Convert C strings to Rust strings
    let name = unsafe { CStr::from_ptr(name).to_str().unwrap() };
    let value = unsafe { CStr::from_ptr(value).to_str().unwrap() };

    println!("Setting {} = {}", name, value);

    // Apply the setting
    if let Err(e) = builder.set(name, value) {
        eprintln!("Failed to set {}: {}", name, e);
        // Handle the error as appropriate (e.g., return an error code)
    }
}

#[no_mangle]
pub extern "C" fn Flags_new(settings_builder: *mut settings::Builder) -> *mut Flags {
    assert!(!settings_builder.is_null());
    let builder = unsafe { Box::from_raw(settings_builder) };
    return Box::into_raw(Box::new(Flags::new(*builder)));
}

type IsaBuilder = isa::IsaBuilder<Result<Arc<dyn TargetIsa>, CodegenError>>;

#[no_mangle]
pub extern "C" fn IsaBuilder_native_builder() -> *mut IsaBuilder {
    Box::into_raw(Box::new(cranelift::native::builder().unwrap()))
}
pub struct TargetIsaHandle {
    isa: Arc<dyn TargetIsa>,
}
#[no_mangle]
pub extern "C" fn IsaBuilder_finish(
    builder: *mut IsaBuilder,
    flags: *mut Flags,
) -> *mut TargetIsaHandle {
    if builder.is_null() || flags.is_null() {
        eprintln!("IsaBuilder_finish called with null pointer");
        return std::ptr::null_mut();
    }
    let builder = unsafe { Box::from_raw(builder) };

    let flags = unsafe { &*flags };

    match builder.finish(flags.clone()) {
        Ok(isa) => {
            return Box::into_raw(Box::new(TargetIsaHandle { isa }));
        }
        Err(e) => {
            eprintln!("Failed to finish IsaBuilder: {}", e);
            std::ptr::null_mut()
        }
    }
}

#[no_mangle]
pub extern "C" fn ObjectBuilder_new(
    isa: *mut TargetIsaHandle,
    name: *const c_char,
) -> *mut ObjectBuilder {
    let isa = unsafe { &mut *isa };
    let name = unsafe { CStr::from_ptr(name).to_str().unwrap() };
    let object_builder = ObjectBuilder::new(
        isa.isa.clone(),
        name,
        cranelift::module::default_libcall_names(),
    )
    .unwrap();

    return Box::into_raw(Box::new(object_builder));
}

#[no_mangle]
pub extern "C" fn ObjectModule_new(object_builder: *mut ObjectBuilder) -> *mut ObjectModule {
    let object_builder = unsafe { Box::from_raw(object_builder) };
    let object_module = ObjectModule::new(*object_builder);
    return Box::into_raw(Box::new(object_module));
}

#[no_mangle]
pub extern "C" fn ObjectModule_declare_function(
    object_module: *mut ObjectModule,
    name: *const c_char,
    signature: *const Signature,
) -> u32 {
    let object_module = unsafe { &mut *object_module };
    let name = unsafe { CStr::from_ptr(name).to_str().unwrap() };
    let signature = unsafe { &*signature };
    let func_id = object_module
        .declare_function(name, Linkage::Export, signature)
        .unwrap();
    return func_id.as_u32();
}
#[no_mangle]
pub extern "C" fn ObjectModule_define_function(
    object_module: *mut ObjectModule,
    func_id: u32,
    ctx: *mut codegen::Context,
) {
    let object_module = unsafe { &mut *object_module };
    let func_id = FuncId::from_u32(func_id);
    let ctx = unsafe { &mut *ctx };
    object_module.define_function(func_id, ctx).unwrap();
}

#[no_mangle]
pub extern "C" fn ObjectModule_finish(object_module: *mut ObjectModule) -> *mut ObjectProduct {
    let object_module = unsafe { Box::from_raw(object_module) };
    let product = object_module.finish();
    return Box::into_raw(Box::new(product));
}

#[no_mangle]
pub extern "C" fn ObjectProduct_emit(product: *mut ObjectProduct) -> *mut ByteBuffer {
    let product = unsafe { Box::from_raw(product) };
    let bytes = ByteBuffer::from_vec(product.emit().unwrap());
    return Box::into_raw(Box::new(bytes));
}

#[no_mangle]
pub extern "C" fn codegen_Context_for_function(func: *mut Function) -> *mut codegen::Context {
    let func = unsafe { Box::from_raw(func) };
    let ctx = codegen::Context::for_function(*func);
    return Box::into_raw(Box::new(ctx));
}

#[no_mangle]
pub extern "C" fn hello_world() {
    println!("Hello from Rust!!!");
}
#[no_mangle]
pub extern "C" fn Signature_new(conv: CCallConv) -> *mut Signature {
    let sig = Signature::new(conv.into());
    return Box::into_raw(Box::new(sig));
}
#[no_mangle]
pub extern "C" fn Signature_add_param(sig: *mut Signature, param: CType) {
    let sig = unsafe { &mut *sig };
    sig.params.push(AbiParam::new(param.into()));
}
#[no_mangle]
pub extern "C" fn Signature_add_return(sig: *mut Signature, ret: CType) {
    let sig = unsafe { &mut *sig };
    sig.returns.push(AbiParam::new(ret.into()));
}
#[no_mangle]
pub extern "C" fn Signature_free(sig: *mut Signature) {
    if !sig.is_null() {
        unsafe { drop(Box::from_raw(sig)) };
    }
}
// #[allow(non_snake_case)]
// pub extern "C" fn UserFuncName_user(namespace: u32, index: u32) -> *mut UserFuncName {
//     return Box::into_raw(Box::new(UserFuncName::user(namespace, index)));
// }
// #[no_mangle]
// pub extern "C" fn UserFuncName_free(name: *mut UserFuncName) {
//     if !name.is_null() {
//         unsafe { drop(Box::from_raw(name)) };
//     }
// }

#[no_mangle]
pub extern "C" fn Function_new() -> *mut Function {
    let func = Function::new();
    return Box::into_raw(Box::new(func));
}
#[no_mangle]
pub extern "C" fn Function_free(func: *mut Function) {
    if !func.is_null() {
        unsafe { drop(Box::from_raw(func)) };
    }
}

#[no_mangle]
pub extern "C" fn Function_verify(func: *mut Function) {
    let flags = settings::Flags::new(settings::builder());
    let func = unsafe { &mut *func };
    if let Err(errors) = verify_function(func, &flags) {
        panic!("{}\n{}", func.display(), errors)
    }
}

#[no_mangle]
pub extern "C" fn Function_dump(func: *mut Function) {
    let func = unsafe { &mut *func };
    println!("{:?}", func);
}

#[no_mangle]
pub extern "C" fn Function_with_name_signature(
    namespace: u32,
    index: u32,
    signature: *mut Signature,
) -> *mut Function {
    assert!(!signature.is_null());
    let signature = unsafe { Box::from_raw(signature) };
    return Box::into_raw(Box::new(Function::with_name_signature(
        UserFuncName::user(namespace, index),
        *signature,
    )));
}
#[no_mangle]
pub extern "C" fn FunctionBuilderContext_new() -> *mut FunctionBuilderContext {
    let func = FunctionBuilderContext::new();
    return Box::into_raw(Box::new(func));
}
#[no_mangle]
pub extern "C" fn FunctionBuilderContext_free(ctx: *mut FunctionBuilderContext) {
    if !ctx.is_null() {
        unsafe { drop(Box::from_raw(ctx)) };
    }
}

// Function builder functions
#[no_mangle]
pub extern "C" fn FunctionBuilder_new<'a>(
    func: *mut Function,
    ctx: *mut FunctionBuilderContext,
) -> *mut FunctionBuilder<'a> {
    let func = unsafe { &mut *func };
    let ctx = unsafe { &mut *ctx };
    let builder = FunctionBuilder::new(func, ctx);
    return Box::into_raw(Box::new(builder));
}

#[no_mangle]
pub extern "C" fn FunctionBuilder_free(builder: *mut FunctionBuilder) {
    if !builder.is_null() {
        unsafe { drop(Box::from_raw(builder)) };
    }
}

#[no_mangle]
pub extern "C" fn FunctionBuilder_create_block(builder: *mut FunctionBuilder) -> u32 {
    let builder = unsafe { &mut *builder };
    let block = builder.create_block();
    return block.as_u32();
}

#[no_mangle]
pub extern "C" fn FunctionBuilder_declare_var(
    builder: *mut FunctionBuilder,
    variable: u32,
    ty: CType,
) {
    let builder = unsafe { &mut *builder };
    builder.declare_var(Variable::new(variable as usize), ty.into());
}

#[no_mangle]
pub extern "C" fn FunctionBuilder_append_block_params_for_function_params(
    builder: *mut FunctionBuilder,
    block: u32,
) {
    let builder = unsafe { &mut *builder };
    builder.append_block_params_for_function_params(Block::from_u32(block));
    println!("block: {:?}", builder.block_params(Block::from_u32(block)));
}

#[no_mangle]
pub extern "C" fn FunctionBuilder_switch_to_block(builder: *mut FunctionBuilder, block: u32) {
    let builder = unsafe { &mut *builder };
    builder.switch_to_block(Block::from_u32(block));
}

#[no_mangle]
pub extern "C" fn FunctionBuilder_seal_block(builder: *mut FunctionBuilder, block: u32) {
    let builder = unsafe { &mut *builder };
    builder.seal_block(Block::from_u32(block));
}

#[no_mangle]
pub extern "C" fn FunctionBuilder_seal_all_blocks(builder: *mut FunctionBuilder) {
    let builder = unsafe { &mut *builder };
    builder.seal_all_blocks();
}

#[no_mangle]
pub extern "C" fn FunctionBuilder_finalize(builder: *mut FunctionBuilder) {
    assert!(!builder.is_null());
    let builder = unsafe { Box::from_raw(builder) };
    builder.finalize();
}

#[no_mangle]
pub extern "C" fn FunctionBuilder_def_var(
    builder: *mut FunctionBuilder,
    variable: u32,
    value: u32,
) {
    let builder = unsafe { &mut *builder };
    builder.def_var(Variable::new(variable as usize), Value::new(value as usize));
}

#[no_mangle]
pub extern "C" fn FunctionBuilder_use_var(builder: *mut FunctionBuilder, variable: u32) -> u32 {
    let builder = unsafe { &mut *builder };
    let value = builder.use_var(Variable::new(variable as usize));
    return value.as_u32();
}

#[no_mangle]
pub extern "C" fn FunctionBuilder_block_param(
    builder: *mut FunctionBuilder,
    block: u32,
    index: u32,
) -> u32 {
    let builder = unsafe { &mut *builder };
    let block = builder.block_params(Block::from_u32(block));
    return block[index as usize].as_u32();
}

#[no_mangle]
pub extern "C" fn FunctionBuilder_ins_iconst(
    builder: *mut FunctionBuilder,
    narrow_int: CType,
    value: i64,
) -> u32 {
    let builder = unsafe { &mut *builder };
    let tmp = builder.ins().iconst(narrow_int.into(), value);
    return tmp.as_u32();
}

// #[no_mangle]
// pub extern "C" fn FunctionBuilder_ins_f16const(builder: *mut FunctionBuilder, value: f32) -> u32 {
//     let builder = unsafe { &mut *builder };
//     let tmp = builder.ins().f16const(value);
//     return tmp.as_u32();
// }

#[no_mangle]
pub extern "C" fn FunctionBuilder_ins_f32const(builder: *mut FunctionBuilder, value: f32) -> u32 {
    let builder = unsafe { &mut *builder };
    let tmp = builder.ins().f32const(value);
    return tmp.as_u32();
}

#[no_mangle]
pub extern "C" fn FunctionBuilder_ins_f64const(builder: *mut FunctionBuilder, value: f64) -> u32 {
    let builder = unsafe { &mut *builder };
    let tmp = builder.ins().f64const(value);
    return tmp.as_u32();
}

#[no_mangle]
pub extern "C" fn FunctionBuilder_ins_iadd(builder: *mut FunctionBuilder, a: u32, b: u32) -> u32 {
    let builder = unsafe { &mut *builder };
    let tmp = builder.ins().iadd(convert_value(a), convert_value(b));
    return tmp.as_u32();
}

#[no_mangle]
pub extern "C" fn FunctionBuilder_ins_isub(builder: *mut FunctionBuilder, a: u32, b: u32) -> u32 {
    let builder = unsafe { &mut *builder };
    let tmp = builder.ins().isub(convert_value(a), convert_value(b));
    return tmp.as_u32();
}

#[no_mangle]
pub extern "C" fn FunctionBuilder_ins_imul(builder: *mut FunctionBuilder, a: u32, b: u32) -> u32 {
    let builder = unsafe { &mut *builder };
    let tmp = builder.ins().imul(convert_value(a), convert_value(b));
    return tmp.as_u32();
}

#[no_mangle]
pub extern "C" fn FunctionBuilder_ins_jump(
    builder: *mut FunctionBuilder,
    block: u32,
    values: *const Value,
    count: u32,
) {
    let builder = unsafe { &mut *builder };
    builder
        .ins()
        .jump(Block::from_u32(block), convert_value_slice(values, count));
}

#[no_mangle]
pub extern "C" fn FunctionBuilder_ins_brif(
    builder: *mut FunctionBuilder,
    condition: u32,
    block_then: u32,
    block_then_args: *const Value,
    block_then_args_count: u32,
    block_else: u32,
    block_else_args: *const Value,
    block_else_args_count: u32,
) {
    let builder = unsafe { &mut *builder };
    builder.ins().brif(
        convert_value(condition),
        Block::from_u32(block_then),
        convert_value_slice(block_then_args, block_then_args_count),
        Block::from_u32(block_else),
        convert_value_slice(block_else_args, block_else_args_count),
    );
}

// #[no_mangle]
// pub extern "C" fn FunctionBuilder_ins_br_table(
//     builder: *mut FunctionBuilder,
//     index: u32,
//     table: u32,
// ) {
//     // let builder = unsafe { &mut *builder };
//     // let table = builder.create_jump_table({});
//     // builder.ins().br_table(convert_value(index), convert_value(table));
// }

#[no_mangle]
pub extern "C" fn FunctionBuilder_ins_return(
    builder: *mut FunctionBuilder,
    values: *const Value,
    count: u32,
) {
    let builder = unsafe { &mut *builder };
    let values_slice: &[Value] = match count {
        0 => &[],
        _ => unsafe { std::slice::from_raw_parts(values, count as usize) },
    };
    builder.ins().return_(&values_slice);
}

fn convert_value_slice<'a>(values: *const Value, count: u32) -> &'a [Value] {
    match count {
        0 => &[],
        _ => unsafe { std::slice::from_raw_parts(values, count as usize) },
    }
}

fn convert_value(value: u32) -> Value {
    Value::new(value as usize)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_hello_world() {
        let mut settings_builder = SettingsBuilder_new();
        let key: *const c_char = CStr::from_bytes_with_nul(b"opt_level\0").unwrap().as_ptr();
        let value: *const c_char = CStr::from_bytes_with_nul(b"speed\0").unwrap().as_ptr();
        println!("key: {:?}", key);
        println!("value: {:?}", value);
        // SettingsBuilder_set(settings_builder, key, value);
        // let flags = Flags_new(&mut settings_builder);
        // println!("{:?}", flags);
    }
}
#[repr(C)]
pub struct ByteBuffer {
    data: *mut u8,
    len: usize,
}

impl ByteBuffer {
    // Create a new ByteBuffer from a Vec<u8>
    fn from_vec(vec: Vec<u8>) -> Self {
        let len = vec.len();
        let data = vec.as_ptr() as *mut u8;
        // Prevent Rust from deallocating the vector
        std::mem::forget(vec);
        ByteBuffer { data, len }
    }

    // Convert back to Vec<u8> for deallocation
    unsafe fn to_vec(self) -> Vec<u8> {
        let data = std::slice::from_raw_parts_mut(self.data, self.len);
        Vec::from_raw_parts(data.as_mut_ptr(), self.len, self.len)
    }
}

#[no_mangle]
pub extern "C" fn create_byte_buffer(data: *const u8, len: usize) -> *mut ByteBuffer {
    if data.is_null() || len == 0 {
        return std::ptr::null_mut();
    }

    let slice = unsafe { std::slice::from_raw_parts(data, len) };
    let vec = slice.to_vec();
    let buffer = ByteBuffer::from_vec(vec);
    Box::into_raw(Box::new(buffer))
}

#[no_mangle]
pub extern "C" fn byte_buffer_data(buffer: *const ByteBuffer) -> *const u8 {
    if buffer.is_null() {
        return std::ptr::null();
    }
    unsafe { (*buffer).data }
}

#[no_mangle]
pub extern "C" fn byte_buffer_len(buffer: *const ByteBuffer) -> usize {
    if buffer.is_null() {
        return 0;
    }
    unsafe { (*buffer).len }
}

#[no_mangle]
pub extern "C" fn free_byte_buffer(buffer: *mut ByteBuffer) {
    if buffer.is_null() {
        return;
    }

    let buffer = unsafe { Box::from_raw(buffer) };
    unsafe { buffer.to_vec() };
}
