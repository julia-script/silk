//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
const std = @import("std");

test {
    _ = @import("Lexer.zig");
    _ = @import("Ast.zig");
    _ = @import("AstGen.zig");
    _ = @import("Hir.zig");
    _ = @import("HirBuilder.zig");
    _ = @import("Mir.zig");
    _ = @import("MirBuilder.zig");
    // _ = @import("WasmWriter.zig");
    _ = @import("./backend/wasm/WasmBuilder.zig");
    _ = @import("Compilation.zig");
    _ = @import("serializer.zig");
    _ = @import("dir.zig");
    _ = @import("chunked_array.zig");
    _ = @import("interned-lists.zig");
    // _ = @import("Os.zig");
    // _ = @import("Fs.zig");
    _ = @import("Tracer.zig");
}
