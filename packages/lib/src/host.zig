const builtin = @import("builtin");
const std = @import("std");
const Env = enum {
    wasm,
    system,
};
var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
pub usingnamespace if (builtin.target.isWasm() and builtin.target.os.tag == .freestanding) struct {
    // const OsWriter = @import("root-wasm.zig").OsWriter;
    // var stderr_writer = OsWriter.init(0);
    pub extern fn throw(pointer: [*]const u8, length: usize) noreturn;
    pub extern fn write(message: [*]const u8, length: usize) void;
    pub const env = Env.wasm;
    pub fn getStdErrWriter() std.io.AnyWriter {
        @panic("getStdErrWriter not implemented for wasm");
        // return stderr_writer.writer().any();
    }
    pub const allocator = general_purpose_allocator.allocator();
} else struct {
    pub const allocator = if (builtin.is_test) std.testing.allocator else general_purpose_allocator.allocator();
    pub fn throw(message: [*]const u8, length: usize) noreturn {
        @panic(message[0..length]);
    }

    pub const env = Env.system;
    pub fn getStdErrWriter() std.io.AnyWriter {
        return std.io.getStdErr().writer().any();
    }
    pub fn write(message: [*]const u8, length: usize) void {
        const stderr = std.io.getStdErr();
        _ = stderr.write(message[0..length]) catch {
            @panic("Failed to write to stdout");
        };
    }
};
