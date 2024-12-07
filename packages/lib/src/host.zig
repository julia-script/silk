const builtin = @import("builtin");
const std = @import("std");
const Env = enum {
    wasm,
    system,
};
var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
pub usingnamespace if (builtin.target.isWasm() and builtin.target.os.tag != .wasi) struct {
    // const OsWriter = @import("root-wasm.zig").OsWriter;
    // var stderr_writer = OsWriter.init(0);
    pub extern fn throw(pointer: [*]const u8, length: usize) noreturn;
    pub extern fn write(message: [*]const u8, length: usize) void;
    fn dummyWrite(context: void, bytes: []const u8) error{}!usize {
        _ = context; // autofix
        _ = bytes; // autofix
        // return bytes.len;
        return 0;
    }
    pub const env = Env.wasm;
    pub fn getStdErrWriter() std.io.AnyWriter {
        const dummy_writer = std.io.Writer(void, error{}, dummyWrite){
            .context = {},
        };
        return dummy_writer.any();
        // return std.io.getStdErr().writer().any();
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
