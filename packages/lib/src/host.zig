const builtin = @import("builtin");
const std = @import("std");

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
pub usingnamespace if (builtin.target.isWasm()) struct {
    pub extern fn hostThrow(pointer: [*]const u8, length: usize) noreturn;
    pub extern fn hostWrite(message: [*]const u8, length: usize) void;
    pub const gpa = general_purpose_allocator.allocator();
} else struct {
    pub const gpa = if (builtin.is_test) std.testing.allocator else general_purpose_allocator.allocator();
    pub fn hostThrow(message: [*]const u8, length: usize) noreturn {
        @panic(message[0..length]);
    }
    pub fn hostWrite(message: [*]const u8, length: usize) void {
        const stderr = std.io.getStdErr();
        _ = stderr.write(message[0..length]) catch {
            @panic("Failed to write to stdout");
        };
    }
};
