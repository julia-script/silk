const std = @import("std");
const Array = std.ArrayListUnmanaged;

const ErrorManager = @This();
pub const Slice = struct {
    start: usize,
    len: usize,
};
const Error = struct {
    tag: Tag,
    start: u32,
    end: u32,
    payload: u32 = 0,
    pub const Tag = enum {
        unexpected_token,
        expected_token,

        unexpected_node,
        expected_node,

        expected_type,
        unexpected_type,

        expected_expression,

        expression_not_parenthesized,

        expected_const_or_var,

        expected_explicit_type,

        expected_init_value,
        expected_identifier,

        duplicate_definition,
    };
};
errors: Array(Error),
allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) !ErrorManager {
    return .{
        .allocator = allocator,
        .errors = try Array(Error).initCapacity(allocator, 0),
    };
}
pub fn deinit(self: *ErrorManager) void {
    self.errors.deinit(self.allocator);
}
pub fn addError(self: *ErrorManager, err: Error) !void {
    try self.errors.append(self.allocator, err);
}
