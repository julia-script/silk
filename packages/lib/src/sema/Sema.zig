const std = @import("std");
const Array = std.ArrayListUnmanaged;
const ArrayHashMap = std.AutoArrayHashMapUnmanaged;
const Tracer = @import("../Tracer.zig");
const Builder = @import("./gen.zig").Builder;
const Hir = @import("../Hir.zig");
const Ast = @import("../Ast.zig");
const ErrorManager = @import("../ErrorManager.zig");
const Entity = @import("./gen.zig").Entity;
const InternedList = @import("../interned-lists.zig").InternedLists;
pub const Lists = InternedList(usize);
pub const Strings = InternedList(u8);

allocator: std.mem.Allocator,

instructions: Array(Instruction) = .{},
values: ArrayHashMap(Value.Key, Value) = .{},
types: ArrayHashMap(Type.Key, Type) = .{},
strings: Strings,
lists: Lists,

const Self = @This();

pub fn build(
    allocator: std.mem.Allocator,
    hir: *Hir,
    errors_manager: *ErrorManager,
    options: Builder.BuildOptions,
) !Self {
    return Builder.build(
        allocator,
        hir,
        errors_manager,
        options,
    );
}
pub fn deinit(self: *Self) void {
    self.instructions.deinit(self.allocator);
    self.values.deinit(self.allocator);
    self.types.deinit(self.allocator);
    self.strings.deinit();
    self.lists.deinit();
}

pub const Type = struct {
    hash: u64,
    data: Data,

    pub const Key = union(enum) {
        simple: Simple,
        complex: usize,
        const SIMPLE_COUNT = std.meta.fields(Type.Simple).len;
        pub fn encode(self: Key) usize {
            return switch (self) {
                .simple => |s| @intFromEnum(s),
                .complex => |c| c + SIMPLE_COUNT,
            };
        }
        pub fn decode(self: usize) Key {
            return if (self < SIMPLE_COUNT)
                .{ .simple = @enumFromInt(self) }
            else
                .{ .complex = self - SIMPLE_COUNT };
        }
        // pub fn complexFromIndex(self: usize) Key {
        //     return .{ .complex = self + SIMPLE_COUNT };
        // }

        pub fn isEqual(self: Key, other: Key) bool {
            return switch (self) {
                .simple => |s| s == other.simple,
                .complex => |c| c == other.complex,
            };
        }
    };
    pub const List = Lists.Range;
    pub const Simple = enum {
        type,
        number,
        i8,
        i16,
        i32,
        i64,
        u8,
        u16,
        u32,
        u64,
        usize,
        f32,
        f64,
        bool,
        string,
        void,
    };

    pub fn simple(self: Simple) Key {
        return .{ .simple = self };
    }
    pub fn complex(self: usize) Key {
        return .{ .complex = self };
    }
    pub const Data = union(enum) {
        module: Ent,

        @"struct": struct {
            fields: Type.List,
        },
        function: struct {
            params: Type.List,
            ret: Type.Key,
        },
        array: struct {
            child: Type.Key,
            size: u32,
        },
        pointer: struct {
            child: Type.Key,
        },
        pub const Ent = struct {
            entity: Entity.Key,
        };
    };
};
pub const Value = struct {
    hash: u64,
    data: Data,
    pub const Data = union(enum) {
        integer: i64,
        float: f64,
        type: Type.Key,

        struct_instance: struct {
            struct_entity: Entity.Key,
            values: Value.List,
        },

        function: struct {
            type: Type.Key,
            init: ?Instruction.Index,
        },
    };

    pub const List = Lists.Range;
    pub const Key = union(enum) {
        simple: Simple,
        complex: usize,

        pub const SIMPLE_COUNT = std.meta.fields(Value.Simple).len;
        pub fn complexFromIndex(self: usize) Key {
            return .{ .complex = self + SIMPLE_COUNT };
        }
        pub fn decode(self: usize) Key {
            return if (self < SIMPLE_COUNT)
                .{ .simple = @enumFromInt(self) }
            else
                .{ .complex = self - SIMPLE_COUNT };
        }

        pub fn encode(self: Key) usize {
            return switch (self) {
                .simple => |s| @intFromEnum(s),
                .complex => |c| c + SIMPLE_COUNT,
            };
        }
    };
    pub const Simple = enum {
        unknown,
        runtime,
        true,
        false,
        undefined,
        nil,
        void,

        type_number,
        type_i8,
        type_i16,
        type_i32,
        type_i64,

        type_u8,
        type_u16,
        type_u32,
        type_u64,

        type_usize,

        type_f32,
        type_f64,

        type_bool,
        type_string,
    };
    pub fn simple(self: Simple) Key {
        return .{ .simple = self };
    }
    pub fn complex(self: usize) Key {
        return .{ .complex = self };
    }
};

pub const Instruction = struct {
    op: Op,
    type: Type.Key,
    value: Value.Key,
    data: Data,

    pub const Data = union(enum) {
        void,
        // param: struct {
        //     name: Value.Key,
        //     value: Value.Key,
        // },
        alloc: struct {
            type: Type.Key,
            mutable: bool,
        },
        store: struct {
            operand: Instruction.Index,
            value: Instruction.Index,
        },
        cast: struct {
            operand: Instruction.Index,
        },
    };
    pub const Op = enum {
        param,
        constant,
        type,
        alloc,
        store,
        cast,
    };

    pub const Index = usize;
};

test "Sema" {
    const allocator = std.testing.allocator;
    var errors_manager = try ErrorManager.init(allocator);
    defer errors_manager.deinit();
    const source =
        \\ pub fn fib(n: i32): i32 {
        \\   var a:i32 = 0
        \\   var b:i32 = 1
        \\   if (n > 0) {
        \\     while (n > 1) {
        \\       var t:i32 = a + b
        \\       a = b
        \\       b = t
        \\       n = n - 1
        \\     }
        \\     return b
        \\   }
        \\   return a
        \\ }
        \\
        // \\type A = struct {
        // \\  a: usize,
        // \\  b: i32 = 2,
        // \\  c: i32 = 3,
        // \\  pub fn foo(): void {
        // \\    const b: B = B{
        // \\      d = 4,
        // \\      e = 5,
        // \\      f = 6,
        // \\    }
        // \\    const a: A = A{
        // \\      a = 1,
        // \\      b = 2,
        // \\      c = 3,
        // \\    }
        // \\  }
        // \\}
        // \\type B = struct {
        // \\  d: usize,
        // \\  e: i32 = 2,
        // \\  f: i32 = 3,
        // \\  pub fn bar(): void {
        // \\    const a: A = A{
        // \\      a = 1,
        // \\      b = 2,
        // \\      c = 3,
        // \\    }
        // \\    const b: B = B{
        // \\      d = 4,
        // \\      e = 5,
        // \\      f = 6,
        // \\    }
        // \\  }
        // \\}
        // \\fn main(b: i32): void {
        // \\  const a: A = A{
        // \\    a = 1,
        // \\    b = 2,
        // \\    c = 3,
        // \\  }
        // \\  a.foo();
        // \\}
    ;
    var ast = try Ast.parse(allocator, &errors_manager, source[0..], .{});
    defer ast.deinit();
    var hir = try Hir.build(allocator, &ast, &errors_manager, .{});
    defer hir.deinit();
    std.debug.print("hir: {s}\n", .{hir});
    var sema = try Builder.build(allocator, &hir, &errors_manager, .{});
    defer sema.deinit();
    try sema.collectRoot();
    try sema.analyze("%::fib");
    // defer std.debug.panic("stop", .{});
}
