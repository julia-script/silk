const utils = @import("./utils.zig");
const Op = @import("./opcodes.zig").Op;
const std = @import("std");
const Block = @import("./Block.zig");
const Value = @import("./val.zig").Value;
const Dfg = @import("./Dfg.zig");
const Module = @import("./Module.zig");
const FunctionDeclaration = @import("./FunctionDeclaration.zig");
const TypedValue = Module.TypedValue;

pub const InstData = union(enum) {
    binary: struct {
        op: Op,
        args: [2]TypedValue,
    },
    operand: struct {
        op: Op,
        value: TypedValue,
    },
    @"return": struct {
        op: Op,
        value: TypedValue,
    },
    branch: struct {
        cond: TypedValue,
        // then, else, finally
        args: [3]?Block.Ref,
    },
    loop: struct {
        // body, finally
        args: [2]?Block.Ref,
    },
    @"break": struct {
        target: InstData.Ref,
        value: TypedValue,
    },
    store: struct {
        local: Dfg.Local.Ref,
        value: TypedValue,
    },
    storeGlobal: struct {
        global: Module.Decl.Ref,
        value: TypedValue,
    },
    block_call: struct {
        args: [2]?Block.Ref,
    },
    call: struct {
        callee: TypedValue,
        args: []TypedValue,
    },
    init_array: struct {
        ty: Module.Ty,
        items: []TypedValue,
    },
    init_struct: struct {
        ty: Module.Ty,
        keys: []const []const u8,
        values: []TypedValue,
    },
    cast: struct {
        ty: Module.Ty,
        value: TypedValue,
    },
    property_by_name: struct {
        tyv: TypedValue,
        name: []const u8,
    },
    builtin_property_access: struct {
        tyv: TypedValue,
        name: []const u8,
    },
    property_by_index: struct {
        tyv: TypedValue,
        index: TypedValue,
    },

    pub const Ref = utils.MakeRef(.inst, InstData, "\x1b[33mins{d}\x1b[0m");

    fn displayFn(self: InstData, writer: std.io.AnyWriter, module: *const Module) anyerror!void {
        switch (self) {
            .binary => |binary| {
                try writer.print("{} {} {}", .{ binary.op, binary.args[0].display(module), binary.args[1].display(module) });
            },
            .store => |store| {
                try writer.print("store {} {}", .{ store.local, store.value.display(module) });
            },
            .storeGlobal => |store| {
                try writer.print("store_global {} {}", .{ store.global, store.value.display(module) });
            },
            .branch => |branch| {
                try writer.print("branch {} {?}, {?}, {?}", .{ branch.cond.display(module), branch.args[0], branch.args[1], branch.args[2] });
            },
            .loop => |loop| {
                try writer.print("loop {?}, {?}", .{ loop.args[0], loop.args[1] });
            },
            .operand => |operand| {
                try writer.print("{} {}", .{ operand.op, operand.value.display(module) });
            },
            .@"return" => |@"return"| {
                try writer.print("return {}", .{@"return".value.display(module)});
            },
            .@"break" => |@"break"| {
                try writer.print("break {} {}", .{ @"break".target, @"break".value.display(module) });
            },
            .block_call => |block_call| {
                try writer.print("block_call {?}, {?}", .{ block_call.args[0], block_call.args[1] });
            },
            .cast => |cast| {
                try writer.print("cast {} {}", .{ cast.ty.display(module), cast.value.display(module) });
            },

            .call => |call| {
                try writer.print("call {}", .{call.callee.display(module)});
                if (call.args.len > 0) {
                    try writer.print(" with ", .{});
                }
                for (call.args, 0..) |arg, i| {
                    if (i > 0) {
                        try writer.print(", ", .{});
                    }
                    try writer.print("{}", .{arg.display(module)});
                }
            },
            .init_array => |init_array| {
                try writer.print("init_array {}", .{init_array.ty.display(module)});
                if (init_array.items.len > 0) {
                    try writer.print(" with ", .{});
                }
                for (init_array.items, 0..) |item, i| {
                    if (i > 0) {
                        try writer.print(", ", .{});
                    }
                    try writer.print("{}", .{item.display(module)});
                }
            },
            .init_struct => |init_struct| {
                try writer.print("init_struct {}", .{init_struct.ty.display(module)});
                if (init_struct.keys.len > 0) {
                    try writer.print(" with ", .{});
                }
                for (init_struct.keys, 0..) |key, i| {
                    if (i > 0) {
                        try writer.print(", ", .{});
                    }
                    try writer.print("{s} = {}", .{ key, init_struct.values[i].display(module) });
                }
            },
            .property_by_name => |property_by_name| {
                try writer.print("property_by_name {} '{s}'", .{ property_by_name.tyv.display(module), property_by_name.name });
            },
            .property_by_index => |property_by_index| {
                try writer.print("property_by_index {} {}", .{ property_by_index.tyv.display(module), property_by_index.index.display(module) });
            },
            .builtin_property_access => |builtin_property_access| {
                try writer.print("builtin_property_access {} '{s}'", .{ builtin_property_access.tyv.display(module), builtin_property_access.name });
            },

            // else => {
            //     // try writer.print("{} = {}", .{ inst_ref, inst });
            // },
        }
    }
    pub fn display(self: InstData, module: *const Module) Module.utils.MakeDisplay(InstData, displayFn) {
        return .{ .value = self, .module = module };
    }
};
