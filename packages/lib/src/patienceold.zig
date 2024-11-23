// pub const __builtin_strlen = @import("std").zig.c_builtins.__builtin_strlen;

// pub const __builtin_object_size = @import("std").zig.c_builtins.__builtin_object_size;
// pub const __builtin___memset_chk = @import("std").zig.c_builtins.__builtin___memset_chk;

// pub extern fn malloc(__size: c_ulong) ?*anyopaque;
// pub extern fn calloc(__count: c_ulong, __size: c_ulong) ?*anyopaque;
// pub extern fn free(?*anyopaque) void;

// pub extern fn strlen(__s: [*c]const u8) c_ulong;

// pub extern fn strncmp(__s1: [*c]const u8, __s2: [*c]const u8, __n: c_ulong) c_int;

// pub const struct_s_xpparam = extern struct {
//     flags: c_ulong = @import("std").mem.zeroes(c_ulong),

//     anchors: [*c][*c]u8 = @import("std").mem.zeroes([*c][*c]u8),
//     anchors_nr: usize = @import("std").mem.zeroes(usize),
// };
// pub const xpparam_t = struct_s_xpparam;

// pub const struct_s_chanode = extern struct {
//     next: [*c]struct_s_chanode = @import("std").mem.zeroes([*c]struct_s_chanode),
//     icurr: c_long = @import("std").mem.zeroes(c_long),
// };
// pub const chanode_t = struct_s_chanode;
// pub const struct_s_chastore = extern struct {
//     head: [*c]chanode_t = @import("std").mem.zeroes([*c]chanode_t),
//     tail: [*c]chanode_t = @import("std").mem.zeroes([*c]chanode_t),
//     isize: c_long = @import("std").mem.zeroes(c_long),
//     nsize: c_long = @import("std").mem.zeroes(c_long),
//     ancur: [*c]chanode_t = @import("std").mem.zeroes([*c]chanode_t),
//     sncur: [*c]chanode_t = @import("std").mem.zeroes([*c]chanode_t),
//     scurr: c_long = @import("std").mem.zeroes(c_long),
// };
// pub const chastore_t = struct_s_chastore;
// pub const struct_s_xrecord = extern struct {
//     next: [*c]struct_s_xrecord = @import("std").mem.zeroes([*c]struct_s_xrecord),
//     ptr: [*c]const u8 = @import("std").mem.zeroes([*c]const u8),
//     size: c_long = @import("std").mem.zeroes(c_long),
//     ha: c_ulong = @import("std").mem.zeroes(c_ulong),
// };
// pub const xrecord_t = struct_s_xrecord;
// pub const struct_s_xdfile = extern struct {
//     rcha: chastore_t = @import("std").mem.zeroes(chastore_t),
//     nrec: c_long = @import("std").mem.zeroes(c_long),
//     hbits: c_uint = @import("std").mem.zeroes(c_uint),
//     rhash: [*c][*c]xrecord_t = @import("std").mem.zeroes([*c][*c]xrecord_t),
//     dstart: c_long = @import("std").mem.zeroes(c_long),
//     dend: c_long = @import("std").mem.zeroes(c_long),
//     recs: [*c][*c]xrecord_t = @import("std").mem.zeroes([*c][*c]xrecord_t),
//     rchg: [*c]u8 = @import("std").mem.zeroes([*c]u8),
//     rindex: [*c]c_long = @import("std").mem.zeroes([*c]c_long),
//     nreff: c_long = @import("std").mem.zeroes(c_long),
//     ha: [*c]c_ulong = @import("std").mem.zeroes([*c]c_ulong),
// };
// pub const xdfile_t = struct_s_xdfile;
// pub const struct_s_xdfenv = extern struct {
//     xdf1: xdfile_t = @import("std").mem.zeroes(xdfile_t),
//     xdf2: xdfile_t = @import("std").mem.zeroes(xdfile_t),
// };
// pub const xdfenv_t = struct_s_xdfenv;

// pub extern fn xdl_fall_back_diff(diff_env: [*c]xdfenv_t, xpp: [*c]const xpparam_t, line1: c_int, count1: c_int, line2: c_int, count2: c_int) c_int;

// pub export fn xdl_do_patience_diff(arg_xpp: [*c]const xpparam_t, arg_env: [*c]xdfenv_t) c_int {
//     var xpp = arg_xpp;
//     _ = &xpp;
//     var env = arg_env;
//     _ = &env;
//     return patience_diff(xpp, env, @as(c_int, 1), @as(c_int, @bitCast(@as(c_int, @truncate(env.*.xdf1.nrec)))), @as(c_int, 1), @as(c_int, @bitCast(@as(c_int, @truncate(env.*.xdf2.nrec)))));
// }

// pub const struct_entry_4 = struct {};
// pub const struct_hashmap = extern struct {
//     nr: c_int = @import("std").mem.zeroes(c_int),
//     alloc: c_int = @import("std").mem.zeroes(c_int),
//     entries: ?*struct_entry_4 = @import("std").mem.zeroes(?*struct_entry_4),
//     first: ?*struct_entry_4 = @import("std").mem.zeroes(?*struct_entry_4),
//     last: ?*struct_entry_4 = @import("std").mem.zeroes(?*struct_entry_4),
//     has_matches: c_ulong = @import("std").mem.zeroes(c_ulong),
//     env: [*c]xdfenv_t = @import("std").mem.zeroes([*c]xdfenv_t),
//     xpp: [*c]const xpparam_t = @import("std").mem.zeroes([*c]const xpparam_t),
// };
// pub fn is_anchor(arg_xpp: [*c]const xpparam_t, arg_line: [*c]const u8) callconv(.C) c_int {
//     var xpp = arg_xpp;
//     _ = &xpp;
//     var line = arg_line;
//     _ = &line;
//     var i: c_int = undefined;
//     _ = &i;
//     {
//         i = 0;
//         while (@as(usize, @bitCast(@as(c_long, i))) < xpp.*.anchors_nr) : (i += 1) {
//             if (!(strncmp(line, (blk: {
//                 const tmp = i;
//                 if (tmp >= 0) break :blk xpp.*.anchors + @as(usize, @intCast(tmp)) else break :blk xpp.*.anchors - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//             }).*, strlen((blk: {
//                 const tmp = i;
//                 if (tmp >= 0) break :blk xpp.*.anchors + @as(usize, @intCast(tmp)) else break :blk xpp.*.anchors - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//             }).*)) != 0)) return 1;
//         }
//     }
//     return 0;
// }
// pub fn insert_record(arg_xpp: [*c]const xpparam_t, arg_line: c_int, arg_map: [*c]struct_hashmap, arg_pass: c_int) callconv(.C) void {
//     var xpp = arg_xpp;
//     _ = &xpp;
//     var line = arg_line;
//     _ = &line;
//     var map = arg_map;
//     _ = &map;
//     var pass = arg_pass;
//     _ = &pass;
//     var records: [*c][*c]xrecord_t = if (pass == @as(c_int, 1)) map.*.env.*.xdf1.recs else map.*.env.*.xdf2.recs;
//     _ = &records;
//     var record: [*c]xrecord_t = (blk: {
//         const tmp = line - @as(c_int, 1);
//         if (tmp >= 0) break :blk records + @as(usize, @intCast(tmp)) else break :blk records - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//     }).*;
//     _ = &record;
//     var index_1: c_int = @as(c_int, @bitCast(@as(c_uint, @truncate((record.*.ha << @intCast(1)) % @as(c_ulong, @bitCast(@as(c_long, map.*.alloc)))))));
//     _ = &index_1;
//     while ((blk: {
//         const tmp = index_1;
//         if (tmp >= 0) break :blk map.*.entries + @as(usize, @intCast(tmp)) else break :blk map.*.entries - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//     }).*.line1 != 0) {
//         if ((blk: {
//             const tmp = index_1;
//             if (tmp >= 0) break :blk map.*.entries + @as(usize, @intCast(tmp)) else break :blk map.*.entries - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//         }).*.hash != record.*.ha) {
//             if ((blk: {
//                 const ref = &index_1;
//                 ref.* += 1;
//                 break :blk ref.*;
//             }) >= map.*.alloc) {
//                 index_1 = 0;
//             }
//             continue;
//         }
//         if (pass == @as(c_int, 2)) {
//             map.*.has_matches = 1;
//         }
//         if ((pass == @as(c_int, 1)) or ((blk: {
//             const tmp = index_1;
//             if (tmp >= 0) break :blk map.*.entries + @as(usize, @intCast(tmp)) else break :blk map.*.entries - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//         }).*.line2 != 0)) {
//             (blk: {
//                 const tmp = index_1;
//                 if (tmp >= 0) break :blk map.*.entries + @as(usize, @intCast(tmp)) else break :blk map.*.entries - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//             }).*.line2 = (@as(c_ulong, @bitCast(@as(c_long, 9223372036854775807))) *% @as(c_ulong, 2)) +% @as(c_ulong, 1);
//         } else {
//             (blk: {
//                 const tmp = index_1;
//                 if (tmp >= 0) break :blk map.*.entries + @as(usize, @intCast(tmp)) else break :blk map.*.entries - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//             }).*.line2 = @as(c_ulong, @bitCast(@as(c_long, line)));
//         }
//         return;
//     }
//     if (pass == @as(c_int, 2)) return;
//     (blk: {
//         const tmp = index_1;
//         if (tmp >= 0) break :blk map.*.entries + @as(usize, @intCast(tmp)) else break :blk map.*.entries - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//     }).*.line1 = @as(c_ulong, @bitCast(@as(c_long, line)));
//     (blk: {
//         const tmp = index_1;
//         if (tmp >= 0) break :blk map.*.entries + @as(usize, @intCast(tmp)) else break :blk map.*.entries - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//     }).*.hash = record.*.ha;
//     (blk: {
//         const tmp = index_1;
//         if (tmp >= 0) break :blk map.*.entries + @as(usize, @intCast(tmp)) else break :blk map.*.entries - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//     }).*.anchor = @as(c_uint, @bitCast(is_anchor(xpp, (blk: {
//         const tmp = line - @as(c_int, 1);
//         if (tmp >= 0) break :blk map.*.env.*.xdf1.recs + @as(usize, @intCast(tmp)) else break :blk map.*.env.*.xdf1.recs - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//     }).*.*.ptr)));
//     if (!(map.*.first != null)) {
//         map.*.first = map.*.entries + @as(usize, @bitCast(@as(isize, @intCast(index_1))));
//     }
//     if (map.*.last != null) {
//         map.*.last.*.next = map.*.entries + @as(usize, @bitCast(@as(isize, @intCast(index_1))));
//         (blk: {
//             const tmp = index_1;
//             if (tmp >= 0) break :blk map.*.entries + @as(usize, @intCast(tmp)) else break :blk map.*.entries - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//         }).*.previous = map.*.last;
//     }
//     map.*.last = map.*.entries + @as(usize, @bitCast(@as(isize, @intCast(index_1))));
//     map.*.nr += 1;
// }
// pub fn fill_hashmap(arg_xpp: [*c]const xpparam_t, arg_env: [*c]xdfenv_t, arg_result: [*c]struct_hashmap, arg_line1: c_int, arg_count1: c_int, arg_line2: c_int, arg_count2: c_int) callconv(.C) c_int {
//     var xpp = arg_xpp;
//     _ = &xpp;
//     var env = arg_env;
//     _ = &env;
//     var result = arg_result;
//     _ = &result;
//     var line1 = arg_line1;
//     _ = &line1;
//     var count1 = arg_count1;
//     _ = &count1;
//     var line2 = arg_line2;
//     _ = &line2;
//     var count2 = arg_count2;
//     _ = &count2;
//     result.*.xpp = xpp;
//     result.*.env = env;
//     result.*.alloc = count1 * @as(c_int, 2);
//     if (!((blk: {
//         const tmp = @as(?*struct_entry_4, @ptrCast(calloc(@as(c_ulong, @bitCast(@as(c_long, result.*.alloc))), @sizeOf(struct_entry_4))));
//         result.*.entries = tmp;
//         break :blk tmp;
//     }) != null)) return -@as(c_int, 1);
//     while ((blk: {
//         const ref = &count1;
//         const tmp = ref.*;
//         ref.* -= 1;
//         break :blk tmp;
//     }) != 0) {
//         insert_record(xpp, blk: {
//             const ref = &line1;
//             const tmp = ref.*;
//             ref.* += 1;
//             break :blk tmp;
//         }, result, @as(c_int, 1));
//     }
//     while ((blk: {
//         const ref = &count2;
//         const tmp = ref.*;
//         ref.* -= 1;
//         break :blk tmp;
//     }) != 0) {
//         insert_record(xpp, blk: {
//             const ref = &line2;
//             const tmp = ref.*;
//             ref.* += 1;
//             break :blk tmp;
//         }, result, @as(c_int, 2));
//     }
//     return 0;
// }
// pub fn binary_search(arg_sequence: [*c]?*struct_entry_4, arg_longest: c_int, arg_entry: ?*struct_entry_4) callconv(.C) c_int {
//     var sequence = arg_sequence;
//     _ = &sequence;
//     var longest = arg_longest;
//     _ = &longest;
//     var entry = arg_entry;
//     _ = &entry;
//     var left: c_int = -@as(c_int, 1);
//     _ = &left;
//     var right: c_int = longest;
//     _ = &right;
//     while ((left + @as(c_int, 1)) < right) {
//         var middle: c_int = left + @divTrunc(right - left, @as(c_int, 2));
//         _ = &middle;
//         if ((blk: {
//             const tmp = middle;
//             if (tmp >= 0) break :blk sequence + @as(usize, @intCast(tmp)) else break :blk sequence - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//         }).*.*.line2 > entry.*.line2) {
//             right = middle;
//         } else {
//             left = middle;
//         }
//     }
//     return left;
// }
// pub fn find_longest_common_sequence(arg_map: [*c]struct_hashmap, arg_res: [*c]?*struct_entry_4) callconv(.C) c_int {
//     var map = arg_map;
//     _ = &map;
//     var res = arg_res;
//     _ = &res;
//     var sequence: [*c]?*struct_entry_4 = undefined;
//     _ = &sequence;
//     var longest: c_int = 0;
//     _ = &longest;
//     var i: c_int = undefined;
//     _ = &i;
//     var entry: ?*struct_entry_4 = undefined;
//     _ = &entry;
//     var anchor_i: c_int = -@as(c_int, 1);
//     _ = &anchor_i;
//     if (!((blk: {
//         const tmp = @as([*c]?*struct_entry_4, @ptrCast(@alignCast(if ((@as(c_ulong, 18446744073709551615) / @sizeOf(?*struct_entry_4)) >= @as(usize, @bitCast(@as(c_long, map.*.nr)))) malloc(@as(c_ulong, @bitCast(@as(c_long, map.*.nr))) *% @sizeOf(?*struct_entry_4)) else @as(?*anyopaque, @ptrFromInt(@as(c_int, 0))))));
//         sequence = tmp;
//         break :blk tmp;
//     }) != null)) return -@as(c_int, 1);
//     {
//         entry = map.*.first;
//         while (entry != null) : (entry = entry.*.next) {
//             if (!(entry.*.line2 != 0) or (entry.*.line2 == ((@as(c_ulong, @bitCast(@as(c_long, 9223372036854775807))) *% @as(c_ulong, 2)) +% @as(c_ulong, 1)))) continue;
//             i = binary_search(sequence, longest, entry);
//             entry.*.previous = if (i < @as(c_int, 0)) null else (blk: {
//                 const tmp = i;
//                 if (tmp >= 0) break :blk sequence + @as(usize, @intCast(tmp)) else break :blk sequence - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//             }).*;
//             i += 1;
//             if (i <= anchor_i) continue;
//             (blk: {
//                 const tmp = i;
//                 if (tmp >= 0) break :blk sequence + @as(usize, @intCast(tmp)) else break :blk sequence - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//             }).* = entry;
//             if (entry.*.anchor != 0) {
//                 anchor_i = i;
//                 longest = anchor_i + @as(c_int, 1);
//             } else if (i == longest) {
//                 longest += 1;
//             }
//         }
//     }
//     if (!(longest != 0)) {
//         res.* = null;
//         free(@as(?*anyopaque, @ptrCast(sequence)));
//         return 0;
//     }
//     entry = (blk: {
//         const tmp = longest - @as(c_int, 1);
//         if (tmp >= 0) break :blk sequence + @as(usize, @intCast(tmp)) else break :blk sequence - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//     }).*;
//     entry.*.next = null;
//     while (entry.*.previous != null) {
//         entry.*.previous.*.next = entry;
//         entry = entry.*.previous;
//     }
//     res.* = entry;
//     free(@as(?*anyopaque, @ptrCast(sequence)));
//     return 0;
// }
// pub fn match(arg_map: [*c]struct_hashmap, arg_line1: c_int, arg_line2: c_int) callconv(.C) c_int {
//     var map = arg_map;
//     _ = &map;
//     var line1 = arg_line1;
//     _ = &line1;
//     var line2 = arg_line2;
//     _ = &line2;
//     var record1: [*c]xrecord_t = (blk: {
//         const tmp = line1 - @as(c_int, 1);
//         if (tmp >= 0) break :blk map.*.env.*.xdf1.recs + @as(usize, @intCast(tmp)) else break :blk map.*.env.*.xdf1.recs - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//     }).*;
//     _ = &record1;
//     var record2: [*c]xrecord_t = (blk: {
//         const tmp = line2 - @as(c_int, 1);
//         if (tmp >= 0) break :blk map.*.env.*.xdf2.recs + @as(usize, @intCast(tmp)) else break :blk map.*.env.*.xdf2.recs - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//     }).*;
//     _ = &record2;
//     return @intFromBool(record1.*.ha == record2.*.ha);
// }
// pub fn patience_diff(arg_xpp: [*c]const xpparam_t, arg_env: [*c]xdfenv_t, arg_line1: c_int, arg_count1: c_int, arg_line2: c_int, arg_count2: c_int) callconv(.C) c_int {
//     var xpp = arg_xpp;
//     _ = &xpp;
//     var env = arg_env;
//     _ = &env;
//     var line1 = arg_line1;
//     _ = &line1;
//     var count1 = arg_count1;
//     _ = &count1;
//     var line2 = arg_line2;
//     _ = &line2;
//     var count2 = arg_count2;
//     _ = &count2;
//     var map: struct_hashmap = undefined;
//     _ = &map;
//     var first: ?*struct_entry_4 = undefined;
//     _ = &first;
//     var result: c_int = 0;
//     _ = &result;
//     if (!(count1 != 0)) {
//         while ((blk: {
//             const ref = &count2;
//             const tmp = ref.*;
//             ref.* -= 1;
//             break :blk tmp;
//         }) != 0) {
//             (blk: {
//                 const tmp = (blk_1: {
//                     const ref = &line2;
//                     const tmp_2 = ref.*;
//                     ref.* += 1;
//                     break :blk_1 tmp_2;
//                 }) - @as(c_int, 1);
//                 if (tmp >= 0) break :blk env.*.xdf2.rchg + @as(usize, @intCast(tmp)) else break :blk env.*.xdf2.rchg - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//             }).* = 1;
//         }
//         return 0;
//     } else if (!(count2 != 0)) {
//         while ((blk: {
//             const ref = &count1;
//             const tmp = ref.*;
//             ref.* -= 1;
//             break :blk tmp;
//         }) != 0) {
//             (blk: {
//                 const tmp = (blk_1: {
//                     const ref = &line1;
//                     const tmp_2 = ref.*;
//                     ref.* += 1;
//                     break :blk_1 tmp_2;
//                 }) - @as(c_int, 1);
//                 if (tmp >= 0) break :blk env.*.xdf1.rchg + @as(usize, @intCast(tmp)) else break :blk env.*.xdf1.rchg - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//             }).* = 1;
//         }
//         return 0;
//     }
//     _ = __builtin___memset_chk(@as(?*anyopaque, @ptrCast(&map)), @as(c_int, 0), @sizeOf(struct_hashmap), __builtin_object_size(@as(?*const anyopaque, @ptrCast(&map)), @as(c_int, 0)));
//     if (fill_hashmap(xpp, env, &map, line1, count1, line2, count2) != 0) return -@as(c_int, 1);
//     if (!(map.has_matches != 0)) {
//         while ((blk: {
//             const ref = &count1;
//             const tmp = ref.*;
//             ref.* -= 1;
//             break :blk tmp;
//         }) != 0) {
//             (blk: {
//                 const tmp = (blk_1: {
//                     const ref = &line1;
//                     const tmp_2 = ref.*;
//                     ref.* += 1;
//                     break :blk_1 tmp_2;
//                 }) - @as(c_int, 1);
//                 if (tmp >= 0) break :blk env.*.xdf1.rchg + @as(usize, @intCast(tmp)) else break :blk env.*.xdf1.rchg - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//             }).* = 1;
//         }
//         while ((blk: {
//             const ref = &count2;
//             const tmp = ref.*;
//             ref.* -= 1;
//             break :blk tmp;
//         }) != 0) {
//             (blk: {
//                 const tmp = (blk_1: {
//                     const ref = &line2;
//                     const tmp_2 = ref.*;
//                     ref.* += 1;
//                     break :blk_1 tmp_2;
//                 }) - @as(c_int, 1);
//                 if (tmp >= 0) break :blk env.*.xdf2.rchg + @as(usize, @intCast(tmp)) else break :blk env.*.xdf2.rchg - ~@as(usize, @bitCast(@as(isize, @intCast(tmp)) +% -1));
//             }).* = 1;
//         }
//         free(@as(?*anyopaque, @ptrCast(map.entries)));
//         return 0;
//     }
//     result = find_longest_common_sequence(&map, &first);
//     if (result != 0) {
//         free(@as(?*anyopaque, @ptrCast(map.entries)));
//         return result;
//     }
//     if (first != null) {
//         result = walk_common_sequence(&map, first, line1, count1, line2, count2);
//     } else {
//         result = fall_back_to_classic_diff(&map, line1, count1, line2, count2);
//     }
//     free(@as(?*anyopaque, @ptrCast(map.entries)));
//     return result;
// }
// pub fn walk_common_sequence(arg_map: [*c]struct_hashmap, arg_first: ?*struct_entry_4, arg_line1: c_int, arg_count1: c_int, arg_line2: c_int, arg_count2: c_int) callconv(.C) c_int {
//     var map = arg_map;
//     _ = &map;
//     var first = arg_first;
//     _ = &first;
//     var line1 = arg_line1;
//     _ = &line1;
//     var count1 = arg_count1;
//     _ = &count1;
//     var line2 = arg_line2;
//     _ = &line2;
//     var count2 = arg_count2;
//     _ = &count2;
//     var end1: c_int = line1 + count1;
//     _ = &end1;
//     var end2: c_int = line2 + count2;
//     _ = &end2;
//     var next1: c_int = undefined;
//     _ = &next1;
//     var next2: c_int = undefined;
//     _ = &next2;
//     while (true) {
//         if (first != null) {
//             next1 = @as(c_int, @bitCast(@as(c_uint, @truncate(first.*.line1))));
//             next2 = @as(c_int, @bitCast(@as(c_uint, @truncate(first.*.line2))));
//             while (((next1 > line1) and (next2 > line2)) and (match(map, next1 - @as(c_int, 1), next2 - @as(c_int, 1)) != 0)) {
//                 next1 -= 1;
//                 next2 -= 1;
//             }
//         } else {
//             next1 = end1;
//             next2 = end2;
//         }
//         while (((line1 < next1) and (line2 < next2)) and (match(map, line1, line2) != 0)) {
//             line1 += 1;
//             line2 += 1;
//         }
//         if ((next1 > line1) or (next2 > line2)) {
//             if (patience_diff(map.*.xpp, map.*.env, line1, next1 - line1, line2, next2 - line2) != 0) return -@as(c_int, 1);
//         }
//         if (!(first != null)) return 0;
//         while (((first.*.next != null) and (first.*.next.*.line1 == (first.*.line1 +% @as(c_ulong, @bitCast(@as(c_long, @as(c_int, 1))))))) and (first.*.next.*.line2 == (first.*.line2 +% @as(c_ulong, @bitCast(@as(c_long, @as(c_int, 1))))))) {
//             first = first.*.next;
//         }
//         line1 = @as(c_int, @bitCast(@as(c_uint, @truncate(first.*.line1 +% @as(c_ulong, @bitCast(@as(c_long, @as(c_int, 1))))))));
//         line2 = @as(c_int, @bitCast(@as(c_uint, @truncate(first.*.line2 +% @as(c_ulong, @bitCast(@as(c_long, @as(c_int, 1))))))));
//         first = first.*.next;
//     }
//     return 0;
// }
// pub fn fall_back_to_classic_diff(arg_map: [*c]struct_hashmap, arg_line1: c_int, arg_count1: c_int, arg_line2: c_int, arg_count2: c_int) callconv(.C) c_int {
//     var map = arg_map;
//     _ = &map;
//     var line1 = arg_line1;
//     _ = &line1;
//     var count1 = arg_count1;
//     _ = &count1;
//     var line2 = arg_line2;
//     _ = &line2;
//     var count2 = arg_count2;
//     _ = &count2;
//     var xpp: xpparam_t = undefined;
//     _ = &xpp;
//     _ = __builtin___memset_chk(@as(?*anyopaque, @ptrCast(&xpp)), @as(c_int, 0), @sizeOf(xpparam_t), __builtin_object_size(@as(?*const anyopaque, @ptrCast(&xpp)), @as(c_int, 0)));
//     xpp.flags = map.*.xpp.*.flags & @as(c_ulong, @bitCast(@as(c_long, ~((@as(c_int, 1) << @intCast(14)) | (@as(c_int, 1) << @intCast(15))))));
//     return xdl_fall_back_diff(map.*.env, &xpp, line1, count1, line2, count2);
// }

const std = @import("std");
const Array = std.ArrayList;
const Record = struct {
    slice: []const u8,
    pub fn match(self: Record, other: Record) bool {
        return self.slice == other.slice;
    }
};

const File = struct {
    contents: []const u8,
    records: []Record,
    allocator: std.mem.Allocator,
    pub fn init(allocator: std.mem.Allocator, contents: []const u8) !File {
        var records = Array(Record).init(allocator);
        var last_i: usize = 0;
        for (contents, 0..) |byte, i| {
            if (byte == '\n') {
                try records.append(.{ .slice = contents[last_i..i] });
                last_i = i + 1;
            }
        }
        try records.append(.{ .slice = contents[last_i..] });
        return .{
            .contents = contents,
            .records = try records.toOwnedSlice(),
            .allocator = allocator,
        };
    }
    pub fn deinit(self: *File) void {
        self.allocator.free(self.records);
    }
    pub fn linesMatch(self: *File, line1: usize, line2: usize) bool {
        return self.records[line1 - 1].match(self.records[line2 - 1]);
    }
};
const Env = struct {
    file_1: File,
    file_2: File,
};
pub const HashMap = struct {
    // entries: Array(Entry),
    entries: std.StringArrayHashMap(Entry),
    first: *Entry = null,
    last: *Entry = null,
    has_matches: bool = false,
    env: *Env,
    allocator: std.mem.Allocator,
    pub const Entry = struct {
        hash: u64,
        line1: usize,
        line2: usize,
        next: ?*Entry,
        previous: ?*Entry,
        anchor: bool,
    };
    pub fn init(
        allocator: std.mem.Allocator,
        env: *Env,
        line1: usize,
        count1: usize,
        line2: usize,
        count2: usize,
    ) !HashMap {
        var map = HashMap{
            .entries = Array(Entry).init(allocator),
            .env = env,
            .allocator = allocator,
        };
        try map.fill(
            env,
            line1,
            count1,
            line2,
            count2,
        );
        return map;
    }
    pub fn insertRecord(self: *HashMap, file: *File, line1: usize, line2: usize) !void {
        _ = file; // autofix
        _ = self; // autofix
        _ = line1; // autofix
        _ = line2; // autofix
    }
    pub fn fill(
        self: *HashMap,
        env: *Env,
        line1: usize,
        count1: usize,
        line2: usize,
        count2: usize,
    ) !void {
        try self.entries.ensureUnusedCapacity(count1 * 2);
        _ = env; // autofix
        _ = line1; // autofix
        _ = line2; // autofix
        _ = count2; // autofix
    }
};

pub const Patience = struct {
    env: Env,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, source_1: []const u8, source_2: []const u8) !Patience {
        return .{
            .map = undefined,
            .env = .{
                .file_1 = try File.init(allocator, source_1),
                .file_2 = try File.init(allocator, source_2),
            },
            .allocator = allocator,
        };
    }
    pub fn diff(
        self: *Patience,
        line1: usize,
        count1: usize,
        line2: usize,
        count2: usize,
    ) !void {
        // var map = HashMap{};
        // _ = map; // autofix
        _ = self; // autofix
        _ = line1; // autofix
        _ = count1; // autofix
        _ = line2; // autofix
        _ = count2; // autofix
    }
};
test "File" {
    const allocator = std.testing.allocator;
    var file = try File.init(allocator, "hello\nworld\n");
    defer file.deinit();

    std.debug.print("{any}\n", .{file.records});
    try std.testing.expectEqualStrings(file.records[0].slice, "hello");
    try std.testing.expectEqualStrings(file.records[1].slice, "world");
    try std.testing.expectEqual(file.records.len, 3);
}

test "Patience" {
    const allocator = std.testing.allocator;
    var patience = try Patience.init(allocator,
        \\Hello
        \\World!
        \\I'm
        \\Julia
    ,
        \\Hello
        \\Universe!
        \\My 
        \\name
        \\is
        \\Julia
    );
    defer patience.deinit();
}
