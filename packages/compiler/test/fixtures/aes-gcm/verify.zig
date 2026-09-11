const std = @import("std");
comptime {
    @setEvalBranchQuota(1000000);
    {
        const m = [_]u8{};
        const ad = [_]u8{};
        var c: [0]u8 = undefined;
        var tag: [16]u8 = undefined;
        std.crypto.aead.aes_gcm.Aes128Gcm.encrypt(&c, &tag, &m, &ad, .{ 60, 129, 157, 154, 155, 237, 8, 118, 21, 3, 11, 101 }, .{ 17, 117, 76, 215, 42, 236, 48, 155, 245, 47, 118, 135, 33, 46, 137, 87 });
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(c, .lower), "") or !std.mem.eql(u8, &std.fmt.bytesToHex(tag, .lower), "250327c674aaf477aef2675748cf6971")) @compileError("vector 0 mismatch");
    }
    {
        const m = [_]u8{ 195, 179, 196, 31, 17, 58, 49, 183, 61, 154, 92, 212, 50, 16, 48, 105 };
        const ad = [_]u8{ 36, 130, 86, 2, 189, 18, 169, 132, 224, 9, 45, 62, 68, 142, 218, 95 };
        var c: [16]u8 = undefined;
        var tag: [16]u8 = undefined;
        std.crypto.aead.aes_gcm.Aes128Gcm.encrypt(&c, &tag, &m, &ad, .{ 179, 216, 204, 1, 124, 187, 137, 179, 158, 15, 103, 226 }, .{ 201, 57, 204, 19, 57, 124, 29, 55, 222, 106, 224, 225, 203, 124, 66, 60 });
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(c, .lower), "93fe7d9e9bfd10348a5606e5cafa7354") or !std.mem.eql(u8, &std.fmt.bytesToHex(tag, .lower), "0032a1dc85f1c9786925a2e71d8272dd")) @compileError("vector 1 mismatch");
    }
    {
        const m = [_]u8{ 245, 75, 195, 80, 31, 237, 79, 111, 109, 251, 94, 168, 1, 6, 223, 11, 216, 54, 230, 130, 98, 37, 183, 92, 2, 34, 246, 232, 89, 179, 89, 131 };
        const ad = [_]u8{};
        var c: [32]u8 = undefined;
        var tag: [16]u8 = undefined;
        std.crypto.aead.aes_gcm.Aes128Gcm.encrypt(&c, &tag, &m, &ad, .{ 7, 169, 169, 94, 163, 130, 30, 156, 19, 198, 50, 81 }, .{ 153, 113, 7, 16, 89, 171, 192, 9, 228, 242, 189, 105, 134, 157, 179, 56 });
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(c, .lower), "0556c159f84ef36cb1602b4526b12009c775611bffb64dc0d9ca9297cd2c6a01") or !std.mem.eql(u8, &std.fmt.bytesToHex(tag, .lower), "7870d9117f54811a346970f1de090c41")) @compileError("vector 2 mismatch");
    }
    {
        const m = [_]u8{};
        const ad = [_]u8{};
        var c: [0]u8 = undefined;
        var tag: [16]u8 = undefined;
        std.crypto.aead.aes_gcm.Aes256Gcm.encrypt(&c, &tag, &m, &ad, .{ 81, 108, 51, 146, 157, 245, 163, 40, 79, 244, 99, 215 }, .{ 181, 44, 80, 90, 55, 215, 142, 218, 93, 211, 79, 32, 194, 37, 64, 234, 27, 88, 150, 60, 248, 229, 191, 143, 250, 133, 249, 242, 73, 37, 5, 180 });
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(c, .lower), "") or !std.mem.eql(u8, &std.fmt.bytesToHex(tag, .lower), "bdc1ac884d332457a1d2664f168c76f0")) @compileError("vector 3 mismatch");
    }
    {
        const m = [_]u8{ 45, 113, 188, 250, 145, 78, 74, 192, 69, 178, 170, 96, 149, 95, 173, 36 };
        const ad = [_]u8{ 30, 8, 137, 1, 111, 103, 96, 28, 142, 190, 164, 148, 59, 194, 58, 214 };
        var c: [16]u8 = undefined;
        var tag: [16]u8 = undefined;
        std.crypto.aead.aes_gcm.Aes256Gcm.encrypt(&c, &tag, &m, &ad, .{ 172, 147, 161, 166, 20, 82, 153, 189, 233, 2, 242, 26 }, .{ 146, 225, 29, 205, 170, 134, 111, 92, 231, 144, 253, 36, 80, 31, 146, 80, 154, 172, 244, 203, 139, 19, 57, 213, 12, 156, 18, 64, 147, 93, 208, 139 });
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(c, .lower), "8995ae2e6df3dbf96fac7b7137bae67f") or !std.mem.eql(u8, &std.fmt.bytesToHex(tag, .lower), "eca5aa77d51d4a0a14d9c51e1da474ab")) @compileError("vector 4 mismatch");
    }
    {
        const m = [_]u8{ 254, 41, 164, 13, 142, 191, 87, 38, 43, 219, 135, 25, 29, 1, 132, 63, 76, 164, 178, 222, 151, 216, 130, 115, 21, 74, 11, 125, 158, 47, 219, 128 };
        const ad = [_]u8{};
        var c: [32]u8 = undefined;
        var tag: [16]u8 = undefined;
        std.crypto.aead.aes_gcm.Aes256Gcm.encrypt(&c, &tag, &m, &ad, .{ 158, 217, 216, 24, 5, 100, 224, 233, 69, 245, 229, 212 }, .{ 38, 142, 209, 181, 215, 201, 199, 48, 79, 156, 174, 95, 196, 55, 180, 205, 58, 235, 226, 236, 101, 240, 216, 92, 57, 24, 211, 211, 181, 187, 168, 155 });
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(c, .lower), "791a4a026f16f3a5ea06274bf02baab469860abde5e645f3dd473a5acddeecfc") or !std.mem.eql(u8, &std.fmt.bytesToHex(tag, .lower), "05b2b74db0662550435ef1900e136b15")) @compileError("vector 5 mismatch");
    }
    {
        const m = [_]u8{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 };
        const ad = [_]u8{ 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46 };
        var c: [15]u8 = undefined;
        var tag: [16]u8 = undefined;
        std.crypto.aead.aes_gcm.Aes128Gcm.encrypt(&c, &tag, &m, &ad, .{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 }, .{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 });
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(c, .lower), "936da5cd621ef15343db6b813aae7e") or !std.mem.eql(u8, &std.fmt.bytesToHex(tag, .lower), "9d286e1de80dcfcff6da4dc7e58c2f49")) @compileError("vector 6 mismatch");
    }
    {
        const m = [_]u8{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
        const ad = [_]u8{ 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48 };
        var c: [17]u8 = undefined;
        var tag: [16]u8 = undefined;
        std.crypto.aead.aes_gcm.Aes128Gcm.encrypt(&c, &tag, &m, &ad, .{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 }, .{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 });
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(c, .lower), "936da5cd621ef15343db6b813aae7e07a3") or !std.mem.eql(u8, &std.fmt.bytesToHex(tag, .lower), "8e7125ccce70e14362069747f5cae80c")) @compileError("vector 7 mismatch");
    }
    {
        const m = [_]u8{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 };
        const ad = [_]u8{ 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46 };
        var c: [15]u8 = undefined;
        var tag: [16]u8 = undefined;
        std.crypto.aead.aes_gcm.Aes256Gcm.encrypt(&c, &tag, &m, &ad, .{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 }, .{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 });
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(c, .lower), "4703d418c1e0c41c85489d80bde476") or !std.mem.eql(u8, &std.fmt.bytesToHex(tag, .lower), "699e5c7db35b495e9afc781a88f77a3d")) @compileError("vector 8 mismatch");
    }
    {
        const m = [_]u8{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
        const ad = [_]u8{ 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48 };
        var c: [17]u8 = undefined;
        var tag: [16]u8 = undefined;
        std.crypto.aead.aes_gcm.Aes256Gcm.encrypt(&c, &tag, &m, &ad, .{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 }, .{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 });
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(c, .lower), "4703d418c1e0c41c85489d80bde4766293") or !std.mem.eql(u8, &std.fmt.bytesToHex(tag, .lower), "7e20bf8a37f30c57948b02e3cce92a77")) @compileError("vector 9 mismatch");
    }
    {
        const m = [_]u8{};
        const ad = [_]u8{ 122, 67, 236, 29, 156, 10, 90, 120, 160, 177, 101, 51, 166, 33, 60, 171 };
        var c: [0]u8 = undefined;
        var tag: [16]u8 = undefined;
        std.crypto.aead.aes_gcm.Aes128Gcm.encrypt(&c, &tag, &m, &ad, .{ 224, 224, 15, 25, 254, 215, 186, 1, 54, 167, 151, 243 }, .{ 119, 190, 99, 112, 137, 113, 196, 226, 64, 209, 203, 121, 232, 215, 127, 235 });
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(c, .lower), "") or !std.mem.eql(u8, &std.fmt.bytesToHex(tag, .lower), "209fcc8d3675ed938e9c7166709dd946")) @compileError("vector 10 mismatch");
    }
    {
        const m = [_]u8{};
        const ad = [_]u8{ 185, 107, 170, 140, 28, 117, 166, 113, 191, 178, 208, 141, 6, 190, 95, 54 };
        var c: [0]u8 = undefined;
        var tag: [16]u8 = undefined;
        std.crypto.aead.aes_gcm.Aes256Gcm.encrypt(&c, &tag, &m, &ad, .{ 215, 156, 242, 45, 80, 76, 199, 147, 195, 251, 108, 138 }, .{ 120, 220, 78, 10, 175, 82, 217, 53, 195, 192, 30, 234, 87, 66, 143, 0, 202, 31, 212, 117, 245, 218, 134, 164, 156, 141, 215, 61, 104, 200, 226, 35 });
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(c, .lower), "") or !std.mem.eql(u8, &std.fmt.bytesToHex(tag, .lower), "3e5d486aa2e30b22e040b85723a06e76")) @compileError("vector 11 mismatch");
    }
}
