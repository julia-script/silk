const std = @import("std");
comptime {
    @setEvalBranchQuota(100000000);
    {
        const result = std.crypto.dh.X25519.scalarmult(.{ 165, 70, 227, 107, 240, 82, 124, 157, 59, 22, 21, 75, 130, 70, 94, 221, 98, 20, 76, 10, 193, 252, 90, 24, 80, 106, 34, 68, 186, 68, 154, 196 }, .{ 230, 219, 104, 103, 88, 48, 48, 219, 53, 148, 193, 164, 36, 177, 95, 124, 114, 102, 36, 236, 38, 179, 53, 59, 16, 169, 3, 166, 208, 171, 28, 76 });
        const output = result catch @compileError("unexpected rejection");
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(output, .lower), "c3da55379de9c6908e94ea4df28d084f32eccf03491c71f754b4075577a28552")) @compileError("vector0 mismatch");
    }
    {
        const result = std.crypto.dh.X25519.scalarmult(.{ 75, 102, 233, 212, 209, 180, 103, 60, 90, 210, 38, 145, 149, 125, 106, 245, 193, 27, 100, 33, 224, 234, 1, 212, 44, 164, 22, 158, 121, 24, 186, 13 }, .{ 229, 33, 15, 18, 120, 104, 17, 211, 244, 183, 149, 157, 5, 56, 174, 44, 49, 219, 231, 16, 111, 192, 60, 62, 252, 76, 213, 73, 199, 21, 164, 147 });
        const output = result catch @compileError("unexpected rejection");
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(output, .lower), "95cbde9476e8907d7aade45cb4b873f88b595a68799fa152e6f8f7647aac7957")) @compileError("vector1 mismatch");
    }
    {
        const result = std.crypto.dh.X25519.scalarmult(.{ 119, 7, 109, 10, 115, 24, 165, 125, 60, 22, 193, 114, 81, 178, 102, 69, 223, 76, 47, 135, 235, 192, 153, 42, 177, 119, 251, 165, 29, 185, 44, 42 }, .{ 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 });
        const output = result catch @compileError("unexpected rejection");
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(output, .lower), "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a")) @compileError("vector2 mismatch");
    }
    {
        const result = std.crypto.dh.X25519.scalarmult(.{ 93, 171, 8, 126, 98, 74, 138, 75, 121, 225, 127, 139, 131, 128, 14, 230, 111, 59, 177, 41, 38, 24, 182, 253, 28, 47, 139, 39, 255, 136, 224, 235 }, .{ 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 });
        const output = result catch @compileError("unexpected rejection");
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(output, .lower), "de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f")) @compileError("vector3 mismatch");
    }
    {
        const result = std.crypto.dh.X25519.scalarmult(.{ 119, 7, 109, 10, 115, 24, 165, 125, 60, 22, 193, 114, 81, 178, 102, 69, 223, 76, 47, 135, 235, 192, 153, 42, 177, 119, 251, 165, 29, 185, 44, 42 }, .{ 222, 158, 219, 125, 123, 125, 193, 180, 211, 91, 97, 194, 236, 228, 53, 55, 63, 131, 67, 200, 91, 120, 103, 77, 173, 252, 126, 20, 111, 136, 43, 79 });
        const output = result catch @compileError("unexpected rejection");
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(output, .lower), "4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742")) @compileError("vector4 mismatch");
    }
    {
        const result = std.crypto.dh.X25519.scalarmult(.{ 93, 171, 8, 126, 98, 74, 138, 75, 121, 225, 127, 139, 131, 128, 14, 230, 111, 59, 177, 41, 38, 24, 182, 253, 28, 47, 139, 39, 255, 136, 224, 235 }, .{ 133, 32, 240, 9, 137, 48, 167, 84, 116, 139, 125, 220, 180, 62, 247, 90, 13, 191, 58, 13, 38, 56, 26, 244, 235, 164, 169, 142, 170, 155, 78, 106 });
        const output = result catch @compileError("unexpected rejection");
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(output, .lower), "4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742")) @compileError("vector5 mismatch");
    }
    {
        const result = std.crypto.dh.X25519.scalarmult(.{ 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }, .{ 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 });
        const output = result catch @compileError("unexpected rejection");
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(output, .lower), "422c8e7a6227d7bca1350b3e2bb7279f7897b87bb6854b783c60e80311ae3079")) @compileError("vector6 mismatch");
    }
    {
        const result = std.crypto.dh.X25519.scalarmult(.{ 119, 7, 109, 10, 115, 24, 165, 125, 60, 22, 193, 114, 81, 178, 102, 69, 223, 76, 47, 135, 235, 192, 153, 42, 177, 119, 251, 165, 29, 185, 44, 42 }, .{ 222, 158, 219, 125, 123, 125, 193, 180, 211, 91, 97, 194, 236, 228, 53, 55, 63, 131, 67, 200, 91, 120, 103, 77, 173, 252, 126, 20, 111, 136, 43, 207 });
        const output = result catch @compileError("unexpected rejection");
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(output, .lower), "4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742")) @compileError("vector7 mismatch");
    }
    {
        const result = std.crypto.dh.X25519.scalarmult(.{ 119, 7, 109, 10, 115, 24, 165, 125, 60, 22, 193, 114, 81, 178, 102, 69, 223, 76, 47, 135, 235, 192, 153, 42, 177, 119, 251, 165, 29, 185, 44, 42 }, .{ 246, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 127 });
        const output = result catch @compileError("unexpected rejection");
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(output, .lower), "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a")) @compileError("vector8 mismatch");
    }
    {
        const result = std.crypto.dh.X25519.scalarmult(.{ 119, 7, 109, 10, 115, 24, 165, 125, 60, 22, 193, 114, 81, 178, 102, 69, 223, 76, 47, 135, 235, 192, 153, 42, 177, 119, 251, 165, 29, 185, 44, 42 }, .{ 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 });
        const output = result catch @compileError("unexpected rejection");
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(output, .lower), "e80c0be9d3a1c5d71edd6316e8c9115ca35397cd47109bd38e32864f1adecf4d")) @compileError("vector9 mismatch");
    }
    {
        const result = std.crypto.dh.X25519.scalarmult(.{ 112, 7, 109, 10, 115, 24, 165, 125, 60, 22, 193, 114, 81, 178, 102, 69, 223, 76, 47, 135, 235, 192, 153, 42, 177, 119, 251, 165, 29, 185, 44, 234 }, .{ 9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 });
        const output = result catch @compileError("unexpected rejection");
        if (!std.mem.eql(u8, &std.fmt.bytesToHex(output, .lower), "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a")) @compileError("vector10 mismatch");
    }
    {
        const result = std.crypto.dh.X25519.scalarmult(.{ 119, 7, 109, 10, 115, 24, 165, 125, 60, 22, 193, 114, 81, 178, 102, 69, 223, 76, 47, 135, 235, 192, 153, 42, 177, 119, 251, 165, 29, 185, 44, 42 }, .{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 });
        if (result) |_| {
            @compileError("expected all-zero rejection");
        } else |err| {
            if (err != error.IdentityElement) @compileError("wrong failure");
        }
    }
    {
        const result = std.crypto.dh.X25519.scalarmult(.{ 119, 7, 109, 10, 115, 24, 165, 125, 60, 22, 193, 114, 81, 178, 102, 69, 223, 76, 47, 135, 235, 192, 153, 42, 177, 119, 251, 165, 29, 185, 44, 42 }, .{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 });
        if (result) |_| {
            @compileError("expected all-zero rejection");
        } else |err| {
            if (err != error.IdentityElement) @compileError("wrong failure");
        }
    }
    {
        const result = std.crypto.dh.X25519.scalarmult(.{ 119, 7, 109, 10, 115, 24, 165, 125, 60, 22, 193, 114, 81, 178, 102, 69, 223, 76, 47, 135, 235, 192, 153, 42, 177, 119, 251, 165, 29, 185, 44, 42 }, .{ 236, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 127 });
        if (result) |_| {
            @compileError("expected all-zero rejection");
        } else |err| {
            if (err != error.IdentityElement) @compileError("wrong failure");
        }
    }
    {
        const result = std.crypto.dh.X25519.scalarmult(.{ 119, 7, 109, 10, 115, 24, 165, 125, 60, 22, 193, 114, 81, 178, 102, 69, 223, 76, 47, 135, 235, 192, 153, 42, 177, 119, 251, 165, 29, 185, 44, 42 }, .{ 237, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 127 });
        if (result) |_| {
            @compileError("expected all-zero rejection");
        } else |err| {
            if (err != error.IdentityElement) @compileError("wrong failure");
        }
    }
    {
        const result = std.crypto.dh.X25519.scalarmult(.{ 119, 7, 109, 10, 115, 24, 165, 125, 60, 22, 193, 114, 81, 178, 102, 69, 223, 76, 47, 135, 235, 192, 153, 42, 177, 119, 251, 165, 29, 185, 44, 42 }, .{ 238, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 127 });
        if (result) |_| {
            @compileError("expected all-zero rejection");
        } else |err| {
            if (err != error.IdentityElement) @compileError("wrong failure");
        }
    }
}
