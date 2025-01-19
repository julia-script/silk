const Self = @This();
arch: Arch,
vendor: Vendor,
os: Os,

pub const Arch = enum {
    wasm32,
    wasm64,
};
pub const Os = enum {
    unknown,
    wasi,
};

pub const Vendor = enum {
    unknown,
};

pub fn getPointerSize(self: Self) u8 {
    return switch (self.arch) {
        .wasm32 => 4,
        .wasm64 => 8,
    };
}
