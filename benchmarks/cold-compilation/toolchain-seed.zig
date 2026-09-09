// Seed only Zig's toolchain support, never a measured fixture's application code.
pub fn main() void {
    var value: u32 = 7;
    value *= 3;
    if (value != 21) @trap();
}
