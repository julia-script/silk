const T = struct {
    a: i32,
    b: i32,
};

pub fn main() !void {
    const t = T{
        .a = 1,
        .b = 2,
    };
    _ = t; // autofix

}
