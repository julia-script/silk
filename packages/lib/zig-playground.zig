fn foo() !void {
    return error.SomeError;
}
fn bar() !void {
    try foo();
}
pub fn main() !void {
    bar() catch {
        return error.CaughtError;
    };
}
