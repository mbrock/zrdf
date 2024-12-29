const std = @import("std");
const testing = std.testing;
const url_parser = @import("url_parser");

export fn add(a: i32, b: i32) i32 {
    return a + b;
}

test "basic add functionality" {
    try testing.expect(add(3, 7) == 10);
}

comptime {
    std.testing.refAllDecls(@import("url_parser"));
}
