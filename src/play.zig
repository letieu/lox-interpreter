const std = @import("std");

const StA = struct {
    fn sayHi(self: *const StA) !void {
        std.debug.print("Xin chao {} \n", .{self});
    }
};

const StB = struct {
    fn sayHi(self: *const StB) !void {
        std.debug.print("Hello {} \n", .{self});
    }
};

fn doHi(comptime T: type, person: T) !void {
    try person.sayHi();
}

pub fn main() !void {
    const a = StA{};
    const b = StB{};

    try doHi(StA, a);
    try doHi(StB, b);
}
