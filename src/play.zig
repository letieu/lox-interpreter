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
    var sl: [3]usize = undefined;
    sl[1] = 3;
    std.debug.print("{d}", .{sl.len});
}
