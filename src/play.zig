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
    var a: usize = 1;
    var tmp = a;
    const b: *usize = &tmp;
    a = 5;
    
    std.debug.print("a {d} \n", .{a});
    std.debug.print("b {d} \n", .{b.*});
}
