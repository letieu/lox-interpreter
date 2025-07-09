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
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();
    var map = std.StringHashMap(usize).init(alloc);

    var map2 = &map;

    try map2.put("test", 3);

    std.debug.print("{?}", .{map.get("test")});
}
