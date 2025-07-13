const std = @import("std");

const StrA = struct {
    map: std.StringHashMap(u8),

    pub fn init(alloc: std.mem.Allocator) !StrA {
        return StrA{
            .map = std.StringHashMap(u8).init(alloc),
        };
    }

    pub fn set(self: *StrA, key: []const u8, value: u8) !void {
        return self.map.put(key, value);
    }

    pub fn get(self: *const StrA, key: []const u8) ?u8 {
        return self.map.get(key);
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    var map1 = std.StringHashMap(u8).init(alloc);
    var map2 = map1;

    try map1.put("name", 2);
    std.debug.print("{d} \n", .{map1.get("name").?});
    std.debug.print("{d} \n", .{map2.get("name").?});
}
