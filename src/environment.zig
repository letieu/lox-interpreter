const std = @import("std");
const EvaluateResult = @import("evaluate.zig").EvalResult;

pub const Environment = struct {
    values: std.StringHashMap(EvaluateResult),
    alloc: std.mem.Allocator,
    enclosing: ?*Environment,

    pub fn init(alloc: std.mem.Allocator, enclosing: ?*Environment) !Environment {
        return Environment{
            .values = std.StringHashMap(EvaluateResult).init(alloc),
            .alloc = alloc,
            .enclosing = enclosing,
        };
    }

    pub fn assign(self: *Environment, key: []const u8, value: EvaluateResult) !void {
        if (self.enclosing == null) return try self.values.put(key, value);

        _ = self.values.get(key) orelse {
            try self.enclosing.?.assign(key, value);
            return;
        };

        try self.values.put(key, value);
    }

    pub fn define(self: *Environment, key: []const u8, value: EvaluateResult) !void {
        try self.values.put(key, value);
    }

    pub fn get(self: *Environment, key: []const u8) ?EvaluateResult {
        const value = self.values.get(key);
        if (value != null) return value;
        if (self.enclosing == null) return null;

        return self.enclosing.?.get(key);
    }
};
