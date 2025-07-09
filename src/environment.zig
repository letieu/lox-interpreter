const std = @import("std");
const EvaluateResult = @import("evaluate.zig").EvalResult;

pub const Environment = struct {
    values: std.StringHashMap(EvaluateResult),
    alloc: std.mem.Allocator,
    enclosing: ?*Environment,

    pub fn init(alloc: std.mem.Allocator, enclosing: ?*Environment) !*Environment {
        const self = try alloc.create(Environment);
        self.* = .{
            .values = std.StringHashMap(EvaluateResult).init(alloc),
            .alloc = alloc,
            .enclosing = enclosing,
        };

        return self;
    }

    pub fn assign(self: *Environment, key: []const u8, value: EvaluateResult) !void {
        if (self.values.get(key) != null) {
            try self.values.put(key, value);
            return;
        }

        if (self.enclosing != null) {
            try self.enclosing.?.assign(key, value);
            return;
        }
    }

    pub fn define(self: *Environment, key: []const u8, value: EvaluateResult) !void {
        try self.values.put(key, value);
    }

    pub fn get(self: *Environment, key: []const u8) ?EvaluateResult {
        return self.values.get(key);
    }

    pub fn getAt(self: *Environment, distance: usize, key: []const u8) ?EvaluateResult {
        return self.ancestor(distance).get(key);
    }

    pub fn assignAt(self: *Environment, distance: usize, key: []const u8, value: EvaluateResult) void {
        _ = self.ancestor(distance).values.put(key, value) catch @panic("Error assign at");
    }

    fn ancestor(self: *Environment, distance: usize) *Environment {
        var env = self;
        var i: usize = 0;
        while (i < distance) : (i += 1) {
            if (env.enclosing != null) {
                env = env.enclosing.?;
            }
        }

        return env;
    }
};
