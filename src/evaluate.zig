const parser = @import("parse.zig");
const std = @import("std");

pub fn evaluate(expr: *const parser.Expr) []const u8 {
    switch (expr.*) {
        .Literal => |literal| return evaluateLiteral(literal),
        else => {
            return &[_]u8{};
        },
    }
}

fn evaluateLiteral(expr: parser.LiteralExpr) []const u8 {
    switch (expr) {
        .TRUE => return "true",
        .FALSE => return "false",
        .NIL => return "nil",
        .STRING => return expr.STRING,
        .NUMBER => {
            return std.fmt.allocPrint(std.heap.page_allocator, "{d}", .{expr.NUMBER}) catch "0.0";
        },
    }
}
