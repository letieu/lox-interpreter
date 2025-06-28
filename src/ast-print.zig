const parser = @import("parse.zig");
const std = @import("std");

pub fn printExpr(expr: *const parser.Expr) void {
    switch (expr.*) {
        .Binary => |group| printBinary(group),
        .Unary => |group| printUnary(group),
        .Literal => |literal| printLiteral(literal),
        .Grouping => |group| printGrouping(group),
    }
}

pub fn printGrouping(expr: parser.GroupingExpr) void {
    std.debug.print("( group ", .{});
    printExpr(expr.expression);
    std.debug.print(" )", .{});
}

pub fn printLiteral(expr: parser.LiteralExpr) void {
    switch (expr) {
        .FALSE => std.debug.print("FALSE", .{}),
        .TRUE => std.debug.print("TRUE", .{}),
        .NIL => std.debug.print("NIL", .{}),
        .NUMBER => |num| std.debug.print("{d}", .{num}),
        .STRING => |str| std.debug.print("{s}", .{str}),
    }
}

pub fn printBinary(expr: parser.BinaryExpr) void {
    std.debug.print("Binary\n", .{});

    std.debug.print("Left:\n", .{});
    printExpr(expr.left);

    std.debug.print("Operator: {s}\n", .{expr.operator.lexeme});

    std.debug.print("Right:\n", .{});
    printExpr(expr.right);
}

pub fn printUnary(expr: parser.UnaryExpr) void {
    std.debug.print("Unary\n", .{});

    std.debug.print("Operator: {s}\n", .{expr.operator.lexeme});

    std.debug.print("Right:\n", .{});
    printExpr(expr.right);
}
