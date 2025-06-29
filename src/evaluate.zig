const parser = @import("parse.zig");
const std = @import("std");
const TokenType = @import("scan.zig").TokenType;

const EvalResult = union(enum) {
    string: []const u8,
    number: f64,
    boolean: bool,
    nil,
};

pub const EvalError = error{
    AllocationError,
    Invalid,
};

pub fn evaluate(expr: *const parser.Expr) EvalError!EvalResult {
    switch (expr.*) {
        .Literal => |literal| return try evaluateLiteral(literal),
        .Grouping => |grouping| return try evaluateGrouping(grouping),
        .Unary => |unary| return try evaluateUnary(unary),
        else => return EvalResult.nil,
    }
}

fn evaluateLiteral(expr: parser.LiteralExpr) EvalError!EvalResult {
    switch (expr) {
        .TRUE => return EvalResult{ .boolean = true },
        .FALSE => return EvalResult{ .boolean = false },
        .NIL => return EvalResult.nil,
        .STRING => return EvalResult{ .string = expr.STRING },
        .NUMBER => return EvalResult{ .number = expr.NUMBER },
    }
}

fn evaluateGrouping(grouping: parser.GroupingExpr) EvalError!EvalResult {
    return evaluate(grouping.expression);
}

fn evaluateUnary(unary: parser.UnaryExpr) EvalError!EvalResult {
    const right = try evaluate(unary.right);

    if (unary.operator.tokenType == TokenType.MINUS) {
        switch (right) {
            .number => return EvalResult{ .number = -right.number },
            else => return EvalError.Invalid,
        }
    }

    if (unary.operator.tokenType == TokenType.BANG) {
        switch (right) {
            .boolean => return EvalResult{ .boolean = !right.boolean },
            .number => return EvalResult{ .boolean = false },
            .nil => return EvalResult{ .boolean = true },
            else => return EvalError.Invalid,
        }
    }

    return EvalError.Invalid;
}
