const parser = @import("parse.zig");
const std = @import("std");
const TokenType = @import("scan.zig").TokenType;

const EvalResultType = enum {
    string,
    number,
    boolean,
    nil,
};

const EvalResult = union(EvalResultType) {
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
        .Binary => |binary| return try evaluateBinary(binary),
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

fn evaluateBinary(binary: parser.BinaryExpr) EvalError!EvalResult {
    const left = try evaluate(binary.left);
    const right = try evaluate(binary.right);

    switch (binary.operator.tokenType) {
        .PLUS => {
            if (left == .number and right == .number) {
                return EvalResult{ .number = left.number + right.number };
            }
            if (left == .string and right == .string) {
                const allocator = std.heap.page_allocator;
                const str = std.mem.concat(allocator, u8, &.{ left.string, right.string }) catch {
                    return EvalError.AllocationError;
                };
                return EvalResult{ .string = str };
            }
            return EvalError.Invalid;
        },
        .MINUS => return EvalResult{ .number = left.number - right.number },
        .STAR => return EvalResult{ .number = left.number * right.number },
        .SLASH => return EvalResult{ .number = left.number / right.number },
        .GREATER => return EvalResult{ .boolean = left.number > right.number },
        .GREATER_EQUAL => return EvalResult{ .boolean = left.number >= right.number },
        .LESS => return EvalResult{ .boolean = left.number < right.number },
        .LESS_EQUAL => return EvalResult{ .boolean = left.number <= right.number },
        .EQUAL_EQUAL => {
            if (left == .number and right == .number) {
                return EvalResult{ .boolean = left.number == right.number };
            }
            if (left == .string and right == .string) {
                return EvalResult{ .boolean = std.mem.eql(u8, left.string, right.string) };
            }

            return EvalResult{ .boolean = false };
        },
        else => return EvalError.Invalid,
    }

    return EvalError.Invalid;
}
