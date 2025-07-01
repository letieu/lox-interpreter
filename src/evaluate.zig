const parser = @import("parse.zig");
const scan = @import("scan.zig");
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
    NotANumber,
};

pub fn evaluate(expr: *const parser.Expr, errorLine: *usize) EvalError!EvalResult {
    switch (expr.*) {
        .Literal => |literal| return try evaluateLiteral(literal),
        .Grouping => |grouping| return try evaluateGrouping(grouping, errorLine),
        .Unary => |unary| return try evaluateUnary(unary, errorLine),
        .Binary => |binary| return try evaluateBinary(binary, errorLine),
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

fn evaluateGrouping(grouping: parser.GroupingExpr, errorLine: *usize) EvalError!EvalResult {
    return evaluate(grouping.expression, errorLine);
}

fn evaluateUnary(unary: parser.UnaryExpr, errorLine: *usize) EvalError!EvalResult {
    const right = try evaluate(unary.right, errorLine);

    if (unary.operator.tokenType == TokenType.MINUS) {
        switch (right) {
            .number => return EvalResult{ .number = -right.number },
            else => {
                errorLine.* = unary.operator.line;
                return EvalError.NotANumber;
            },
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

fn evaluateBinary(binary: parser.BinaryExpr, errorLine: *usize) EvalError!EvalResult {
    const left = try evaluate(binary.left, errorLine);
    const right = try evaluate(binary.right, errorLine);

    switch (binary.operator.tokenType) {
        .PLUS => {
            if (left == .string and right == .string) {
                const allocator = std.heap.page_allocator;
                const str = std.mem.concat(allocator, u8, &.{ left.string, right.string }) catch {
                    return EvalError.AllocationError;
                };
                return EvalResult{ .string = str };
            }

            try validateNumberOperand(left, right, errorLine, binary.operator);
            return EvalResult{ .number = left.number + right.number };
        },
        .MINUS => {
            try validateNumberOperand(left, right, errorLine, binary.operator);
            return EvalResult{ .number = left.number - right.number };
        },
        .STAR => {
            try validateNumberOperand(left, right, errorLine, binary.operator);
            return EvalResult{ .number = left.number * right.number };
        },
        .SLASH => {
            try validateNumberOperand(left, right, errorLine, binary.operator);
            return EvalResult{ .number = left.number / right.number };
        },
        .GREATER => {
            try validateNumberOperand(left, right, errorLine, binary.operator);
            return EvalResult{ .boolean = left.number > right.number };
        },
        .GREATER_EQUAL => {
            try validateNumberOperand(left, right, errorLine, binary.operator);
            return EvalResult{ .boolean = left.number >= right.number };
        },
        .LESS => {
            try validateNumberOperand(left, right, errorLine, binary.operator);
            return EvalResult{ .boolean = left.number < right.number };
        },
        .LESS_EQUAL => {
            try validateNumberOperand(left, right, errorLine, binary.operator);
            return EvalResult{ .boolean = left.number <= right.number };
        },
        .EQUAL_EQUAL => {
            if (left == .string and right == .string) {
                return EvalResult{ .boolean = std.mem.eql(u8, left.string, right.string) };
            }

            validateNumberOperand(left, right, errorLine, binary.operator) catch {
                return EvalResult{ .boolean = false };
            };
            return EvalResult{ .boolean = left.number == right.number };
        },
        .BANG_EQUAL => {
            if (left == .string and right == .string) {
                return EvalResult{ .boolean = !std.mem.eql(u8, left.string, right.string) };
            }

            validateNumberOperand(left, right, errorLine, binary.operator) catch {
                return EvalResult{ .boolean = true };
            };
            return EvalResult{ .boolean = left.number != right.number };
        },
        else => return EvalError.Invalid,
    }

    return EvalError.Invalid;
}

fn validateNumberOperand(left: EvalResult, right: EvalResult, errorLine: *usize, token: scan.Token) EvalError!void {
    if (left != .number or right != .number) {
        errorLine.* = token.line;
        return EvalError.NotANumber;
    }
}
