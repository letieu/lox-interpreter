const parser = @import("parse.zig");
const Expr = @import("parse.zig").Expr;
const scan = @import("scan.zig");
const std = @import("std");
const TokenType = @import("scan.zig").TokenType;

pub const EvalResult = union(enum) {
    string: []const u8,
    number: f64,
    boolean: bool,
    nil,
    native_fn: NativeFunction,
};

pub const NativeFunction = *const fn (args: []const EvalResult) EvalError!EvalResult;

pub const EvalError = error{
    AllocationError,
    Invalid,
    NotANumber,
    UndefinedVar,
    OutOfMemory,
};

pub fn evaluate(expr: *const parser.Expr, errorLine: *usize, comptime envType: type, env: *envType) EvalError!EvalResult {
    switch (expr.*) {
        .literal => |literal| return try evaluateLiteral(literal),
        .grouping => |grouping| return try evaluateGrouping(grouping, errorLine, envType, env),
        .unary => |unary| return try evaluateUnary(unary, errorLine, envType, env),
        .binary => |binary| return try evaluateBinary(binary, errorLine, envType, env),
        .identifier => |identifier| return try evaluateIdenfifier(identifier, errorLine, envType, env),
        .assign => |assign| return try evaluateAssign(assign, errorLine, envType, env),
        .call => |call| return try evaluateCall(call, errorLine, envType, env),
    }
}

fn evaluateCall(expr: Expr.CallExpr, errorLine: *usize, comptime envType: type, env: *envType) EvalError!EvalResult {
    const callee = try evaluate(expr.callee, errorLine, envType, env);
    const function = callee.native_fn;
    var evaluatedArgs: [250]EvalResult = undefined;
    for (expr.args, 0..) |arg, i| {
        evaluatedArgs[i] = try evaluate(&arg, errorLine, envType, env);
    }
    return function(&evaluatedArgs);
}

fn evaluateAssign(expr: Expr.AssignExpr, errorLine: *usize, comptime envType: type, env: *envType) EvalError!EvalResult {
    const res = try evaluate(expr.left, errorLine, envType, env);
    env.assign(expr.name.lexeme, res) catch {
        return EvalError.AllocationError;
    };

    return res;
}

fn evaluateIdenfifier(expr: Expr.Identifier, errorLine: *usize, comptime envType: type, env: *envType) EvalError!EvalResult {
    const name = expr.token.lexeme;
    return env.get(name) orelse {
        errorLine.* = expr.token.line;
        return EvalError.UndefinedVar;
    };
}

fn evaluateLiteral(expr: Expr.LiteralExpr) EvalError!EvalResult {
    switch (expr) {
        .TRUE => return EvalResult{ .boolean = true },
        .FALSE => return EvalResult{ .boolean = false },
        .NIL => return EvalResult.nil,
        .STRING => return EvalResult{ .string = expr.STRING },
        .NUMBER => return EvalResult{ .number = expr.NUMBER },
    }
}

fn evaluateGrouping(grouping: Expr.GroupingExpr, errorLine: *usize, comptime envType: type, env: *envType) EvalError!EvalResult {
    return evaluate(grouping.expression, errorLine, envType, env);
}

fn evaluateUnary(unary: Expr.UnaryExpr, errorLine: *usize, comptime envType: type, env: *envType) EvalError!EvalResult {
    const right = try evaluate(unary.right, errorLine, envType, env);

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

fn evaluateBinary(binary: Expr.BinaryExpr, errorLine: *usize, comptime envType: type, env: *envType) EvalError!EvalResult {
    const left = try evaluate(binary.left, errorLine, envType, env);
    switch (binary.operator.tokenType) {
        .OR => {
            if (isTruthy(left)) return left;
            return try evaluate(binary.right, errorLine, envType, env);
        },
        .AND => {
            if (!isTruthy(left)) return left;
            return try evaluate(binary.right, errorLine, envType, env);
        },
        else => {},
    }

    const right = try evaluate(binary.right, errorLine, envType, env);

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

            if (left == .boolean and right == .boolean) {
                return EvalResult{ .boolean = left.boolean == right.boolean };
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

            if (left == .boolean and right == .boolean) {
                return EvalResult{ .boolean = left.boolean != right.boolean };
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

pub fn isTruthy(value: EvalResult) bool {
    switch (value) {
        .boolean => return value.boolean,
        .number => return value.number != 0,
        .string => return true,
        .nil => return false,
        .native_fn => return true,
    }
}
