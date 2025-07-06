const parser = @import("parse.zig");
const Expr = @import("parse.zig").Expr;
const Statement = @import("parse.zig").Statement;
const scan = @import("scan.zig");
const std = @import("std");
const TokenType = @import("scan.zig").TokenType;
const Environment = @import("environment.zig").Environment;
const Token = @import("scan.zig").Token;
const Interpreter = @import("interpreter.zig").Intepreter;

pub const EvalResult = union(enum) {
    string: []const u8,
    number: f64,
    boolean: bool,
    nil,
    native_fn: NativeFunction,
    user_fn: UserFunction,
};

pub const NativeFunction = *const fn (args: []const EvalResult) EvalError!EvalResult;
pub const UserFunction = struct {
    closure: Environment,
    body_block: Statement.BlockStatement,
    params: []Token,

    pub fn call(self: *UserFunction, errorLine: *usize, interpreter: *Interpreter, args: []const Expr) EvalError!EvalResult {
        var block_env = try Environment.init(interpreter.alloc, &self.closure);
        if (args.len != self.params.len) {
            return EvalError.WrongArgsCount;
        }

        for (args, 0..) |arg, i| {
            const param_token = self.params[i];
            const evaluatedArg = try evaluate(&arg, errorLine, interpreter);
            try block_env.define(param_token.lexeme, evaluatedArg);
        }
        return interpreter.runBlock(self.body_block, block_env);
    }
};

pub const EvalError = error{
    AllocationError,
    Invalid,
    NotANumber,
    UndefinedVar,
    OutOfMemory,
    WrongArgsCount,
};

pub fn evaluate(expr: *const parser.Expr, errorLine: *usize, interpreter: *Interpreter) EvalError!EvalResult {
    switch (expr.*) {
        .literal => |literal| return try evaluateLiteral(literal),
        .grouping => |grouping| return try evaluateGrouping(grouping, errorLine, interpreter),
        .unary => |unary| return try evaluateUnary(unary, errorLine, interpreter),
        .binary => |binary| return try evaluateBinary(binary, errorLine, interpreter),
        .identifier => |identifier| return try evaluateIdenfifier(identifier, errorLine, interpreter),
        .assign => |assign| return try evaluateAssign(assign, errorLine, interpreter),
        .call => |call| return try evaluateCall(call, errorLine, interpreter),
    }
}

fn evaluateCall(expr: Expr.CallExpr, errorLine: *usize, interpreter: *Interpreter) EvalError!EvalResult {
    const callee = try evaluate(expr.callee, errorLine, interpreter);
    switch (callee) {
        .native_fn => {
            const function = callee.native_fn;
            var evaluatedArgs: [250]EvalResult = undefined;
            for (expr.args, 0..) |arg, i| {
                evaluatedArgs[i] = try evaluate(&arg, errorLine, interpreter);
            }
            return function(&evaluatedArgs);
        },
        .user_fn => {
            var function = callee.user_fn;
            return function.call(errorLine, interpreter, expr.args);
        },
        else => {
            return EvalError.Invalid;
        },
    }
}

fn evaluateAssign(expr: Expr.AssignExpr, errorLine: *usize, interpreter: *Interpreter) EvalError!EvalResult {
    const res = try evaluate(expr.left, errorLine, interpreter);
    interpreter.environment.assign(expr.name.lexeme, res) catch {
        return EvalError.AllocationError;
    };

    return res;
}

fn evaluateIdenfifier(expr: Expr.Identifier, errorLine: *usize, interpreter: *Interpreter) EvalError!EvalResult {
    const name = expr.token.lexeme;
    return interpreter.environment.get(name) orelse {
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

fn evaluateGrouping(grouping: Expr.GroupingExpr, errorLine: *usize, interpreter: *Interpreter) EvalError!EvalResult {
    return evaluate(grouping.expression, errorLine, interpreter);
}

fn evaluateUnary(unary: Expr.UnaryExpr, errorLine: *usize, interpreter: *Interpreter) EvalError!EvalResult {
    const right = try evaluate(unary.right, errorLine, interpreter);

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

fn evaluateBinary(binary: Expr.BinaryExpr, errorLine: *usize, interpreter: *Interpreter) EvalError!EvalResult {
    const left = try evaluate(binary.left, errorLine, interpreter);
    switch (binary.operator.tokenType) {
        .OR => {
            if (isTruthy(left)) return left;
            return try evaluate(binary.right, errorLine, interpreter);
        },
        .AND => {
            if (!isTruthy(left)) return left;
            return try evaluate(binary.right, errorLine, interpreter);
        },
        else => {},
    }

    const right = try evaluate(binary.right, errorLine, interpreter);

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
        .user_fn => return true,
    }
}
