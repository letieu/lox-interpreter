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
    class: *Class,
    instance: *Instance,
};

pub const Instance = struct {
    class: *const Class,
    fields: std.StringHashMap(EvalResult),

    pub fn init(class: *const Class, alloc: std.mem.Allocator) !Instance {
        return Instance{ .class = class, .fields = std.StringHashMap(EvalResult).init(alloc) };
    }

    pub fn get(self: *Instance, name: []const u8) !EvalResult {
        return self.fields.get(name) orelse {
            std.debug.print("Null on get {s}", .{name});
            return EvalResult.nil;
        };
    }

    pub fn set(self: *Instance, name: []const u8, value: EvalResult) !void {
        return self.fields.put(name, value);
    }
};

pub const Class = struct {
    name: []const u8,
    alloc: std.mem.Allocator,

    pub fn init(name: []const u8, alloc: std.mem.Allocator) Class {
        return Class{
            .name = name,
            .alloc = alloc,
        };
    }

    pub fn call(self: *const Class) EvalError!EvalResult {
        const instance_ptr = try self.alloc.create(Instance);
        instance_ptr.* = try Instance.init(self, self.alloc);
        return EvalResult{ .instance = instance_ptr };
    }
};

pub const NativeFunction = *const fn (args: []const EvalResult) EvalError!EvalResult;
pub const UserFunction = struct {
    closure: *Environment,
    body_block: Statement.BlockStatement,
    params: []Token,

    pub fn call(self: *UserFunction, errorLine: *usize, interpreter: *Interpreter, args: []*Expr) EvalError!EvalResult {
        var block_env = try Environment.init(interpreter.alloc, self.closure);

        if (args.len != self.params.len) {
            return EvalError.WrongArgsCount;
        }

        for (args, 0..) |arg, i| {
            const param_token = self.params[i];
            const evaluatedArg = try evaluate(arg, errorLine, interpreter);
            try block_env.define(param_token.lexeme, evaluatedArg);
        }
        return interpreter.runBlock(&self.body_block, block_env);
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

pub fn evaluate(expr: *Expr, errorLine: *usize, interpreter: *Interpreter) EvalError!EvalResult {
    switch (expr.*) {
        .literal => |literal| return try evaluateLiteral(literal),
        .grouping => |grouping| return try evaluateGrouping(grouping.*, errorLine, interpreter),
        .unary => |unary| return try evaluateUnary(unary.*, errorLine, interpreter),
        .binary => |binary| return try evaluateBinary(binary.*, errorLine, interpreter),
        .identifier => |identifier| return try evaluateIdenfifier(expr, &identifier, errorLine, interpreter),
        .assign => |assign| return try evaluateAssign(expr, assign.*, errorLine, interpreter),
        .call => |call| return try evaluateCall(call.*, errorLine, interpreter),
        .get => |get| return try evaluateGet(get.*, errorLine, interpreter),
        .set => |set| return try evaluateSet(set.*, errorLine, interpreter),
    }
}

fn evaluateSet(expr: Expr.SetExpr, errorLine: *usize, interpreter: *Interpreter) EvalError!EvalResult {
    var object = try evaluate(expr.object, errorLine, interpreter);
    switch (object) {
        .instance => {
            const value = try evaluate(expr.value, errorLine, interpreter);
            try object.instance.set(expr.name.lexeme, value);
            return EvalResult.nil;
        },
        else => {
            errorLine.* = expr.name.line;
            return EvalError.Invalid;
        },
    }
    return object;
}

fn evaluateGet(expr: Expr.GetExpr, errorLine: *usize, interpreter: *Interpreter) EvalError!EvalResult {
    var object = try evaluate(expr.object, errorLine, interpreter);
    switch (object) {
        .instance => {
            return object.instance.get(expr.name.lexeme);
        },
        else => {
            errorLine.* = expr.name.line;
            return EvalError.Invalid;
        },
    }
    return object;
}

fn evaluateCall(expr: Expr.CallExpr, errorLine: *usize, interpreter: *Interpreter) EvalError!EvalResult {
    const callee = try evaluate(expr.callee, errorLine, interpreter);
    switch (callee) {
        .native_fn => {
            const function = callee.native_fn;
            var evaluatedArgs: [250]EvalResult = undefined;
            for (expr.args, 0..) |arg, i| {
                evaluatedArgs[i] = try evaluate(arg, errorLine, interpreter);
            }
            return function(&evaluatedArgs);
        },
        .user_fn => {
            var function = callee.user_fn;
            return function.call(errorLine, interpreter, expr.args);
        },
        .class => |class| {
            return class.call();
        },
        else => {
            return EvalError.Invalid;
        },
    }
}

fn evaluateAssign(expr_ptr: *Expr, expr: Expr.AssignExpr, errorLine: *usize, interpreter: *Interpreter) EvalError!EvalResult {
    const value = try evaluate(expr.left, errorLine, interpreter);
    const distance = interpreter.id_distance.get(expr_ptr) orelse @panic("var not found");

    interpreter.environment.assignAt(distance, expr.name.lexeme, value);
    return value;
}

fn evaluateIdenfifier(expr_ptr: *Expr, expr: *const Expr.Identifier, errorLine: *usize, interpreter: *Interpreter) EvalError!EvalResult {
    const distance = interpreter.id_distance.get(expr_ptr) orelse {
        std.debug.print("Eval Line 95 \n", .{});
        return EvalError.UndefinedVar;
    };

    const name = expr.token.lexeme;
    return interpreter.environment.getAt(distance, name) orelse {
        errorLine.* = expr.token.line;
        std.debug.print("Eval Line 102 \n", .{});
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
        .class => return true,
        .instance => return true,
    }
}
