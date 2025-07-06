const Statement = @import("parse.zig").Statement;
const Expr = @import("parse.zig").Expr;
const Declaration = @import("parse.zig").Declaration;
const VarDecl = @import("parse.zig").VarDecl;
const FunctionDecl = @import("parse.zig").FunctionDecl;
const Environment = @import("environment.zig").Environment;

const std = @import("std");
const evaluate = @import("evaluate.zig").evaluate;
const EvalResult = @import("evaluate.zig").EvalResult;
const UserFunction = @import("evaluate.zig").UserFunction;
const EvalError = @import("evaluate.zig").EvalError;
const isTruthy = @import("evaluate.zig").isTruthy;

const StmtResult = union(enum) {
    none,
    return_value: EvalResult,
};

fn nativeClock(_: []const EvalResult) EvalError!EvalResult {
    const now_ns = std.time.timestamp();
    return EvalResult{ .number = @floatFromInt(now_ns - 2) };
}

pub const Intepreter = struct {
    declarations: []const Declaration,
    alloc: std.mem.Allocator,
    stdOut: std.fs.File,
    stdErr: std.fs.File,

    environment: Environment,

    pub fn init(statements: []const Declaration, alloc: std.mem.Allocator, stdOut: std.fs.File, stdErr: std.fs.File) Intepreter {
        var interpreter = Intepreter{
            .declarations = statements,
            .alloc = alloc,
            .stdOut = stdOut,
            .stdErr = stdErr,
            .environment = Environment.init(alloc, null) catch {
                std.debug.print("Failed to init environment", .{});
            },
        };
        interpreter.initNativeFn() catch {
            std.debug.print("Failed to init environment var", .{});
        };
        return interpreter;
    }

    fn initNativeFn(self: *Intepreter) !void {
        try self.environment.define("clock", EvalResult{ .native_fn = nativeClock });
    }

    pub fn run(self: *Intepreter) !void {
        for (self.declarations) |decl| {
            _ = try self.execDecl(decl);
        }
    }

    pub fn runBlock(self: *Intepreter, block: Statement.BlockStatement, env: Environment) EvalError!EvalResult {
        const prevEnv = self.environment;
        self.environment = env;

        for (block.declarations) |decl| {
            const result = try self.execDecl(decl);
            if (result == .return_value) {
                self.environment = prevEnv;
                return result.return_value;
            }
        }

        self.environment = prevEnv;
        return EvalResult.nil;
    }

    fn execDecl(self: *Intepreter, decl: Declaration) EvalError!StmtResult {
        switch (decl) {
            .var_decl => |var_declaration| {
                try self.execVarDecl(var_declaration);
                return StmtResult.none;
            },
            .function_decl => |fun_decl| {
                try self.execFunDecl(fun_decl);
                return StmtResult.none;
            },
            .stmt => |exprStmt| {
                return try self.execStmt(exprStmt);
            },
        }
    }

    fn execStmt(self: *Intepreter, stmt: Statement) EvalError!StmtResult {
        switch (stmt) {
            .block => |blockStmt| {
                return self.execBlock(blockStmt);
            },
            .print => |printStmt| {
                try self.execPrint(printStmt);
                return .none;
            },
            .ifStmt => |ifStmt| {
                return self.execIfStmt(ifStmt);
            },
            .while_stmt => |while_stmt| {
                return self.execWhileStmt(while_stmt);
            },
            .for_stmt => |for_stmt| {
                return self.execForStmt(for_stmt);
            },
            .return_stmt => |return_stmt| {
                return try self.execReturnStmt(return_stmt);
            },
            .expression => |expr_stmt| {
                _ = try self.execExpr(expr_stmt.expr);
                return .none;
            },
        }
    }

    fn execBlock(self: *Intepreter, block: Statement.BlockStatement) !StmtResult {
        var prevEnv = self.environment;
        const blockEnv = try Environment.init(self.alloc, &prevEnv);
        self.environment = blockEnv;

        for (block.declarations) |decl| {
            const result = try self.execDecl(decl);
            if (result == .return_value) {
                self.environment = prevEnv;
                return result;
            }
        }

        self.environment = prevEnv;
        return .none;
    }

    fn printEvalError(self: *Intepreter, e: EvalError, errorLine: *const usize) !void {
        switch (e) {
            error.AllocationError => self.printErr("Allocation Error.\n", .{}),
            error.NotANumber => self.printErr("Operand must be a number.\n", .{}),
            error.Invalid => self.printErr("Invalid.\n", .{}),
            error.UndefinedVar => self.printErr("Undefined var.\n", .{}),
            error.OutOfMemory => self.printErr("OutOfMemory.\n", .{}),
            error.WrongArgsCount => self.printErr("Not enough args.\n", .{}),
        }
        self.printErr("[line {d}]", .{errorLine.*});
    }

    fn execReturnStmt(self: *Intepreter, stmt: Statement.ReturnStatement) !StmtResult {
        if (stmt.expr != null) {
            const result = try self.execExpr(stmt.expr.?);
            return StmtResult{ .return_value = result };
        }

        return StmtResult{ .return_value = EvalResult.nil };
    }

    fn execForStmt(self: *Intepreter, stmt: Statement.ForStatement) !StmtResult {
        if (stmt.initial != null) {
            _ = try self.execDecl(stmt.initial.?.*);
        }
        while (isTruthy(try self.execExpr(stmt.condition))) {
            const result = try self.execStmt(stmt.body.*);
            if (result == .return_value) return result;

            if (stmt.increment != null) {
                _ = try self.execExpr(stmt.increment.?);
            }
        }

        return .none;
    }

    fn execWhileStmt(self: *Intepreter, stmt: Statement.WhileStatement) !StmtResult {
        while (isTruthy(try self.execExpr(stmt.condition))) {
            const result = try self.execStmt(stmt.inner.*);
            if (result == .return_value) {
                return result;
            }
        }

        return .none;
    }

    fn execIfStmt(self: *Intepreter, stmt: Statement.IfStatement) !StmtResult {
        const conditional_result = try self.execExpr(stmt.condition);

        if (isTruthy(conditional_result)) {
            return try self.execStmt(stmt.inner.*);
        } else {
            if (stmt.elseStmt != null) {
                return try self.execStmt(stmt.elseStmt.?.*);
            }
        }

        return .none;
    }

    fn execPrint(self: *Intepreter, stmt: Statement.PrintStatement) !void {
        const result = try self.execExpr(stmt.expr);

        switch (result) {
            .native_fn => self.printOut("hihi", .{}),
            .user_fn => self.printOut("<fn {s}>", .{stmt.expr.identifier.token.lexeme}),
            .boolean => self.printOut("{?}", .{result.boolean}),
            .number => self.printOut("{d}", .{result.number}),
            .string => self.printOut("{s}", .{result.string}),
            .nil => self.printOut("nil", .{}),
        }

        self.printOut("\n", .{});
    }

    fn execFunDecl(self: *Intepreter, decl: FunctionDecl) !void {
        const user_fn = UserFunction{
            .closure = self.environment,
            .body_block = decl.function.body,
            .params = decl.function.params,
        };

        try self.environment.define(decl.function.name, EvalResult{ .user_fn = user_fn });
    }

    fn execVarDecl(self: *Intepreter, decl: VarDecl) !void {
        const initializer = decl.initializer;
        if (initializer == null) {
            try self.environment.define(decl.name, EvalResult.nil);
            return;
        }

        const result = try self.execExpr(initializer.?);
        try self.environment.define(decl.name, result);
    }

    fn execExpr(self: *Intepreter, expr: Expr) !EvalResult {
        var errorLine: usize = 0;
        return evaluate(&expr, &errorLine, self) catch |e| {
            self.printEvalError(e, &errorLine) catch {
                std.debug.print("print error", .{});
            };
            std.process.exit(70);
            return;
        };
    }

    fn printErr(self: *Intepreter, comptime format: []const u8, args: anytype) void {
        self.stdErr.writer().print(format, args) catch {
            std.debug.print("Print error", .{});
        };
    }

    fn printOut(self: *Intepreter, comptime format: []const u8, args: anytype) void {
        self.stdOut.writer().print(format, args) catch {
            std.debug.print("Print error", .{});
        };
    }
};
