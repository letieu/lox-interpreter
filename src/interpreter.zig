const Statement = @import("parse.zig").Statement;
const Expr = @import("parse.zig").Expr;
const Declaration = @import("parse.zig").Declaration;
const VarDecl = @import("parse.zig").VarDecl;
const FunctionDecl = @import("parse.zig").FunctionDecl;
const Environment = @import("environment.zig").Environment;

const std = @import("std");
const evaluate = @import("evaluate.zig").evaluate;
const EvalResult = @import("evaluate.zig").EvalResult;
const EvalError = @import("evaluate.zig").EvalError;
const isTruthy = @import("evaluate.zig").isTruthy;

fn nativeClock(_: []const EvalResult) EvalError!EvalResult {
    const now_ns = std.time.timestamp();
    return EvalResult{ .number = @floatFromInt(now_ns) };
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
            try self.execDecl(decl);
        }
    }

    fn execDecl(self: *Intepreter, decl: Declaration) EvalError!void {
        switch (decl) {
            .var_decl => |var_declaration| try self.execVarDecl(var_declaration),
            .function_decl => |fun_decl| try self.execFunDecl(fun_decl),
            .stmt => |exprStmt| {
                _ = try self.execStmt(exprStmt);
            },
        }
    }

    fn execStmt(self: *Intepreter, stmt: Statement) EvalError!void {
        switch (stmt) {
            .block => |blockStmt| try self.execBlock(blockStmt),
            .print => |printStmt| try self.execPrint(printStmt),
            .ifStmt => |ifStmt| try self.execIfStmt(ifStmt),
            .while_stmt => |while_stmt| try self.execWhileStmt(while_stmt),
            .for_stmt => |for_stmt| try self.execForStmt(for_stmt),
            .expression => |expr_stmt| {
                _ = try self.execExpr(expr_stmt.expr);
            },
        }
    }

    fn execBlock(self: *Intepreter, block: Statement.BlockStatement) !void {
        var prevEnv = self.environment;
        const blockEnv = try Environment.init(self.alloc, &prevEnv);
        self.environment = blockEnv;

        for (block.declarations) |decl| {
            _ = try self.execDecl(decl);
        }

        self.environment = prevEnv;
    }

    fn printEvalError(self: *Intepreter, e: EvalError, errorLine: *const usize) !void {
        switch (e) {
            error.AllocationError => self.printErr("Allocation Error.\n", .{}),
            error.NotANumber => self.printErr("Operand must be a number.\n", .{}),
            error.Invalid => self.printErr("Invalid.\n", .{}),
            error.UndefinedVar => self.printErr("Undefined var.\n", .{}),
            error.OutOfMemory => self.printErr("OutOfMemory.\n", .{}),
        }
        self.printErr("[line {d}]", .{errorLine.*});
    }

    fn execForStmt(self: *Intepreter, stmt: Statement.ForStatement) !void {
        if (stmt.initial != null) {
            try self.execDecl(stmt.initial.?.*);
        }
        while (isTruthy(try self.execExpr(stmt.condition))) {
            try self.execStmt(stmt.body.*);
            if (stmt.increment != null) {
                _ = try self.execExpr(stmt.increment.?);
            }
        }
    }

    fn execWhileStmt(self: *Intepreter, stmt: Statement.WhileStatement) !void {
        while (isTruthy(try self.execExpr(stmt.condition))) {
            try self.execStmt(stmt.inner.*);
        }
    }

    fn execIfStmt(self: *Intepreter, stmt: Statement.IfStatement) !void {
        const conditional_result = try self.execExpr(stmt.condition);

        if (isTruthy(conditional_result)) {
            try self.execStmt(stmt.inner.*);
        } else {
            if (stmt.elseStmt != null) {
                try self.execStmt(stmt.elseStmt.?.*);
            }
        }
    }

    fn execPrint(self: *Intepreter, stmt: Statement.PrintStatement) !void {
        const result = try self.execExpr(stmt.expr);

        switch (result) {
            .native_fn => self.printOut("hihi", .{}),
            .boolean => self.printOut("{?}", .{result.boolean}),
            .number => self.printOut("{d}", .{result.number}),
            .string => self.printOut("{s}", .{result.string}),
            .nil => self.printOut("nil", .{}),
        }

        self.printOut("\n", .{});
    }

    fn execFunDecl(self: *Intepreter, decl: FunctionDecl) !void {
        _ = self; // autofix
        _ = decl; // autofix
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
        return evaluate(&expr, &errorLine, @TypeOf(self.environment), &self.environment) catch |e| {
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
