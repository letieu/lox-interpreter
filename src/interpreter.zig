const Statement = @import("parse.zig").Statement;

const PrintStatement = @import("parse.zig").PrintStatement;
const ExpressionStatement = @import("parse.zig").ExpressionStatement;
const VarStatement = @import("parse.zig").VarStatement;

const std = @import("std");
const evaluate = @import("evaluate.zig").evaluate;
const EvalResult = @import("evaluate.zig").EvalResult;
const EvalError = @import("evaluate.zig").EvalError;

pub const Intepreter = struct {
    statements: []const Statement,
    alloc: std.mem.Allocator,
    stdOut: std.fs.File,
    stdErr: std.fs.File,

    environment: std.StringHashMap(EvalResult),

    pub fn init(statements: []const Statement, alloc: std.mem.Allocator, stdOut: std.fs.File, stdErr: std.fs.File) Intepreter {
        return Intepreter{
            .statements = statements,
            .alloc = alloc,
            .stdOut = stdOut,
            .stdErr = stdErr,
            .environment = std.StringHashMap(EvalResult).init(alloc),
        };
    }

    pub fn run(self: *Intepreter) !void {
        for (self.statements) |stmt| {
            switch (stmt) {
                .Print => |printStmt| try self.execPrint(printStmt),
                .Var => |varDeclaration| try self.execVarDec(varDeclaration),
                .Expression => |exprStmt| try self.execExpr(exprStmt),
            }
        }
    }

    fn printEvalError(self: *Intepreter, e: EvalError, errorLine: *const usize) !void {
        switch (e) {
            error.AllocationError => try self.stdErr.writer().print("Allocation Error.\n", .{}),
            error.NotANumber => try self.stdErr.writer().print("Operand must be a number.\n", .{}),
            error.Invalid => try self.stdErr.writer().print("Invalid.\n", .{}),
            error.UndefinedVar => try self.stdErr.writer().print("Undefined var.\n", .{}),
        }
        try self.stdErr.writer().print("[line {d}]", .{errorLine.*});
    }

    fn execPrint(self: *Intepreter, stmt: PrintStatement) !void {
        var errorLine: usize = 0;
        const result = evaluate(&stmt.expr, &errorLine, @TypeOf(self.environment), &self.environment) catch |e| {
            try self.printEvalError(e, &errorLine);
            std.process.exit(70);
            return;
        };

        switch (result) {
            .boolean => try std.io.getStdOut().writer().print("{?}", .{result.boolean}),
            .number => try std.io.getStdOut().writer().print("{d}", .{result.number}),
            .string => try std.io.getStdOut().writer().print("{s}", .{result.string}),
            .nil => try std.io.getStdOut().writer().print("nil", .{}),
        }

        try std.io.getStdOut().writer().print("\n", .{});
    }

    fn execVarDec(self: *Intepreter, stmt: VarStatement) !void {
        var errorLine: usize = 0;
        const initializer = stmt.initializer;
        if (initializer == null) {
            try self.environment.put(stmt.name, EvalResult.nil);
            return;
        }

        const result = evaluate(&initializer.?, &errorLine, @TypeOf(self.environment), &self.environment) catch |e| {
            try self.printEvalError(e, &errorLine);
            std.process.exit(70);
            return;
        };

        try self.environment.put(stmt.name, result);
    }

    fn execExpr(self: *Intepreter, stmt: ExpressionStatement) !void {
        var errorLine: usize = 0;
        _ = evaluate(&stmt.expr, &errorLine, @TypeOf(self.environment), &self.environment) catch |e| {
            try self.printEvalError(e, &errorLine);
            std.process.exit(70);
            return;
        };
    }
};
