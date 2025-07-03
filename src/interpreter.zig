const Statement = @import("parse.zig").Statement;
const Expr = @import("parse.zig").Expr;
const Declaration = @import("parse.zig").Declaration;
const VarDecl = @import("parse.zig").VarDecl;

const std = @import("std");
const evaluate = @import("evaluate.zig").evaluate;
const EvalResult = @import("evaluate.zig").EvalResult;
const EvalError = @import("evaluate.zig").EvalError;

pub const Intepreter = struct {
    declarations: []const Declaration,
    alloc: std.mem.Allocator,
    stdOut: std.fs.File,
    stdErr: std.fs.File,

    environment: std.StringHashMap(EvalResult),

    pub fn init(statements: []const Declaration, alloc: std.mem.Allocator, stdOut: std.fs.File, stdErr: std.fs.File) Intepreter {
        return Intepreter{
            .declarations = statements,
            .alloc = alloc,
            .stdOut = stdOut,
            .stdErr = stdErr,
            .environment = std.StringHashMap(EvalResult).init(alloc),
        };
    }

    pub fn run(self: *Intepreter) !void {
        for (self.declarations) |decl| {
            try self.execDecl(decl);
        }
    }

    fn execDecl(self: *Intepreter, decl: Declaration) !void {
        switch (decl) {
            .var_decl => |varDeclaration| try self.execVarDecl(varDeclaration),
            .stmt => |exprStmt| {
                _ = try self.execStmt(exprStmt);
            },
        }
    }

    fn execStmt(self: *Intepreter, stmt: Statement) !void {
        switch (stmt) {
            .print => |printStmt| try self.execPrint(printStmt),
            .expression => |exprStmt| {
                _ = try self.execExpr(exprStmt.expr);
            },
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

    fn execPrint(self: *Intepreter, stmt: Statement.PrintStatement) !void {
        const result = try self.execExpr(stmt.expr);

        switch (result) {
            .boolean => try std.io.getStdOut().writer().print("{?}", .{result.boolean}),
            .number => try std.io.getStdOut().writer().print("{d}", .{result.number}),
            .string => try std.io.getStdOut().writer().print("{s}", .{result.string}),
            .nil => try std.io.getStdOut().writer().print("nil", .{}),
        }

        try std.io.getStdOut().writer().print("\n", .{});
    }

    fn execVarDecl(self: *Intepreter, decl: VarDecl) !void {
        const initializer = decl.initializer;
        if (initializer == null) {
            try self.environment.put(decl.name, EvalResult.nil);
            return;
        }

        const result = try self.execExpr(initializer.?);
        try self.environment.put(decl.name, result);
    }

    fn execExpr(self: *Intepreter, expr: Expr) !EvalResult {
        var errorLine: usize = 0;
        return evaluate(&expr, &errorLine, @TypeOf(self.environment), &self.environment) catch |e| {
            try self.printEvalError(e, &errorLine);
            std.process.exit(70);
            return;
        };
    }
};
