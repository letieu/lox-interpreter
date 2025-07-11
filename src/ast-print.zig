const parser = @import("parse.zig");
const Expr = @import("parse.zig").Expr;
const std = @import("std");

const PrintError = error{
    WriteFail,
};

const WriterType = @TypeOf(std.io.getStdOut().writer());

pub const AstPrinter = struct {
    writer: WriterType,

    pub fn init() AstPrinter {
        const writer = std.io.getStdOut().writer();
        return AstPrinter{
            .writer = writer,
        };
    }

    fn write(self: AstPrinter, comptime format: []const u8, args: anytype) PrintError!void {
        self.writer.print(format, args) catch return PrintError.WriteFail;
    }

    pub fn printDeclaration(self: *const AstPrinter, declaration: *const parser.Declaration) PrintError!void {
        switch (declaration.*) {
            .var_decl => |var_decl| try self.printVarDeclaration(var_decl),
            .function_decl => |fun_decl| try self.printFunDeclaration(fun_decl),
            .stmt => |stmt| try self.printStatement(&stmt),
        }
        try self.write("\n", .{});
    }

    pub fn printStatement(self: *const AstPrinter, stmt: *const parser.Statement) PrintError!void {
        switch (stmt.*) {
            .expression => |exprStmt| try self.printExpression(exprStmt.expr),
            .print => |printStmt| try self.printPrint(printStmt.expr),
            .block => |block| try self.printBlockStmt(block),
            .ifStmt => |ifStmt| try self.printIfStmt(ifStmt),
            .while_stmt => |while_stmt| try self.printWhileStmt(while_stmt),
            .for_stmt => |for_stmt| try self.printForStmt(for_stmt),
            .return_stmt => |return_stmt| try self.printReturnStmt(return_stmt),
        }
        try self.write("\n", .{});
    }

    pub fn printReturnStmt(self: *const AstPrinter, stmt: parser.Statement.ReturnStatement) PrintError!void {
        try self.write("(return ", .{});
        if (stmt.expr != null) {
            try self.printExpression(stmt.expr.?);
        }
        try self.write(" )", .{});
    }

    pub fn printForStmt(self: *const AstPrinter, stmt: parser.Statement.ForStatement) PrintError!void {
        try self.write("(for ", .{});
        if (stmt.initial != null) {
            const decl = stmt.initial.?;
            try self.printDeclaration(decl);
        }
        try self.printExpression(stmt.condition);
        try self.printStatement(stmt.body);
        try self.write(")", .{});
    }

    pub fn printWhileStmt(self: *const AstPrinter, stmt: parser.Statement.WhileStatement) PrintError!void {
        try self.write("(while ", .{});
        try self.printExpression(stmt.condition);
        try self.printStatement(stmt.inner);
        try self.write(")", .{});
    }

    pub fn printIfStmt(self: *const AstPrinter, stmt: parser.Statement.IfStatement) PrintError!void {
        try self.write("(if ", .{});
        try self.printExpression(stmt.condition);
        try self.printStatement(stmt.inner);
        try self.write(")", .{});
    }

    pub fn printFunDeclaration(self: *const AstPrinter, fun_decl: parser.FunctionDecl) PrintError!void {
        try self.write("(fun {s} ", .{fun_decl.function.name});
        try self.printBlockStmt(fun_decl.function.body);
        try self.write(")", .{});
    }

    pub fn printVarDeclaration(self: *const AstPrinter, var_decl: parser.VarDecl) PrintError!void {
        try self.write("(var {s} ", .{var_decl.name});
        const maybeExpr = var_decl.initializer;
        if (maybeExpr != null) {
            try self.printExpression(maybeExpr.?);
        }
        try self.write(")", .{});
    }

    fn printBlockStmt(self: *const AstPrinter, block: parser.Statement.BlockStatement) PrintError!void {
        try self.write("(block \n", .{});
        for (block.declarations) |decl| try self.printDeclaration(&decl);
        try self.write(")", .{});
    }

    pub fn printPrint(self: *const AstPrinter, expr: *const parser.Expr) PrintError!void {
        try self.write("(print ", .{});
        try self.printExpression(expr);
        try self.write(")", .{});
    }

    pub fn printExpression(self: *const AstPrinter, expr: *const parser.Expr) PrintError!void {
        switch (expr.*) {
            .binary => |group| try self.printBinary(group),
            .unary => |group| try self.printUnary(group),
            .literal => |literal| try self.printLiteral(literal),
            .grouping => |group| try self.printGrouping(group),
            .identifier => |identifier| try self.printIdentifier(&identifier),
            .assign => |assign| try self.printAssign(assign),
            .call => |call| try self.printCall(call),
        }
    }

    pub fn printCall(self: *const AstPrinter, call: Expr.CallExpr) PrintError!void {
        try self.write("(call ", .{});
        try self.printExpression(call.callee);
        try self.write(" ", .{});
        for (call.args) |arg| {
            try self.printExpression(&arg);
        }
        try self.write(" )", .{});
    }

    pub fn printAssign(self: *const AstPrinter, expr: *const Expr.AssignExpr) PrintError!void {
        try self.write("(assign {s} ", .{expr.name.lexeme});
        try self.printExpression(expr.left);
        try self.write(" )", .{});
    }

    pub fn printIdentifier(self: *const AstPrinter, expr: *const Expr.Identifier) PrintError!void {
        try self.write("{s}", .{expr.token.lexeme});
    }

    pub fn printGrouping(self: *const AstPrinter, expr: *const Expr.GroupingExpr) PrintError!void {
        try self.write("(group ", .{});
        try self.printExpression(expr.expression);
        try self.write(")", .{});
    }

    pub fn printLiteral(self: *const AstPrinter, expr: Expr.LiteralExpr) PrintError!void {
        switch (expr) {
            .FALSE => try self.write("false", .{}),
            .TRUE => try self.write("true", .{}),
            .NIL => try self.write("nil", .{}),
            .NUMBER => |num| {
                if (@floor(num) == num) {
                    try self.write("{d:.1}", .{num});
                } else {
                    try self.write("{d}", .{num});
                }
            },
            .STRING => |str| try self.write("{s}", .{str}),
        }
    }

    pub fn printBinary(self: *const AstPrinter, expr: *const Expr.BinaryExpr) PrintError!void {
        try self.write("({s} ", .{expr.operator.lexeme});
        try self.printExpression(expr.left);
        try self.write(" ", .{});
        try self.printExpression(expr.right);
        try self.write(")", .{});
    }

    pub fn printUnary(self: *const AstPrinter, expr: *const Expr.UnaryExpr) PrintError!void {
        try self.write("({s} ", .{expr.operator.lexeme});
        try self.printExpression(expr.right);
        try self.write(")", .{});
    }
};
