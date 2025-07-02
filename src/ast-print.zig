const parser = @import("parse.zig");
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

    pub fn printStatement(self: *const AstPrinter, stmt: *const parser.Statement) PrintError!void {
        switch (stmt.*) {
            .Expression => |exprStmt| try self.printExpression(&exprStmt.expr),
            .Print => |printStmt| try self.printPrint(&printStmt.expr),
            .Var => |varStmt| try self.printVarDeclaration(&varStmt.initializer, varStmt.name),
        }
        try self.write("\n", .{});
    }

    pub fn printVarDeclaration(self: *const AstPrinter, initializer: *const ?parser.Expr, name: []const u8) PrintError!void {
        try self.write("(var {s} ", .{name});
        const maybeExpr = initializer.*;
        if (maybeExpr != null) {
            try self.printExpression(&maybeExpr.?);
        }
        try self.write(")", .{});
    }

    pub fn printPrint(self: *const AstPrinter, expr: *const parser.Expr) PrintError!void {
        try self.write("(print ", .{});
        try self.printExpression(expr);
        try self.write(")", .{});
    }

    pub fn printExpression(self: *const AstPrinter, expr: *const parser.Expr) PrintError!void {
        switch (expr.*) {
            .Binary => |group| try self.printBinary(group),
            .Unary => |group| try self.printUnary(group),
            .Literal => |literal| try self.printLiteral(literal),
            .Grouping => |group| try self.printGrouping(group),
            .Variable => |variable| try self.printVariable(variable),
        }
    }

    pub fn printVariable(self: *const AstPrinter, expr: parser.VariableExpr) PrintError!void {
        try self.write("{s}", .{expr.token.lexeme});
    }

    pub fn printGrouping(self: *const AstPrinter, expr: parser.GroupingExpr) PrintError!void {
        try self.write("(group ", .{});
        try self.printExpression(expr.expression);
        try self.write(")", .{});
    }

    pub fn printLiteral(self: *const AstPrinter, expr: parser.LiteralExpr) PrintError!void {
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

    pub fn printBinary(self: *const AstPrinter, expr: parser.BinaryExpr) PrintError!void {
        try self.write("({s} ", .{expr.operator.lexeme});
        try self.printExpression(expr.left);
        try self.write(" ", .{});
        try self.printExpression(expr.right);
        try self.write(")", .{});
    }

    pub fn printUnary(self: *const AstPrinter, expr: parser.UnaryExpr) PrintError!void {
        try self.write("({s} ", .{expr.operator.lexeme});
        try self.printExpression(expr.right);
        try self.write(")", .{});
    }
};
