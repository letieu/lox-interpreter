const std = @import("std");

const scan = @import("scan.zig");

// program        → statement* EOF ;
// statement      → exprStmt | printStmt ;
// exprStmt       → expression ";";
// printStmt      → "print" expression ";" ;
// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")" ;

pub const Statement = union(enum) { Print: PrintStatement, Expression: ExpressionStatement };

pub const PrintStatement = struct { expr: Expr };

pub const ExpressionStatement = struct { expr: Expr };

pub const Expr = union(enum) {
    Literal: LiteralExpr,
    Unary: UnaryExpr,
    Binary: BinaryExpr,
    Grouping: GroupingExpr,
};

pub const LiteralType = enum {
    NUMBER,
    STRING,
    TRUE,
    FALSE,
    NIL,
};
pub const LiteralExpr = union(LiteralType) {
    NUMBER: f64,
    STRING: []const u8,
    TRUE,
    FALSE,
    NIL,
};

pub const UnaryExpr = struct {
    operator: scan.Token,
    right: *Expr,
};

pub const BinaryExpr = struct {
    left: *Expr,
    right: *Expr,
    operator: scan.Token,
};

pub const GroupingExpr = struct {
    expression: *Expr,
};

pub const ParseMessage = struct {
    token: scan.Token,
    message: []const u8,
};

pub const ParseError = error{
    SyntaxError,
    AllocationError,
};

pub const Parser = struct {
    alloc: std.mem.Allocator,
    tokens: []scan.Token,
    current: usize = 0,
    errors: std.ArrayList(ParseMessage),

    pub fn init(
        tokens: []scan.Token,
        alloc: std.mem.Allocator,
    ) !Parser {
        return .{
            .tokens = tokens,
            .current = 0,
            .alloc = alloc,
            .errors = std.ArrayList(ParseMessage).init(alloc),
        };
    }

    pub fn deinit(self: *Parser) void {
        self.errors.deinit();
    }

    pub fn parse(self: *Parser) ![]Statement {
        var statements = std.ArrayList(Statement).init(self.alloc);
        while (!self.isAtEnd()) {
            const statement = self.parseStatement() orelse {
                return ParseError.SyntaxError;
            };
            statements.append(statement) catch {
                return ParseError.AllocationError;
            };
        }

        if (self.errors.items.len > 0) {
            return ParseError.SyntaxError;
        }

        return statements.toOwnedSlice();
    }

    fn reportError(self: *Parser, token: scan.Token, message: []const u8) void {
        self.errors.append(.{ .message = message, .token = token }) catch unreachable;
        std.io.getStdErr().writer().print("[line {}] Error at '{s}': {s}\n", .{ token.line, token.lexeme, message }) catch {
            std.debug.print("Print error failed", .{});
        };
    }

    fn parseStatement(self: *Parser) ?Statement {
        // statement      →  printStmt | exprStmt ;
        if (self.is(scan.TokenType.PRINT)) {
            return self.parsePrintStatement();
        }

        return self.parseExpressionStatement();
    }

    fn parsePrintStatement(self: *Parser) ?Statement {
        const printToken = self.currentToken();
        self.advance();

        const expr = self.parseExpression() orelse {
            self.reportError(printToken, "Invalid print");
            return null;
        };

        if (!self.is(scan.TokenType.SEMICOLON)) {
            self.reportError(self.currentToken(), "Expected ';'");
            return null;
        }
        self.advance();

        return Statement{ .Print = PrintStatement{ .expr = expr } };
    }

    fn parseExpressionStatement(self: *Parser) ?Statement {
        const expr = self.parseExpression() orelse {
            self.reportError(self.currentToken(), "Invalid expression statement");
            return null;
        };
        if (!self.is(scan.TokenType.SEMICOLON)) {
            self.reportError(self.currentToken(), "Expected ';'");
            return null;
        }
        self.advance();
        return Statement{ .Expression = ExpressionStatement{ .expr = expr } };
    }

    fn parseExpression(self: *Parser) ?Expr {
        // expression     → equality ;
        return self.parseEquality();
    }

    fn parseEquality(self: *Parser) ?Expr {
        // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
        var expr = self.parseComparison() orelse return null;

        while (self.is(scan.TokenType.BANG_EQUAL) or self.is(scan.TokenType.EQUAL_EQUAL)) {
            const operator = self.currentToken();
            self.advance();

            const left = self.alloc.create(Expr) catch unreachable;
            const right = self.alloc.create(Expr) catch unreachable;

            left.* = expr;
            right.* = self.parseComparison() orelse return null;

            expr = Expr{ .Binary = BinaryExpr{ .left = left, .operator = operator, .right = right } };
        }
        return expr;
    }

    fn parseComparison(self: *Parser) ?Expr {
        // comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
        var expr = self.parseTerm() orelse return null;
        while (self.is(scan.TokenType.GREATER) or
            self.is(scan.TokenType.GREATER_EQUAL) or
            self.is(scan.TokenType.LESS) or
            self.is(scan.TokenType.LESS_EQUAL))
        {
            const operator = self.currentToken();
            self.advance();

            const left = self.alloc.create(Expr) catch unreachable;
            const right = self.alloc.create(Expr) catch unreachable;

            left.* = expr;
            right.* = self.parseTerm() orelse return null;

            expr = Expr{ .Binary = BinaryExpr{ .left = left, .operator = operator, .right = right } };
        }
        return expr;
    }

    fn parseTerm(self: *Parser) ?Expr {
        // term           → factor ( ( "-" | "+" ) factor )* ;
        var expr = self.parseFactor() orelse return null;

        while (self.is(scan.TokenType.MINUS) or self.is(scan.TokenType.PLUS)) {
            const operator = self.currentToken();
            self.advance();

            const left = self.alloc.create(Expr) catch unreachable;
            const right = self.alloc.create(Expr) catch unreachable;

            left.* = expr;
            right.* = self.parseFactor() orelse return null;

            expr = Expr{ .Binary = BinaryExpr{ .left = left, .operator = operator, .right = right } };
        }
        return expr;
    }

    fn parseFactor(self: *Parser) ?Expr {
        // factor         → unary ( ( "/" | "*" ) unary )* ;
        var expr = self.parseUnary() orelse return null;
        while (self.is(scan.TokenType.STAR) or self.is(scan.TokenType.SLASH)) {
            const operator = self.currentToken();
            self.advance();

            const left = self.alloc.create(Expr) catch unreachable;
            const right = self.alloc.create(Expr) catch unreachable;

            left.* = expr;
            right.* = self.parseUnary() orelse return null;

            expr = Expr{ .Binary = BinaryExpr{ .left = left, .operator = operator, .right = right } };
        }
        return expr;
    }

    fn parseUnary(self: *Parser) ?Expr {
        // unary          → ( "!" | "-" ) unary | primary
        while (self.is(scan.TokenType.BANG) or self.is(scan.TokenType.MINUS)) {
            const operator = self.currentToken();
            self.advance();

            const right = self.alloc.create(Expr) catch unreachable;
            right.* = self.parseUnary() orelse return null;

            return Expr{ .Unary = UnaryExpr{ .operator = operator, .right = right } };
        }

        return self.parsePrimary();
    }

    fn parsePrimary(self: *Parser) ?Expr {
        // primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;

        if (self.is(scan.TokenType.TRUE)) {
            self.advance();
            return Expr{ .Literal = LiteralExpr.TRUE };
        }

        if (self.is(scan.TokenType.FALSE)) {
            self.advance();
            return Expr{ .Literal = LiteralExpr.FALSE };
        }

        if (self.currentToken().tokenType == scan.TokenType.NIL) {
            self.advance();
            return Expr{ .Literal = LiteralExpr.NIL };
        }

        if (self.currentToken().tokenType == scan.TokenType.NUMBER) {
            const numberValue = self.currentToken().literal.?.number;
            self.advance();
            return Expr{ .Literal = LiteralExpr{ .NUMBER = numberValue } };
        }

        if (self.currentToken().tokenType == scan.TokenType.STRING) {
            const stringValue = self.currentToken().literal.?.string;
            self.advance();
            return Expr{ .Literal = LiteralExpr{ .STRING = stringValue } };
        }

        if (self.currentToken().tokenType == scan.TokenType.LEFT_PAREN) {
            const expr = self.alloc.create(Expr) catch unreachable;
            self.advance();
            expr.* = self.parseExpression() orelse return null;
            const token = self.currentToken();
            if (token.tokenType != scan.TokenType.RIGHT_PAREN) {
                self.reportError(token, "Expected ')'.");
                return null;
            }
            self.advance();
            return Expr{ .Grouping = GroupingExpr{ .expression = expr } };
        }

        const token = self.currentToken();
        self.reportError(token, "Expected expression.");
        return null;
    }

    fn previousToken(self: *Parser) scan.Token {
        return self.tokens[self.current - 1];
    }

    fn currentToken(self: *Parser) scan.Token {
        return self.tokens[self.current];
    }

    fn advance(self: *Parser) void {
        if (self.isAtEnd()) {
            return;
        }

        self.current = self.current + 1;
    }

    fn isAtEnd(self: *Parser) bool {
        return self.currentToken().tokenType == scan.TokenType.EOF;
    }

    fn is(self: *Parser, tokenType: scan.TokenType) bool {
        const token = self.currentToken();
        return token.tokenType == tokenType;
    }
};
