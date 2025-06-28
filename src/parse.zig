const std = @import("std");

const scan = @import("scan.zig");

// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")" ;

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

pub const Parser = struct {
    alloc: std.mem.Allocator,
    tokens: []scan.Token,
    current: usize = 0,

    pub fn init(
        tokens: []scan.Token,
        alloc: std.mem.Allocator,
    ) !Parser {
        return .{
            .tokens = tokens,
            .current = 0,
            .alloc = alloc,
        };
    }

    pub fn parseExpression(self: *Parser) Expr {
        return self.parseEquality();
    }

    pub fn parseEquality(self: *Parser) Expr {
        // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
        var expr = self.parseComparison();

        while (self.is(scan.TokenType.BANG_EQUAL) or self.is(scan.TokenType.EQUAL_EQUAL)) {
            const operator = self.currentToken();
            self.advance();

            const left = self.alloc.create(Expr) catch unreachable;
            const right = self.alloc.create(Expr) catch unreachable;

            left.* = expr;
            right.* = self.parseComparison();

            expr = Expr{ .Binary = BinaryExpr{ .left = left, .operator = operator, .right = right } };
        }
        return expr;
    }

    pub fn parseComparison(self: *Parser) Expr {
        // comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
        var expr = self.parseTerm();
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
            right.* = self.parseTerm();

            expr = Expr{ .Binary = BinaryExpr{ .left = left, .operator = operator, .right = right } };
        }
        return expr;
    }

    pub fn parseTerm(self: *Parser) Expr {
        // term           → factor ( ( "-" | "+" ) factor )* ;
        var expr = self.parseFactor();

        while (self.is(scan.TokenType.MINUS) or self.is(scan.TokenType.PLUS)) {
            const operator = self.currentToken();
            self.advance();

            const left = self.alloc.create(Expr) catch unreachable;
            const right = self.alloc.create(Expr) catch unreachable;

            left.* = expr;
            right.* = self.parseFactor();

            expr = Expr{ .Binary = BinaryExpr{ .left = left, .operator = operator, .right = right } };
        }
        return expr;
    }

    pub fn parseFactor(self: *Parser) Expr {
        // factor         → unary ( ( "/" | "*" ) unary )* ;
        var expr = self.parseUnary();
        while (self.is(scan.TokenType.STAR) or self.is(scan.TokenType.SLASH)) {
            const operator = self.currentToken();
            self.advance();

            const left = self.alloc.create(Expr) catch unreachable;
            const right = self.alloc.create(Expr) catch unreachable;

            left.* = expr;
            right.* = self.parseUnary();

            expr = Expr{ .Binary = BinaryExpr{ .left = left, .operator = operator, .right = right } };
        }
        return expr;
    }

    pub fn parseUnary(self: *Parser) Expr {
        // unary          → ( "!" | "-" ) unary | primary
        while (self.is(scan.TokenType.BANG) or self.is(scan.TokenType.MINUS)) {
            const operator = self.currentToken();
            self.advance();

            const right = self.alloc.create(Expr) catch unreachable;
            right.* = self.parseUnary();

            return Expr{ .Unary = UnaryExpr{ .operator = operator, .right = right } };
        }

        return self.parsePrimary();
    }

    pub fn parsePrimary(self: *Parser) Expr {
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
            expr.* = self.parseExpression();
            if (self.currentToken().tokenType != scan.TokenType.RIGHT_PAREN) {
                std.debug.print("Expected ')'", .{});
                std.process.exit(1); // TODO: handle error
            }
            self.advance();
            return Expr{ .Grouping = GroupingExpr{ .expression = expr } };
        }

        std.debug.print("Unexpected token: {}\n", .{self.currentToken().tokenType});
        std.process.exit(1); // Or return an error instead
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
