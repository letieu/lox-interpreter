const std = @import("std");

const scan = @import("scan.zig");

// program        → statement* EOF ;
// statement      → exprStmt | varDecl | printStmt ;
// exprStmt       → expression ";";
// varDecl        → "var" IDENTIFIER ( "=" expression)? ";" ;
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

pub const StatementType = enum { Print, Expression, Var };
pub const Statement = union(StatementType) { Print: PrintStatement, Expression: ExpressionStatement, Var: VarStatement };

pub const PrintStatement = struct { expr: Expr };

pub const ExpressionStatement = struct { expr: Expr };

pub const VarStatement = struct {
    name: []const u8,
    initializer: ?Expr,
};

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

pub const SyntaxError = error{
    MissingSemicolon,
    MissingVarInit,
    MissingVarIdentifier,
    MissingExpression,
    MissingRightParen,
    UnexpectedToken,
};

pub const ParseError = SyntaxError || error{OutOfMemory};

pub const Parser = struct {
    alloc: std.mem.Allocator,
    tokens: []scan.Token,
    current: usize = 0,

    stdOut: std.fs.File,
    stdErr: std.fs.File,

    errorLine: usize,

    pub fn init(tokens: []scan.Token, alloc: std.mem.Allocator, stdOut: std.fs.File, stdErr: std.fs.File) !Parser {
        return .{
            .tokens = tokens,
            .current = 0,
            .alloc = alloc,
            .stdOut = stdOut,
            .stdErr = stdErr,
            .errorLine = 0,
        };
    }

    pub fn parse(self: *Parser) ![]Statement {
        var statements = std.ArrayList(Statement).init(self.alloc);
        var err: ?ParseError = null;

        while (!self.isAtEnd()) {
            const statement = self.parseStatement() catch |e| {
                err = e;
                try self.printError(e);
                self.synchronize();
                continue;
            };

            try statements.append(statement);
        }

        if (err != null) return err.?;
        return statements.toOwnedSlice();
    }

    pub fn printError(self: *Parser, e: ParseError) !void {
        const prevToken = self.previousToken();
        const current = self.currentToken();
        const writer = self.stdErr.writer();

        switch (e) {
            error.MissingSemicolon => try writer.print("[line {d}] Error at '{s}': Expected ';'.\n", .{ prevToken.line, prevToken.lexeme }),
            error.MissingVarIdentifier => try writer.print("[line {d}] Error at '{s}': Expected var identifier.\n", .{ prevToken.line, prevToken.lexeme }),
            error.MissingVarInit => try writer.print("[line {d}] Error at '{s}': Expected var initializer.\n", .{ prevToken.line, prevToken.lexeme }),
            error.MissingExpression => try writer.print("[line {d}] Error at '{s}': Miss expression.\n", .{ prevToken.line, prevToken.lexeme }),
            error.MissingRightParen => try writer.print("[line {d}] Error at '{s}': Miss ')'.\n", .{ prevToken.line, prevToken.lexeme }),
            error.OutOfMemory => try writer.print("Out of memory.\n", .{}),
            error.UnexpectedToken => try writer.print("[Line {d}] Unexpected token '{s}'", .{ current.line, current.lexeme }),
        }
    }

    fn synchronize(self: *Parser) void {
        while (!self.isAtEnd()) {
            const token = self.currentToken();
            switch (token.tokenType) {
                .SEMICOLON => {
                    self.advance();
                    return;
                },
                else => {
                    self.advance();
                },
            }
        }
    }

    fn parseStatement(self: *Parser) ParseError!Statement {
        // statement      →  printStmt | exprStmt ;
        if (self.is(scan.TokenType.PRINT)) {
            return self.parsePrintStatement();
        }

        if (self.is(scan.TokenType.VAR)) {
            return self.parseVarStatement();
        }

        return self.parseExpressionStatement();
    }

    fn parsePrintStatement(self: *Parser) ParseError!Statement {
        self.advance();
        const expr = try self.parseExpression();

        if (!self.is(scan.TokenType.SEMICOLON)) {
            return ParseError.MissingSemicolon;
        }
        self.advance();

        return Statement{ .Print = PrintStatement{ .expr = expr } };
    }

    fn parseVarStatement(self: *Parser) ParseError!Statement {
        self.advance();

        if (!self.is(scan.TokenType.IDENTIFIER)) {
            return ParseError.MissingVarIdentifier;
        }

        const varIdentifier = self.currentToken();
        self.advance();

        var sttm = Statement{ .Var = VarStatement{ .name = varIdentifier.lexeme, .initializer = null } };

        if (self.is(scan.TokenType.EQUAL)) {
            self.advance();
            sttm.Var.initializer = self.parseExpression() catch {
                return error.MissingVarInit;
            };
        }

        if (!self.is(scan.TokenType.SEMICOLON)) {
            return ParseError.MissingSemicolon;
        }
        self.advance();

        return sttm;
    }

    fn parseExpressionStatement(self: *Parser) ParseError!Statement {
        const expr = try self.parseExpression();

        if (!self.is(scan.TokenType.SEMICOLON)) {
            return ParseError.MissingSemicolon;
        }
        self.advance();
        return Statement{ .Expression = ExpressionStatement{ .expr = expr } };
    }

    fn parseExpression(self: *Parser) ParseError!Expr {
        // expression     → equality ;
        return self.parseEquality();
    }

    fn parseEquality(self: *Parser) ParseError!Expr {
        // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
        var expr = try self.parseComparison();

        while (self.is(scan.TokenType.BANG_EQUAL) or self.is(scan.TokenType.EQUAL_EQUAL)) {
            const operator = self.currentToken();
            self.advance();

            const left = try self.alloc.create(Expr);
            const right = try self.alloc.create(Expr);

            left.* = expr;
            right.* = try self.parseComparison();

            expr = Expr{ .Binary = BinaryExpr{ .left = left, .operator = operator, .right = right } };
        }
        return expr;
    }

    fn parseComparison(self: *Parser) ParseError!Expr {
        // comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
        var expr = try self.parseTerm();

        while (self.is(scan.TokenType.GREATER) or
            self.is(scan.TokenType.GREATER_EQUAL) or
            self.is(scan.TokenType.LESS) or
            self.is(scan.TokenType.LESS_EQUAL))
        {
            const operator = self.currentToken();
            self.advance();

            const left = try self.alloc.create(Expr);
            const right = try self.alloc.create(Expr);

            left.* = expr;
            right.* = try self.parseTerm();

            expr = Expr{ .Binary = BinaryExpr{ .left = left, .operator = operator, .right = right } };
        }
        return expr;
    }

    fn parseTerm(self: *Parser) ParseError!Expr {
        // term           → factor ( ( "-" | "+" ) factor )* ;
        var expr = try self.parseFactor();

        while (self.is(scan.TokenType.MINUS) or self.is(scan.TokenType.PLUS)) {
            const operator = self.currentToken();
            self.advance();

            const left = try self.alloc.create(Expr);
            const right = try self.alloc.create(Expr);

            left.* = expr;
            right.* = try self.parseFactor();

            expr = Expr{ .Binary = BinaryExpr{ .left = left, .operator = operator, .right = right } };
        }
        return expr;
    }

    fn parseFactor(self: *Parser) ParseError!Expr {
        // factor         → unary ( ( "/" | "*" ) unary )* ;
        var expr = try self.parseUnary();
        while (self.is(scan.TokenType.STAR) or self.is(scan.TokenType.SLASH)) {
            const operator = self.currentToken();
            self.advance();

            const left = try self.alloc.create(Expr);
            const right = try self.alloc.create(Expr);

            left.* = expr;
            right.* = try self.parseUnary();

            expr = Expr{ .Binary = BinaryExpr{ .left = left, .operator = operator, .right = right } };
        }
        return expr;
    }

    fn parseUnary(self: *Parser) ParseError!Expr {
        // unary          → ( "!" | "-" ) unary | primary
        while (self.is(scan.TokenType.BANG) or self.is(scan.TokenType.MINUS)) {
            const operator = self.currentToken();
            self.advance();

            const right = try self.alloc.create(Expr);
            right.* = try self.parseUnary();

            return Expr{ .Unary = UnaryExpr{ .operator = operator, .right = right } };
        }

        return self.parsePrimary();
    }

    fn parsePrimary(self: *Parser) ParseError!Expr {
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
            expr.* = try self.parseExpression();
            const token = self.currentToken();
            if (token.tokenType != scan.TokenType.RIGHT_PAREN) {
                return ParseError.MissingRightParen;
            }
            self.advance();
            return Expr{ .Grouping = GroupingExpr{ .expression = expr } };
        }

        return ParseError.UnexpectedToken;
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
