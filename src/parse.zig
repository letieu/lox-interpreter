const std = @import("std");

const scan = @import("scan.zig");
const TokenType = scan.TokenType;
const Token = scan.Token;

// program        → declaration* EOF ;
// declaration    → varDecl | statement ;
// statement      → ifStmt | exprStmt | printStmt | block ;
// ifStmt         → "if" "(" expression ")" statement
//                ( "else" statement )? ;
// block          → "{" declaration* "}" ;
// exprStmt       → expression ";" ;
// varDecl        → "var" IDENTIFIER ( "=" expression)? ";" ;
// printStmt      → "print" expression ";" ;
// expression     → assignment ;
// assignment     → IDENTIFIER "=" assignment
//                  | AND ;
// AND            → OR ( and OR )* ;
// OR             → equality ( or equality )* ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary      → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")"
//                | IDENTIFIER;

pub const Declaration = union(enum) {
    var_decl: VarDecl,
    stmt: Statement,
    // function_decl: FunctionDecl,
    // class_decl: ClassDecl,
};

pub const VarDecl = struct {
    name: []const u8,
    initializer: ?Expr,
};

pub const Statement = union(enum) {
    print: PrintStatement,
    expression: ExpressionStatement,
    block: BlockStatement,
    ifStmt: IfStatement,

    pub const PrintStatement = struct { expr: Expr };

    pub const ExpressionStatement = struct { expr: Expr };

    pub const BlockStatement = struct { declarations: []Declaration };

    pub const IfStatement = struct { condition: Expr, inner: *const Statement, elseStmt: ?*const Statement };
};

pub const Expr = union(enum) {
    assign: AssignExpr,
    identifier: Identifier,
    literal: LiteralExpr,
    unary: UnaryExpr,
    binary: BinaryExpr,
    grouping: GroupingExpr,

    pub const LiteralType = enum {
        NUMBER,
        STRING,
        TRUE,
        FALSE,
        NIL,
    };

    pub const AssignExpr = struct {
        name: Token,
        left: *Expr,
    };

    pub const Identifier = struct {
        token: Token,
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
};

pub const SyntaxError = error{
    MissingSemicolon,
    MissingVarInit,
    MissingVarIdentifier,
    MissingExpression,
    MissingRightParen,
    MissingLeftParen,
    MissingRightBrace,
    UnexpectedToken,
    InvalidAssignmentTarget,
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

    pub fn parse(self: *Parser) ![]Declaration {
        var declarations = std.ArrayList(Declaration).init(self.alloc);
        var err: ?ParseError = null;

        while (!self.isAtEnd()) {
            const declaration = self.parseDeclaration() catch |e| {
                err = e;
                try self.printError(e);
                self.synchronize();
                continue;
            };

            try declarations.append(declaration);
        }

        if (err != null) return err.?;
        return declarations.toOwnedSlice();
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
            error.MissingRightParen => try writer.print("[line {d}] Error at '{s}': Miss '('.\n", .{ prevToken.line, prevToken.lexeme }),
            error.MissingLeftParen => try writer.print("[line {d}] Error at '{s}': Miss ')'.\n", .{ prevToken.line, prevToken.lexeme }),
            error.OutOfMemory => try writer.print("Out of memory.\n", .{}),
            error.UnexpectedToken => try writer.print("[Line {d}] Unexpected token '{s}'\n", .{ current.line, current.lexeme }),
            error.InvalidAssignmentTarget => try writer.print("[Line {d}] Invalid assignment target: '{s}'\n", .{ prevToken.line, prevToken.lexeme }),
            error.MissingRightBrace => try writer.print("[line {d}] Error at '{s}': Expected '}}'.\n", .{ prevToken.line, prevToken.lexeme }),
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

    fn parseDeclaration(self: *Parser) ParseError!Declaration {
        // declaration    → varDecl | statement ;
        if (self.is(scan.TokenType.VAR)) {
            return Declaration{ .var_decl = try self.parseVarDecl() };
        }

        return Declaration{ .stmt = try self.parseStatement() };
    }

    fn parseStatement(self: *Parser) ParseError!Statement {
        // statement      → ifStmt | exprStmt | printStmt | block ;
        if (self.is(scan.TokenType.PRINT)) {
            return self.parsePrintStatement();
        }
        if (self.is(scan.TokenType.LEFT_BRACE)) {
            return self.parseBlockStatement();
        }
        if (self.is(scan.TokenType.IF)) {
            return self.parseIfStatement();
        }

        return self.parseExpressionStatement();
    }

    fn parseIfStatement(self: *Parser) ParseError!Statement {
        // ifStmt         → "if" "(" expression ")" statement
        //                ( "else" statement )? ;
        _ = try self.consume(TokenType.IF);
        _ = self.consume(TokenType.LEFT_PAREN) catch return ParseError.MissingLeftParen;
        const condition = try self.parseExpression();
        _ = self.consume(TokenType.RIGHT_PAREN) catch return ParseError.MissingRightParen;

        const inner = try self.alloc.create(Statement);
        inner.* = try self.parseStatement();

        if (self.is(TokenType.ELSE)) {
            _ = try self.consume(TokenType.ELSE);
            const elseStmt = try self.alloc.create(Statement);
            elseStmt.* = try self.parseStatement();

            return Statement{ .ifStmt = Statement.IfStatement{
                .condition = condition,
                .inner = inner,
                .elseStmt = elseStmt,
            } };
        }

        return Statement{ .ifStmt = Statement.IfStatement{
            .condition = condition,
            .inner = inner,
            .elseStmt = null,
        } };
    }

    fn parseBlockStatement(self: *Parser) ParseError!Statement {
        _ = try self.consume(TokenType.LEFT_BRACE);

        var declarations = std.ArrayList(Declaration).init(self.alloc);

        while (!self.is(TokenType.RIGHT_BRACE) and !self.isAtEnd()) {
            const declaration = try self.parseDeclaration();
            try declarations.append(declaration);
        }
        _ = self.consume(TokenType.RIGHT_BRACE) catch return ParseError.MissingRightBrace;

        return Statement{ .block = Statement.BlockStatement{ .declarations = try declarations.toOwnedSlice() } };
    }

    fn parsePrintStatement(self: *Parser) ParseError!Statement {
        _ = try self.consume(TokenType.PRINT);
        const expr = try self.parseExpression();
        _ = self.consume(TokenType.SEMICOLON) catch return ParseError.MissingSemicolon;
        return Statement{ .print = Statement.PrintStatement{ .expr = expr } };
    }

    fn parseVarDecl(self: *Parser) ParseError!VarDecl {
        _ = try self.consume(TokenType.VAR);
        const var_identifier = self.consume(TokenType.IDENTIFIER) catch return error.MissingVarIdentifier;

        var var_decl = VarDecl{ .name = var_identifier.lexeme, .initializer = null };

        if (self.is(scan.TokenType.EQUAL)) {
            self.advance();
            var_decl.initializer = self.parseExpression() catch {
                return error.MissingVarInit;
            };
        }

        _ = self.consume(TokenType.SEMICOLON) catch return ParseError.MissingSemicolon;
        return var_decl;
    }

    fn parseExpressionStatement(self: *Parser) ParseError!Statement {
        const expr = try self.parseExpression();

        if (!self.is(scan.TokenType.SEMICOLON)) {
            return ParseError.MissingSemicolon;
        }
        self.advance();
        return Statement{ .expression = Statement.ExpressionStatement{ .expr = expr } };
    }

    fn parseExpression(self: *Parser) ParseError!Expr {
        // expression     → assignment ;
        return self.parseAssignment();
    }

    fn parseAssignment(self: *Parser) ParseError!Expr {
        // [R] assignment → IDENTIFIER "=" assignment
        //                  | AND ;
        const expr = try self.parseAnd();

        if (self.is(TokenType.EQUAL)) {
            self.advance();
            const left = try self.alloc.create(Expr);
            left.* = try self.parseAssignment();

            if (expr != .identifier) {
                return ParseError.InvalidAssignmentTarget;
            }

            return Expr{ .assign = Expr.AssignExpr{
                .name = expr.identifier.token,
                .left = left,
            } };
        }

        return expr;
    }

    fn parseAnd(self: *Parser) ParseError!Expr {
        // AND             → OR ( and OR )* ;
        var expr = try self.parseOr();

        while (self.is(TokenType.AND)) {
            const operator = try self.consume(TokenType.AND);

            const left = try self.alloc.create(Expr);
            const right = try self.alloc.create(Expr);

            left.* = expr;
            right.* = try self.parseEquality();

            expr = Expr{ .binary = Expr.BinaryExpr{ .left = left, .operator = operator, .right = right } };
        }
        return expr;
    }

    fn parseOr(self: *Parser) ParseError!Expr {
        // OR             → equality ( or equality )* ;
        var expr = try self.parseEquality();

        while (self.is(TokenType.OR)) {
            const operator = try self.consume(TokenType.OR);

            const left = try self.alloc.create(Expr);
            const right = try self.alloc.create(Expr);

            left.* = expr;
            right.* = try self.parseEquality();

            expr = Expr{ .binary = Expr.BinaryExpr{ .left = left, .operator = operator, .right = right } };
        }
        return expr;
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

            expr = Expr{ .binary = Expr.BinaryExpr{ .left = left, .operator = operator, .right = right } };
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

            expr = Expr{ .binary = Expr.BinaryExpr{ .left = left, .operator = operator, .right = right } };
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

            expr = Expr{ .binary = Expr.BinaryExpr{ .left = left, .operator = operator, .right = right } };
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

            expr = Expr{ .binary = Expr.BinaryExpr{ .left = left, .operator = operator, .right = right } };
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

            return Expr{ .unary = Expr.UnaryExpr{ .operator = operator, .right = right } };
        }

        return self.parsePrimary();
    }

    fn parsePrimary(self: *Parser) ParseError!Expr {
        // primary        → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;

        if (self.is(scan.TokenType.TRUE)) {
            self.advance();
            return Expr{ .literal = Expr.LiteralExpr.TRUE };
        }

        if (self.is(scan.TokenType.FALSE)) {
            self.advance();
            return Expr{ .literal = Expr.LiteralExpr.FALSE };
        }

        if (self.currentToken().tokenType == scan.TokenType.NIL) {
            self.advance();
            return Expr{ .literal = Expr.LiteralExpr.NIL };
        }

        if (self.currentToken().tokenType == scan.TokenType.NUMBER) {
            const numberValue = self.currentToken().literal.?.number;
            self.advance();
            return Expr{ .literal = Expr.LiteralExpr{ .NUMBER = numberValue } };
        }

        if (self.currentToken().tokenType == scan.TokenType.STRING) {
            const stringValue = self.currentToken().literal.?.string;
            self.advance();
            return Expr{ .literal = Expr.LiteralExpr{ .STRING = stringValue } };
        }

        if (self.currentToken().tokenType == scan.TokenType.LEFT_PAREN) {
            const expr = self.alloc.create(Expr) catch unreachable;
            self.advance();
            expr.* = try self.parseExpression();
            const token = self.currentToken();
            if (token.tokenType != scan.TokenType.RIGHT_PAREN) {
                return ParseError.MissingRightBrace;
            }
            self.advance();
            return Expr{ .grouping = Expr.GroupingExpr{ .expression = expr } };
        }

        if (self.is(TokenType.IDENTIFIER)) {
            const token = try self.consume(TokenType.IDENTIFIER);
            return Expr{ .identifier = Expr.Identifier{ .token = token } };
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

    fn consume(self: *Parser, tokenType: scan.TokenType) !Token {
        if (!self.is(tokenType)) {
            return ParseError.UnexpectedToken;
        }
        self.advance();
        return self.previousToken();
    }
};
