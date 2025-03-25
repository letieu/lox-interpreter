const std = @import("std");

fn isInt(number: f64) bool {
    const intVal: usize = @intFromFloat(number);
    const casted: f64 = @floatFromInt(intVal);
    return number == casted;
}

fn parseNumber(str: []const u8) f64 {
    return std.fmt.parseFloat(f64, str) catch {
        return 0.0;
    };
}

pub const TokenType = enum {
    IDENTIFIER, // variable names
    EQUAL, // =
    STRING, // "string"
    NUMBER, // 123
    LEFT_PAREN, // (
    RIGHT_PAREN, // )
    LEFT_BRACE, // {
    RIGHT_BRACE, // }
    SEMICOLON, // ;
    COMMA, // ,
    DOT, // .
    MINUS, // -
    PLUS, // +
    SLASH, // /
    STAR, // *
    EOF,
    EQUAL_EQUAL, // ==
    BANG, // !
    BANG_EQUAL, // !=
    LESS, // <
    LESS_EQUAL, // <=
    GREATER, // >
    GREATER_EQUAL, // >=

    // Keywords
    AND, // and
    VAR, // var
    WHILE, // while
    TRUE, // true
    THIS, // this
    SUPER, // super
    RETURN, // return
    PRINT, // print
    OR, // or
    NIL, // nil
    IF, // if
    FUN, // fun
    FOR, // for
    FALSE, // false
    ELSE, // else
    CLASS, // class
};

const keywordMap = std.StaticStringMap(TokenType).initComptime(.{
    .{ "and", TokenType.AND },
    .{ "var", TokenType.VAR },
    .{ "while", TokenType.WHILE },
    .{ "true", TokenType.TRUE },
    .{ "this", TokenType.THIS },
    .{ "super", TokenType.SUPER },
    .{ "return", TokenType.RETURN },
    .{ "print", TokenType.PRINT },
    .{ "or", TokenType.OR },
    .{ "nil", TokenType.NIL },
    .{ "if", TokenType.IF },
    .{ "fun", TokenType.FUN },
    .{ "for", TokenType.FOR },
    .{ "false", TokenType.FALSE },
    .{ "else", TokenType.ELSE },
    .{ "class", TokenType.CLASS },
});

const Literal = union {
    string: []const u8,
    number: f64,
};

pub const Token = struct {
    tokenType: TokenType, // VAR
    lexeme: []const u8, // 123
    literal: ?Literal, // 123

    pub fn print(self: Token) !void {
        const writer = std.io.getStdOut().writer();

        if (self.tokenType == TokenType.STRING) {
            try writer.print("{s} \"{s}\" {?s}\n", .{ @tagName(self.tokenType), self.lexeme, self.literal.?.string });
            return;
        }

        if (self.tokenType == TokenType.NUMBER) {
            const number = self.literal.?.number;
            if (isInt(number)) {
                try writer.print("{s} {s} {?d:.1}\n", .{ @tagName(self.tokenType), self.lexeme, number });
            } else {
                try writer.print("{s} {s} {?d}\n", .{ @tagName(self.tokenType), self.lexeme, number });
            }
            return;
        }

        try writer.print("{s} {s} {any}\n", .{ @tagName(self.tokenType), self.lexeme, self.literal });
    }
};

pub const ScannerError = struct {
    message: []const u8,
    line: u32,
};

pub const MatchResultType = enum {
    token,
    scan_error,
    none,
};

pub const MatchResult = union(MatchResultType) {
    token: Token,
    scan_error: ScannerError,
    none: void,
};

pub const Scanner = struct {
    source: []u8,

    current_start: u32,
    current_end: u32,
    line: u32,

    pub fn init(source: []u8) Scanner {
        return .{
            .source = source,
            .current_start = 0,
            .current_end = 0,
            .line = 1,
        };
    }

    fn advance(self: *Scanner) void {
        self.current_end += 1;
    }

    fn currentStr(self: Scanner) []const u8 {
        return self.source[self.current_start .. self.current_end + 1];
    }

    fn matchToken(self: *Scanner, tokenType: TokenType) MatchResult {
        // TODO: Handle error
        const str = self.currentStr();
        const token = Token{
            .tokenType = tokenType,
            .lexeme = str,
            .literal = switch (tokenType) {
                TokenType.STRING => Literal{ .string = str },
                TokenType.NUMBER => Literal{ .number = parseNumber(str) },
                else => null,
            },
        };

        return MatchResult{ .token = token };
    }

    fn peek(self: Scanner) u8 {
        if (self.isEnd()) {
            return 0;
        }

        if (self.current_end + 1 >= self.source.len) {
            return 0;
        }

        return self.source[self.current_end + 1];
    }

    fn isEnd(self: Scanner) bool {
        return self.current_end >= self.source.len;
    }

    pub fn scanToken(self: *Scanner) MatchResult {
        if (self.isEnd()) {
            return MatchResult{ .token = Token{ .tokenType = TokenType.EOF, .lexeme = "", .literal = null } };
        }

        const char = self.source[self.current_end];

        switch (char) {
            '\n' => {
                self.line += 1;
                return MatchResult{ .none = void{} };
            },
            ' ' => {
                return MatchResult{ .none = void{} };
            },
            '\t' => {
                return MatchResult{ .none = void{} };
            },
            '(' => return self.matchToken(TokenType.LEFT_PAREN),
            ')' => return self.matchToken(TokenType.RIGHT_PAREN),
            '{' => return self.matchToken(TokenType.LEFT_BRACE),
            '}' => return self.matchToken(TokenType.RIGHT_BRACE),
            ';' => return self.matchToken(TokenType.SEMICOLON),
            ',' => return self.matchToken(TokenType.COMMA),
            '.' => return self.matchToken(TokenType.DOT),
            '+' => return self.matchToken(TokenType.PLUS),
            '-' => return self.matchToken(TokenType.MINUS),
            '*' => return self.matchToken(TokenType.STAR),
            '"' => {
                while (self.peek() != '"' and self.peek() != '\n' and !self.isEnd()) {
                    self.advance();
                }

                if (self.peek() == '"') {
                    self.current_start += 1;
                    const result = self.matchToken(TokenType.STRING);
                    self.advance();
                    return result;
                } else {
                    return MatchResult{ .scan_error = ScannerError{ .message = "Unterminated string.", .line = self.line } };
                }
            },
            '0'...'9' => {
                while (std.ascii.isDigit(self.peek()) or self.peek() == '.') {
                    self.advance();
                }

                return self.matchToken(TokenType.NUMBER);
            },
            '=' => {
                const next_char = self.peek();
                if (next_char == '=') {
                    self.advance();
                    return self.matchToken(TokenType.EQUAL_EQUAL);
                } else {
                    return self.matchToken(TokenType.EQUAL);
                }
            },
            '!' => {
                const next_char = self.peek();
                if (next_char == '=') {
                    self.advance();
                    return self.matchToken(TokenType.BANG_EQUAL);
                } else {
                    return self.matchToken(TokenType.BANG);
                }
            },
            '>' => {
                const next_char = self.peek();
                if (next_char == '=') {
                    self.advance();
                    return self.matchToken(TokenType.GREATER_EQUAL);
                } else {
                    return self.matchToken(TokenType.GREATER);
                }
            },
            '<' => {
                const next_char = self.peek();
                if (next_char == '=') {
                    self.advance();
                    return self.matchToken(TokenType.LESS_EQUAL);
                } else {
                    return self.matchToken(TokenType.LESS);
                }
            },
            '/' => {
                const next_char = self.peek();
                if (next_char == '/') {
                    while (!self.isEnd() and self.peek() != '\n') {
                        self.advance();
                    }

                    return MatchResult{ .none = void{} };
                } else {
                    return self.matchToken(TokenType.SLASH);
                }
            },
            'a'...'z', 'A'...'Z', '_' => {
                while (std.ascii.isAlphanumeric(self.peek()) or self.peek() == '_') {
                    self.advance();
                }

                const str = self.currentStr();
                const keyword = keywordMap.get(str);
                if (keyword != null) {
                    return self.matchToken(keyword.?);
                }

                return self.matchToken(TokenType.IDENTIFIER);
            },
            0 => return self.matchToken(TokenType.EOF),
            else => {
                const alloc = std.heap.page_allocator;
                const message = std.fmt.allocPrint(alloc, "Unexpected character: {c}", .{char}) catch {
                    return MatchResult{ .scan_error = ScannerError{
                        .message = "Unexpected character.",
                        .line = self.line,
                    } };
                };
                return MatchResult{ .scan_error = ScannerError{
                    .message = message,
                    .line = self.line,
                } };
            },
        }
    }

    pub fn prepare_next_token(self: *Scanner) void {
        self.advance();
        self.current_start = self.current_end;
    }
};

pub fn scanTokens(alloc: std.mem.Allocator, file_contents: []u8, should_print: bool) ![]Token {
    var tokens = std.ArrayList(Token).init(alloc);
    defer tokens.deinit();

    var have_error = false;

    var scanner = Scanner.init(file_contents);
    while (true) {
        const scan_result = scanner.scanToken();

        switch (scan_result) {
            .none => {},
            .scan_error => {
                have_error = true;
                std.debug.print("[line {d}] Error: {s}\n", .{ scanner.line, scan_result.scan_error.message });
                scanner.prepare_next_token();
                continue;
            },
            .token => |token| {
                if (should_print) {
                    try token.print();
                }

                try tokens.append(token);
                if (token.tokenType == TokenType.EOF) {
                    break;
                }
            },
        }

        if (scanner.isEnd()) {
            break;
        }

        scanner.prepare_next_token();
    }

    if (have_error) {
        return error.ScanningError;
    }

    return tokens.toOwnedSlice();
}
