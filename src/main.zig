const std = @import("std");

const TokenType = enum {
    VAR, // var
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
};

const Token = struct {
    tokenType: TokenType, // VAR
    lexeme: []const u8, // 123
    literal: ?[]u8, // 123

    fn print(self: Token) !void {
        if (self.tokenType == TokenType.STRING) {
            try std.io.getStdOut().writer().print("{s} \"{s}\" {?s}\n", .{ @tagName(self.tokenType), self.lexeme, self.literal });
        } else if (self.tokenType == TokenType.NUMBER) {
            const number = try std.fmt.parseFloat(f32, self.lexeme);
            if (std.mem.indexOfScalar(u8, self.lexeme, '.') == null) {
                try std.io.getStdOut().writer().print("{s} {s} {d:.1}\n", .{ @tagName(self.tokenType), self.lexeme, number });
            } else {
                try std.io.getStdOut().writer().print("{s} {s} {d}\n", .{ @tagName(self.tokenType), self.lexeme, number });
            }
        } else {
            try std.io.getStdOut().writer().print("{s} {s} {any}\n", .{ @tagName(self.tokenType), self.lexeme, self.literal });
        }
    }
};

const Scanner = struct {
    source: []u8,
    tokens: std.ArrayList(Token),

    current_start: u32,
    current_end: u32,
    line: u32,

    have_error: bool,

    fn init(source: []u8, allocator: std.mem.Allocator) Scanner {
        return .{
            .source = source,
            .tokens = std.ArrayList(Token).init(allocator),
            .current_start = 0,
            .current_end = 0,
            .line = 1,
            .have_error = false,
        };
    }

    fn advance(self: *Scanner) void {
        self.current_end += 1;
    }

    fn reset_start(self: *Scanner) void {
        self.current_start = self.current_end;
    }

    fn addToken(self: *Scanner, tokenType: TokenType, is_literal: bool) void {
        const str = self.source[self.current_start .. self.current_end + 1];
        const token = Token{
            .tokenType = tokenType,
            .lexeme = str,
            .literal = if (is_literal) str else null,
        };

        self.tokens.append(token) catch unreachable;
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

    fn match(self: *Scanner) void {
        const char = self.source[self.current_end];

        switch (char) {
            '\n' => {
                self.line += 1;
            },
            ' ' => {},
            '\t' => {},
            '(' => self.addToken(TokenType.LEFT_PAREN, false),
            ')' => self.addToken(TokenType.RIGHT_PAREN, false),
            '{' => self.addToken(TokenType.LEFT_BRACE, false),
            '}' => self.addToken(TokenType.RIGHT_BRACE, false),
            ';' => self.addToken(TokenType.SEMICOLON, false),
            ',' => self.addToken(TokenType.COMMA, false),
            '.' => self.addToken(TokenType.DOT, false),
            '+' => self.addToken(TokenType.PLUS, false),
            '-' => self.addToken(TokenType.MINUS, false),
            '*' => self.addToken(TokenType.STAR, false),
            '"' => {
                while (self.peek() != '"' and self.peek() != '\n' and !self.isEnd()) {
                    self.advance();
                }

                if (self.peek() == '"') {
                    self.current_start += 1;
                    self.addToken(TokenType.STRING, true);
                    self.advance();
                } else {
                    std.debug.print("[line {d}] Error: Unterminated string.\n", .{self.line});
                    self.have_error = true;
                }
            },
            '0'...'9' => {
                while (std.ascii.isDigit(self.peek()) or self.peek() == '.') {
                    self.advance();
                }

                self.addToken(TokenType.NUMBER, true);
            },
            '=' => {
                const next_char = self.peek();
                if (next_char == '=') {
                    self.advance();
                    self.addToken(TokenType.EQUAL_EQUAL, false);
                } else {
                    self.addToken(TokenType.EQUAL, false);
                }
            },
            '!' => {
                const next_char = self.peek();
                if (next_char == '=') {
                    self.advance();
                    self.addToken(TokenType.BANG_EQUAL, false);
                } else {
                    self.addToken(TokenType.BANG, false);
                }
            },
            '>' => {
                const next_char = self.peek();
                if (next_char == '=') {
                    self.advance();
                    self.addToken(TokenType.GREATER_EQUAL, false);
                } else {
                    self.addToken(TokenType.GREATER, false);
                }
            },
            '<' => {
                const next_char = self.peek();
                if (next_char == '=') {
                    self.advance();
                    self.addToken(TokenType.LESS_EQUAL, false);
                } else {
                    self.addToken(TokenType.LESS, false);
                }
            },
            '/' => {
                const next_char = self.peek();
                if (next_char == '/') {
                    while (!self.isEnd() and self.peek() != '\n') {
                        self.advance();
                    }
                } else {
                    self.addToken(TokenType.SLASH, false);
                }
            },
            0 => self.addToken(TokenType.EOF, false),
            else => {
                std.debug.print("[line {d}] Error: Unexpected character: {c}\n", .{ self.line, char });
                self.have_error = true;
            },
        }
    }

    fn isEnd(self: Scanner) bool {
        return self.current_end >= self.source.len;
    }

    fn scan(self: *Scanner) void {
        while (!self.isEnd()) {
            self.match();
            self.advance();
            self.reset_start();
        }

        self.tokens.append(Token{
            .tokenType = TokenType.EOF,
            .lexeme = "",
            .literal = null,
        }) catch unreachable;
    }
};

pub fn main() !void {
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: ./your_program.sh tokenize <filename>\n", .{});
        std.process.exit(1);
    }

    const command = args[1];
    const filename = args[2];

    if (!std.mem.eql(u8, command, "tokenize")) {
        std.debug.print("Unknown command: {s}\n", .{command});
        std.process.exit(0);
    }

    const file_contents = try std.fs.cwd().readFileAlloc(std.heap.page_allocator, filename, std.math.maxInt(usize));
    defer std.heap.page_allocator.free(file_contents);

    var scanner = Scanner.init(file_contents, std.heap.page_allocator);
    scanner.scan();

    for (scanner.tokens.items) |token| {
        try token.print();
    }

    if (scanner.have_error) {
        std.process.exit(65);
    }
}
