const std = @import("std");

const TokenType = enum {
    VAR, // var
    IDENTIFIER, // variable names
    EQUAL, // =
    STRING, // "string"
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
        try std.io.getStdOut().writer().print("{s} {s} {any}\n", .{ @tagName(self.tokenType), self.lexeme, self.literal });
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
            .line = 0,
            .have_error = false,
        };
    }

    fn advance(self: *Scanner) void {
        self.current_end += 1;
    }

    fn reset_start(self: *Scanner) void {
        self.current_start = self.current_end + 1;
    }

    fn addToken(self: *Scanner, tokenType: TokenType) void {
        const str = self.source[self.current_start .. self.current_end + 1];
        const token = Token{
            .tokenType = tokenType,
            .lexeme = str,
            .literal = null, // 123
        };

        self.tokens.append(token) catch unreachable;
        self.reset_start();
    }

    fn peek(self: Scanner) u8 {
        if (self.isEnd()) {
            return 0;
        }

        return self.source[self.current_end + 1];
    }

    fn match(self: *Scanner) void {
        const char = self.source[self.current_end];

        switch (char) {
            '\n' => {
                self.reset_start();
                self.line += 1;
            },
            '(' => self.addToken(TokenType.LEFT_PAREN),
            ')' => self.addToken(TokenType.RIGHT_PAREN),
            '{' => self.addToken(TokenType.LEFT_BRACE),
            '}' => self.addToken(TokenType.RIGHT_BRACE),
            ';' => self.addToken(TokenType.SEMICOLON),
            ',' => self.addToken(TokenType.COMMA),
            '.' => self.addToken(TokenType.DOT),
            '+' => self.addToken(TokenType.PLUS),
            '-' => self.addToken(TokenType.MINUS),
            '*' => self.addToken(TokenType.STAR),
            '=' => {
                const next_char = self.peek();
                if (next_char == '=') {
                    self.advance();
                    self.addToken(TokenType.EQUAL_EQUAL);
                } else {
                    self.addToken(TokenType.EQUAL);
                }
            },
            '!' => {
                const next_char = self.peek();
                if (next_char == '=') {
                    self.advance();
                    self.addToken(TokenType.BANG_EQUAL);
                } else {
                    self.addToken(TokenType.BANG);
                }
            },
            '>' => {
                const next_char = self.peek();
                if (next_char == '=') {
                    self.advance();
                    self.addToken(TokenType.GREATER_EQUAL);
                } else {
                    self.addToken(TokenType.GREATER);
                }
            },
            '<' => {
                const next_char = self.peek();
                if (next_char == '=') {
                    self.advance();
                    self.addToken(TokenType.LESS_EQUAL);
                } else {
                    self.addToken(TokenType.LESS);
                }
            },
            '/' => {
                const next_char = self.peek();
                if (next_char == '/') {
                    while (self.peek() != '\n' and !self.isEnd()) {
                        self.advance();
                    }

                    self.reset_start();
                } else {
                    self.addToken(TokenType.SLASH);
                }
            },
            0 => self.addToken(TokenType.EOF),
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
        }
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
        std.process.exit(1);
    }

    const file_contents = try std.fs.cwd().readFileAlloc(std.heap.page_allocator, filename, std.math.maxInt(usize));
    defer std.heap.page_allocator.free(file_contents);

    if (file_contents.len == 0) {
        std.process.exit(1);
    }

    var scanner = Scanner.init(file_contents, std.heap.page_allocator);
    scanner.scan();

    for (scanner.tokens.items) |token| {
        try token.print();
    }
}
