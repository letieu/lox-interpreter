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
    STAR, // *
    EOF,
    EQUAL_EQUAL, // ==
    BANG, // !
    BANG_EQUAL, // !=
};

const Token = struct {
    tokenType: TokenType, // VAR
    lexeme: []const u8, // 123
    literal: ?[]u8, // 123
};

const EOFToken = Token{
    .tokenType = TokenType.EOF,
    .lexeme = "",
    .literal = null,
};

const LParenToken = Token{
    .tokenType = TokenType.LEFT_PAREN,
    .lexeme = "(",
    .literal = null,
};

const RParenToken = Token{
    .tokenType = TokenType.RIGHT_PAREN,
    .lexeme = ")",
    .literal = null,
};

const LBraceToken = Token{
    .tokenType = TokenType.LEFT_BRACE,
    .lexeme = "{",
    .literal = null,
};

const RBraceToken = Token{
    .tokenType = TokenType.RIGHT_BRACE,
    .lexeme = "}",
    .literal = null,
};

const CommaToken = Token{
    .tokenType = TokenType.COMMA,
    .lexeme = ",",
    .literal = null,
};

const DotToken = Token{
    .tokenType = TokenType.DOT,
    .lexeme = ".",
    .literal = null,
};

const MinusToken = Token{
    .tokenType = TokenType.MINUS,
    .lexeme = "-",
    .literal = null,
};

const PlusToken = Token{
    .tokenType = TokenType.PLUS,
    .lexeme = "+",
    .literal = null,
};

const StarToken = Token{
    .tokenType = TokenType.STAR,
    .lexeme = "*",
    .literal = null,
};

const SemiColonToken = Token{
    .tokenType = TokenType.SEMICOLON,
    .lexeme = ";",
    .literal = null,
};

const EquaToken = Token{
    .tokenType = TokenType.EQUAL,
    .lexeme = "=",
    .literal = null,
};

const EqualEqualToken = Token{
    .tokenType = TokenType.EQUAL_EQUAL,
    .lexeme = "==",
    .literal = null,
};

const BangToken = Token{
    .tokenType = TokenType.BANG,
    .lexeme = "!",
    .literal = null,
};

const BangEqualToken = Token{
    .tokenType = TokenType.BANG_EQUAL,
    .lexeme = "!=",
    .literal = null,
};

const MyErrors = error{
    TokenNotFound,
};

fn match(char: u8, index: usize, file_contents: []const u8) !Token {
    switch (char) {
        '(' => return LParenToken,
        ')' => return RParenToken,
        '{' => return LBraceToken,
        '}' => return RBraceToken,
        ';' => return SemiColonToken,
        ',' => return CommaToken,
        '.' => return DotToken,
        '+' => return PlusToken,
        '-' => return MinusToken,
        '*' => return StarToken,
        '=' => {
            if (file_contents.len > index + 1 and file_contents[index + 1] == '=') {
                return EqualEqualToken;
            }
            return EquaToken;
        },
        '!' => {
            if (file_contents.len > index + 1 and file_contents[index + 1] == '=') {
                return BangEqualToken;
            }
            return BangToken;
        },
        0 => return EOFToken,
        else => return MyErrors.TokenNotFound,
    }
}

fn printToken(token: Token) !void {
    try std.io.getStdOut().writer().print("{s} {s} {any}\n", .{ @tagName(token.tokenType), token.lexeme, token.literal });
}

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

    var exit_code: u8 = 0;

    if (file_contents.len == 0) {
        try printToken(EOFToken);
        std.process.exit(exit_code);
    }

    var line_number: usize = 1;
    var index: usize = 0;

    while (index < file_contents.len) {
        const char = file_contents[index];

        if (char == '\n') {
            line_number += 1;
            index += 1;
            continue;
        }

        const token = match(char, index, file_contents) catch {
            std.debug.print("[line {d}] Error: Unexpected character: {c}\n", .{ line_number, char });
            exit_code = 65;
            index += 1;
            continue;
        };

        try printToken(token);

        if (token.tokenType == TokenType.EOF) {
            break;
        }

        index += token.lexeme.len;
    }

    try printToken(EOFToken);
    std.process.exit(exit_code);
}
