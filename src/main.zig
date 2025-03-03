const std = @import("std");

const TokenType = enum {
    VAR, // var
    IDENTIFIER, // variable names
    EQUAL, // =
    STRING, // "string"
    LEFT_PAREN, // (
    RIGHT_PAREN, // )
    SEMICOLON, // ;
    EOF,
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

const MyErrors = error{
    TokenNotFound,
};

fn match(i: u8) MyErrors!Token {
    switch (i) {
        '(' => {
            return LParenToken;
        },
        ')' => {
            return RParenToken;
        },
        0 => {
            return EOFToken;
        },
        else => {
            return MyErrors.TokenNotFound;
        },
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

    if (file_contents.len > 0) {
        for (file_contents) |i| {
            if (i == '\n') {
                continue;
            }
            const token = try match(i);
            try printToken(token);
        }
    } else {
        // try std.io.getStdOut().writer().print("EOF  nulln", .{});
    }

    try printToken(EOFToken);
}
