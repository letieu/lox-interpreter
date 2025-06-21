const std = @import("std");

const scan = @import("scan.zig");

pub const Statement = struct {
    token: scan.Token,

    pub fn print(self: Statement) !void {
        const writer = std.io.getStdOut().writer();
        try writer.print("{s}\n", .{self.token.lexeme});
    }
};

pub const Parser = struct {
    alloc: std.mem.Allocator,
    statements: std.ArrayList(Statement),

    pub fn init(
        tokens: []scan.Token,
        alloc: std.mem.Allocator,
    ) !Parser {
        var statements = std.ArrayList(Statement).init(alloc);

        for (tokens) |token| {
            if (token.tokenType == scan.TokenType.EOF) {
                continue;
            }

            const statement = Statement{
                .token = token,
            };

            try statements.append(statement);
        }

        return .{
            .alloc = alloc,
            .statements = statements,
        };
    }

    pub fn parse(self: *Parser) !void {
        for (self.statements.items) |item| {
            switch (item.token.tokenType) {
                .STRING => {
                    try std.io.getStdOut().writer().print("{s}\n", .{item.token.literal.?.string});
                },
                .NUMBER => {
                    if (@rem(item.token.literal.?.number, 1.0) == 0.0) {
                        try std.io.getStdOut().writer().print("{d:.1}\n", .{item.token.literal.?.number});
                    } else {
                        try std.io.getStdOut().writer().print("{d}\n", .{item.token.literal.?.number});
                    }
                },
                else => {
                    try std.io.getStdOut().writer().print("{s}\n", .{item.token.lexeme});
                },
            }
        }
        return;
    }
};
