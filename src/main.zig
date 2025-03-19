const std = @import("std");
const scan = @import("scan.zig");
const parse = @import("parse.zig");

const Command = enum {
    Tokenize,
    Parse,
};

const commands = std.StaticStringMap(Command).initComptime(.{
    .{ "tokenize", .Tokenize },
    .{ "parse", .Parse },
});

pub fn main() !void {
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: ./your_program.sh tokenize <filename>\n", .{});
        std.process.exit(1);
    }

    const filename = args[2];
    const command = commands.get(args[1]);
    if (command == null) {
        std.debug.print("Unknown command: {s}\n", .{args[1]});
        std.process.exit(1);
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    const file_contents = try std.fs.cwd().readFileAlloc(std.heap.page_allocator, filename, std.math.maxInt(usize));
    defer std.heap.page_allocator.free(file_contents);

    const tokens = try scanTokens(alloc, file_contents, command == Command.Tokenize);
    if (command == Command.Tokenize) {
        return;
    }

    var parser = try parse.Parser.init(tokens, alloc);
    try parser.parse();
}

fn scanTokens(alloc: std.mem.Allocator, file_contents: []u8, should_print: bool) ![]scan.Token {
    var tokens = std.ArrayList(scan.Token).init(alloc);
    defer tokens.deinit();

    var scanner = scan.Scanner.init(file_contents);
    while (true) {
        const scan_result = scanner.scanToken();

        switch (scan_result) {
            .none => {},
            .scan_error => {
                std.debug.print("[line {d}] Error: {s}\n", .{ scanner.line, scan_result.scan_error.message });
                break;
            },
            .token => |token| {
                if (should_print) {
                    try token.print();
                }

                try tokens.append(token);
                if (token.tokenType == scan.TokenType.EOF) {
                    break;
                }
            },
        }

        scanner.prepare_next_token();
    }

    return tokens.toOwnedSlice();
}
