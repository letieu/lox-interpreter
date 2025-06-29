const std = @import("std");
const scan = @import("scan.zig");
const parse = @import("parse.zig");
const astPrint = @import("ast-print.zig");

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

    const tokens = scan.scanTokens(alloc, file_contents, command == Command.Tokenize) catch {
        std.process.exit(65);
    };

    if (command == Command.Tokenize) {
        return;
    }

    var parser = try parse.Parser.init(tokens, alloc);
    const expr = parser.parseExpression();

    const printer = astPrint.AstPrinter.init();
    try printer.printExpression(&expr);
}
