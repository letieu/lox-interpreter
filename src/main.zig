const std = @import("std");
const scan = @import("scan.zig");
const parse = @import("parse.zig");
const astPrint = @import("ast-print.zig");
const evaluate = @import("evaluate.zig");
const Interpreter = @import("interpreter.zig").Intepreter;

const Command = enum {
    Tokenize,
    Parse,
    Evaluate,
    Run,
};

const commands = std.StaticStringMap(Command).initComptime(.{
    .{ "tokenize", .Tokenize },
    .{ "parse", .Parse },
    .{ "evaluate", .Evaluate },
    .{ "run", .Run },
});

pub fn printAst(statements: []parse.Statement) !void {
    const printer = astPrint.AstPrinter.init();

    for (statements) |statement| {
        try printer.printStatement(&statement);
    }
    return;
}

pub fn main() !void {
    const args = try std.process.argsAlloc(std.heap.page_allocator);
    defer std.process.argsFree(std.heap.page_allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: ./your_program.sh tokenize <filename>\n", .{});
        std.process.exit(1);
    }

    const stdOut = std.io.getStdOut();
    const stdErr = std.io.getStdErr();

    const filename = args[2];
    const command = commands.get(args[1]);
    if (command == null) {
        try stdOut.writer().print("Unknown command: {s}\n", .{args[1]});
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

    var parser = try parse.Parser.init(tokens, alloc, stdOut, stdErr);
    const statements = parser.parse() catch {
        std.process.exit(65);
    };
    if (command == Command.Parse) {
        try printAst(statements);
        return;
    }

    var interpreter = Interpreter.init(statements, alloc, stdOut, stdErr);
    try interpreter.run();

    return;
}
