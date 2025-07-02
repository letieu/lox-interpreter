const std = @import("std");
const scan = @import("scan.zig");
const parse = @import("parse.zig");
const astPrint = @import("ast-print.zig");
const evaluate = @import("evaluate.zig");

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

pub fn printEvalError(e: evaluate.EvalError, errorLine: *const usize) !void {
    const writer = std.io.getStdErr().writer();
    switch (e) {
        error.AllocationError => try writer.print("Allocation Error.\n", .{}),
        error.NotANumber => try writer.print("Operand must be a number.\n", .{}),
        error.Invalid => try writer.print("Invalid.\n", .{}),
    }
    try writer.print("[line {d}]", .{errorLine.*});
}

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
    const statements = parser.parse() catch {
        std.process.exit(65);
    };
    if (command == Command.Parse) {
        try printAst(statements);
        return;
    }

    for (statements) |stmt| {
        var errorLine: usize = 0;
        const expr = switch (stmt) {
            .Print => stmt.Print.expr,
            .Expression => stmt.Expression.expr,
        };
        const result = evaluate.evaluate(&expr, &errorLine) catch |e| {
            try printEvalError(e, &errorLine);
            std.process.exit(70);
            return;
        };

        if (stmt == .Print) {
            switch (result) {
                .boolean => try std.io.getStdOut().writer().print("{?}", .{result.boolean}),
                .number => try std.io.getStdOut().writer().print("{d}", .{result.number}),
                .string => try std.io.getStdOut().writer().print("{s}", .{result.string}),
                .nil => try std.io.getStdOut().writer().print("nil", .{}),
            }

            try std.io.getStdOut().writer().print("\n", .{});
        }
    }

    return;
}
