const std = @import("std");
const turtle = @import("turtle.zig");

pub fn parseTurtleFile(allocator: std.mem.Allocator, file_path: []const u8) !turtle.Graph {
    // Open and map file
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const mapped_mem = try std.posix.mmap(
        null,
        file_size,
        std.posix.PROT.READ,
        .{ .TYPE = .PRIVATE },
        file.handle,
        0,
    );
    defer std.posix.munmap(mapped_mem);

    // Initialize parser state
    var state = turtle.ParserState.init(allocator);
    defer state.deinit();

    // Parse document
    if (turtle.parseTurtleDocument(&state, mapped_mem)) |result| {
        _ = result;
        return state.graph;
    } else |err| {
        return err;
    }
}

pub fn main() !void {
    // Get args
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len != 2) {
        const stderr = std.io.getStdErr().writer();
        try stderr.writeAll("Usage: turtle-parser <file.ttl>\n");
        std.process.exit(1);
    }

    const file_path = args[1];

    // Open and map file
    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const mapped_mem = try std.posix.mmap(
        null,
        file_size,
        std.posix.PROT.READ,
        .{ .TYPE = .PRIVATE },
        file.handle,
        0,
    );
    defer std.posix.munmap(mapped_mem);

    // Initialize parser state
    var state = turtle.ParserState.init(allocator);
    defer state.deinit();
    defer state.graph.deinit();

    // Parse document
    if (turtle.parseTurtleDocument(&state, mapped_mem)) |result| {
        _ = result; // autofix
    } else |err| {
        const stdout = std.io.getStdOut().writer();
        try stdout.print("error: {}\n", .{err});
        try stdout.print("line: {d}\n", .{turtle.line_count});

        // Print context around error
        try printErrorContext(stdout, mapped_mem, turtle.line_count, allocator);
    }

    // Print parsed triples
    const stdout = std.io.getStdOut().writer();
    try stdout.print("parsed {d} triples\n", .{state.graph.triples.items.len});

    for (state.graph.triples.items) |statement| {
        try state.graph.printTriple(stdout, statement);
    }
}

fn printTerm(writer: anytype, graph: *turtle.Graph, term: turtle.Term) !void {
    switch (term) {
        .uri => |u| {
            const uri = try graph.getURI(u);
            try writer.print("<{s}>", .{uri.str});
        },
        .bno => |b| try writer.print("_:{s}", .{b.str}),
        .lit => |l| {
            if (l.datatype) |dt| {
                try writer.print("\"{s}\"^^<{s}>", .{ l.value, dt.str });
            } else if (l.lang) |lang| {
                try writer.print("\"{s}\"@{s}", .{ l.value, lang });
            } else {
                try writer.print("\"{s}\"", .{l.value});
            }
        },
    }
}

fn printErrorContext(
    writer: anytype,
    content: []const u8,
    error_line: usize,
    allocator: std.mem.Allocator,
) !void {
    var current_line: usize = 1;
    var line_starts = std.ArrayList(usize).init(allocator);
    defer line_starts.deinit();

    try line_starts.append(0);

    // Find all line starts
    for (content, 0..) |c, i| {
        if (c == '\n') {
            current_line += 1;
            try line_starts.append(i + 1);
        }
    }

    if (line_starts.items.len == 0) {
        try writer.print("(Single line file)\n", .{});
        return;
    }

    // Print context (10 lines before and after)
    const start_line = if (error_line > 10) error_line - 10 else 1;
    const end_line = @min(
        error_line + 10,
        line_starts.items.len,
    );

    for (start_line..end_line + 1) |line_num| {
        if (line_num >= line_starts.items.len) break;

        const line_start = line_starts.items[line_num - 1];
        const line_end = if (line_num < line_starts.items.len)
            line_starts.items[line_num]
        else
            content.len;

        const prefix = if (line_num == error_line) ">" else " ";
        try writer.print("{s}{d:4}| {s}\n", .{
            prefix,
            line_num,
            content[line_start..line_end],
        });
    }
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit();
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
