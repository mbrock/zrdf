const std = @import("std");
const root = @import("root.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var state = root.ParserState.init(allocator);
    defer state.deinit();

    // Example Turtle document
    const turtle_doc =
        \\@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        \\@prefix foaf: <http://xmlns.com/foaf/0.1/> .
        \\@prefix ex: <http://example.org/> .
        \\
        \\ex:john a foaf:Person ;
        \\    foaf:name "John Smith" ;
        \\    foaf:age 42 ;
        \\    foaf:knows (
        \\        ex:jane
        \\        ex:bob
        \\    ) .
    ;

    // Parse document
    var rest = turtle_doc;
    while (rest.len > 0) {
        rest = std.mem.trimLeft(u8, rest, std.ascii.whitespace);
        if (rest.len == 0) break;

        const result = try root.parseStatement(&state, rest);
        rest = result.rest;
    }

    // Print parsed triples
    const stdout = std.io.getStdOut().writer();
    try stdout.print("\nParsed {d} triples:\n", .{state.graph.statements.items.len});

    for (state.graph.statements.items) |statement| {
        try printTerm(stdout, statement.subject);
        try stdout.writeAll(" ");
        try printTerm(stdout, statement.predicate);
        try stdout.writeAll(" ");
        try printTerm(stdout, statement.object);
        try stdout.writeAll(" .\n");
    }
}

fn printTerm(writer: anytype, term: root.Term) !void {
    switch (term) {
        .uri => |u| try writer.print("<{s}>", .{u.url}),
        .bnode => |b| try writer.print("_:{s}", .{b.id}),
        .lit => |l| {
            if (l.datatype) |dt| {
                try writer.print("\"{s}\"^^<{s}>", .{ l.value, dt.url });
            } else if (l.lang) |lang| {
                try writer.print("\"{s}\"@{s}", .{ l.value, lang });
            } else {
                try writer.print("\"{s}\"", .{l.value});
            }
        },
    }
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
