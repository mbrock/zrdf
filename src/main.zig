const std = @import("std");
const turtle = @import("turtle.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var state = turtle.ParserState.init(allocator);
    // No need to defer state.deinit() since arena will free everything

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
    _ = try turtle.parseTurtleDocument(&state, turtle_doc);

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

fn printTerm(writer: anytype, term: turtle.Term) !void {
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
    defer list.deinit();
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
