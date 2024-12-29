const std = @import("std");

pub const ParseError = error{
    MissingOpenBracket,
    MissingCloseBracket,
    EmptyURL,
    InvalidPrefix,
    InvalidBase,
    InvalidStatement,
    InvalidPredicate,
    InvalidObject,
    UnexpectedEndOfInput,
    InvalidTriple,
    InvalidLiteral,
    InvalidIRI,
    InvalidBlankNode,
    InvalidPredicateObjectList,
    InvalidLanguageTag,
    InvalidNumber,
    InvalidBoolean,
    InvalidString,
    InvalidPrefixedName,
    InvalidCollection,
    OutOfMemory,
};

pub const URI = struct {
    str: []const u8,
};

pub const Term = union(enum) {
    uri: u28,
    bno: BNode,
    lit: Literal,
};

pub const BNode = struct {
    str: []const u8,
};

pub const Literal = struct {
    value: []const u8,
    lang: ?[]const u8,
    datatype: ?URI,
};

pub const Triple = struct {
    s: Term,
    p: Term,
    o: Term,
};

pub const Graph = struct {
    triples: std.ArrayList(Triple),
    uris: std.ArrayList(URI),
    uri_map: std.StringHashMap(u28),

    pub fn init(allocator: std.mem.Allocator) Graph {
        return .{
            .triples = std.ArrayList(Triple).init(allocator),
            .uris = std.ArrayList(URI).init(allocator),
            .uri_map = std.StringHashMap(u28).init(allocator),
        };
    }

    pub fn deinit(self: *Graph) void {
        self.triples.deinit();
        self.uris.deinit();
        self.uri_map.deinit();
    }

    pub fn intern(self: *Graph, uri: []const u8) !u28 {
        // Check if URI already exists in hash map
        if (self.uri_map.get(uri)) |id| {
            return id;
        }

        // If not found, add to list and map
        const id: u28 = @intCast(self.uris.items.len);
        try self.uris.append(URI{ .str = uri });
        try self.uri_map.put(uri, id);
        return id;
    }

    pub fn getURI(self: *Graph, id: u28) !URI {
        return self.uris.items[id];
    }

    pub fn getURIString(self: *Graph, id: u28) ![]const u8 {
        return self.uris.items[id].str;
    }

    pub fn printTerm(self: *Graph, stdout: anytype, term: Term) !void {
        switch (term) {
            .uri => |uri| stdout.print("<{s}>", .{self.getURIString(uri) catch return}) catch {},
            .bno => |bnode| stdout.print("_:{s}", .{bnode.str}) catch {},
            .lit => |lit| stdout.print("\"{s}\"", .{lit.value}) catch {},
        }
    }

    pub fn printTriple(self: *Graph, stdout: anytype, triple: Triple) !void {
        try self.printTerm(stdout, triple.s);
        try stdout.print(" ", .{});
        try self.printTerm(stdout, triple.p);
        try stdout.print(" ", .{});
        try self.printTerm(stdout, triple.o);
        try stdout.print(" .\n", .{});
    }
};

pub const ParserState = struct {
    base_uri: ?URI,
    prefixes: std.StringHashMap(URI),
    bnode_labels: std.StringHashMap(BNode),
    cur_subject: ?Term,
    cur_predicate: ?Term,
    allocator: std.mem.Allocator,
    graph: Graph,

    pub fn initWithGraph(allocator: std.mem.Allocator, graph: Graph) ParserState {
        return .{
            .base_uri = null,
            .prefixes = std.StringHashMap(URI).init(allocator),
            .bnode_labels = std.StringHashMap(BNode).init(allocator),
            .cur_subject = null,
            .cur_predicate = null,
            .allocator = allocator,
            .graph = graph,
        };
    }

    pub fn init(allocator: std.mem.Allocator) ParserState {
        return .{
            .base_uri = null,
            .prefixes = std.StringHashMap(URI).init(allocator),
            .bnode_labels = std.StringHashMap(BNode).init(allocator),
            .cur_subject = null,
            .cur_predicate = null,
            .allocator = allocator,
            .graph = Graph.init(allocator),
        };
    }

    pub fn deinit(self: *ParserState) void {
        self.prefixes.deinit();
        self.bnode_labels.deinit();
    }

    pub fn emitTriple(self: *ParserState, object: Term) !void {
        if (self.cur_subject == null or self.cur_predicate == null) {
            return ParseError.InvalidTriple;
        }

        try self.graph.triples.append(Triple{
            .s = self.cur_subject.?,
            .p = self.cur_predicate.?,
            .o = object,
        });
    }
};

pub fn ParseResult(T: type) type {
    return struct {
        value: T,
        rest: []const u8,
    };
}

pub var line_count: usize = 0;

/// Convenience function for trimming whitespace off the front of an input slice.
/// Returns the trimmed slice.
fn consumeWhitespace(input: []const u8) []const u8 {
    var i: usize = 0;
    while (i < input.len and std.ascii.isWhitespace(input[i])) : (i += 1) {
        if (input[i] == '\n') {
            line_count += 1;
        }
    }
    return input[i..];
}

/// Helper to skip a single token (like '.', ',', ';'), trimming whitespace after it.
/// Returns the trimmed slice.
fn consumeSingleToken(input: []const u8, token: u8) ParseError![]const u8 {
    var rest = consumeWhitespace(input);
    if (rest.len == 0 or rest[0] != token) {
        return ParseError.InvalidStatement;
    }
    return consumeWhitespace(rest[1..]);
}

/// Helper to read a bracketed IRI like `<http://...>`
/// and return the URL plus the remaining input.
pub fn readBracketedURI(input: []const u8) ParseError!ParseResult(URI) {
    var rest = consumeWhitespace(input);

    if (rest.len < 2) return ParseError.EmptyURL;
    if (rest[0] != '<') return ParseError.MissingOpenBracket;

    var pos: usize = 1;
    while (pos < rest.len) {
        if (rest[pos] == '>') {
            if (pos == 1) return ParseError.EmptyURL; // empty < >
            return ParseResult(URI){
                .value = URI{ .str = rest[1..pos] },
                .rest = rest[pos + 1 ..],
            };
        }
        pos += 1;
    }
    return ParseError.MissingCloseBracket;
}

fn parseEndOfStatement(input: []const u8, T: type, value: T) ParseError!ParseResult(T) {
    var rest = consumeWhitespace(input);
    rest = try consumeSingleToken(rest, '.');
    return ParseResult(T){ .value = value, .rest = rest };
}

fn parseDirective(state: *ParserState, input: []const u8) ParseError!ParseResult(void) {
    const rest = consumeWhitespace(input);

    if (parsePrefix(state, rest)) |result| {
        return try parseEndOfStatement(result.rest, void, {});
    } else |_| {
        if (parseBase(state, rest)) |result| {
            return try parseEndOfStatement(result.rest, void, {});
        } else |_| {
            return ParseError.InvalidStatement;
        }
    }
}

fn parseExactString(input: []const u8, expected: []const u8, err: ParseError) ParseError![]const u8 {
    var rest = consumeWhitespace(input);
    if (!std.mem.startsWith(u8, rest, expected)) {
        return err;
    }
    return consumeWhitespace(rest[expected.len..]);
}

pub fn parseBase(state: *ParserState, input: []const u8) ParseError!ParseResult(void) {
    const rest = try parseExactString(input, "@base", ParseError.InvalidBase);

    const iri = try readBracketedURI(rest);
    state.base_uri = iri.value;

    return ParseResult(void){
        .value = {},
        .rest = iri.rest,
    };
}

pub fn parsePrefix(state: *ParserState, input: []const u8) ParseError!ParseResult(void) {
    var rest = try parseExactString(input, "@prefix", ParseError.InvalidPrefix);

    // Parse prefix name until ':'
    const colon_pos = std.mem.indexOf(u8, rest, ":");
    if (colon_pos == null) return ParseError.InvalidPrefix;

    const prefix_name = rest[0..colon_pos.?];
    rest = consumeWhitespace(rest[colon_pos.? + 1 ..]);

    // Parse bracketed IRI
    const iri = try readBracketedURI(rest);
    try state.prefixes.put(prefix_name, iri.value);

    return ParseResult(void){
        .value = {},
        .rest = iri.rest,
    };
}

// Blank node parsing
pub fn parseBlankNode(state: *ParserState, input: []const u8) ParseError!ParseResult(Term) {
    var rest = consumeWhitespace(input);
    if (!std.mem.startsWith(u8, rest, "_:")) {
        return ParseError.InvalidBlankNode;
    }

    rest = rest[2..];
    var end: usize = 0;
    while (end < rest.len and !std.ascii.isWhitespace(rest[end]) and !isTurtleDelimiter(rest[end])) {
        end += 1;
    }

    if (end == 0) return ParseError.InvalidBlankNode;

    const label = rest[0..end];
    rest = rest[end..];

    if (state.bnode_labels.get(label)) |existing| {
        return ParseResult(Term){
            .value = Term{ .bno = existing },
            .rest = rest,
        };
    }

    // Create new blank node
    const bnode = BNode{ .str = label };
    try state.bnode_labels.put(label, bnode);

    return ParseResult(Term){
        .value = Term{ .bno = bnode },
        .rest = rest,
    };
}

/// Check if a given character is a Turtle delimiter (commas, semicolons, parentheses, etc.)
fn isTurtleDelimiter(c: u8) bool {
    return switch (c) {
        '.', ',', ';', '(', ')', '[', ']' => true,
        else => false,
    };
}

// Factor out for parsePredicateObjectList
pub fn parsePredicateObjectList(state: *ParserState, input: []const u8) ParseError!ParseResult(void) {
    var rest = consumeWhitespace(input);

    while (true) {
        // Parse predicate
        const pred = try parsePredicate(state, rest);
        rest = consumeWhitespace(pred.rest);
        state.cur_predicate = pred.value;

        // Parse object list
        const obj_list = try parseObjectList(state, rest);
        rest = consumeWhitespace(obj_list.rest);

        // Check for another predicate-object pair
        if (rest.len > 0 and rest[0] == ';') {
            rest = consumeWhitespace(rest[1..]);
            if (rest.len > 0 and rest[0] != '.') {
                continue;
            }
        }
        break;
    }

    return ParseResult(void){
        .value = {},
        .rest = rest,
    };
}

pub fn parsePredicate(state: *ParserState, input: []const u8) ParseError!ParseResult(Term) {
    var rest = consumeWhitespace(input);
    if (rest.len == 0) return ParseError.UnexpectedEndOfInput;

    // rdf:type shorthand
    if (rest[0] == 'a' and
        (rest.len == 1 or std.ascii.isWhitespace(rest[1]) or isTurtleDelimiter(rest[1])))
    {
        const rdf_type = try state.graph.intern("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");
        return ParseResult(Term){ .value = Term{ .uri = rdf_type }, .rest = rest[1..] };
    }

    // Bracketed IRI
    if (rest[0] == '<') {
        const iri = try readBracketedURI(rest);
        return ParseResult(Term){
            .value = Term{ .uri = try state.graph.intern(iri.value.str) },
            .rest = iri.rest,
        };
    }

    // Prefixed name
    const prefixed = try parsePrefixedName(state, rest);
    return ParseResult(Term){
        .value = Term{ .uri = try state.graph.intern(prefixed.value.str) },
        .rest = prefixed.rest,
    };
}

pub fn parseObjectList(state: *ParserState, input: []const u8) ParseError!ParseResult(void) {
    var rest = consumeWhitespace(input);

    while (true) {
        // Parse object
        const obj = try parseObject(state, rest);
        rest = consumeWhitespace(obj.rest);

        // Emit triple
        try state.emitTriple(obj.value);

        // Check for another object
        if (rest.len > 0 and rest[0] == ',') {
            rest = consumeWhitespace(rest[1..]);
            continue;
        }
        break;
    }

    return ParseResult(void){
        .value = {},
        .rest = rest,
    };
}

pub fn parseObject(state: *ParserState, input: []const u8) ParseError!ParseResult(Term) {
    const rest = consumeWhitespace(input);
    if (rest.len == 0) return ParseError.UnexpectedEndOfInput;

    // Collection
    if (rest[0] == '(') {
        return parseCollection(state, rest);
    }
    // Blank node property list
    if (rest[0] == '[') {
        return parseBlankNodePropertyList(state, rest);
    }
    // IRI
    if (rest[0] == '<') {
        const iri = try readBracketedURI(rest);
        return ParseResult(Term){
            .value = Term{ .uri = try state.graph.intern(iri.value.str) },
            .rest = iri.rest,
        };
    }
    // Blank node
    if (std.mem.startsWith(u8, rest, "_:")) {
        return parseBlankNode(state, rest);
    }
    // Literal (string, numeric, boolean)
    if (rest[0] == '"' or rest[0] == '\'' or
        isDigit(rest[0]) or rest[0] == '+' or rest[0] == '-' or
        std.mem.startsWith(u8, rest, "true") or std.mem.startsWith(u8, rest, "false"))
    {
        return parseLiteral(state, rest);
    }

    // Prefixed name
    const prefixed = try parsePrefixedName(state, rest);
    return ParseResult(Term){
        .value = Term{ .uri = try state.graph.intern(prefixed.value.str) },
        .rest = prefixed.rest,
    };
}

// String literal parsing
pub fn parseString(input: []const u8) ParseError!ParseResult([]const u8) {
    const rest = consumeWhitespace(input);
    if (rest.len < 2) return ParseError.InvalidString;

    if (std.mem.startsWith(u8, rest, "\"\"\"")) {
        return parseLongString(rest, "\"\"\"");
    } else if (std.mem.startsWith(u8, rest, "'''")) {
        return parseLongString(rest, "'''");
    } else if (rest[0] == '"') {
        return parseQuotedString(rest, "\"");
    } else if (rest[0] == '\'') {
        return parseQuotedString(rest, "'");
    }

    return ParseError.InvalidString;
}

fn parseQuotedString(input: []const u8, quote: []const u8) ParseError!ParseResult([]const u8) {
    if (input.len < 2) return ParseError.InvalidString;
    if (!std.mem.startsWith(u8, input, quote)) return ParseError.InvalidString;

    var pos: usize = quote.len; // typically 1
    var escaped = false;
    while (pos < input.len) {
        if (escaped) {
            escaped = false;
            pos += 1;
            continue;
        }
        if (input[pos] == '\\') {
            escaped = true;
            pos += 1;
            continue;
        }
        if (input[pos] == quote[0]) {
            return ParseResult([]const u8){
                .value = input[quote.len..pos],
                .rest = input[pos + 1 ..],
            };
        }
        pos += 1;
    }
    return ParseError.InvalidString;
}

fn parseLongString(input: []const u8, quotes: []const u8) ParseError!ParseResult([]const u8) {
    if (input.len < quotes.len * 2) return ParseError.InvalidString;
    if (!std.mem.startsWith(u8, input, quotes)) return ParseError.InvalidString;

    var pos: usize = quotes.len; // typically 3
    while (pos + quotes.len <= input.len) {
        if (std.mem.startsWith(u8, input[pos..], quotes) and (pos == quotes.len or input[pos - 1] != '\\')) {
            return ParseResult([]const u8){
                .value = input[quotes.len..pos],
                .rest = input[pos + quotes.len ..],
            };
        }
        pos += 1;
    }
    return ParseError.InvalidString;
}

fn parseLanguageTag(input: []const u8) ParseError!ParseResult([]const u8) {
    var rest = consumeWhitespace(input);
    if (rest.len < 2) return ParseError.InvalidLanguageTag;
    if (rest[0] != '@') return ParseError.InvalidLanguageTag;

    var pos: usize = 1;
    while (pos < rest.len and (std.ascii.isAlphabetic(rest[pos]) or rest[pos] == '-')) {
        pos += 1;
    }
    if (pos == 1) return ParseError.InvalidLanguageTag;

    return ParseResult([]const u8){
        .value = rest[1..pos],
        .rest = rest[pos..],
    };
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn parseNumber(input: []const u8) ParseError!ParseResult(Term) {
    var rest = consumeWhitespace(input);

    var pos: usize = 0;
    var has_decimal = false;
    var has_exponent = false;

    // Optional sign
    if (pos < rest.len and (rest[pos] == '+' or rest[pos] == '-')) {
        pos += 1;
    }

    const start_digits = pos;
    // integer part
    while (pos < rest.len and isDigit(rest[pos])) : (pos += 1) {}

    // decimal point
    if (pos < rest.len and rest[pos] == '.') {
        has_decimal = true;
        pos += 1;
        while (pos < rest.len and isDigit(rest[pos])) : (pos += 1) {}
    }

    // exponent
    if (pos < rest.len and (rest[pos] == 'e' or rest[pos] == 'E')) {
        has_exponent = true;
        pos += 1;
        if (pos < rest.len and (rest[pos] == '+' or rest[pos] == '-')) {
            pos += 1;
        }
        var has_exp_digits = false;
        while (pos < rest.len and isDigit(rest[pos])) : (pos += 1) {
            has_exp_digits = true;
        }
        if (!has_exp_digits) return ParseError.InvalidNumber;
    }

    if (pos == start_digits and !has_decimal) {
        return ParseError.InvalidNumber;
    }

    const number = rest[0..pos];
    const datatype = if (has_exponent)
        "http://www.w3.org/2001/XMLSchema#double"
    else if (has_decimal)
        "http://www.w3.org/2001/XMLSchema#decimal"
    else
        "http://www.w3.org/2001/XMLSchema#integer";

    return ParseResult(Term){
        .value = Term{ .lit = .{
            .value = number,
            .lang = null,
            .datatype = URI{ .str = datatype },
        } },
        .rest = rest[pos..],
    };
}

pub fn parseLiteral(state: *ParserState, input: []const u8) ParseError!ParseResult(Term) {
    _ = state; // autofix
    var rest = consumeWhitespace(input);
    if (rest.len == 0) return ParseError.UnexpectedEndOfInput;

    // string literal
    if (rest[0] == '"' or rest[0] == '\'') {
        const str_result = try parseString(rest);
        rest = consumeWhitespace(str_result.rest);

        // optional language tag or datatype
        if (rest.len > 0 and rest[0] == '@') {
            const lang = try parseLanguageTag(rest);
            return ParseResult(Term){
                .value = Term{ .lit = .{
                    .value = str_result.value,
                    .lang = lang.value,
                    .datatype = null,
                } },
                .rest = lang.rest,
            };
        } else if (rest.len > 1 and rest[0] == '^' and rest[1] == '^') {
            rest = rest[2..];
            const dtype = try readBracketedURI(rest);
            return ParseResult(Term){
                .value = Term{ .lit = .{
                    .value = str_result.value,
                    .lang = null,
                    .datatype = dtype.value,
                } },
                .rest = dtype.rest,
            };
        }
        // plain string
        return ParseResult(Term){
            .value = Term{ .lit = .{
                .value = str_result.value,
                .lang = null,
                .datatype = null,
            } },
            .rest = str_result.rest,
        };
    }

    // boolean
    if (std.mem.startsWith(u8, rest, "true") or std.mem.startsWith(u8, rest, "false")) {
        const bool_str = if (rest[0] == 't') "true" else "false";
        const val = rest[0..bool_str.len];
        return ParseResult(Term){
            .value = Term{ .lit = .{
                .value = val,
                .lang = null,
                .datatype = URI{ .str = "http://www.w3.org/2001/XMLSchema#boolean" },
            } },
            .rest = rest[bool_str.len..],
        };
    }

    // numeric
    if (isDigit(rest[0]) or
        ((rest[0] == '+' or rest[0] == '-') and rest.len > 1 and isDigit(rest[1])) or
        (rest[0] == '.' and rest.len > 1 and isDigit(rest[1])))
    {
        return parseNumber(rest);
    }

    return ParseError.InvalidLiteral;
}

// Collection parsing
pub fn parseCollection(state: *ParserState, input: []const u8) ParseError!ParseResult(Term) {
    var rest = consumeWhitespace(input);
    if (rest.len == 0 or rest[0] != '(') return ParseError.InvalidCollection;
    rest = consumeWhitespace(rest[1..]);

    // Empty collection
    if (rest.len > 0 and rest[0] == ')') {
        return ParseResult(Term){
            .value = Term{
                .uri = try state.graph.intern("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"),
            },
            .rest = rest[1..],
        };
    }

    // first object
    const first_obj = try parseObject(state, rest);
    rest = consumeWhitespace(first_obj.rest);

    const first_bnode = BNode{ .str = try generateBNodeId(state) };
    try state.bnode_labels.put(first_bnode.str, first_bnode);

    // store for later restore
    const saved_subject = state.cur_subject;
    const saved_predicate = state.cur_predicate;

    const first_pred = URI{ .str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first" };
    const rest_pred = URI{ .str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest" };

    // first triple
    state.cur_subject = Term{ .bno = first_bnode };
    state.cur_predicate = Term{ .uri = try state.graph.intern(first_pred.str) };
    try state.emitTriple(first_obj.value);

    var current_bnode = first_bnode;

    // parse remaining objects
    while (rest.len > 0 and rest[0] != ')') {
        const next_obj = try parseObject(state, rest);
        rest = consumeWhitespace(next_obj.rest);

        const next_bnode = BNode{ .str = try generateBNodeId(state) };
        try state.bnode_labels.put(next_bnode.str, next_bnode);

        // link current
        state.cur_subject = Term{ .bno = current_bnode };
        state.cur_predicate = Term{ .uri = try state.graph.intern(rest_pred.str) };
        try state.emitTriple(Term{ .bno = next_bnode });

        // next triple
        state.cur_subject = Term{ .bno = next_bnode };
        state.cur_predicate = Term{ .uri = try state.graph.intern(first_pred.str) };
        try state.emitTriple(next_obj.value);

        current_bnode = next_bnode;
    }

    // final rest -> nil
    state.cur_subject = Term{ .bno = current_bnode };
    state.cur_predicate = Term{ .uri = try state.graph.intern(rest_pred.str) };
    try state.emitTriple(Term{ .uri = try state.graph.intern("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil") });

    // restore
    state.cur_subject = saved_subject;
    state.cur_predicate = saved_predicate;

    if (rest.len == 0 or rest[0] != ')') return ParseError.InvalidCollection;

    return ParseResult(Term){
        .value = Term{ .bno = first_bnode },
        .rest = rest[1..],
    };
}

fn generateBNodeId(state: *ParserState) ![]const u8 {
    const count = state.bnode_labels.count();
    return std.fmt.allocPrint(state.allocator, "b{d}", .{count});
}

pub fn parsePrefixedName(state: *ParserState, input: []const u8) ParseError!ParseResult(URI) {
    var rest = consumeWhitespace(input);
    var prefix_end: usize = 0;

    // find the colon
    while (prefix_end < rest.len and rest[prefix_end] != ':') {
        prefix_end += 1;
    }
    if (prefix_end >= rest.len) return ParseError.InvalidPrefixedName;

    const prefix = rest[0..prefix_end];
    rest = rest[prefix_end + 1 ..];

    // find end of local name
    var local_end: usize = 0;
    while (local_end < rest.len and
        !std.ascii.isWhitespace(rest[local_end]) and
        !isTurtleDelimiter(rest[local_end]))
    {
        local_end += 1;
    }
    if (local_end == 0) return ParseError.InvalidPrefixedName;

    const local = rest[0..local_end];
    rest = rest[local_end..];

    // look up prefix in state
    const base_iri = if (prefix.len == 0)
        state.prefixes.get("") orelse return ParseError.InvalidPrefix
    else
        state.prefixes.get(prefix) orelse return ParseError.InvalidPrefix;

    const full_url = try std.fmt.allocPrint(state.allocator, "{s}{s}", .{ base_iri.str, local });

    return ParseResult(URI){
        .value = URI{ .str = full_url },
        .rest = rest,
    };
}

// High-level: parse an entire Turtle document
pub fn parseTurtleDocument(state: *ParserState, input: []const u8) ParseError!ParseResult(void) {
    var rest = consumeWhitespace(input);

    while (rest.len > 0) {
        rest = consumeWhitespace(rest);
        if (rest.len == 0) break;

        // Either parse a directive (@prefix/@base) or a statement
        // In some Turtle variants, there might also be @keywords, but let's keep it simple.
        if (std.mem.startsWith(u8, rest, "@base") or std.mem.startsWith(u8, rest, "@prefix")) {
            const dir_result = try parseDirective(state, rest);
            rest = consumeWhitespace(dir_result.rest);
        } else {
            const stmt_result = try parseStatement(state, rest);
            rest = consumeWhitespace(stmt_result.rest);
        }
    }

    return ParseResult(void){
        .value = {},
        .rest = rest,
    };
}

pub fn parseStatement(state: *ParserState, input: []const u8) ParseError!ParseResult(void) {
    var rest = consumeWhitespace(input);
    if (rest.len == 0) return ParseError.UnexpectedEndOfInput;

    // Attempt to parse a triple
    const triple_result = try parseTriples(state, rest);
    rest = consumeWhitespace(triple_result.rest);

    // Expect a '.' at the end
    if (rest.len == 0 or rest[0] != '.') {
        return ParseError.InvalidStatement;
    }
    return ParseResult(void){ .value = {}, .rest = rest[1..] };
}

pub fn parseTriples(state: *ParserState, input: []const u8) ParseError!ParseResult(void) {
    var rest = consumeWhitespace(input);

    // subject
    const subject = try parseSubject(state, rest);
    rest = consumeWhitespace(subject.rest);
    state.cur_subject = subject.value;

    // predicate-object list
    const po_list = try parsePredicateObjectList(state, rest);
    rest = po_list.rest;

    return ParseResult(void){ .value = {}, .rest = rest };
}

pub fn parseSubject(state: *ParserState, input: []const u8) ParseError!ParseResult(Term) {
    const rest = consumeWhitespace(input);
    if (rest.len == 0) return ParseError.UnexpectedEndOfInput;

    if (rest[0] == '<') {
        // IRI
        const iri = try readBracketedURI(rest);
        return ParseResult(Term){
            .value = Term{ .uri = try state.graph.intern(iri.value.str) },
            .rest = iri.rest,
        };
    }
    if (std.mem.startsWith(u8, rest, "_:")) {
        // blank node
        return parseBlankNode(state, rest);
    }
    // prefixed
    const prefixed = try parsePrefixedName(state, rest);
    return ParseResult(Term){
        .value = Term{ .uri = try state.graph.intern(prefixed.value.str) },
        .rest = prefixed.rest,
    };
}

pub fn parseBlankNodePropertyList(state: *ParserState, input: []const u8) ParseError!ParseResult(Term) {
    var rest = consumeWhitespace(input);
    if (rest.len == 0 or rest[0] != '[') return ParseError.InvalidBlankNode;
    rest = consumeWhitespace(rest[1..]);

    const bnode = BNode{ .str = try generateBNodeId(state) };
    try state.bnode_labels.put(bnode.str, bnode);

    // Save current subject/predicate
    const saved_subject = state.cur_subject;
    const saved_predicate = state.cur_predicate;

    // Switch subject to new blank node
    state.cur_subject = Term{ .bno = bnode };

    // If not empty, parse the predicate-object list
    if (rest.len > 0 and rest[0] != ']') {
        const po_list = try parsePredicateObjectList(state, rest);
        rest = consumeWhitespace(po_list.rest);
    }

    // Restore
    state.cur_subject = saved_subject;
    state.cur_predicate = saved_predicate;

    if (rest.len == 0 or rest[0] != ']') return ParseError.InvalidBlankNode;

    return ParseResult(Term){
        .value = Term{ .bno = bnode },
        .rest = rest[1..],
    };
}

// -------------------
// Example tests follow
// -------------------

test "parse string literal" {
    const input = "\"hello world\"";
    const result = try parseString(input);
    try std.testing.expectEqualStrings("hello world", result.value);
}

test "parse literal with language tag" {
    var state = ParserState.init(std.testing.allocator);
    defer state.deinit();
    defer state.graph.deinit();

    const input = "\"hello\"@en";
    const result = try parseLiteral(&state, input);
    try std.testing.expectEqualStrings("hello", result.value.lit.value);
    try std.testing.expectEqualStrings("en", result.value.lit.lang.?);
}

test "parse long string literal" {
    const input = "\"\"\"hello\nworld\"\"\"";
    const result = try parseString(input);
    try std.testing.expectEqualStrings("hello\nworld", result.value);
}

test "parse boolean literal" {
    var state = ParserState.init(std.testing.allocator);
    defer state.deinit();
    defer state.graph.deinit();

    const input = "true";
    const result = try parseLiteral(&state, input);
    try std.testing.expectEqualStrings("true", result.value.lit.value);
    try std.testing.expectEqualStrings(
        "http://www.w3.org/2001/XMLSchema#boolean",
        result.value.lit.datatype.?.str,
    );
}

test "parse integer" {
    var state = ParserState.init(std.testing.allocator);
    defer state.deinit();
    defer state.graph.deinit();

    const input = "42";
    const result = try parseLiteral(&state, input);
    try std.testing.expectEqualStrings("42", result.value.lit.value);
    try std.testing.expectEqualStrings(
        "http://www.w3.org/2001/XMLSchema#integer",
        result.value.lit.datatype.?.str,
    );
}

test "parse empty collection" {
    var state = ParserState.init(std.testing.allocator);
    defer state.deinit();
    defer state.graph.deinit();

    const input = "()";
    const result = try parseCollection(&state, input);
    try std.testing.expectEqualStrings(
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil",
        (try state.graph.getURIString(result.value.uri)),
    );
}

test "parse collection with items" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var state = ParserState.init(allocator);

    const input = "(<http://ex.org/a> <http://ex.org/b>)";
    const result = try parseCollection(&state, input);
    try std.testing.expect(result.value == .bno);
}

test "parse prefixed name" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var state = ParserState.init(allocator);
    // No need to defer state.deinit() since arena will free everything

    // Add a prefix mapping
    try state.prefixes.put("ex", URI{ .str = "http://example.org/" });

    const input = "ex:something";
    const result = try parsePrefixedName(&state, input);
    try std.testing.expectEqualStrings("http://example.org/something", result.value.str);
}

test "parse turtle document" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var state = ParserState.init(allocator);

    const input =
        \\@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        \\@prefix foaf: <http://xmlns.com/foaf/0.1/> .
        \\@prefix ex: <http://example.org/> .
        \\
        \\ex:john a foaf:Person ;
        \\    foaf:name "John Smith" ;
        \\    foaf:age 42 .
    ;

    const result = try parseTurtleDocument(&state, input);
    try std.testing.expect(result.rest.len == 0);
    try std.testing.expect(state.graph.triples.items.len > 0);
}

test "verify stored triple" {
    var state = ParserState.init(std.testing.allocator);
    defer state.deinit();
    defer state.graph.deinit();

    const input = "<http://ex.org/s> <http://ex.org/p> <http://ex.org/o> .";
    _ = try parseStatement(&state, input);

    try std.testing.expectEqual(@as(usize, 1), state.graph.triples.items.len);

    const triple = state.graph.triples.items[0];

    try std.testing.expectEqualStrings(
        "http://ex.org/s",
        try state.graph.getURIString(triple.s.uri),
    );
    try std.testing.expectEqualStrings(
        "http://ex.org/p",
        try state.graph.getURIString(triple.p.uri),
    );
    try std.testing.expectEqualStrings(
        "http://ex.org/o",
        try state.graph.getURIString(triple.o.uri),
    );
}

test "verify stored collection" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var state = ParserState.init(allocator);

    const input = "<http://ex.org/s> <http://ex.org/p> ( <http://ex.org/a> <http://ex.org/b> ) .";
    _ = try parseStatement(&state, input);

    // Should have 4 triples:
    // 1. s p b0
    // 2. b0 first a
    // 3. b0 rest b1
    // 4. b1 first b
    // 5. b1 rest nil
    try std.testing.expectEqual(@as(usize, 5), state.graph.triples.items.len);
}

test "parse nested collections" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var state = ParserState.init(allocator);

    const input = "<http://ex.org/s> <http://ex.org/p> ( <http://ex.org/a> ( <http://ex.org/b> <http://ex.org/c> ) ) .";
    _ = try parseStatement(&state, input);

    // Should have triples for:
    // 1. s p b0
    // 2. b0 first a
    // 3. b0 rest b1
    // 4. b1 first b2
    // 5. b2 first b
    // 6. b2 rest b3
    // 7. b3 first c
    // 8. b3 rest nil
    // 9. b1 rest nil
    try std.testing.expectEqual(@as(usize, 9), state.graph.triples.items.len);
}

test "parse complex turtle document" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var state = ParserState.init(allocator);

    const input =
        \\@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        \\@prefix foaf: <http://xmlns.com/foaf/0.1/> .
        \\@prefix ex: <http://example.org/> .
        \\
        \\ex:john a foaf:Person ;
        \\    foaf:name "John Smith" ;
        \\    foaf:age 42 ;
        \\    foaf:knows [
        \\        a foaf:Person ;
        \\        foaf:name "Jane Doe" ;
        \\        foaf:interests ( ex:reading ex:hiking ex:photography )
        \\    ] .
    ;

    _ = try parseTurtleDocument(&state, input);

    // Verify some key triples
    var found_name = false;
    var found_age = false;
    var found_interests = false;

    for (state.graph.triples.items) |stmt| {
        if (std.mem.eql(
            u8,
            try state.graph.getURIString(stmt.p.uri),
            "http://xmlns.com/foaf/0.1/name",
        )) {
            if (std.mem.eql(u8, stmt.o.lit.value, "John Smith")) {
                found_name = true;
            }
        } else if (std.mem.eql(
            u8,
            try state.graph.getURIString(stmt.p.uri),
            "http://xmlns.com/foaf/0.1/age",
        )) {
            if (std.mem.eql(u8, stmt.o.lit.value, "42")) {
                found_age = true;
            }
        } else if (std.mem.eql(
            u8,
            try state.graph.getURIString(stmt.p.uri),
            "http://xmlns.com/foaf/0.1/interests",
        )) {
            found_interests = true;
        }
    }

    try std.testing.expect(found_name);
    try std.testing.expect(found_age);
    try std.testing.expect(found_interests);
}

test "parse list with different literal types" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var state = ParserState.init(allocator);

    const input = "<http://ex.org/s> <http://ex.org/p> ( \"string\" 42 true 3.14 ) .";
    _ = try parseStatement(&state, input);

    // Find the first triple with rdf:first predicate
    var found_string = false;
    var found_number = false;
    var found_boolean = false;
    var found_decimal = false;

    for (state.graph.triples.items) |stmt| {
        if (std.mem.eql(
            u8,
            try state.graph.getURIString(stmt.p.uri),
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#first",
        )) {
            switch (stmt.o) {
                .lit => |lit| {
                    if (std.mem.eql(u8, lit.value, "string")) {
                        found_string = true;
                    } else if (std.mem.eql(u8, lit.value, "42")) {
                        found_number = true;
                    } else if (std.mem.eql(u8, lit.value, "true")) {
                        found_boolean = true;
                    } else if (std.mem.eql(u8, lit.value, "3.14")) {
                        found_decimal = true;
                    }
                },
                else => {},
            }
        }
    }

    try std.testing.expect(found_string);
    try std.testing.expect(found_number);
    try std.testing.expect(found_boolean);
    try std.testing.expect(found_decimal);
}

test "parse empty list in different contexts" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var state = ParserState.init(allocator);

    const input =
        \\@prefix ex: <http://example.org/> .
        \\ex:s1 ex:p1 () .
        \\ex:s2 ex:p2 ( () ) .
        \\ex:s3 ex:p3 ( ex:a () ex:b ) .
    ;

    _ = try parseTurtleDocument(&state, input);

    // Verify we have the correct number of nil references
    var nil_count: usize = 0;
    for (state.graph.triples.items) |stmt| {
        if (stmt.o == .uri and
            std.mem.eql(u8, try state.graph.getURIString(stmt.o.uri), "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))
        {
            nil_count += 1;
        }
    }

    try std.testing.expect(nil_count >= 3);
}

test "parse blank node property list" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var state = ParserState.init(allocator);

    const input = "<http://ex.org/s> <http://ex.org/p> [ <http://ex.org/p2> \"value\" ] .";
    _ = try parseStatement(&state, input);

    // Should have 2 triples:
    // 1. s p b0
    // 2. b0 p2 "value"
    try std.testing.expectEqual(@as(usize, 2), state.graph.triples.items.len);

    // print the triples
    for (state.graph.triples.items) |stmt| {
        switch (stmt.s) {
            .uri => std.debug.print("Subject: {s}\n", .{try state.graph.getURIString(stmt.s.uri)}),
            .bno => std.debug.print("Subject: {s}\n", .{stmt.s.bno.str}),
            else => @panic("Invalid subject"),
        }
        switch (stmt.p) {
            .uri => std.debug.print("Predicate: {s}\n", .{try state.graph.getURIString(stmt.p.uri)}),
            else => @panic("Invalid predicate"),
        }
        switch (stmt.o) {
            .lit => std.debug.print("Object: {s}\n", .{stmt.o.lit.value}),
            .bno => std.debug.print("Object: {s}\n", .{stmt.o.bno.str}),
            .uri => std.debug.print("Object: {s}\n", .{try state.graph.getURIString(stmt.o.uri)}),
        }
    }

    const second_triple = state.graph.triples.items[0];
    try std.testing.expect(second_triple.s == .bno);
    try std.testing.expect(second_triple.p == .uri);
    try std.testing.expectEqualStrings("http://ex.org/p2", try state.graph.getURIString(second_triple.p.uri));
    try std.testing.expect(second_triple.o == .lit);
    try std.testing.expectEqualStrings("value", second_triple.o.lit.value);

    const first_triple = state.graph.triples.items[1];
    try std.testing.expect(first_triple.s == .uri);
    try std.testing.expectEqualStrings("http://ex.org/s", try state.graph.getURIString(first_triple.s.uri));
    try std.testing.expect(first_triple.p == .uri);
    try std.testing.expectEqualStrings("http://ex.org/p", try state.graph.getURIString(first_triple.p.uri));
    try std.testing.expect(first_triple.o == .bno);
}

test "parse nested blank node property lists" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var state = ParserState.init(allocator);

    const input =
        \\@prefix ex: <http://example.org/> .
        \\ex:s ex:p [ ex:p1 [ ex:p2 "nested" ] ] .
    ;

    _ = try parseTurtleDocument(&state, input);

    // Should have 3 triples:
    // 1. s p b0
    // 2. b0 p1 b1
    // 3. b1 p2 "nested"
    try std.testing.expectEqual(@as(usize, 3), state.graph.triples.items.len);
}
