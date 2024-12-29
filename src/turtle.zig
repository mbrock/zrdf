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

pub const URL = struct {
    url: []const u8,
};

pub const Term = union(enum) {
    uri: URL,
    bnode: BNode,
    lit: Literal,
};

pub const BNode = struct {
    id: []const u8,
};

pub const Literal = struct {
    value: []const u8,
    lang: ?[]const u8,
    datatype: ?URL,
};

pub const Statement = struct {
    subject: Term,
    predicate: Term,
    object: Term,
};

pub const Graph = struct {
    statements: std.ArrayList(Statement),
};

pub const ParserState = struct {
    base_uri: ?URL,
    prefixes: std.StringHashMap(URL),
    bnode_labels: std.StringHashMap(BNode),
    cur_subject: ?Term,
    cur_predicate: ?Term,
    allocator: std.mem.Allocator,
    graph: Graph,

    pub fn init(allocator: std.mem.Allocator) ParserState {
        return .{
            .base_uri = null,
            .prefixes = std.StringHashMap(URL).init(allocator),
            .bnode_labels = std.StringHashMap(BNode).init(allocator),
            .cur_subject = null,
            .cur_predicate = null,
            .allocator = allocator,
            .graph = Graph{ .statements = std.ArrayList(Statement).init(allocator) },
        };
    }

    pub fn deinit(self: *ParserState) void {
        self.prefixes.deinit();
        self.bnode_labels.deinit();
        self.graph.statements.deinit();
    }

    pub fn emitTriple(self: *ParserState, object: Term) !void {
        if (self.cur_subject == null or self.cur_predicate == null) {
            return ParseError.InvalidTriple;
        }

        try self.graph.statements.append(Statement{
            .subject = self.cur_subject.?,
            .predicate = self.cur_predicate.?,
            .object = object,
        });
    }
};

pub fn ParseResult(T: type) type {
    return struct {
        value: T,
        rest: []const u8,
    };
}

/// Parses a URL from N-Quads format: <http://example.org>
/// Returns the URL without the angle brackets
pub fn readBracketedURI(input: []const u8) ParseError!ParseResult(URL) {
    if (input.len < 2)
        return ParseError.EmptyURL;
    if (input[0] != '<')
        return ParseError.MissingOpenBracket;

    var pos: usize = 1;
    while (pos < input.len) {
        if (input[pos] == '>') {
            if (pos == 1) return ParseError.EmptyURL;
            return ParseResult(URL){
                .value = URL{ .url = input[1..pos] },
                .rest = input[pos + 1 ..],
            };
        }
        pos += 1;
    }

    return ParseError.MissingCloseBracket;
}

// Add helper function to skip whitespace
fn skipWhitespace(input: []const u8) []const u8 {
    var rest = input;
    while (rest.len > 0 and std.ascii.isWhitespace(rest[0])) {
        rest = rest[1..];
    }
    return rest;
}

pub fn parseBase(state: *ParserState, input: []const u8) ParseError!ParseResult(void) {
    // Skip @base
    if (!std.mem.startsWith(u8, input, "@base")) {
        return ParseError.InvalidBase;
    }

    var rest = input[5..];
    // Skip whitespace
    while (rest.len > 0 and std.ascii.isWhitespace(rest[0])) {
        rest = rest[1..];
    }

    // Parse IRI
    const iri = try readBracketedURI(rest);
    state.base_uri = iri.value;

    return ParseResult(void){
        .value = {},
        .rest = iri.rest,
    };
}

pub fn parsePrefix(state: *ParserState, input: []const u8) ParseError!ParseResult(void) {
    // Skip @prefix
    if (!std.mem.startsWith(u8, input, "@prefix")) {
        return ParseError.InvalidPrefix;
    }

    var rest = input[7..];
    // Skip whitespace
    while (rest.len > 0 and std.ascii.isWhitespace(rest[0])) {
        rest = rest[1..];
    }

    // Parse prefix name until :
    const colon_pos = std.mem.indexOf(u8, rest, ":");
    if (colon_pos == null) return ParseError.InvalidPrefix;

    const prefix_name = rest[0..colon_pos.?];
    rest = rest[colon_pos.? + 1 ..];

    // Skip whitespace
    while (rest.len > 0 and std.ascii.isWhitespace(rest[0])) {
        rest = rest[1..];
    }

    // Parse IRI
    const iri = try readBracketedURI(rest);
    try state.prefixes.put(prefix_name, iri.value);

    return ParseResult(void){
        .value = {},
        .rest = iri.rest,
    };
}

// Add blank node parsing
pub fn parseBlankNode(state: *ParserState, input: []const u8) ParseError!ParseResult(Term) {
    if (!std.mem.startsWith(u8, input, "_:")) {
        return ParseError.InvalidBlankNode;
    }

    var rest = input[2..];
    var end: usize = 0;

    // Find end of blank node label
    while (end < rest.len and !std.ascii.isWhitespace(rest[end])) {
        end += 1;
    }

    if (end == 0) return ParseError.InvalidBlankNode;

    const label = rest[0..end];

    // Check if we've seen this blank node before
    if (state.bnode_labels.get(label)) |existing| {
        return ParseResult(Term){
            .value = Term{ .bnode = existing },
            .rest = rest[end..],
        };
    }

    // Create new blank node
    const bnode = BNode{ .id = label };
    try state.bnode_labels.put(label, bnode);

    return ParseResult(Term){
        .value = Term{ .bnode = bnode },
        .rest = rest[end..],
    };
}

// Add predicate-object list parsing
pub fn parsePredicateObjectList(
    state: *ParserState,
    input: []const u8,
) ParseError!ParseResult(void) {
    var rest = skipWhitespace(input);

    while (true) {
        // Parse predicate
        const pred = try parsePredicate(state, rest);
        rest = skipWhitespace(pred.rest);
        state.cur_predicate = pred.value;

        // Parse object list
        const obj_list = try parseObjectList(state, rest);
        rest = skipWhitespace(obj_list.rest);

        // Check for another predicate-object pair
        if (rest.len > 0 and rest[0] == ';') {
            rest = skipWhitespace(rest[1..]);
            if (rest.len > 0 and rest[0] != '.') continue;
        }
        break;
    }

    return ParseResult(void){
        .value = {},
        .rest = rest,
    };
}

pub fn parsePredicate(state: *ParserState, input: []const u8) ParseError!ParseResult(Term) {
    const rest = skipWhitespace(input);
    if (rest.len == 0) return ParseError.UnexpectedEndOfInput;

    // Handle 'a' as rdf:type shorthand
    if (rest[0] == 'a' and
        (rest.len == 1 or std.ascii.isWhitespace(rest[1])))
    {
        const rdf_type = URL{ .url = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" };
        return ParseResult(Term){
            .value = Term{ .uri = rdf_type },
            .rest = rest[1..],
        };
    }

    // Try parsing IRI
    if (rest[0] == '<') {
        const iri = try readBracketedURI(rest);
        return ParseResult(Term){
            .value = Term{ .uri = iri.value },
            .rest = iri.rest,
        };
    }

    // Try parsing prefixed name
    const prefixed = try parsePrefixedName(state, rest);
    return ParseResult(Term){
        .value = Term{ .uri = prefixed.value },
        .rest = prefixed.rest,
    };
}

pub fn parseObjectList(state: *ParserState, input: []const u8) ParseError!ParseResult(void) {
    var rest = skipWhitespace(input);

    while (true) {
        // Parse object
        const obj = try parseObject(state, rest);
        rest = skipWhitespace(obj.rest);

        // Emit triple
        try state.emitTriple(obj.value);

        // Check for another object
        if (rest.len > 0 and rest[0] == ',') {
            rest = skipWhitespace(rest[1..]);
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
    const rest = skipWhitespace(input);
    if (rest.len == 0) return ParseError.UnexpectedEndOfInput;

    // Try parsing collection
    if (rest[0] == '(') {
        return parseCollection(state, rest);
    }

    // Try parsing blank node property list
    if (rest[0] == '[') {
        return parseBlankNodePropertyList(state, rest);
    }

    // Try parsing IRI
    if (rest[0] == '<') {
        const iri = try readBracketedURI(rest);
        return ParseResult(Term){
            .value = Term{ .uri = iri.value },
            .rest = iri.rest,
        };
    }

    // Try parsing blank node
    if (std.mem.startsWith(u8, rest, "_:")) {
        return parseBlankNode(state, rest);
    }

    // Try parsing literal
    if (rest[0] == '"' or rest[0] == '\'' or
        isDigit(rest[0]) or rest[0] == '+' or rest[0] == '-' or
        std.mem.startsWith(u8, rest, "true") or
        std.mem.startsWith(u8, rest, "false"))
    {
        return parseLiteral(state, rest);
    }

    // Try parsing prefixed name
    const prefixed = try parsePrefixedName(state, rest);
    return ParseResult(Term){
        .value = Term{ .uri = prefixed.value },
        .rest = prefixed.rest,
    };
}

// Add string literal parsing
pub fn parseString(input: []const u8) ParseError!ParseResult([]const u8) {
    if (input.len < 2) return ParseError.InvalidString;

    // Handle different quote types
    if (std.mem.startsWith(u8, input, "\"\"\"")) {
        return parseLongString(input, "\"\"\"");
    } else if (std.mem.startsWith(u8, input, "'''")) {
        return parseLongString(input, "'''");
    } else if (input[0] == '"') {
        return parseQuotedString(input, "\"");
    } else if (input[0] == '\'') {
        return parseQuotedString(input, "'");
    }

    return ParseError.InvalidString;
}

fn parseQuotedString(input: []const u8, quote: []const u8) ParseError!ParseResult([]const u8) {
    if (input.len < 2) return ParseError.InvalidString;
    if (!std.mem.startsWith(u8, input, quote)) return ParseError.InvalidString;

    var pos: usize = 1;
    while (pos < input.len) {
        if (input[pos] == quote[0] and
            (pos == 1 or input[pos - 1] != '\\'))
        {
            return ParseResult([]const u8){
                .value = input[1..pos],
                .rest = input[pos + 1 ..],
            };
        }
        pos += 1;
    }
    return ParseError.InvalidString;
}

fn parseLongString(input: []const u8, quotes: []const u8) ParseError!ParseResult([]const u8) {
    if (input.len < 6) return ParseError.InvalidString;
    if (!std.mem.startsWith(u8, input, quotes)) return ParseError.InvalidString;

    // Find closing quotes
    var pos: usize = 3;
    while (pos < input.len - 2) {
        if (std.mem.startsWith(u8, input[pos..], quotes) and
            input[pos - 1] != '\\')
        {
            return ParseResult([]const u8){
                .value = input[3..pos],
                .rest = input[pos + 3 ..],
            };
        }
        pos += 1;
    }
    return ParseError.InvalidString;
}

fn parseLanguageTag(input: []const u8) ParseError!ParseResult([]const u8) {
    if (input.len < 2) return ParseError.InvalidLanguageTag;
    if (input[0] != '@') return ParseError.InvalidLanguageTag;

    var pos: usize = 1;
    while (pos < input.len and
        (std.ascii.isAlphabetic(input[pos]) or
        input[pos] == '-'))
    {
        pos += 1;
    }

    if (pos == 1) return ParseError.InvalidLanguageTag;

    return ParseResult([]const u8){
        .value = input[1..pos],
        .rest = input[pos..],
    };
}

// Add these helper functions for number parsing
fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn parseNumber(input: []const u8) ParseError!ParseResult(Term) {
    var rest = input;
    var has_sign = false;
    var has_decimal = false;
    var has_exponent = false;
    var pos: usize = 0;

    // Handle optional sign
    if (pos < rest.len and (rest[pos] == '+' or rest[pos] == '-')) {
        has_sign = true;
        pos += 1;
    }

    // Parse integer part
    const start_digits = pos;
    while (pos < rest.len and isDigit(rest[pos])) {
        pos += 1;
    }

    // Check for decimal point
    if (pos < rest.len and rest[pos] == '.') {
        has_decimal = true;
        pos += 1;
        // Parse decimal digits
        while (pos < rest.len and isDigit(rest[pos])) {
            pos += 1;
        }
    }

    // Check for exponent
    if (pos < rest.len and (rest[pos] == 'e' or rest[pos] == 'E')) {
        has_exponent = true;
        pos += 1;
        // Handle optional exponent sign
        if (pos < rest.len and (rest[pos] == '+' or rest[pos] == '-')) {
            pos += 1;
        }
        // Parse exponent digits
        var has_exp_digits = false;
        while (pos < rest.len and isDigit(rest[pos])) {
            has_exp_digits = true;
            pos += 1;
        }
        if (!has_exp_digits) return ParseError.InvalidNumber;
    }

    // Validate number format
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
            .datatype = URL{ .url = datatype },
        } },
        .rest = rest[pos..],
    };
}

pub fn parseLiteral(state: *ParserState, input: []const u8) ParseError!ParseResult(Term) {
    _ = state; // autofix
    var rest = skipWhitespace(input);
    if (rest.len == 0) return ParseError.UnexpectedEndOfInput;

    // Try parsing string literal
    if (rest[0] == '"' or rest[0] == '\'') {
        const str = try parseString(rest);
        rest = skipWhitespace(str.rest);

        // Check for language tag or datatype
        if (rest.len > 0 and rest[0] == '@') {
            const lang = try parseLanguageTag(rest);
            return ParseResult(Term){
                .value = Term{ .lit = .{
                    .value = str.value,
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
                    .value = str.value,
                    .lang = null,
                    .datatype = dtype.value,
                } },
                .rest = dtype.rest,
            };
        }

        // Plain string literal
        return ParseResult(Term){
            .value = Term{ .lit = .{
                .value = str.value,
                .lang = null,
                .datatype = null,
            } },
            .rest = str.rest,
        };
    }

    // Try parsing boolean
    if (std.mem.startsWith(u8, rest, "true") or
        std.mem.startsWith(u8, rest, "false"))
    {
        const bool_str = if (rest[0] == 't') "true" else "false";
        const val = rest[0..bool_str.len];
        return ParseResult(Term){
            .value = Term{ .lit = .{
                .value = val,
                .lang = null,
                .datatype = URL{ .url = "http://www.w3.org/2001/XMLSchema#boolean" },
            } },
            .rest = rest[bool_str.len..],
        };
    }

    // Try parsing number
    if (isDigit(rest[0]) or
        ((rest[0] == '+' or rest[0] == '-') and rest.len > 1 and isDigit(rest[1])) or
        (rest[0] == '.' and rest.len > 1 and isDigit(rest[1])))
    {
        return parseNumber(rest);
    }

    return ParseError.InvalidLiteral;
}

// Add collection parsing
pub fn parseCollection(state: *ParserState, input: []const u8) ParseError!ParseResult(Term) {
    var rest = skipWhitespace(input);
    if (rest.len == 0 or rest[0] != '(') return ParseError.InvalidCollection;
    rest = skipWhitespace(rest[1..]);

    // Handle empty collection
    if (rest.len > 0 and rest[0] == ')') {
        return ParseResult(Term){
            .value = Term{
                .uri = URL{
                    .url = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil",
                },
            },
            .rest = rest[1..],
        };
    }

    // Parse first object
    const first_obj = try parseObject(state, rest);
    rest = skipWhitespace(first_obj.rest);

    // Create first blank node
    const first_bnode = BNode{ .id = try generateBNodeId(state) };
    try state.bnode_labels.put(first_bnode.id, first_bnode);

    // Save current subject/predicate
    const saved_subject = state.cur_subject;
    const saved_predicate = state.cur_predicate;

    // Set first/rest predicates
    const first_pred = URL{ .url = "http://www.w3.org/1999/02/22-rdf-syntax-ns#first" };
    const rest_pred = URL{ .url = "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest" };

    // Create first triple
    state.cur_subject = Term{ .bnode = first_bnode };
    state.cur_predicate = Term{ .uri = first_pred };
    try state.emitTriple(first_obj.value);

    var current_bnode = first_bnode;

    // Parse remaining objects
    while (rest.len > 0 and rest[0] != ')') {
        const next_obj = try parseObject(state, rest);
        rest = skipWhitespace(next_obj.rest);

        // Create next blank node
        const next_bnode = BNode{ .id = try generateBNodeId(state) };
        try state.bnode_labels.put(next_bnode.id, next_bnode);

        // Link current to next with rdf:rest
        state.cur_subject = Term{ .bnode = current_bnode };
        state.cur_predicate = Term{ .uri = rest_pred };
        try state.emitTriple(Term{ .bnode = next_bnode });

        // Add rdf:first triple for next object
        state.cur_subject = Term{ .bnode = next_bnode };
        state.cur_predicate = Term{ .uri = first_pred };
        try state.emitTriple(next_obj.value);

        current_bnode = next_bnode;
    }

    // Add final rdf:rest triple pointing to rdf:nil
    state.cur_subject = Term{ .bnode = current_bnode };
    state.cur_predicate = Term{ .uri = rest_pred };
    try state.emitTriple(Term{ .uri = URL{ .url = "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil" } });

    // Restore subject/predicate
    state.cur_subject = saved_subject;
    state.cur_predicate = saved_predicate;

    if (rest.len == 0 or rest[0] != ')') return ParseError.InvalidCollection;

    return ParseResult(Term){
        .value = Term{ .bnode = first_bnode },
        .rest = rest[1..],
    };
}

// Helper to generate unique blank node IDs
fn generateBNodeId(state: *ParserState) ![]const u8 {
    // For now just use a counter - could be more sophisticated
    const count = state.bnode_labels.count();
    const id = try std.fmt.allocPrint(state.allocator, "b{d}", .{count});
    return id;
}

pub fn parsePrefixedName(state: *ParserState, input: []const u8) ParseError!ParseResult(URL) {
    var rest = input;
    var prefix_end: usize = 0;

    // Find the colon separator
    while (prefix_end < rest.len and rest[prefix_end] != ':') {
        prefix_end += 1;
    }
    if (prefix_end >= rest.len) return ParseError.InvalidPrefixedName;

    const prefix = rest[0..prefix_end];
    rest = rest[prefix_end + 1 ..];

    // Find end of local name
    var local_end: usize = 0;
    while (local_end < rest.len and !std.ascii.isWhitespace(rest[local_end]) and
        rest[local_end] != '.' and rest[local_end] != ';' and
        rest[local_end] != ',' and rest[local_end] != ')')
    {
        local_end += 1;
    }
    if (local_end == 0) return ParseError.InvalidPrefixedName;

    const local = rest[0..local_end];

    // Look up prefix in state
    const base_iri = if (prefix.len == 0)
        state.prefixes.get("") orelse return ParseError.InvalidPrefix
    else
        state.prefixes.get(prefix) orelse return ParseError.InvalidPrefix;

    // Create full URL
    const full_url = try std.fmt.allocPrint(
        state.allocator,
        "{s}{s}",
        .{ base_iri.url, local },
    );

    return ParseResult(URL){
        .value = URL{ .url = full_url },
        .rest = rest[local_end..],
    };
}

// Add turtle document parsing
pub fn parseTurtleDocument(state: *ParserState, input: []const u8) ParseError!ParseResult(void) {
    var rest = skipWhitespace(input);
    while (rest.len > 0) {
        rest = skipWhitespace(rest);
        if (rest.len == 0) break;

        const result = try parseStatement(state, rest);
        rest = result.rest;
    }

    return ParseResult(void){
        .value = {},
        .rest = rest,
    };
}

test "parse string literal" {
    const input = "\"hello world\"";
    const result = try parseString(input);
    try std.testing.expectEqualStrings("hello world", result.value);
}

test "parse long string literal" {
    const input = "\"\"\"hello\nworld\"\"\"";
    const result = try parseString(input);
    try std.testing.expectEqualStrings("hello\nworld", result.value);
}

test "parse literal with language tag" {
    var state = ParserState.init(std.testing.allocator);
    defer state.deinit();

    const input = "\"hello\"@en";
    const result = try parseLiteral(&state, input);
    try std.testing.expectEqualStrings("hello", result.value.lit.value);
    try std.testing.expectEqualStrings("en", result.value.lit.lang.?);
}

test "parse boolean literal" {
    var state = ParserState.init(std.testing.allocator);
    defer state.deinit();

    const input = "true";
    const result = try parseLiteral(&state, input);
    try std.testing.expectEqualStrings("true", result.value.lit.value);
    try std.testing.expectEqualStrings(
        "http://www.w3.org/2001/XMLSchema#boolean",
        result.value.lit.datatype.?.url,
    );
}

test "parse integer" {
    var state = ParserState.init(std.testing.allocator);
    defer state.deinit();

    const input = "42";
    const result = try parseLiteral(&state, input);
    try std.testing.expectEqualStrings("42", result.value.lit.value);
    try std.testing.expectEqualStrings(
        "http://www.w3.org/2001/XMLSchema#integer",
        result.value.lit.datatype.?.url,
    );
}

test "parse empty collection" {
    var state = ParserState.init(std.testing.allocator);
    defer state.deinit();

    const input = "()";
    const result = try parseCollection(&state, input);
    try std.testing.expectEqualStrings(
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil",
        result.value.uri.url,
    );
}

test "parse collection with items" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var state = ParserState.init(allocator);

    const input = "(<http://ex.org/a> <http://ex.org/b>)";
    const result = try parseCollection(&state, input);
    try std.testing.expect(result.value == .bnode);
}

test "parse prefixed name" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var state = ParserState.init(allocator);
    // No need to defer state.deinit() since arena will free everything

    // Add a prefix mapping
    try state.prefixes.put("ex", URL{ .url = "http://example.org/" });

    const input = "ex:something";
    const result = try parsePrefixedName(&state, input);
    try std.testing.expectEqualStrings("http://example.org/something", result.value.url);
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
    try std.testing.expect(state.graph.statements.items.len > 0);
}

test "verify stored triple" {
    var state = ParserState.init(std.testing.allocator);
    defer state.deinit();

    const input = "<http://ex.org/s> <http://ex.org/p> <http://ex.org/o> .";
    _ = try parseStatement(&state, input);

    try std.testing.expectEqual(@as(usize, 1), state.graph.statements.items.len);
    const triple = state.graph.statements.items[0];
    try std.testing.expectEqualStrings("http://ex.org/s", triple.subject.uri.url);
    try std.testing.expectEqualStrings("http://ex.org/p", triple.predicate.uri.url);
    try std.testing.expectEqualStrings("http://ex.org/o", triple.object.uri.url);
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
    try std.testing.expectEqual(@as(usize, 5), state.graph.statements.items.len);
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
    try std.testing.expectEqual(@as(usize, 9), state.graph.statements.items.len);
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

    for (state.graph.statements.items) |stmt| {
        if (std.mem.eql(u8, stmt.predicate.uri.url, "http://xmlns.com/foaf/0.1/name")) {
            if (std.mem.eql(u8, stmt.object.lit.value, "John Smith")) {
                found_name = true;
            }
        } else if (std.mem.eql(u8, stmt.predicate.uri.url, "http://xmlns.com/foaf/0.1/age")) {
            if (std.mem.eql(u8, stmt.object.lit.value, "42")) {
                found_age = true;
            }
        } else if (std.mem.eql(u8, stmt.predicate.uri.url, "http://xmlns.com/foaf/0.1/interests")) {
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

    for (state.graph.statements.items) |stmt| {
        if (std.mem.eql(u8, stmt.predicate.uri.url, "http://www.w3.org/1999/02/22-rdf-syntax-ns#first")) {
            switch (stmt.object) {
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
    for (state.graph.statements.items) |stmt| {
        if (stmt.object == .uri and
            std.mem.eql(u8, stmt.object.uri.url, "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))
        {
            nil_count += 1;
        }
    }

    try std.testing.expect(nil_count >= 3);
}

// Add statement parsing
pub fn parseStatement(state: *ParserState, input: []const u8) ParseError!ParseResult(void) {
    var rest = skipWhitespace(input);
    if (rest.len == 0) return ParseError.UnexpectedEndOfInput;

    // Try parsing directive
    if (std.mem.startsWith(u8, rest, "@prefix")) {
        const result = try parsePrefix(state, rest);
        rest = skipWhitespace(result.rest);
        if (rest.len == 0 or rest[0] != '.') {
            return ParseError.InvalidStatement;
        }
        return ParseResult(void){ .value = {}, .rest = rest[1..] };
    } else if (std.mem.startsWith(u8, rest, "@base")) {
        const result = try parseBase(state, rest);
        rest = skipWhitespace(result.rest);
        if (rest.len == 0 or rest[0] != '.') {
            return ParseError.InvalidStatement;
        }
        return ParseResult(void){ .value = {}, .rest = rest[1..] };
    }

    // Otherwise try parsing triples
    const triple_result = try parseTriples(state, rest);
    rest = skipWhitespace(triple_result.rest);
    if (rest.len == 0 or rest[0] != '.') {
        return ParseError.InvalidStatement;
    }
    return ParseResult(void){
        .value = {},
        .rest = rest[1..],
    };
}

pub fn parseTriples(state: *ParserState, input: []const u8) ParseError!ParseResult(void) {
    var rest = skipWhitespace(input);

    // Parse subject
    const subject = try parseSubject(state, rest);
    rest = skipWhitespace(subject.rest);
    state.cur_subject = subject.value;

    // Parse predicate-object list
    const po_list = try parsePredicateObjectList(state, rest);
    rest = po_list.rest;

    return ParseResult(void){
        .value = {},
        .rest = rest,
    };
}

pub fn parseSubject(state: *ParserState, input: []const u8) ParseError!ParseResult(Term) {
    const rest = skipWhitespace(input);
    if (rest.len == 0) return ParseError.UnexpectedEndOfInput;

    // Try parsing IRI
    if (rest[0] == '<') {
        const iri = try readBracketedURI(rest);
        return ParseResult(Term){
            .value = Term{ .uri = iri.value },
            .rest = iri.rest,
        };
    }

    // Try parsing blank node
    if (std.mem.startsWith(u8, rest, "_:")) {
        return parseBlankNode(state, rest);
    }

    // Try parsing prefixed name
    const prefixed = try parsePrefixedName(state, rest);
    return ParseResult(Term){
        .value = Term{ .uri = prefixed.value },
        .rest = prefixed.rest,
    };
}

// Add blank node property list parsing
pub fn parseBlankNodePropertyList(state: *ParserState, input: []const u8) ParseError!ParseResult(Term) {
    var rest = skipWhitespace(input);
    if (rest.len == 0 or rest[0] != '[') return ParseError.InvalidBlankNode;
    rest = skipWhitespace(rest[1..]);

    // Create new blank node
    const bnode = BNode{ .id = try generateBNodeId(state) };
    try state.bnode_labels.put(bnode.id, bnode);

    // Save current subject/predicate
    const saved_subject = state.cur_subject;
    const saved_predicate = state.cur_predicate;

    // Set new subject
    state.cur_subject = Term{ .bnode = bnode };

    // Parse predicate-object list if not empty
    if (rest.len > 0 and rest[0] != ']') {
        const po_list = try parsePredicateObjectList(state, rest);
        rest = skipWhitespace(po_list.rest);
    }

    // Restore subject/predicate
    state.cur_subject = saved_subject;
    state.cur_predicate = saved_predicate;

    if (rest.len == 0 or rest[0] != ']') return ParseError.InvalidBlankNode;

    return ParseResult(Term){
        .value = Term{ .bnode = bnode },
        .rest = rest[1..],
    };
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
    try std.testing.expectEqual(@as(usize, 2), state.graph.statements.items.len);

    // print the triples
    for (state.graph.statements.items) |stmt| {
        switch (stmt.subject) {
            .uri => std.debug.print("Subject: {s}\n", .{stmt.subject.uri.url}),
            .bnode => std.debug.print("Subject: {s}\n", .{stmt.subject.bnode.id}),
            else => @panic("Invalid subject"),
        }
        switch (stmt.predicate) {
            .uri => std.debug.print("Predicate: {s}\n", .{stmt.predicate.uri.url}),
            else => @panic("Invalid predicate"),
        }
        switch (stmt.object) {
            .lit => std.debug.print("Object: {s}\n", .{stmt.object.lit.value}),
            .bnode => std.debug.print("Object: {s}\n", .{stmt.object.bnode.id}),
            .uri => std.debug.print("Object: {s}\n", .{stmt.object.uri.url}),
        }
    }

    const second_triple = state.graph.statements.items[0];
    try std.testing.expect(second_triple.subject == .bnode);
    try std.testing.expect(second_triple.predicate == .uri);
    try std.testing.expectEqualStrings("http://ex.org/p2", second_triple.predicate.uri.url);
    try std.testing.expect(second_triple.object == .lit);
    try std.testing.expectEqualStrings("value", second_triple.object.lit.value);

    const first_triple = state.graph.statements.items[1];
    try std.testing.expect(first_triple.subject == .uri);
    try std.testing.expectEqualStrings("http://ex.org/s", first_triple.subject.uri.url);
    try std.testing.expect(first_triple.predicate == .uri);
    try std.testing.expectEqualStrings("http://ex.org/p", first_triple.predicate.uri.url);
    try std.testing.expect(first_triple.object == .bnode);
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
    try std.testing.expectEqual(@as(usize, 3), state.graph.statements.items.len);
}