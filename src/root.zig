const std = @import("std");
const testing = std.testing;

pub const ParseError = error{
    MissingOpenBracket,
    MissingCloseBracket,
    EmptyURL,
};

/// Parses a URL from N-Quads format: <http://example.org>
/// Returns the URL without the angle brackets
pub fn parseURL(input: []const u8) ParseError![]const u8 {
    if (input.len < 2) return ParseError.EmptyURL;
    if (input[0] != '<') return ParseError.MissingOpenBracket;
    if (input[input.len - 1] != '>') return ParseError.MissingCloseBracket;
    
    // Return the URL without the angle brackets
    return input[1 .. input.len - 1];
}

test "parse valid URL" {
    const input = "<http://example.org>";
    const result = try parseURL(input);
    try testing.expectEqualStrings("http://example.org", result);
}

test "detect missing open bracket" {
    const input = "http://example.org>";
    try testing.expectError(ParseError.MissingOpenBracket, parseURL(input));
}

test "detect missing close bracket" {
    const input = "<http://example.org";
    try testing.expectError(ParseError.MissingCloseBracket, parseURL(input));
}

test "detect empty URL" {
    const input = "";
    try testing.expectError(ParseError.EmptyURL, parseURL(input));
}
