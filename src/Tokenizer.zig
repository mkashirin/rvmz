source: []const u8,
index: usize,
line: usize,
column: usize,
const Tokenizer = @This();

pub fn init(source: []const u8) Tokenizer {
    return .{ .source = source, .index = 0, .line = 1, .column = 1 };
}

pub fn deinit(t: *Tokenizer) void {
    t.* = undefined;
}

pub fn peekNext(t: *Tokenizer) Token {
    const current = .{ t.index, t.line, t.column };
    const token = t.next();
    t.index, t.line, t.column = current;
    return token;
}

pub fn next(t: *Tokenizer) Token {
    var token: Token = .{ .tag = .invalid };
    t.skipWhitespaces();
    if (t.index >= t.source.len) {
        return .{ .tag = .eof, .lexeme = "EOF" };
    }
    const start = t.index;
    const char = t.step().?;

    token.tag = sw: switch (char) {
        '+' => .plus,
        '-' => .minus,
        '*' => .star,
        '/' => .slash,
        '^' => .carrot,

        '(' => .left_paren,
        ')' => .right_paren,
        '[' => .left_bracket,
        ']' => .right_bracket,
        '{' => .left_brace,
        '}' => .right_brace,
        ',' => .comma,
        ';' => .semicolon,
        ':' => .colon,

        '=' => switch (t.source[t.index]) {
            '=' => {
                _ = t.step();
                break :sw .double_equal;
            },
            else => .equal,
        },
        '!' => switch (t.source[t.index]) {
            '=' => {
                _ = t.step();
                break :sw .bang_equal;
            },
            else => .invalid,
        },
        '<' => switch (t.source[t.index]) {
            '=' => {
                _ = t.step();
                break :sw .less_or_equal_than;
            },
            else => .less_than,
        },
        '>' => switch (t.source[t.index]) {
            '=' => {
                _ = t.step();
                break :sw .greater_or_equal_than;
            },
            else => .greater_than,
        },

        '"' => {
            _ = t.step();
            while (t.index < t.source.len and
                t.source[t.index] != '"') _ = t.step();
            _ = t.step();
            token.lexeme = t.source[start + 1 .. t.index - 1];
            break :sw .string_literal;
        },

        '0'...'9' => {
            while (t.index < t.source.len) {
                const digit = t.source[t.index];
                switch (digit) {
                    '0'...'9' => _ = t.step(),
                    else => break,
                }
            }
            token.lexeme = t.source[start..t.index];
            break :sw .int_literal;
        },

        'a'...'z', 'A'...'Z', '_' => {
            while (t.index < t.source.len) {
                const sub = t.source[t.index];
                switch (sub) {
                    'a'...'z', 'A'...'Z', '0'...'9', '_' => _ = t.step(),
                    else => break,
                }
            }
            const lexeme = t.source[start..t.index];
            if (Token.getKeyword(lexeme)) |tag|
                break :sw tag
            else {
                token.lexeme = lexeme;
                break :sw .ident;
            }
        },

        else => .invalid,
    };
    token.location = .{ .line = t.line, .column = t.column };
    return token;
}

pub const Token = struct {
    tag: Tag,
    lexeme: ?[]const u8 = null,
    location: Location = undefined,

    pub const Tag = enum {
        plus,
        minus,
        star,
        slash,
        carrot,
        left_paren,
        right_paren,
        left_bracket,
        right_bracket,
        left_brace,
        right_brace,
        comma,
        semicolon,
        colon,
        equal,
        double_equal,
        bang_equal,
        less_than,
        less_or_equal_than,
        greater_than,
        greater_or_equal_than,

        eof,
        invalid,
        ident,
        string_literal,
        int_literal,

        keyword_true,
        keyword_false,
        keyword_and,
        keyword_or,
        keyword_if,
        keyword_else,
        keyword_def,
        keyword_return,
        keyword_for,
        keyword_in,
    };

    pub const Location = struct { line: usize, column: usize };

    pub const keywords_map: @import("std").StaticStringMap(Tag) =
        .initComptime(.{
            .{ "true", .keyword_true },
            .{ "false", .keyword_false },
            .{ "and", .keyword_and },
            .{ "or", .keyword_or },
            .{ "if", .keyword_if },
            .{ "else", .keyword_else },
            .{ "def", .keyword_def },
            .{ "return", .keyword_return },
            .{ "for", .keyword_for },
            .{ "in", .keyword_in },
        });

    pub fn getKeyword(bytes: []const u8) ?Tag {
        return keywords_map.get(bytes);
    }
};

fn skipWhitespaces(t: *Tokenizer) void {
    while (t.index < t.source.len) {
        switch (t.source[t.index]) {
            ' ', '\t', '\n', '\r' => _ = t.step(),
            else => break,
        }
    }
}

fn step(t: *Tokenizer) ?u8 {
    if (t.index >= t.source.len) return null;
    const char = t.source[t.index];
    if (char == '\n')
        t.column, t.line = .{ 1, t.line + 1 }
    else if (char == '\t') t.column += 8 else t.column += 1;
    t.index += 1;
    return char;
}

test {
    _ = @import("Tokenizer.zig");
}
