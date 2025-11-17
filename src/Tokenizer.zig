source: []const u8,
index: usize,
const Tokenizer = @This();

pub fn init(source: []const u8) Tokenizer {
    return Tokenizer{ .source = source, .index = 0 };
}

pub fn peekNext(t: *Tokenizer) Token {
    const init_index = t.index;
    const token = t.next();
    t.index = init_index;
    return token;
}

pub fn next(t: *Tokenizer) Token {
    var res: Token = .{ .tag = .invalid };
    t.skipWhitespaces();
    if (t.index >= t.source.len) {
        return Token{ .tag = .eof, .lexeme = "EOF" };
    }

    const start = t.index;
    const char = t.step().?;

    // identifiers and keywords
    switch (char) {
        'a'...'z', 'A'...'Z', '_' => {
            while (t.index < t.source.len) {
                const sub = t.source[t.index];
                switch (sub) {
                    'a'...'z', 'A'...'Z', '0'...'9', '_' => t.index += 1,
                    else => break,
                }
            }
            const lexeme = t.source[start..t.index];
            if (Token.getKeyword(lexeme)) |tag| res.tag = tag else {
                res = Token{ .tag = .identifier, .lexeme = lexeme };
            }
        },

        '0'...'9' => {
            while (t.index < t.source.len) {
                const digit = t.source[t.index];
                switch (digit) {
                    '0'...'9' => t.index += 1,
                    else => break,
                }
            }
            res = Token{
                .tag = .int_literal,
                .lexeme = t.source[start..t.index],
            };
        },

        '"' => {
            t.index += 1;
            while (t.index < t.source.len and
                t.source[t.index] != '"') t.index += 1;
            t.index += 1;
            res = Token{
                .tag = .string_literal,
                .lexeme = t.source[start + 1 .. t.index - 1],
            };
        },

        '+' => res.tag = .plus,
        '-' => res.tag = .minus,
        '*' => res.tag = .star,
        '/' => res.tag = .slash,

        '=' => switch (t.source[t.index]) {
            '=' => {
                res.tag = .double_equal;
                t.index += 1;
            },
            else => res.tag = .equal,
        },
        '!' => switch (t.source[t.index]) {
            '=' => {
                res.tag = .bang_equal;
                t.index += 1;
            },
            else => res.tag = .invalid,
        },
        '<' => switch (t.source[t.index]) {
            '=' => {
                res.tag = .less_or_equal_than;
                t.index += 1;
            },
            else => res.tag = .less_than,
        },
        '>' => switch (t.source[t.index]) {
            '=' => {
                res.tag = .greater_or_equal_than;
                t.index += 1;
            },
            else => res.tag = .greater_than,
        },

        '(' => res.tag = .left_paren,
        ')' => res.tag = .right_paren,
        '[' => res.tag = .left_bracket,
        ']' => res.tag = .right_bracket,
        '{' => res.tag = .left_brace,
        '}' => res.tag = .right_brace,
        ',' => res.tag = .comma,
        ';' => res.tag = .semicolon,
        ':' => res.tag = .colon,

        else => res = .{ .tag = .eof, .lexeme = "EOF" },
    }
    return res;
}

pub const Token = struct {
    tag: Tag,
    lexeme: ?[]const u8 = null,

    pub const Tag = enum {
        eof,
        invalid,
        identifier,
        int_literal,
        string_literal,

        plus,
        minus,
        star,
        slash,
        double_equal,
        bang_equal,
        less_than,
        less_or_equal_than,
        greater_than,
        greater_or_equal_than,
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
        keyword_and,
        keyword_or,
        keyword_if,
        keyword_else,
        keyword_def,
        keyword_return,
        keyword_for,
        keyword_in,
    };

    pub const keywords_map: std.StaticStringMap(Tag) = .initComptime(.{
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
            ' ', '\t', '\n', '\r' => t.index += 1,
            else => break,
        }
    }
}

fn step(t: *Tokenizer) ?u8 {
    if (t.index >= t.source.len) return null;
    const char = t.source[t.index];
    t.index += 1;
    return char;
}

const std = @import("std");
