source: []const u8,
index: usize,
const Tokenizer = @This();

pub fn init(source: []const u8) Tokenizer {
    return Tokenizer{ .source = source, .index = 0 };
}

pub fn peekNext(self: *Tokenizer) Token {
    const init_index = self.index;
    const token = self.next();
    self.index = init_index;
    return token;
}

pub fn next(self: *Tokenizer) Token {
    var result: Token = .{ .tag = .invalid };
    self.skipWhitespaces();
    if (self.index >= self.source.len) {
        return Token{ .tag = .eof, .lexeme = "EOF" };
    }

    const start = self.index;
    const char = self.step().?;

    // identifiers and keywords
    switch (char) {
        'a'...'z', 'A'...'Z', '_' => {
            while (self.index < self.source.len) {
                const sub = self.source[self.index];
                switch (sub) {
                    'a'...'z', 'A'...'Z', '0'...'9', '_' => self.index += 1,
                    else => break,
                }
            }
            const lexeme = self.source[start..self.index];
            if (Token.getKeyword(lexeme)) |tag| result.tag = tag else {
                result = Token{ .tag = .identifier, .lexeme = lexeme };
            }
        },

        '0'...'9' => {
            while (self.index < self.source.len) {
                const digit = self.source[self.index];
                switch (digit) {
                    '0'...'9' => self.index += 1,
                    else => break,
                }
            }
            result = Token{
                .tag = .int_literal,
                .lexeme = self.source[start..self.index],
            };
        },

        '"' => {
            self.index += 1;
            while (self.index < self.source.len and
                self.source[self.index] != '"')
            {
                self.index += 1;
            }
            self.index += 1;
            result = Token{
                .tag = .string_literal,
                .lexeme = self.source[start + 1 .. self.index - 1],
            };
        },

        '+' => result.tag = .plus,
        '-' => result.tag = .minus,
        '*' => result.tag = .star,
        '/' => result.tag = .slash,

        '=' => switch (self.source[self.index]) {
            '=' => {
                result.tag = .double_equal;
                self.index += 1;
            },
            else => result.tag = .equal,
        },
        '!' => switch (self.source[self.index]) {
            '=' => {
                result.tag = .bang_equal;
                self.index += 1;
            },
            else => result.tag = .invalid,
        },
        '<' => switch (self.source[self.index]) {
            '=' => {
                result.tag = .less_or_equal_than;
                self.index += 1;
            },
            else => result.tag = .less_than,
        },
        '>' => switch (self.source[self.index]) {
            '=' => {
                result.tag = .greater_or_equal_than;
                self.index += 1;
            },
            else => result.tag = .greater_than,
        },

        '(' => result.tag = .left_paren,
        ')' => result.tag = .right_paren,
        '{' => result.tag = .left_brace,
        '}' => result.tag = .right_brace,
        ',' => result.tag = .comma,
        ';' => result.tag = .semicolon,

        else => result = .{ .tag = .eof, .lexeme = "EOF" },
    }
    return result;
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
        left_brace,
        right_brace,
        comma,
        semicolon,

        equal,
        keyword_def,
        keyword_if,
        keyword_else,
    };

    pub const keywords_map =
        @import("std").StaticStringMap(Tag).initComptime(.{
            .{ "def", .keyword_def },
            .{ "if", .keyword_if },
            .{ "else", .keyword_else },
        });

    pub fn getKeyword(bytes: []const u8) ?Tag {
        return keywords_map.get(bytes);
    }
};

fn skipWhitespaces(self: *Tokenizer) void {
    while (self.index < self.source.len) {
        switch (self.source[self.index]) {
            ' ', '\t', '\n', '\r' => self.index += 1,
            else => break,
        }
    }
}

fn step(self: *Tokenizer) ?u8 {
    if (self.index >= self.source.len) return null;
    const char = self.source[self.index];
    self.index += 1;
    return char;
}
