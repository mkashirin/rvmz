const std = @import("std");

const TokenTag = enum {
    eof,
    invalid,
    identifier,
    num_literal,
    plus,
    minus,
    star,
    slash,
    equal,
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
    keyword_def,
    keyword_if,
    keyword_else,

    pub fn getLexeme(tag: TokenTag) ?[]const u8 {
        return switch (tag) {
            .eof, .invalid, .identifier, .num_literal => null,
            .plus => "+",
            .minus => "-",
            .star => "*",
            .slash => "/",
            .equal => "=",
            .double_equal => "==",
            .bang_equal => "!=",
            .less_than => "<",
            .less_or_equal_than => "<=",
            .greater_than => ">",
            .greater_or_equal_than => ">=",
            .left_paren => "(",
            .right_paren => ")",
            .left_brace => "{",
            .right_brace => "}",
            .comma => ",",
            .semicolon => ";",
            .keyword_def => "def",
            .keyword_if => "if",
            .keyword_else => "else",
        };
    }
};

const Token = struct {
    tag: TokenTag,
    lexeme: ?[]const u8 = null,

    pub const keywords_map = std.StaticStringMap(TokenTag).initComptime(.{
        .{ "def", .keyword_def },
        .{ "if", .keyword_if },
        .{ "else", .keyword_else },
    });

    pub fn getKeyword(bytes: []const u8) ?TokenTag {
        return keywords_map.get(bytes);
    }
};

const Tokenizer = struct {
    source: []const u8,
    index: usize,

    pub fn init(source: []const u8) Tokenizer {
        return Tokenizer{ .source = source, .index = 0 };
    }

    fn step(self: *Tokenizer) ?u8 {
        if (self.index >= self.source.len) return null;
        const char = self.source[self.index];
        self.index += 1;
        return char;
    }

    fn sws(self: *Tokenizer) void {
        while (self.index < self.source.len) {
            switch (self.source[self.index]) {
                ' ', '\t', '\n', '\r' => self.index += 1,
                else => break,
            }
        }
    }

    pub fn next(self: *Tokenizer) Token {
        var result: Token = .{ .tag = .invalid };
        self.sws();
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
                    .tag = .num_literal,
                    .lexeme = self.source[start..self.index],
                };
            },

            '=' => {
                switch (self.source[self.index]) {
                    '=' => {
                        result.tag = .double_equal;
                        self.index += 1;
                    },
                    else => result.tag = .equal,
                }
            },
            '!' => {
                switch (self.source[self.index]) {
                    '=' => {
                        result.tag = .bang_equal;
                        self.index += 1;
                    },
                    else => result.tag = .invalid,
                }
            },
            '<' => {
                switch (self.source[self.index]) {
                    '=' => {
                        result.tag = .less_or_equal_than;
                        self.index += 1;
                    },
                    else => result.tag = .less_than,
                }
            },
            '>' => {
                switch (self.source[self.index]) {
                    '=' => {
                        result.tag = .greater_or_equal_than;
                        self.index += 1;
                    },
                    else => result.tag = .greater_than,
                }
            },

            '+' => result.tag = .plus,
            '-' => result.tag = .minus,
            '*' => result.tag = .star,
            '/' => result.tag = .slash,

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

    pub fn peek(self: *Tokenizer) Token {
        const original_index = self.index;
        const token = self.next();
        self.index = original_index;
        return token;
    }
};

// AST
const BinOp = enum {
    add,
    subtr,
    mult,
    div,
    equal,
    not_equal,
    less_than,
    less_or_equal_than,
    greater_than,
    greater_or_equal_than,
};

const NodeIndex = u32;

const BinExpr = struct { lhs: NodeIndex, op: BinOp, rhs: NodeIndex };
const FnCall = struct { fn_name: []const u8, args_start: u32, args_len: u32 };
const CondExpr = struct {
    then: NodeIndex,
    if_cond: NodeIndex,
    else_expr: NodeIndex,
};
const AssignStmt = struct { name: []const u8, value: NodeIndex };

const FnDef = struct {
    name: []const u8,
    args_start: u32,
    args_len: u32,
    body_start: u32,
    body_len: u32,
};

const Node = union(enum) {
    int: i64,
    identifier: []const u8,
    bin_expr: BinExpr,
    cond_expr: CondExpr,
    assign_stmt: AssignStmt,
    fn_def: FnDef,
    fn_call: FnCall,
};

// Parser

const Parser = struct {
    tokenizer: *Tokenizer,
    gpa: std.mem.Allocator,
    current: Token,
    nodes: std.ArrayList(Node),
    eib: std.ArrayList(u32),

    pub fn init(tokenizer: *Tokenizer, gpa: std.mem.Allocator) !Parser {
        var self = Parser{
            .tokenizer = tokenizer,
            .gpa = gpa,
            .current = undefined,
            .nodes = .empty,
            .eib = .empty,
        };
        self.step();
        return self;
    }

    fn step(self: *Parser) void {
        self.current = self.tokenizer.next();
        std.debug.print("{any}\n", .{self.current});
    }

    fn expect(self: *Parser, tag: TokenTag) !void {
        if (self.current.tag != tag) return error.ExpectedToken;
        self.step();
    }

    fn pushNode(self: *Parser, node: Node) std.mem.Allocator.Error!NodeIndex {
        try self.nodes.append(self.gpa, node);
        const index: NodeIndex = @intCast(self.nodes.items.len - 1);
        return index;
    }

    pub fn parseExpr(self: *Parser) anyerror!NodeIndex {
        return self.parseAssignExpr();
    }

    fn parseNumLiteral(self: *Parser) !NodeIndex {
        const value = try std.fmt.parseInt(i64, self.current.lexeme.?, 10);
        const index = try self.pushNode(Node{ .int = value });
        self.step();
        return index;
    }

    fn parseIdentifier(self: *Parser) !NodeIndex {
        var index: NodeIndex = undefined;
        const name = self.current.lexeme.?;
        self.step();
        if (self.current.tag == .left_paren) {
            self.step();

            const args_start: u32 = @intCast(self.eib.items.len);
            var args_len: u32 = 0;
            while (self.current.tag != .right_paren and
                self.current.tag != .eof)
            {
                const arg_idx = try self.parseExpr();
                try self.eib.append(self.gpa, arg_idx);
                args_len += 1;
                if (self.current.tag == .right_paren) break;
                try self.expect(.comma);
            }
            try self.expect(.right_paren);

            const call = FnCall{
                .fn_name = name,
                .args_start = args_start,
                .args_len = args_len,
            };
            index = try self.pushNode(Node{ .fn_call = call });
        } else {
            index = try self.pushNode(Node{ .identifier = name });
        }
        return index;
    }

    fn parseBoxed(self: *Parser) !NodeIndex {
        self.step();
        const index = try self.parseExpr();
        try self.expect(.right_paren);
        return index;
    }

    fn parsePrimary(self: *Parser) !NodeIndex {
        var index: NodeIndex = undefined;
        switch (self.current.tag) {
            .identifier => index = try self.parseIdentifier(),
            .num_literal => index = try self.parseNumLiteral(),
            .left_paren => index = try self.parseBoxed(),
            else => return error.ExpectedExpression,
        }
        return index;
    }

    fn parseMultDiv(self: *Parser) !NodeIndex {
        var lhs = try self.parsePrimary();
        while (true) {
            const op: BinOp = switch (self.current.tag) {
                .star => .mult,
                .slash => .div,
                else => break,
            };
            self.step();
            const rhs = try self.parseMultDiv();
            const bin_expr = BinExpr{ .lhs = lhs, .op = op, .rhs = rhs };
            lhs = try self.pushNode(Node{ .bin_expr = bin_expr });
        }
        return lhs;
    }

    fn parseAddSubtr(self: *Parser) !NodeIndex {
        var lhs = try self.parseMultDiv();
        while (true) {
            const op: BinOp = switch (self.current.tag) {
                .plus => .add,
                .minus => .subtr,
                else => break,
            };
            self.step();
            const rhs = try self.parseMultDiv();
            const bin_expr = BinExpr{ .lhs = lhs, .op = op, .rhs = rhs };
            lhs = try self.pushNode(Node{ .bin_expr = bin_expr });
        }
        return lhs;
    }

    fn parseComp(self: *Parser) !NodeIndex {
        var lhs = try self.parseAddSubtr();
        while (true) {
            const op: BinOp = switch (self.current.tag) {
                .double_equal => .equal,
                .bang_equal => .not_equal,
                .less_than => .less_than,
                .less_or_equal_than => .less_or_equal_than,
                .greater_than => .greater_than,
                .greater_or_equal_than => .greater_or_equal_than,
                else => break,
            };
            self.step();
            const rhs = try self.parseAddSubtr();
            const bin_expr = BinExpr{ .lhs = lhs, .op = op, .rhs = rhs };
            lhs = try self.pushNode(Node{ .bin_expr = bin_expr });
        }
        return lhs;
    }

    fn parseCondExpr(self: *Parser) !NodeIndex {
        const then = try self.parseComp();
        if (self.current.tag == .keyword_if) {
            self.step();

            const if_cond = try self.parseComp();
            if (self.current.tag != .keyword_else) return error.ExpectedToken;
            self.step();

            const else_expr = try self.parseCondExpr();
            const cond_expr = CondExpr{
                .then = then,
                .if_cond = if_cond,
                .else_expr = else_expr,
            };
            return try self.pushNode(Node{ .cond_expr = cond_expr });
        }
        return then;
    }

    fn parseAssignExpr(self: *Parser) !NodeIndex {
        var vn = try self.parseIdentifier();
        if (self.current.tag == .equal) {
            const node = self.nodes.items[@intCast(vn)];
            switch (node) {
                .identifier => |name| {
                    self.step();
                    const value = try self.parseAssignExpr();
                    const assign_stmt = AssignStmt{
                        .name = name,
                        .value = value,
                    };

                    vn = try self.pushNode(Node{ .assign_stmt = assign_stmt });
                },
                else => return error.ExpectedExpression,
            }
        }
        return vn;
    }

    fn parseFnDef(self: *Parser) !NodeIndex {
        try self.expect(.keyword_def);
        if (self.current.tag != .identifier) return error.ExpectedIdentifier;
        const name = self.current.lexeme.?;
        self.step();

        try self.expect(.left_paren);
        const args_start: u32 = @intCast(self.eib.items.len);
        var args_len: u32 = 0;
        while (self.current.tag != .right_paren and self.current.tag != .eof) {
            if (self.current.tag != .identifier) {
                return error.ExpectedIdentifier;
            }

            const arg_node = Node{ .identifier = self.current.lexeme.? };
            const arg_index = try self.pushNode(arg_node);
            try self.eib.append(self.gpa, arg_index);
            args_len += 1;
            self.step();

            if (self.current.tag == .right_paren) break;
            try self.expect(.comma);
        }
        try self.expect(.right_paren);

        try self.expect(.left_brace);
        const body_start: u32 = @intCast(self.eib.items.len);
        var body_len: u32 = 0;
        while (self.current.tag != .right_brace and self.current.tag != .eof) {
            const stmt_index = try self.parseStmt();
            try self.eib.append(self.gpa, stmt_index);
            body_len += 1;
        }
        try self.expect(.right_brace);

        const fn_def = FnDef{
            .name = name,
            .args_start = args_start,
            .args_len = args_len,
            .body_start = body_start,
            .body_len = body_len,
        };
        return try self.pushNode(Node{ .fn_def = fn_def });
    }

    fn parseStmt(self: *Parser) anyerror!NodeIndex {
        var result: NodeIndex = undefined;
        if (self.current.tag == .keyword_def) {
            result = try self.parseFnDef();
        } else {
            result = try self.parseExpr();
            try self.expect(.semicolon);
        }
        return result;
    }
};

fn opToStr(op: BinOp) []const u8 {
    return switch (op) {
        .add => "+",
        .subtr => "-",
        .mult => "*",
        .div => "/",
        .equal => "==",
        .not_equal => "!=",
        .less_than => "<",
        .less_or_equal_than => "<=",
        .greater_than => ">",
        .greater_or_equal_than => ">=",
    };
}

fn printIndent(indent: usize) void {
    var i: usize = 0;
    while (i < indent) : (i += 1) std.debug.print(" ", .{});
}

fn printNodeByIndex(
    nodes: []const Node,
    idx_buf: []const u32,
    idx: NodeIndex,
    indent: usize,
) void {
    const node = nodes[@intCast(idx)];
    printIndent(indent);
    switch (node) {
        .int => |v| std.debug.print("int {d}\n", .{v}),
        .identifier => |s| std.debug.print("ident \"{s}\"\n", .{s}),
        .bin_expr => |b| {
            std.debug.print("binary {s}\n", .{opToStr(b.op)});
            printNodeByIndex(nodes, idx_buf, b.lhs, indent + 2);
            printNodeByIndex(nodes, idx_buf, b.rhs, indent + 2);
        },
        .cond_expr => |t| {
            std.debug.print("ternary\n", .{});
            printNodeByIndex(nodes, idx_buf, t.then, indent + 2);
            printNodeByIndex(nodes, idx_buf, t.if_cond, indent + 2);
            printNodeByIndex(nodes, idx_buf, t.else_expr, indent + 2);
        },
        .assign_stmt => |a| {
            std.debug.print("assign {s}\n", .{a.name});
            printNodeByIndex(nodes, idx_buf, a.value, indent + 2);
        },
        .fn_call => |c| {
            std.debug.print("call {s}(\n", .{c.fn_name});
            const start: usize = @intCast(c.args_start);
            const end = start + @as(usize, c.args_len);
            var i: usize = start;
            while (i < end) : (i += 1) {
                printNodeByIndex(nodes, idx_buf, idx_buf[i], indent + 2);
            }
            printIndent(indent);
            std.debug.print(")\n", .{});
        },
        .fn_def => |f| {
            std.debug.print("func {s}(\n", .{f.name});
            const pstart: usize = @intCast(f.args_start);
            const pend = pstart + @as(usize, f.args_len);
            var i: usize = pstart;
            while (i < pend) : (i += 1) {
                const param_idx = idx_buf[i];
                const pn = nodes[@as(usize, param_idx)];
                std.debug.print("  param: {s}\n", .{pn.identifier});
            }
            std.debug.print(") {{\n", .{});
            const bstart = @as(usize, f.body_start);
            const bend = bstart + @as(usize, f.body_len);
            i = bstart;
            while (i < bend) : (i += 1) {
                printNodeByIndex(nodes, idx_buf, idx_buf[i], indent + 2);
            }
            printIndent(indent);
            std.debug.print("}}\n", .{});
        },
    }
}

// Main (test)

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const gpa = arena.allocator();

    const src =
        \\a = 1 + 2 * 3 >= 0;
        \\b = a if a > 3 else 0;
        \\def add(x, y) {
        \\    x + y;
        \\}
        \\c = add(a, b);
    ;

    var tokenizer = Tokenizer.init(src);
    var parser = try Parser.init(&tokenizer, gpa);

    var program_indices: std.ArrayList(u32) = .empty;
    while (parser.current.tag != .eof) {
        const stmt_index = try parser.parseStmt();
        try program_indices.append(gpa, stmt_index);
    }

    std.debug.print("Parsed AST (index-backed):\n", .{});
    const nodes_slice = parser.nodes.items;
    const eib_slice = parser.eib.items;

    for (program_indices.items) |node_index| {
        printNodeByIndex(nodes_slice, eib_slice, node_index, 0);
        std.debug.print("---\n", .{});
    }
}
