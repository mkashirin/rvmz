const std = @import("std");

const TokenTag = enum {
    eof,
    invalid,
    ident,
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
        switch (tag) {
            .eof, .invalid, .ident, .num_literal => null,
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
        }
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
                    result = Token{ .tag = .ident, .lexeme = lexeme };
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

            else => result = .{ .tag = .eof, .lexeme = "EOF" },
        }
        return result;
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

const BinExpr = struct { left: *Node, op: BinOp, right: *Node };
const FnCall = struct { fn_name: []const u8, args: std.ArrayList(*Node) };
const TernExpr = struct { then_expr: *Node, cond: *Node, else_expr: *Node };
const AssignStmt = struct { name: []const u8, value: *Node };
const FuncDef = struct {
    name: []const u8,
    params: std.ArrayList([]const u8),
    body: std.ArrayList(*Node),
};

const Node = union(enum) {
    int: i64,
    ident: []const u8,
    bin_expr: *BinExpr,
    tern_expr: *TernExpr,
    assign_stmt: *AssignStmt,
    fn_def: *FuncDef,
    fn_call: FnCall,
};

// Parser

const Parser = struct {
    tokenizer: *Tokenizer,
    gpa: std.mem.Allocator,
    current: Token,

    pub fn init(tokenizer: *Tokenizer, gpa: std.mem.Allocator) !Parser {
        var p = Parser{
            .tokenizer = tokenizer,
            .gpa = gpa,
            .current = undefined,
        };
        p.step();
        return p;
    }

    fn step(self: *Parser) void {
        self.current = self.tokenizer.next();
        // std.debug.print("{any}\n", .{self.current});
    }

    fn expect(self: *Parser, tag: TokenTag) !void {
        if (self.current.tag != tag) return error.ExpectedToken;
        self.step();
    }

    pub fn parseExpression(self: *Parser) anyerror!*Node {
        return self.parseAssignStmt();
    }

    fn parsePrimary(self: *Parser) !*Node {
        switch (self.current.tag) {
            .num_literal => {
                const val = try std.fmt.parseInt(i64, self.current.lexeme.?, 10);
                const n = try self.gpa.create(Node);
                n.* = Node{ .int = val };
                self.step();
                return n;
            },
            .ident => {
                const name = self.current.lexeme;
                self.step();
                if (self.current.tag == .left_paren) {
                    self.step();
                    var args = std.ArrayList(*Node).empty;
                    if (self.current.tag != .right_paren) {
                        while (true) {
                            const arg = try self.parseExpression();
                            try args.append(self.gpa, arg);
                            if (self.current.tag == .comma) {
                                self.step();
                                continue;
                            } else break;
                        }
                    }
                    try self.expect(.right_paren);
                    const n = try self.gpa.create(Node);
                    n.* = Node{ .fn_call = FnCall{
                        .fn_name = name.?,
                        .args = args,
                    } };
                    return n;
                } else {
                    const n = try self.gpa.create(Node);
                    n.* = Node{ .ident = name.? };
                    return n;
                }
            },
            .left_paren => {
                self.step();
                const inner = try self.parseExpression();
                try self.expect(.right_paren);
                return inner;
            },
            else => return error.ExpectedExpression,
        }
    }

    fn parseMulDiv(self: *Parser) !*Node {
        var left = try self.parsePrimary();
        while (true) {
            const op = switch (self.current.tag) {
                .star => BinOp.mult,
                .slash => BinOp.div,
                else => break,
            };
            self.step();
            const right = try self.parsePrimary();
            const bin = try self.gpa.create(BinExpr);
            bin.* = BinExpr{ .left = left, .op = op, .right = right };
            const n = try self.gpa.create(Node);
            n.* = Node{ .bin_expr = bin };
            left = n;
        }
        return left;
    }

    fn parseAddSub(self: *Parser) !*Node {
        var left = try self.parseMulDiv();
        while (true) {
            const op = switch (self.current.tag) {
                .plus => BinOp.add,
                .minus => BinOp.subtr,
                else => break,
            };
            self.step();
            const right = try self.parseMulDiv();
            const bin = try self.gpa.create(BinExpr);
            bin.* = BinExpr{ .left = left, .op = op, .right = right };
            const n = try self.gpa.create(Node);
            n.* = Node{ .bin_expr = bin };
            left = n;
        }
        return left;
    }

    fn parseComp(self: *Parser) !*Node {
        var left = try self.parseAddSub();
        while (true) {
            const op = switch (self.current.tag) {
                .double_equal => BinOp.equal,
                .bang_equal => BinOp.not_equal,
                .less_than => BinOp.less_than,
                .less_or_equal_than => BinOp.less_or_equal_than,
                .greater_than => BinOp.greater_than,
                .greater_or_equal_than => BinOp.greater_or_equal_than,
                else => break,
            };
            self.step();
            const right = try self.parseAddSub();
            const bin = try self.gpa.create(BinExpr);
            bin.* = BinExpr{ .left = left, .op = op, .right = right };
            const n = try self.gpa.create(Node);
            n.* = Node{ .bin_expr = bin };
            left = n;
        }
        return left;
    }

    fn parseTernExpr(self: *Parser) !*Node {
        const then_expr = try self.parseComp();
        if (self.current.tag == .keyword_if) {
            self.step();
            const cond = try self.parseComp();
            if (self.current.tag != .keyword_else) return error.ExpectedToken;
            self.step();
            const else_expr = try self.parseTernExpr();
            const tern = try self.gpa.create(TernExpr);
            tern.* = TernExpr{
                .then_expr = then_expr,
                .cond = cond,
                .else_expr = else_expr,
            };
            const n = try self.gpa.create(Node);
            n.* = Node{ .tern_expr = tern };
            return n;
        }
        return then_expr;
    }

    fn parseAssignStmt(self: *Parser) !*Node {
        const expr = try self.parseTernExpr();
        if (self.current.tag == .equal) {
            switch (expr.*) {
                .ident => |name| {
                    self.step();
                    const rhs = try self.parseAssignStmt();
                    const a = try self.gpa.create(AssignStmt);
                    a.* = AssignStmt{ .name = name, .value = rhs };
                    const n = try self.gpa.create(Node);
                    n.* = Node{ .assign_stmt = a };
                    return n;
                },
                else => return error.ExpectedExpression,
            }
        }
        return expr;
    }

    fn parseFnDef(self: *Parser) !*Node {
        try self.expect(.keyword_def);
        if (self.current.tag != .ident) return error.ExpectedIdentifier;
        const name = self.current.lexeme;
        self.step();

        try self.expect(.left_paren);
        var params = std.ArrayList([]const u8).empty;
        if (self.current.tag != .right_paren) {
            while (true) {
                if (self.current.tag != .ident) return error.ExpectedIdentifier;
                try params.append(self.gpa, self.current.lexeme.?);
                self.step();
                if (self.current.tag == .comma) {
                    self.step();
                    continue;
                } else break;
            }
        }
        try self.expect(.right_paren);

        try self.expect(.left_brace);
        var body = std.ArrayList(*Node).empty;
        while (self.current.tag != .right_brace and self.current.tag != .eof) {
            const stmt = try self.parseStmt();
            try body.append(self.gpa, stmt);
        }
        try self.expect(.right_brace);

        const fd = try self.gpa.create(FuncDef);
        fd.* = FuncDef{ .name = name.?, .params = params, .body = body };
        const n = try self.gpa.create(Node);
        n.* = Node{ .fn_def = fd };
        return n;
    }

    fn parseStmt(self: *Parser) !*Node {
        switch (self.current.tag) {
            .keyword_def => return self.parseFnDef(),
            else => {
                const expr = try self.parseExpression();
                try self.expect(.semicolon);
                return expr;
            },
        }
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

fn printNode(node: *Node, indent: usize) void {
    var i: usize = 0;
    while (i < indent) : (i += 1) std.debug.print(" ", .{});

    switch (node.*) {
        .int => |v| std.debug.print("int {d}\n", .{v}),
        .ident => |s| std.debug.print("ident \"{s}\"\n", .{s}),
        .fn_call => |c| {
            std.debug.print("call {s}(\n", .{c.fn_name});
            for (c.args.items) |arg| printNode(arg, indent + 2);
            var j: usize = 0;
            while (j < indent) : (j += 1) std.debug.print(" ", .{});
            std.debug.print(")\n", .{});
        },
        .bin_expr => |bptr| {
            std.debug.print("binary {s}\n", .{opToStr(bptr.op)});
            printNode(bptr.left, indent + 2);
            printNode(bptr.right, indent + 2);
        },
        .tern_expr => |tptr| {
            std.debug.print("ternary\n", .{});
            printNode(tptr.then_expr, indent + 2);
            printNode(tptr.cond, indent + 2);
            printNode(tptr.else_expr, indent + 2);
        },
        .assign_stmt => |aptr| {
            std.debug.print("assign {s}\n", .{aptr.name});
            printNode(aptr.value, indent + 2);
        },
        .fn_def => |fptr| {
            std.debug.print("func {s}(", .{fptr.name});
            for (fptr.params.items, 0..) |p, idx| {
                if (idx != 0) std.debug.print(", ", .{});
                std.debug.print("{s}", .{p});
            }
            std.debug.print(") {{\n", .{});
            for (fptr.body.items) |stmt| printNode(stmt, indent + 2);
            var j: usize = 0;
            while (j < indent) : (j += 1) std.debug.print(" ", .{});
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
        \\    sum = x + y;
        \\    sum;
        \\}
        \\c = add(b, 4);
    ;

    var tokenizer = Tokenizer.init(src);
    var parser = try Parser.init(&tokenizer, gpa);

    var program = std.ArrayList(*Node).empty;
    while (parser.current.tag != .eof) {
        const stmt = try parser.parseStmt();
        try program.append(gpa, stmt);
    }

    std.debug.print("Parsed AST:\n", .{});
    for (program.items) |stmt| {
        printNode(stmt, 0);
        std.debug.print("---\n", .{});
    }
}
