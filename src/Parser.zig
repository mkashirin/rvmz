tokenizer: *Tokenizer,
gpa: Allocator,
current: Token,
peeked: Token,
nodes: ArrayList(Node),
eib: ArrayList(u32),
const Parser = @This();

pub fn init(tokenizer: *Tokenizer, gpa: Allocator) !Parser {
    var self = Parser{
        .tokenizer = tokenizer,
        .gpa = gpa,
        .current = undefined,
        .peeked = undefined,
        .nodes = .empty,
        .eib = .empty,
    };
    self.step();
    self.peekNext();
    return self;
}

fn step(self: *Parser) void {
    self.current = self.tokenizer.next();
}

fn peekNext(self: *Parser) void {
    self.peeked = self.tokenizer.peekNext();
}

fn expect(self: *Parser, tag: Tag) !void {
    if (self.current.tag != tag) return error.ExpectedToken;
    self.step();
}

pub fn parseStmt(self: *Parser) anyerror!NodeIndex {
    var result: anyerror!NodeIndex = undefined;
    self.peekNext();
    switch (self.current.tag) {
        .identifier => switch (self.peeked.tag) {
            .equal => {
                result = self.parseAssignStmt();
                try self.expect(.semicolon);
                return result;
            },
            else => result = self.parseExpr(),
        },
        .keyword_def => return self.parseFnDef(),
        else => result = self.parseExpr(),
    }
    try self.expect(.semicolon);
    return result;
}

fn parseAssignStmt(self: *Parser) !NodeIndex {
    var vn = try self.parseCondExpr();
    if (self.current.tag == .equal) {
        const node = self.nodes.items[@intCast(vn)];
        switch (node) {
            .identifier => |name| {
                self.step();
                const value = try self.parseAssignStmt();
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
    return self.pushNode(Node{ .fn_def = fn_def });
}

pub fn parseExpr(self: *Parser) anyerror!NodeIndex {
    return self.parseCondExpr();
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
        return self.pushNode(Node{ .cond_expr = cond_expr });
    }
    return then;
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

fn parsePrimary(self: *Parser) !NodeIndex {
    var index: NodeIndex = undefined;
    switch (self.current.tag) {
        .identifier => index = try self.parseName(),
        .int_literal => index = try self.parseIntLiteral(),
        .string_literal => index = try self.parseStringLiteral(),
        .left_paren => index = try self.parseBoxed(),
        else => return error.ExpectedExpression,
    }
    return index;
}

fn parseName(self: *Parser) !NodeIndex {
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
        return self.pushNode(Node{ .fn_call = call });
    }
    return self.pushNode(Node{ .identifier = name });
}

fn parseIntLiteral(self: *Parser) !NodeIndex {
    const value = try std.fmt.parseInt(i64, self.current.lexeme.?, 10);
    const index = try self.pushNode(Node{ .int = value });
    self.step();
    return index;
}

fn parseStringLiteral(self: *Parser) !NodeIndex {
    const value = self.current.lexeme.?;
    const index = try self.pushNode(Node{ .string = value });
    self.step();
    return index;
}

fn pushNode(self: *Parser, node: Node) Allocator.Error!NodeIndex {
    try self.nodes.append(self.gpa, node);
    const index: NodeIndex = @intCast(self.nodes.items.len - 1);
    return index;
}

fn parseBoxed(self: *Parser) !NodeIndex {
    self.step();
    const index = try self.parseExpr();
    try self.expect(.right_paren);
    return index;
}

pub const Node = union(enum) {
    int: i64,
    string: []const u8,
    identifier: []const u8,
    bin_expr: BinExpr,
    cond_expr: CondExpr,
    assign_stmt: AssignStmt,
    fn_def: FnDef,
    fn_call: FnCall,
};

const BinExpr = struct { lhs: NodeIndex, op: BinOp, rhs: NodeIndex };

pub fn binOpLexeme(bin_op: BinOp) []const u8 {
    return switch (bin_op) {
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

const FnCall = struct { fn_name: []const u8, args_start: u32, args_len: u32 };

const CondExpr = struct {
    then: NodeIndex,
    if_cond: NodeIndex,
    else_expr: NodeIndex,
};
const AssignStmt = struct { name: []const u8, value: NodeIndex };

pub const NodeIndex = u32;

const FnDef = struct {
    name: []const u8,
    args_start: u32,
    args_len: u32,
    body_start: u32,
    body_len: u32,
};

const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const Tag = Token.Tag;
