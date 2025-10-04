tokenizer: *Tokenizer,
gpa: Allocator,
current: Token,
peeked: Token,
nodes: ArrayList(Node),
eib: ArrayList(u32),
const Parser = @This();

pub fn init(tokenizer: *Tokenizer, gpa: Allocator) !Parser {
    var p = Parser{
        .tokenizer = tokenizer,
        .gpa = gpa,
        .current = undefined,
        .peeked = undefined,
        .nodes = .empty,
        .eib = .empty,
    };
    p.step();
    p.peekNext();
    return p;
}

fn step(p: *Parser) void {
    p.current = p.tokenizer.next();
}

fn peekNext(p: *Parser) void {
    p.peeked = p.tokenizer.peekNext();
}

fn expect(p: *Parser, tag: Tag) !void {
    if (p.current.tag != tag) return error.ExpectedToken;
    p.step();
}

pub fn parseStmt(p: *Parser) anyerror!NodeIndex {
    var res: anyerror!NodeIndex = undefined;
    p.peekNext();
    switch (p.current.tag) {
        .identifier => switch (p.peeked.tag) {
            .equal => {
                res = p.parseAssignStmt();
                try p.expect(.semicolon);
                return res;
            },
            else => res = p.parseExpr(),
        },
        .keyword_def => return p.parseFnDef(),
        else => res = p.parseExpr(),
    }
    try p.expect(.semicolon);
    return res;
}

fn parseAssignStmt(p: *Parser) !NodeIndex {
    var variable = try p.parseCondExpr();
    if (p.current.tag == .equal) {
        const node = p.nodes.items[@intCast(variable)];
        switch (node) {
            .identifier => |name| {
                p.step();
                const value = try p.parseAssignStmt();
                const assign_stmt = AssignStmt{
                    .name = name,
                    .value = value,
                };

                variable = try p.pushNode(Node{ .assign_stmt = assign_stmt });
            },
            else => return error.ExpectedExpression,
        }
    }
    return variable;
}

fn parseFnDef(p: *Parser) !NodeIndex {
    try p.expect(.keyword_def);
    if (p.current.tag != .identifier) return error.ExpectedIdentifier;
    const name = p.current.lexeme.?;
    p.step();

    try p.expect(.left_paren);
    const args_start: u32 = @intCast(p.eib.items.len);
    var args_len: u32 = 0;
    while (p.current.tag != .right_paren and p.current.tag != .eof) {
        if (p.current.tag != .identifier) {
            return error.ExpectedIdentifier;
        }

        const arg_node = Node{ .identifier = p.current.lexeme.? };
        const arg_index = try p.pushNode(arg_node);
        try p.eib.append(p.gpa, arg_index);
        args_len += 1;
        p.step();

        if (p.current.tag == .right_paren) break;
        try p.expect(.comma);
    }
    try p.expect(.right_paren);

    try p.expect(.left_brace);
    const body_start: u32 = @intCast(p.eib.items.len);
    var body_len: u32 = 0;
    while (p.current.tag != .right_brace and p.current.tag != .eof) {
        const stmt_index = try p.parseStmt();
        try p.eib.append(p.gpa, stmt_index);
        body_len += 1;
    }
    try p.expect(.right_brace);

    const fn_def = FnDef{
        .name = name,
        .args_start = args_start,
        .args_len = args_len,
        .body_start = body_start,
        .body_len = body_len,
    };
    return p.pushNode(Node{ .fn_def = fn_def });
}

pub fn parseExpr(p: *Parser) anyerror!NodeIndex {
    return p.parseCondExpr();
}

fn parseCondExpr(p: *Parser) !NodeIndex {
    const then = try p.parseComp();
    if (p.current.tag == .keyword_if) {
        p.step();

        const if_cond = try p.parseComp();
        if (p.current.tag != .keyword_else) return error.ExpectedToken;
        p.step();

        const else_expr = try p.parseCondExpr();
        const cond_expr = CondExpr{
            .then = then,
            .if_cond = if_cond,
            .else_expr = else_expr,
        };
        return p.pushNode(Node{ .cond_expr = cond_expr });
    }
    return then;
}

fn parseComp(p: *Parser) !NodeIndex {
    var lhs = try p.parseAddSubtr();
    while (true) {
        const op: BinOp = switch (p.current.tag) {
            .double_equal => .equal,
            .bang_equal => .not_equal,
            .less_than => .less_than,
            .less_or_equal_than => .less_or_equal_than,
            .greater_than => .greater_than,
            .greater_or_equal_than => .greater_or_equal_than,
            else => break,
        };
        p.step();
        const rhs = try p.parseAddSubtr();
        const bin_expr = BinExpr{ .lhs = lhs, .op = op, .rhs = rhs };
        lhs = try p.pushNode(Node{ .bin_expr = bin_expr });
    }
    return lhs;
}

fn parseAddSubtr(p: *Parser) !NodeIndex {
    var lhs = try p.parseMultDiv();
    while (true) {
        const op: BinOp = switch (p.current.tag) {
            .plus => .add,
            .minus => .subtr,
            else => break,
        };
        p.step();
        const rhs = try p.parseMultDiv();
        const bin_expr = BinExpr{ .lhs = lhs, .op = op, .rhs = rhs };
        lhs = try p.pushNode(Node{ .bin_expr = bin_expr });
    }
    return lhs;
}

fn parseMultDiv(p: *Parser) !NodeIndex {
    var lhs = try p.parsePrimary();
    while (true) {
        const op: BinOp = switch (p.current.tag) {
            .star => .mult,
            .slash => .div,
            else => break,
        };
        p.step();
        const rhs = try p.parseMultDiv();
        const bin_expr = BinExpr{ .lhs = lhs, .op = op, .rhs = rhs };
        lhs = try p.pushNode(Node{ .bin_expr = bin_expr });
    }
    return lhs;
}

fn parsePrimary(p: *Parser) !NodeIndex {
    var index: NodeIndex = undefined;
    switch (p.current.tag) {
        .identifier => index = try p.parseName(),
        .int_literal => index = try p.parseIntLiteral(),
        .string_literal => index = try p.parseStringLiteral(),
        .left_paren => index = try p.parseBoxed(),
        else => return error.ExpectedExpression,
    }
    return index;
}

fn parseName(p: *Parser) !NodeIndex {
    const name = p.current.lexeme.?;
    p.step();
    if (p.current.tag == .left_paren) {
        p.step();

        const args_start: u32 = @intCast(p.eib.items.len);
        var args_len: u32 = 0;
        while (p.current.tag != .right_paren and p.current.tag != .eof) {
            const arg_idx = try p.parseExpr();
            try p.eib.append(p.gpa, arg_idx);
            args_len += 1;
            if (p.current.tag == .right_paren) break;
            try p.expect(.comma);
        }
        try p.expect(.right_paren);

        const call = FnCall{
            .fn_name = name,
            .args_start = args_start,
            .args_len = args_len,
        };
        return p.pushNode(Node{ .fn_call = call });
    }
    return p.pushNode(Node{ .identifier = name });
}

fn parseIntLiteral(p: *Parser) !NodeIndex {
    const int = try std.fmt.parseInt(i64, p.current.lexeme.?, 10);
    const index = try p.pushNode(Node{ .int = int });
    p.step();
    return index;
}

fn parseStringLiteral(p: *Parser) !NodeIndex {
    const string = p.current.lexeme.?;
    const index = try p.pushNode(Node{ .string = string });
    p.step();
    return index;
}

fn pushNode(p: *Parser, node: Node) Allocator.Error!NodeIndex {
    try p.nodes.append(p.gpa, node);
    const index: NodeIndex = @intCast(p.nodes.items.len - 1);
    return index;
}

fn parseBoxed(p: *Parser) !NodeIndex {
    p.step();
    const index = try p.parseExpr();
    try p.expect(.right_paren);
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

pub const BinExpr = struct { lhs: NodeIndex, op: BinOp, rhs: NodeIndex };

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

pub const BinOp = enum {
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

pub const FnCall = struct { fn_name: []const u8, args_start: u32, args_len: u32 };

pub const CondExpr = struct {
    then: NodeIndex,
    if_cond: NodeIndex,
    else_expr: NodeIndex,
};
pub const AssignStmt = struct { name: []const u8, value: NodeIndex };

pub const NodeIndex = u32;

pub const FnDef = struct {
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
