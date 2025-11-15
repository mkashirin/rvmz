// TODO: Add list/dict comprehension, collection literals as arguments.

tokenizer: *Tokenizer,
gpa: Allocator,
current: Token,
peeked: Token,
nodes: ArrayList(Node),
eib: ArrayList(u32),
const Parser = @This();

pub const Error = Allocator.Error || fmt.ParseIntError || error{
    OutOfMemory,
    Overflow,
    InvalidCharacter,
    ExpectedToken,
    ExpectedExpression,
    ExpectedIdentifier,
};

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
    if (p.current.tag != tag) return Error.ExpectedToken;
    p.step();
}

pub fn parseStmt(p: *Parser) Error!NodeIndex {
    var res: Error!NodeIndex = undefined;
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
        .keyword_for => return p.parseForStmt(),
        .keyword_return => {
            p.step();
            const value = try p.parseExpr();
            try p.expect(.semicolon);
            const return_stmt: ReturnStmt = .{ .value = value };
            return p.pushNode(Node{ .return_stmt = return_stmt });
        },
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
                const assign_stmt: AssignStmt = .{
                    .name = name,
                    .value = value,
                };

                variable = try p.pushNode(Node{ .assign_stmt = assign_stmt });
            },
            else => return Error.ExpectedExpression,
        }
    }
    return variable;
}

fn parseFnDef(p: *Parser) !NodeIndex {
    try p.expect(.keyword_def);
    if (p.current.tag != .identifier) return Error.ExpectedIdentifier;
    const name = p.current.lexeme.?;
    p.step();

    try p.expect(.left_paren);
    const args_start: u32 = @intCast(p.eib.items.len);
    var args_len: u32 = 0;
    while (p.current.tag != .eof) {
        if (p.current.tag != .identifier) {
            return Error.ExpectedIdentifier;
        }

        const arg_node: Node = .{ .identifier = p.current.lexeme.? };
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

    const fn_def: FnDef = .{
        .name = name,
        .args_start = args_start,
        .args_len = args_len,
        .body_start = body_start,
        .body_len = body_len,
    };
    return p.pushNode(Node{ .fn_def = fn_def });
}

fn parseForStmt(p: *Parser) !NodeIndex {
    try p.expect(.keyword_for);
    if (p.current.tag != .identifier) return Error.ExpectedIdentifier;
    const var_name = p.current.lexeme.?;
    p.step();

    try p.expect(.keyword_in);

    const iterable = try p.parseExpr();

    try p.expect(.left_brace);
    const body_start: u32 = @intCast(p.eib.items.len);
    var body_len: u32 = 0;
    while (p.current.tag != .right_brace and p.current.tag != .eof) {
        const stmt_index = try p.parseStmt();
        try p.eib.append(p.gpa, stmt_index);
        body_len += 1;
    }
    try p.expect(.right_brace);

    const for_stmt: ForStmt = .{
        .var_name = var_name,
        .iterable = iterable,
        .body_start = body_start,
        .body_len = body_len,
    };
    return p.pushNode(Node{ .for_stmt = for_stmt });
}

pub fn parseExpr(p: *Parser) Error!NodeIndex {
    return p.parseCondExpr();
}

fn parseCondExpr(p: *Parser) !NodeIndex {
    const then = try p.parseAndOrIn();
    if (p.current.tag == .keyword_if) {
        p.step();

        const if_cond = try p.parseAndOrIn();
        if (p.current.tag != .keyword_else) return Error.ExpectedToken;
        p.step();

        const else_expr = try p.parseAndOrIn();
        const cond_expr: CondExpr = .{
            .then = then,
            .if_cond = if_cond,
            .else_expr = else_expr,
        };
        return p.pushNode(Node{ .cond_expr = cond_expr });
    }
    return then;
}

fn parseAndOrIn(p: *Parser) !NodeIndex {
    var lhs = try p.parseComp();
    while (true) {
        const op: BinOp = switch (p.current.tag) {
            .keyword_and => .logic_and,
            .keyword_or => .logic_or,
            .keyword_in => .is_in,
            else => break,
        };
        p.step();
        const rhs = try p.parseComp();
        const bin_expr: BinExpr = .{ .lhs = lhs, .op = op, .rhs = rhs };
        lhs = try p.pushNode(Node{ .bin_expr = bin_expr });
    }
    return lhs;
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
        const bin_expr: BinExpr = .{ .lhs = lhs, .op = op, .rhs = rhs };
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
        const bin_expr: BinExpr = .{ .lhs = lhs, .op = op, .rhs = rhs };
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
        const bin_expr: BinExpr = .{ .lhs = lhs, .op = op, .rhs = rhs };
        lhs = try p.pushNode(Node{ .bin_expr = bin_expr });
    }
    return lhs;
}

fn parsePrimary(p: *Parser) !NodeIndex {
    var primary: NodeIndex = undefined;
    switch (p.current.tag) {
        .identifier => primary = try p.parseName(),
        .int_literal => primary = try p.parseIntLiteral(),
        .string_literal => primary = try p.parseStringLiteral(),
        .left_paren => primary = try p.parseBoxed(),
        .left_bracket => primary = try p.parseList(),
        .left_brace => primary = try p.parseDictionary(),
        else => return Error.ExpectedExpression,
    }
    primary = try p.parseIndex(primary);
    return primary;
}

fn parseIndex(p: *Parser, target: NodeIndex) !NodeIndex {
    while (p.current.tag == .left_bracket) {
        p.step();
        const index = try p.parseExpr();
        try p.expect(.right_bracket);

        const index_expr: IndexExpr = .{ .target = target, .index = index };
        return p.pushNode(Node{ .index_expr = index_expr });
    }
    return target;
}

/// This function is rather specific. Not only this one handles identifiers and
/// function calls, it also targets the only language built-in, that is capable
/// of accepting bare comparison predicates as an argument (`Select` function).
fn parseName(p: *Parser) !NodeIndex {
    const name = p.current.lexeme.?;
    p.step();
    if (p.current.tag == .left_paren) {
        p.step();

        const args_start: u32 = @intCast(p.eib.items.len);
        var args_len: u32 = 0;
        while (p.current.tag != .right_paren and p.current.tag != .eof) {
            const arg_idx = p.parseExpr() catch blk: {
                if (!std.mem.eql(u8, name, "Select") or args_len != 2)
                    return Error.ExpectedExpression;

                const pred: SelectorPred = switch (p.current.tag) {
                    .double_equal => .equal_pred,
                    .bang_equal => .not_equal_pred,
                    .less_than => .less_than_pred,
                    .less_or_equal_than => .less_or_equal_than_pred,
                    .greater_than => .greater_than_pred,
                    .greater_or_equal_than => .greater_or_equal_than_pred,
                    else => return Error.ExpectedToken,
                };
                p.step();

                break :blk try p.pushNode(Node{ .selector_pred = pred });
            };
            try p.eib.append(p.gpa, arg_idx);
            args_len += 1;
            if (p.current.tag == .right_paren) break;
            try p.expect(.comma);
        }
        try p.expect(.right_paren);

        const call: FnCall = .{
            .fn_name = name,
            .args_start = args_start,
            .args_len = args_len,
        };
        return p.pushNode(Node{ .fn_call = call });
    }
    return p.pushNode(Node{ .identifier = name });
}

fn parseIntLiteral(p: *Parser) !NodeIndex {
    const int = try fmt.parseInt(i64, p.current.lexeme.?, 10);
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

fn parseList(p: *Parser) !NodeIndex {
    try p.expect(.left_bracket);

    const elements_start: u32 = @intCast(p.eib.items.len);
    var elements_len: u32 = 0;
    while (p.current.tag != .eof) {
        const element_idx = try p.parseExpr();
        try p.eib.append(p.gpa, element_idx);
        elements_len += 1;
        if (p.current.tag == .right_bracket) break;
        try p.expect(.comma);
    }
    try p.expect(.right_bracket);

    const list: List = .{
        .elems_start = elements_start,
        .elems_len = elements_len,
    };
    return p.pushNode(Node{ .list = list });
}

fn parseDictionary(p: *Parser) !NodeIndex {
    try p.expect(.left_brace);

    var keys: std.ArrayList(u32) = .empty;
    defer keys.deinit(p.gpa);
    var values: std.ArrayList(u32) = .empty;
    defer values.deinit(p.gpa);

    while (p.current.tag != .eof) {
        const key_index = try p.parseExpr();
        try keys.append(p.gpa, key_index);
        try p.expect(.colon);

        const value_index = try p.parseExpr();
        try values.append(p.gpa, value_index);

        if (p.current.tag == .right_brace) break;
        try p.expect(.comma);
    }
    try p.expect(.right_brace);

    const keys_start: u32 = @intCast(p.eib.items.len);
    try p.eib.appendSlice(p.gpa, keys.items);

    const values_start: u32 = @intCast(p.eib.items.len);
    try p.eib.appendSlice(p.gpa, values.items);

    const dictionary: Dictionary = .{
        .keys_start = keys_start,
        .keys_len = @intCast(keys.items.len),
        .vals_start = values_start,
        .vals_len = @intCast(values.items.len),
    };
    return p.pushNode(Node{ .dictionary = dictionary });
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
    return_stmt: ReturnStmt,
    list: List,
    dictionary: Dictionary,
    index_expr: IndexExpr,
    for_stmt: ForStmt,
    selector_pred: SelectorPred,
};

pub const ReturnStmt = struct { value: NodeIndex };

pub const List = struct { elems_start: u32, elems_len: u32 };

pub const Dictionary = struct {
    keys_start: u32,
    keys_len: u32,
    vals_start: u32,
    vals_len: u32,
};

pub const IndexExpr = struct { target: NodeIndex, index: NodeIndex };

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

        .logic_and => "and",
        .logic_or => "or",
        .is_in => "in",
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
    logic_and,
    logic_or,
    is_in,
};

pub fn selectorPredLexeme(pred: SelectorPred) []const u8 {
    return switch (pred) {
        .equal_pred => "==",
        .not_equal_pred => "!=",
        .less_than_pred => "<",
        .less_or_equal_than_pred => "<=",
        .greater_than_pred => ">",
        .greater_or_equal_than_pred => ">=",
    };
}

pub const SelectorPred = enum {
    equal_pred,
    not_equal_pred,
    less_than_pred,
    less_or_equal_than_pred,
    greater_than_pred,
    greater_or_equal_than_pred,
};

pub const FnCall = struct {
    fn_name: []const u8,
    args_start: u32,
    args_len: u32,
};

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

pub const ForStmt = struct {
    var_name: []const u8,
    iterable: NodeIndex,
    body_start: u32,
    body_len: u32,
};

const std = @import("std");
const fmt = std.fmt;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const Tag = Token.Tag;
