tokenizer: *Tokenizer,
gpa: Allocator,
current: Token = undefined,
peeked: Token = undefined,
nodes: ArrayList(Node) = .empty,
/// Additional data pointer buffer
adpb: ArrayList(u32) = .empty,
/// Call site argument pointer buffer
csapb: ArrayList(u32) = .empty,
const Parser = @This();

pub const Error = Allocator.Error || fmt.ParseIntError || error{
    OutOfMemory,
    Overflow,
    InvalidCharacter,

    ExpectedIdentifier,
    ExpectedLeftParen,
    ExpectedRightParen,
    ExpectedRightBracket,
    ExpectedLeftBrace,
    ExpectedRightBrace,
    ExpectedComma,
    ExpectedSemicolon,
    ExpectedColon,

    ExpectedKeywordDef,
    ExpectedKeywordIn,
    ExpectedKeywordElse,

    ExpectedSelectorPred,
    ExpectedExpression,
    ExpectedToken,
};

pub fn init(tokenizer: *Tokenizer, gpa: Allocator) !Parser {
    var p: Parser = .{ .tokenizer = tokenizer, .gpa = gpa };
    p.step();
    p.peekNext();
    return p;
}

pub fn deinit(p: *Parser) void {
    p.tokenizer.deinit();
    p.nodes.deinit(p.gpa);
    p.adpb.deinit(p.gpa);
    p.csapb.deinit(p.gpa);
    p.* = undefined;
}

pub fn buildAst(p: *Parser) Allocator.Error!ParseResult {
    var indices_: std.ArrayList(u32) = .empty;
    while (p.current.tag != .eof) {
        const stmt_index = p.parseStmt() catch |err| {
            return .{ .err = .{ .cause = p.current, .value = err } };
        };
        try indices_.append(p.gpa, stmt_index);
    }
    const indices = try indices_.toOwnedSlice(p.gpa);
    const nodes = try p.nodes.toOwnedSlice(p.gpa);
    const adbp = try p.adpb.toOwnedSlice(p.gpa);
    const csapb = try p.csapb.toOwnedSlice(p.gpa);
    return .{ .ok = .{
        .indices = indices,
        .nodes = nodes,
        .adpb = adbp,
        .csapb = csapb,
    } };
}

pub const ParseResult = union(enum) {
    ok: Tree,
    err: ResError,

    const ResError = struct { cause: Tokenizer.Token, value: Error };
    const ERR_CODE: u8 = 1;

    pub fn unwrap(pr: ParseResult, tree: *Tree) void {
        switch (pr) {
            .ok => |ok| tree.* = ok,
            .err => |err| {
                std.debug.print(
                    "Error at line {d}, column {d}: {any}\n",
                    .{
                        err.cause.location.line,
                        err.cause.location.column,
                        err.value,
                    },
                );
                std.process.exit(ERR_CODE);
            },
        }
    }
};

fn parseStmt(p: *Parser) Error!NodeIndex {
    p.peekNext();
    return switch (p.current.tag) {
        .identifier => switch (p.peeked.tag) {
            .equal => p.parseAssignStmt(),
            else => p.parseExprStmt(),
        },
        .keyword_def => p.parseFnDef(),
        .keyword_return => p.parseReturnStmt(),
        .keyword_for => p.parseForStmt(),
        else => p.parseExprStmt(),
    };
}

fn parseAssignStmt(p: *Parser) !NodeIndex {
    var variable = try p.parseCondExpr();
    if (p.current.tag != .equal) return variable;

    const node = p.nodes.items[@intCast(variable)];
    switch (node) {
        .identifier => |name| {
            p.step();
            const value = try p.parseAssignStmt();
            const assign_stmt: AssignStmt = .{
                .name = name,
                .value = value,
            };

            variable = try p.pushNode(.{ .assign_stmt = assign_stmt });
            try p.expect(.semicolon);
            p.step();
            return variable;
        },
        else => return Error.ExpectedExpression,
    }
}

fn parseFnDef(p: *Parser) !NodeIndex {
    try p.expect(.keyword_def);
    p.step();
    try p.expect(.identifier);
    const name = p.current.lexeme.?;
    p.step();

    try p.expect(.left_paren);
    p.step();
    const args_start: u32 = @intCast(p.adpb.items.len);
    var args_len: u32 = 0;
    while (true) {
        try p.expect(.identifier);

        const arg_node: Node = .{ .identifier = p.current.lexeme.? };
        const arg_index = try p.pushNode(arg_node);
        try p.adpb.append(p.gpa, arg_index);
        args_len += 1;
        p.step();

        if (p.current.tag == .right_paren) break;
        try p.expect(.comma);
        p.step();
    }
    try p.expect(.right_paren);
    p.step();

    try p.expect(.left_brace);
    p.step();
    const body_start: u32 = @intCast(p.adpb.items.len);
    var body_len: u32 = 0;
    while (p.current.tag != .right_brace) {
        const stmt_index = try p.parseStmt();
        try p.adpb.append(p.gpa, stmt_index);
        body_len += 1;
    }
    // TODO: If there is no right brace indeed, the location of the cause would
    // be bugged out. This needs to be fixed.
    try p.expect(.right_brace);
    p.step();

    const fn_def: FnDef = .{
        .name = name,
        .args_start = args_start,
        .args_len = args_len,
        .body_start = body_start,
        .body_len = body_len,
    };
    const res = p.pushNode(.{ .fn_def = fn_def });
    return res;
}

fn parseReturnStmt(p: *Parser) !NodeIndex {
    p.step();
    const value = try p.parseExpr();
    try p.expect(.semicolon);
    p.step();
    const return_stmt: ReturnStmt = .{ .value = value };
    return p.pushNode(.{ .return_stmt = return_stmt });
}

fn parseForStmt(p: *Parser) !NodeIndex {
    try p.expect(.keyword_for);
    p.step();
    try p.expect(.identifier);
    const var_name = p.current.lexeme.?;
    p.step();

    try p.expect(.keyword_in);
    p.step();

    const iterable = try p.parseExpr();

    try p.expect(.left_brace);
    p.step();
    const body_start: u32 = @intCast(p.adpb.items.len);
    var body_len: u32 = 0;
    while (p.current.tag != .right_brace) {
        const stmt_index = try p.parseStmt();
        try p.adpb.append(p.gpa, stmt_index);
        body_len += 1;
    }
    try p.expect(.right_brace);
    p.step();

    const for_stmt: ForStmt = .{
        .var_name = var_name,
        .iterable = iterable,
        .body_start = body_start,
        .body_len = body_len,
    };
    return p.pushNode(.{ .for_stmt = for_stmt });
}

fn parseExprStmt(p: *Parser) Error!NodeIndex {
    const res = p.parseExpr();
    try p.expect(.semicolon);
    p.step();
    return res;
}

fn parseExpr(p: *Parser) Error!NodeIndex {
    return p.parseCondExpr();
}

fn parseCondExpr(p: *Parser) !NodeIndex {
    const then = try p.parseAndOrIn();
    if (p.current.tag != .keyword_if) return then;
    p.step();

    const if_cond = try p.parseAndOrIn();
    try p.expect(.keyword_else);
    p.step();

    const else_expr = try p.parseAndOrIn();
    const cond_expr: CondExpr = .{
        .then = then,
        .if_cond = if_cond,
        .else_expr = else_expr,
    };
    return p.pushNode(.{ .cond_expr = cond_expr });
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
        lhs = try p.pushNode(.{ .bin_expr = bin_expr });
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
        lhs = try p.pushNode(.{ .bin_expr = bin_expr });
    }
    return lhs;
}

fn parseAddSubtr(p: *Parser) !NodeIndex {
    var lhs = try p.parseMultDivPow();
    while (true) {
        const op: BinOp = switch (p.current.tag) {
            .plus => .add,
            .minus => .subtr,
            else => break,
        };
        p.step();
        const rhs = try p.parseMultDivPow();
        const bin_expr: BinExpr = .{ .lhs = lhs, .op = op, .rhs = rhs };
        lhs = try p.pushNode(.{ .bin_expr = bin_expr });
    }
    return lhs;
}

fn parseMultDivPow(p: *Parser) !NodeIndex {
    var lhs = try p.parsePrimary();
    while (true) {
        const op: BinOp = switch (p.current.tag) {
            .star => .mult,
            .slash => .div,
            .carrot => .power,
            else => break,
        };
        p.step();
        const rhs = try p.parseMultDivPow();
        const bin_expr: BinExpr = .{ .lhs = lhs, .op = op, .rhs = rhs };
        lhs = try p.pushNode(.{ .bin_expr = bin_expr });
    }
    return lhs;
}

fn parsePrimary(p: *Parser) !NodeIndex {
    var primary: NodeIndex = try switch (p.current.tag) {
        .identifier => p.parseName(),
        .int_literal => p.parseIntLiteral(),
        .string_literal => p.parseStringLiteral(),
        .left_brace => p.parseMap(),
        .left_bracket => p.parseList(),
        .left_paren => p.parseBoxed(),
        .keyword_true, .keyword_false => p.parseBoolLiteral(),
        else => return Error.ExpectedExpression,
    };
    primary = try p.parseIndex(primary);
    return primary;
}

fn parseIndex(p: *Parser, target: NodeIndex) !NodeIndex {
    while (p.current.tag == .left_bracket) {
        p.step();
        const index = try p.parseExpr();
        try p.expect(.right_bracket);
        p.step();

        const index_expr: IndexExpr = .{ .target = target, .index = index };
        return p.pushNode(.{ .index_expr = index_expr });
    }
    return target;
}

/// This function is rather specific. Not only this one handles identifiers and
/// function calls, it also targets the only language built-in, that is capable
/// of accepting bare comparison predicates as an argument (`Select` function).
fn parseName(p: *Parser) !NodeIndex {
    const name = p.current.lexeme.?;
    p.step();
    if (p.current.tag != .left_paren)
        return p.pushNode(.{ .identifier = name });
    p.step();

    const args_start: u32 = @intCast(p.csapb.items.len);
    var args_len: u32 = 0;
    while (p.current.tag != .right_paren) {
        const arg_index = p.parseExpr() catch blk: {
            if (!std.mem.eql(u8, name, "Select") or args_len != 2)
                return Error.ExpectedExpression;

            const pred: SelectorPred = switch (p.current.tag) {
                .double_equal => .equal_pred,
                .bang_equal => .not_equal_pred,
                .less_than => .less_than_pred,
                .less_or_equal_than => .less_or_equal_than_pred,
                .greater_than => .greater_than_pred,
                .greater_or_equal_than => .greater_or_equal_than_pred,
                else => return Error.ExpectedSelectorPred,
            };
            p.step();

            break :blk try p.pushNode(.{ .selector_pred = pred });
        };
        try p.csapb.append(p.gpa, arg_index);
        args_len += 1;
        if (p.current.tag == .right_paren) break;
        try p.expect(.comma);
        p.step();
    }
    try p.expect(.right_paren);
    p.step();

    const call: FnCall = .{
        .fn_name = name,
        .args_start = args_start,
        .args_len = args_len,
    };
    return p.pushNode(.{ .fn_call = call });
}

fn parseIntLiteral(p: *Parser) !NodeIndex {
    const int = try fmt.parseInt(i64, p.current.lexeme.?, 10);
    const index = try p.pushNode(.{ .int = int });
    p.step();
    return index;
}

fn parseStringLiteral(p: *Parser) !NodeIndex {
    const string = p.current.lexeme.?;
    const index = try p.pushNode(.{ .string = string });
    p.step();
    return index;
}

fn parseBoolLiteral(p: *Parser) !NodeIndex {
    const index = switch (p.current.tag) {
        .keyword_true => p.pushNode(.{ .boolean = true }),
        .keyword_false => p.pushNode(.{ .boolean = false }),
        else => unreachable,
    };
    p.step();
    return index;
}

fn parseList(p: *Parser) !NodeIndex {
    p.step();

    if (p.current.tag == .right_bracket) {
        p.step();
        const list: List = .{
            .elems_start = @intCast(p.adpb.items.len),
            .elems_len = 0,
        };
        return p.pushNode(.{ .list = list });
    }

    // Parse the first expression. This could be a list element OR the
    // comprehension expression.
    const expr = try p.parseExpr();

    if (p.current.tag == .keyword_for) {
        try p.expect(.keyword_for);
        p.step();
        try p.expect(.identifier);
        const var_name = p.current.lexeme.?;
        p.step();

        try p.expect(.keyword_in);
        p.step();
        const iterable = try p.parseExpr();
        try p.expect(.right_bracket);
        p.step();
        const list_comp: ListComp = .{
            .expr = expr,
            .variable = var_name,
            .iterable = iterable,
        };
        return p.pushNode(.{ .list_comp = list_comp });
    }

    const elements_start: u32 = @intCast(p.adpb.items.len);
    try p.adpb.append(p.gpa, expr);
    var elements_len: u32 = 1;

    while (p.current.tag == .comma) {
        p.step();
        if (p.current.tag == .right_bracket) break;

        const element = try p.parseExpr();
        try p.adpb.append(p.gpa, element);
        elements_len += 1;
    }
    try p.expect(.right_bracket);
    p.step();

    const list: List = .{
        .elems_start = elements_start,
        .elems_len = elements_len,
    };
    return p.pushNode(.{ .list = list });
}

fn parseMap(p: *Parser) !NodeIndex {
    p.step();

    var keys: std.ArrayList(u32) = .empty;
    defer keys.deinit(p.gpa);
    var values: std.ArrayList(u32) = .empty;
    defer values.deinit(p.gpa);

    while (true) {
        const key_index = try p.parseExpr();
        try keys.append(p.gpa, key_index);
        try p.expect(.colon);
        p.step();

        const value_index = try p.parseExpr();
        try values.append(p.gpa, value_index);

        if (p.current.tag == .right_brace) break;
        try p.expect(.comma);
        p.step();
    }
    try p.expect(.right_brace);
    p.step();

    const keys_start: u32 = @intCast(p.adpb.items.len);
    try p.adpb.appendSlice(p.gpa, keys.items);

    const values_start: u32 = @intCast(p.adpb.items.len);
    try p.adpb.appendSlice(p.gpa, values.items);

    const map: Map = .{
        .keys_start = keys_start,
        .keys_len = @intCast(keys.items.len),
        .vals_start = values_start,
        .vals_len = @intCast(values.items.len),
    };
    return p.pushNode(.{ .map = map });
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
    p.step();
    return index;
}

fn step(p: *Parser) void {
    p.current = p.tokenizer.next();
}

fn peekNext(p: *Parser) void {
    p.peeked = p.tokenizer.peekNext();
}

fn expect(p: *Parser, tag: Tag) !void {
    if (p.current.tag != tag) return switch (tag) {
        .identifier => Error.ExpectedIdentifier,

        .left_paren => Error.ExpectedLeftParen,
        .right_paren => Error.ExpectedRightParen,
        .right_bracket => Error.ExpectedRightBracket,
        .left_brace => Error.ExpectedLeftBrace,
        .right_brace => Error.ExpectedRightBrace,
        .comma => Error.ExpectedComma,
        .semicolon => Error.ExpectedSemicolon,
        .colon => Error.ExpectedColon,

        .keyword_def => Error.ExpectedKeywordDef,
        .keyword_in => Error.ExpectedKeywordIn,
        .keyword_else => Error.ExpectedKeywordElse,

        else => Error.ExpectedToken,
    };
}

pub const Tree = struct {
    indices: []const u32,
    nodes: []const Node,
    adpb: []const u32,
    csapb: []const u32,
};

pub const Node = union(enum) {
    int: i64,
    string: []const u8,
    boolean: bool,
    identifier: []const u8,
    bin_expr: BinExpr,
    cond_expr: CondExpr,
    assign_stmt: AssignStmt,
    fn_def: FnDef,
    fn_call: FnCall,
    return_stmt: ReturnStmt,
    list: List,
    list_comp: ListComp,
    map: Map,
    index_expr: IndexExpr,
    for_stmt: ForStmt,
    selector_pred: SelectorPred,
};

pub const BinExpr = struct { lhs: NodeIndex, op: BinOp, rhs: NodeIndex };

pub fn binOpLexeme(bin_op: BinOp) []const u8 {
    return switch (bin_op) {
        .add => "+",
        .subtr => "-",
        .mult => "*",
        .div => "/",
        .power => "^",

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
    mult,
    power,
    subtr,
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

pub const ReturnStmt = struct { value: NodeIndex };

pub const List = struct { elems_start: u32, elems_len: u32 };

pub const ListComp = struct {
    expr: NodeIndex,
    variable: []const u8,
    iterable: NodeIndex,
};

pub const Map = struct {
    keys_start: u32,
    keys_len: u32,
    vals_start: u32,
    vals_len: u32,
};

pub const IndexExpr = struct { target: NodeIndex, index: NodeIndex };

pub const ForStmt = struct {
    var_name: []const u8,
    iterable: NodeIndex,
    body_start: u32,
    body_len: u32,
};

pub const SelectorPred = enum {
    equal_pred,
    not_equal_pred,
    less_than_pred,
    less_or_equal_than_pred,
    greater_than_pred,
    greater_or_equal_than_pred,
};

const std = @import("std");
const fmt = std.fmt;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const Tag = Token.Tag;
