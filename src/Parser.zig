tokenizer: *Tokenizer,
gpa: Allocator,
current: Token = undefined,
peeked: Token = undefined,
nodes: ArrayList(Node) = .empty,
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
    p.* = undefined;
}

pub fn buildAst(p: *Parser) Allocator.Error!ParseResult {
    var indices_ = NodeIndexList.empty;
    while (p.current.tag != .eof) {
        const stmt_index = p.parseStmt() catch |err| {
            return .{ .err = .{ .cause = p.current, .value = err } };
        };
        try indices_.append(p.gpa, stmt_index);
    }
    const indices = try indices_.toOwnedSlice(p.gpa);
    const nodes = try p.nodes.toOwnedSlice(p.gpa);
    return .{ .ok = .{
        .indices = indices,
        .nodes = nodes,
    } };
}

pub const ParseResult = union(enum) {
    ok: Tree,
    err: ResError,

    const ResError = struct { cause: Tokenizer.Token, value: Error };
    const ERR_CODE: u8 = 1;

    pub fn unwrap(res: ParseResult, tree: *Tree) void {
        switch (res) {
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
        .ident => switch (p.peeked.tag) {
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
        .ident => |name| {
            p.step();

            const value = try p.parseAssignStmt();
            const assign_stmt: AssignStmt = .{ .name = name, .value = value };

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

    try p.expect(.ident);
    const name = p.current.lexeme.?;
    p.step();

    try p.expect(.left_paren);
    p.step();
    var args = NodeIndexList.empty;
    while (true) {
        try p.expect(.ident);
        const arg = try p.pushNode(.{ .ident = p.current.lexeme.? });
        try args.append(p.gpa, arg);
        p.step();

        if (p.current.tag == .right_paren) break;
        try p.expect(.comma);
        p.step();
    }
    p.step();

    try p.expect(.left_brace);
    p.step();

    var body_nodes = NodeIndexList.empty;
    // TODO: If there is no right brace indeed, the location of the cause would
    // be bugged out. This needs to be fixed.
    while (p.current.tag != .right_brace) {
        const stmt_index = try p.parseStmt();
        try body_nodes.append(p.gpa, stmt_index);
    }
    p.step();

    const fn_def: FnDef = .{
        .name = name,
        .args = try args.toOwnedSlice(p.gpa),
        .body = try body_nodes.toOwnedSlice(p.gpa),
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

    try p.expect(.ident);
    const var_name = p.current.lexeme.?;
    p.step();

    try p.expect(.keyword_in);
    p.step();

    const iterable = try p.parseExpr();
    try p.expect(.left_brace);
    p.step();

    var body_nodes = NodeIndexList.empty;
    while (p.current.tag != .right_brace) {
        const stmt_index = try p.parseStmt();
        try body_nodes.append(p.gpa, stmt_index);
    }
    p.step();

    const for_stmt: ForStmt = .{
        .var_name = var_name,
        .iterable = iterable,
        .body = try body_nodes.toOwnedSlice(p.gpa),
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
        .ident => p.parseName(),
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
        return p.pushNode(.{ .ident = name });
    p.step();

    var args = NodeIndexList.empty;
    while (true) {
        const arg = p.parseExpr() catch blk: {
            if (!std.mem.eql(u8, name, "Select") or args.items.len != 2)
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
        try args.append(p.gpa, arg);
        if (p.current.tag == .right_paren) break;
        try p.expect(.comma);
        p.step();
    }
    p.step();
    const call: FnCall = .{
        .name = name,
        .args = try args.toOwnedSlice(p.gpa),
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

    const expr = try p.parseExpr();
    if (p.current.tag != .keyword_for) {
        var elems = NodeIndexList.empty;
        try elems.append(p.gpa, expr);

        while (p.current.tag == .comma) {
            p.step();
            if (p.current.tag == .right_bracket) break;

            const elem = try p.parseExpr();
            try elems.append(p.gpa, elem);
        }
        p.step();

        const list: List = .{ .elems = try elems.toOwnedSlice(p.gpa) };
        return p.pushNode(.{ .list = list });
    }
    try p.expect(.keyword_for);
    p.step();

    try p.expect(.ident);
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

fn parseMap(p: *Parser) !NodeIndex {
    p.step();

    var keys = NodeIndexList.empty;
    var vals = NodeIndexList.empty;
    while (true) {
        const key = try p.parseExpr();
        try keys.append(p.gpa, key);
        try p.expect(.colon);
        p.step();

        const val = try p.parseExpr();
        try vals.append(p.gpa, val);
        if (p.current.tag == .right_brace) break;
        try p.expect(.comma);
        p.step();
    }
    try p.expect(.right_brace);
    p.step();
    const map: Map = .{
        .keys = try keys.toOwnedSlice(p.gpa),
        .vals = try vals.toOwnedSlice(p.gpa),
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
        .ident => Error.ExpectedIdentifier,

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

    pub fn deinit(t: *Tree, gpa: Allocator) void {
        gpa.free(t.indices);
        for (t.nodes) |node| switch (node) {
            .fn_call => |fn_call| gpa.free(fn_call.args),
            .fn_def => |fn_def| inline for (.{ "args", "body" }) |field_name|
                gpa.free(@field(fn_def, field_name)),
            .for_stmt => |for_stmt| gpa.free(for_stmt.body),
            .list => |list| gpa.free(list.elems),
            .map => |map| inline for (.{ "keys", "vals" }) |field_name|
                gpa.free(@field(map, field_name)),
            else => {},
        };
        gpa.free(t.nodes);
        t.* = undefined;
    }
};

pub const Node = union(enum) {
    int: i64,
    string: []const u8,
    boolean: bool,
    ident: []const u8,
    bin_expr: BinExpr,
    cond_expr: CondExpr,
    assign_stmt: AssignStmt,
    fn_call: FnCall,
    fn_def: FnDef,
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
        .power => "^",
        .div => "/",

        .equal => "==",
        .not_equal => "!=",
        .greater_than => ">",
        .greater_or_equal_than => ">=",
        .less_than => "<",
        .less_or_equal_than => "<=",

        .logic_and => "and",
        .logic_or => "or",
        .is_in => "in",
    };
}

pub const BinOp = enum {
    add,
    subtr,
    mult,
    power,
    div,
    equal,
    not_equal,
    greater_than,
    greater_or_equal_than,
    less_than,
    less_or_equal_than,
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

pub const FnCall = struct { name: []const u8, args: []const NodeIndex };

pub const CondExpr = struct {
    then: NodeIndex,
    if_cond: NodeIndex,
    else_expr: NodeIndex,
};
pub const AssignStmt = struct { name: []const u8, value: NodeIndex };

pub const FnDef = struct {
    name: []const u8,
    args: []const NodeIndex,
    body: []const NodeIndex,
};

pub const ReturnStmt = struct { value: NodeIndex };

pub const ForStmt = struct {
    var_name: []const u8,
    iterable: NodeIndex,
    body: []const NodeIndex,
};

pub const List = struct { elems: []const NodeIndex };

pub const ListComp = struct {
    expr: NodeIndex,
    variable: []const u8,
    iterable: NodeIndex,
};

pub const Map = struct { keys: []const NodeIndex, vals: []const NodeIndex };

pub const IndexExpr = struct { target: NodeIndex, index: NodeIndex };

pub const NodeIndexList = ArrayList(NodeIndex);
pub const NodeIndex = u32;

pub const SelectorPred = enum {
    equal_pred,
    not_equal_pred,
    less_than_pred,
    less_or_equal_than_pred,
    greater_than_pred,
    greater_or_equal_than_pred,
};

test {
    const source =
        \\an_int = 4 / 2;
        \\the_int = 2^3;
        \\
        \\
        \\def add(a, b) {
        \\    sum = a + b;
        \\    return sum;
        \\}
        \\
        \\int_sum = add(a_int, the_int);
        \\print("Success") if c > 0 else print(0);
        \\
        \\0 if true and the_int - an_int else int_sum or "Huh?";
        \\
        \\a_list = [1, 2, 3];
        \\a_dict = {"integer": 1, "list": [2, 3]};
        \\the_list = [0, {"one": 1}, 2 + 3];
        \\
        \\zero = the_list[a_list[0]];
        \\
        \\for n in a_list {
        \\    print(n + 1);
        \\}
        \\
        \\zero_in_the_list = 0 in the_list;
        \\
        \\selector = Select([1, 2, 3], [3, 2, 1], !=);
        \\
        \\list_comp = [i + 1 if i > 0 else i for i in a_list];
    ;

    var tokenizer: Tokenizer = .init(source);
    var parser: Parser = try .init(&tokenizer, std.testing.allocator);
    var result = try parser.buildAst();
    defer {
        var tree: Parser.Tree = undefined;
        result.unwrap(&tree);
        tree.deinit(std.testing.allocator);
    }
    try std.testing.expectEqual(.ok, std.meta.activeTag(result));
}

const std = @import("std");
const fmt = std.fmt;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const Tag = Token.Tag;
