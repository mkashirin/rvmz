pub const Parser = struct {
    tokenizer: *Tokenizer,
    gpa: Allocator,
    current: Token = undefined,
    peeked: Token = undefined,
    nodes: ArrayList(Node) = .empty,
    const Self = @This();

    pub fn init(tokenizer: *Tokenizer, gpa: Allocator) !Self {
        var p: Self = .{ .tokenizer = tokenizer, .gpa = gpa };
        p.step();
        p.peekNext();
        return p;
    }

    pub fn deinit(self: *Self) void {
        self.tokenizer.deinit();
        self.nodes.deinit(self.gpa);
        self.* = undefined;
    }

    pub fn buildAst(self: *Self) Error!Tree {
        var indices_ = NodeIndexList.empty;
        while (self.current.tag != .eof) {
            const stmt_index = try self.parseStmt();
            try indices_.append(self.gpa, stmt_index);
        }
        const indices = try indices_.toOwnedSlice(self.gpa);
        const nodes = try self.nodes.toOwnedSlice(self.gpa);
        return .{ .indices = indices, .nodes = nodes };
    }

    fn parseStmt(self: *Self) Error!NodeIndex {
        self.peekNext();
        return switch (self.current.tag) {
            .ident => switch (self.peeked.tag) {
                .equal => self.parseAssignStmt(),
                else => self.parseExprStmt(),
            },
            .keyword_def => self.parseFnDef(),
            .keyword_return => self.parseReturnStmt(),
            .keyword_for => self.parseForStmt(),
            else => self.parseExprStmt(),
        };
    }

    fn parseAssignStmt(self: *Self) !NodeIndex {
        var variable = try self.parseCondExpr();
        if (self.current.tag != .equal) return variable;

        const node = self.nodes.items[@intCast(variable)];
        switch (node) {
            .ident => |name| {
                self.step();

                const value = try self.parseAssignStmt();
                const assign_stmt: AssignStmt = .{
                    .name = name,
                    .value = value,
                };

                variable = try self.pushNode(.{ .assign_stmt = assign_stmt });
                try self.expectToken(.semicolon);
                self.step();
                return variable;
            },
            else => return Error.ExpectedExpression,
        }
    }

    fn parseFnDef(self: *Self) !NodeIndex {
        try self.expectToken(.keyword_def);
        self.step();

        try self.expectToken(.ident);
        const name = self.current.lexeme.?;
        self.step();

        try self.expectToken(.left_paren);
        self.step();
        var args = NodeIndexList.empty;
        while (true) {
            try self.expectToken(.ident);
            const arg = try self.pushNode(.{ .ident = self.current.lexeme.? });
            try args.append(self.gpa, arg);
            self.step();

            if (self.current.tag == .right_paren) break;
            try self.expectToken(.comma);
            self.step();
        }
        self.step();

        try self.expectToken(.left_brace);
        self.step();

        var body_nodes = NodeIndexList.empty;
        // TODO: If there is no right brace indeed, the location of the cause
        // would be bugged out. This needs to be fixed.
        while (self.current.tag != .right_brace) {
            const stmt_index = try self.parseStmt();
            try body_nodes.append(self.gpa, stmt_index);
        }
        self.step();

        const fn_def: FnDef = .{
            .name = name,
            .args = try args.toOwnedSlice(self.gpa),
            .body = try body_nodes.toOwnedSlice(self.gpa),
        };
        const res = self.pushNode(.{ .fn_def = fn_def });
        return res;
    }

    fn parseReturnStmt(self: *Self) !NodeIndex {
        self.step();
        const value = try self.parseExpr();
        try self.expectToken(.semicolon);

        self.step();
        const return_stmt: ReturnStmt = .{ .value = value };
        return self.pushNode(.{ .return_stmt = return_stmt });
    }

    fn parseForStmt(self: *Self) !NodeIndex {
        try self.expectToken(.keyword_for);
        self.step();

        try self.expectToken(.ident);
        const var_name = self.current.lexeme.?;
        self.step();

        try self.expectToken(.keyword_in);
        self.step();

        const iterable = try self.parseExpr();
        try self.expectToken(.left_brace);
        self.step();

        var body_nodes = NodeIndexList.empty;
        while (self.current.tag != .right_brace) {
            const stmt_index = try self.parseStmt();
            try body_nodes.append(self.gpa, stmt_index);
        }
        self.step();

        const for_stmt: ForStmt = .{
            .var_name = var_name,
            .iterable = iterable,
            .body = try body_nodes.toOwnedSlice(self.gpa),
        };
        return self.pushNode(.{ .for_stmt = for_stmt });
    }

    fn parseExprStmt(self: *Self) Error!NodeIndex {
        const res = self.parseExpr();
        try self.expectToken(.semicolon);
        self.step();
        return res;
    }

    fn parseExpr(self: *Self) Error!NodeIndex {
        return self.parseCondExpr();
    }

    fn parseCondExpr(self: *Self) !NodeIndex {
        const then = try self.parseAndOrIn();
        if (self.current.tag != .keyword_if) return then;
        self.step();

        const if_cond = try self.parseAndOrIn();
        try self.expectToken(.keyword_else);
        self.step();

        const else_expr = try self.parseAndOrIn();
        const cond_expr: CondExpr = .{
            .then = then,
            .if_cond = if_cond,
            .else_expr = else_expr,
        };
        return self.pushNode(.{ .cond_expr = cond_expr });
    }

    fn parseAndOrIn(self: *Self) !NodeIndex {
        var lhs = try self.parseComp();
        while (true) {
            const op: BinOp = switch (self.current.tag) {
                .keyword_and => .logic_and,
                .keyword_or => .logic_or,
                .keyword_in => .is_in,
                else => break,
            };
            self.step();

            const rhs = try self.parseComp();
            const bin_expr: BinExpr = .{ .lhs = lhs, .op = op, .rhs = rhs };
            lhs = try self.pushNode(.{ .bin_expr = bin_expr });
        }
        return lhs;
    }

    fn parseComp(self: *Self) !NodeIndex {
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
            const bin_expr: BinExpr = .{ .lhs = lhs, .op = op, .rhs = rhs };
            lhs = try self.pushNode(.{ .bin_expr = bin_expr });
        }
        return lhs;
    }

    fn parseAddSubtr(self: *Self) !NodeIndex {
        var lhs = try self.parseMultDivPow();
        while (true) {
            const op: BinOp = switch (self.current.tag) {
                .plus => .add,
                .minus => .subtr,
                else => break,
            };
            self.step();

            const rhs = try self.parseMultDivPow();
            const bin_expr: BinExpr = .{ .lhs = lhs, .op = op, .rhs = rhs };
            lhs = try self.pushNode(.{ .bin_expr = bin_expr });
        }
        return lhs;
    }

    fn parseMultDivPow(self: *Self) !NodeIndex {
        var lhs = try self.parsePrimary();
        while (true) {
            const op: BinOp = switch (self.current.tag) {
                .star => .mult,
                .slash => .div,
                .carrot => .power,
                else => break,
            };
            self.step();

            const rhs = try self.parseMultDivPow();
            const bin_expr: BinExpr = .{ .lhs = lhs, .op = op, .rhs = rhs };
            lhs = try self.pushNode(.{ .bin_expr = bin_expr });
        }
        return lhs;
    }

    fn parsePrimary(self: *Self) !NodeIndex {
        var primary: NodeIndex = try switch (self.current.tag) {
            .ident => self.parseName(),
            .int_literal => self.parseIntLiteral(),
            .string_literal => self.parseStringLiteral(),
            .left_brace => self.parseMap(),
            .left_bracket => self.parseList(),
            .left_paren => self.parseBoxed(),
            .keyword_true, .keyword_false => self.parseBoolLiteral(),
            else => return Error.ExpectedExpression,
        };
        primary = try self.parseIndex(primary);
        return primary;
    }

    fn parseIndex(self: *Self, target: NodeIndex) !NodeIndex {
        while (self.current.tag == .left_bracket) {
            self.step();

            const index = try self.parseExpr();
            try self.expectToken(.right_bracket);
            self.step();

            const index_expr: IndexExpr = .{
                .target = target,
                .index = index,
            };
            return self.pushNode(.{ .index_expr = index_expr });
        }
        return target;
    }

    /// This function is rather specific. Not only this one handles identifiers
    /// and function calls, it also targets the only language built-in, that is
    /// capable of accepting bare comparison predicates as an argument
    /// (`Select` function).
    fn parseName(self: *Self) !NodeIndex {
        const name = self.current.lexeme.?;
        self.step();

        if (self.current.tag != .left_paren)
            return self.pushNode(.{ .ident = name });
        self.step();

        var args = NodeIndexList.empty;
        while (true) {
            const arg = self.parseExpr() catch blk: {
                if (!std.mem.eql(u8, name, "Select") or args.items.len != 2)
                    return Error.ExpectedExpression;

                const pred: SelectorPred = switch (self.current.tag) {
                    .double_equal => .equal_pred,
                    .bang_equal => .not_equal_pred,
                    .less_than => .less_than_pred,
                    .less_or_equal_than => .less_or_equal_than_pred,
                    .greater_than => .greater_than_pred,
                    .greater_or_equal_than => .greater_or_equal_than_pred,
                    else => return Error.ExpectedSelectorPred,
                };
                self.step();

                break :blk try self.pushNode(.{ .selector_pred = pred });
            };
            try args.append(self.gpa, arg);
            if (self.current.tag == .right_paren) break;
            try self.expectToken(.comma);
            self.step();
        }
        self.step();
        const call: FnCall = .{
            .name = name,
            .args = try args.toOwnedSlice(self.gpa),
        };
        return self.pushNode(.{ .fn_call = call });
    }

    fn parseIntLiteral(self: *Self) !NodeIndex {
        const int = try fmt.parseInt(i64, self.current.lexeme.?, 10);
        const index = try self.pushNode(.{ .int = int });
        self.step();
        return index;
    }

    fn parseStringLiteral(self: *Self) !NodeIndex {
        const string = self.current.lexeme.?;
        const index = try self.pushNode(.{ .string = string });
        self.step();
        return index;
    }

    fn parseBoolLiteral(self: *Self) !NodeIndex {
        const index = switch (self.current.tag) {
            .keyword_true => self.pushNode(.{ .boolean = true }),
            .keyword_false => self.pushNode(.{ .boolean = false }),
            else => unreachable,
        };
        self.step();
        return index;
    }

    fn parseList(self: *Self) !NodeIndex {
        self.step();
        const expr = try self.parseExpr();
        if (self.current.tag != .keyword_for) {
            var elems = NodeIndexList.empty;
            try elems.append(self.gpa, expr);
            while (self.current.tag == .comma) {
                self.step();
                if (self.current.tag == .right_bracket) break;
                const elem = try self.parseExpr();
                try elems.append(self.gpa, elem);
            }
            self.step();

            const list: List = .{ .elems = try elems.toOwnedSlice(self.gpa) };
            return self.pushNode(.{ .list = list });
        }

        self.step();
        try self.expectToken(.ident);
        const var_name = self.current.lexeme.?;
        self.step();

        try self.expectToken(.keyword_in);
        self.step();

        const iterable = try self.parseExpr();
        try self.expectToken(.right_bracket);
        self.step();
        const list_comp: ListComp = .{
            .expr = expr,
            .variable = var_name,
            .iterable = iterable,
        };
        return self.pushNode(.{ .list_comp = list_comp });
    }

    fn parseMap(p: *Self) !NodeIndex {
        p.step();
        var keys = NodeIndexList.empty;
        var vals = NodeIndexList.empty;
        while (true) {
            const key = try p.parseExpr();
            try keys.append(p.gpa, key);
            try p.expectToken(.colon);
            p.step();

            const val = try p.parseExpr();
            try vals.append(p.gpa, val);
            if (p.current.tag == .right_brace) break;
            try p.expectToken(.comma);
            p.step();
        }
        p.step();
        const map: Map = .{
            .keys = try keys.toOwnedSlice(p.gpa),
            .vals = try vals.toOwnedSlice(p.gpa),
        };
        return p.pushNode(.{ .map = map });
    }

    fn pushNode(self: *Self, node: Node) Allocator.Error!NodeIndex {
        try self.nodes.append(self.gpa, node);
        const index: NodeIndex = @intCast(self.nodes.items.len - 1);
        return index;
    }

    fn parseBoxed(self: *Self) !NodeIndex {
        self.step();
        const index = try self.parseExpr();
        try self.expectToken(.right_paren);

        self.step();
        return index;
    }

    fn step(self: *Self) void {
        self.current = self.tokenizer.next();
    }

    fn peekNext(self: *Self) void {
        self.peeked = self.tokenizer.peekNext();
    }

    fn expectToken(self: *Self, tag: Tag) !void {
        if (self.current.tag != tag) return switch (tag) {
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

    const Error = Allocator.Error || fmt.ParseIntError || error{
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
};

pub const Tree = struct {
    indices: []const u32,
    nodes: []const Node,
    const Self = @This();

    pub fn deinit(self: *Self, gpa: Allocator) void {
        gpa.free(self.indices);
        for (self.nodes) |node| switch (node) {
            .fn_call => |fn_call| gpa.free(fn_call.args),
            .fn_def => |fn_def| inline for (.{ "args", "body" }) |field_name|
                gpa.free(@field(fn_def, field_name)),
            .for_stmt => |for_stmt| gpa.free(for_stmt.body),
            .list => |list| gpa.free(list.elems),
            .map => |map| inline for (.{ "keys", "vals" }) |field_name|
                gpa.free(@field(map, field_name)),
            else => {},
        };
        gpa.free(self.nodes);
        self.* = undefined;
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
    const ta = std.testing.allocator;

    var tokenizer: Tokenizer = .init(source);
    var parser: Parser = try .init(&tokenizer, ta);
    var tree: Tree = undefined;
    defer tree.deinit(ta);
    tree = parser.buildAst() catch |err| {
        const err_location = parser.current.location;
        std.debug.print(
            "Error at line {d}, column {d}: {s}\n",
            .{ err_location.line, err_location.column, @errorName(err) },
        );
        parser.deinit();
        return err;
    };
}

const std = @import("std");
const fmt = std.fmt;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;

const Tokenizer = @import("Tokenizer.zig");
const Token = Tokenizer.Token;
const Tag = Token.Tag;
