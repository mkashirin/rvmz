pub const Parser = struct {
    tokenizer: *Tokenizer,
    gpa: Allocator,
    current: Token = undefined,
    peeked: Token = undefined,
    nodes: ArrayList(Node) = .empty,
    const Self = @This();

    pub fn init(tokenizer: *Tokenizer, gpa: Allocator) !Self {
        var self: Self = .{ .tokenizer = tokenizer, .gpa = gpa };
        self.step();
        self.peek();
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.tokenizer.deinit();
        self.nodes.deinit(self.gpa);
        self.* = undefined;
    }

    pub fn buildTree(self: *Self) Error!Tree {
        var indices_ = Indices.empty;
        while (self.current.tag != .eof) {
            const stmt_index = try self.stmt();
            try indices_.append(self.gpa, stmt_index);
        }
        const indices = try indices_.toOwnedSlice(self.gpa);
        const nodes = try self.nodes.toOwnedSlice(self.gpa);
        return .{ .indices = indices, .nodes = nodes };
    }

    fn stmt(self: *Self) Error!Index {
        self.peek();
        return switch (self.current.tag) {
            .ident => if (self.peeked.tag == .equal)
                self.assignStmt()
            else
                self.exprStmt(),
            .keyword_def => self.fnDef(),
            .keyword_return => self.returnStmt(),
            .keyword_for => self.forStmt(),
            else => self.exprStmt(),
        };
    }

    fn assignStmt(self: *Self) !Index {
        var variable = try self.condExpr();
        if (self.current.tag != .equal) return variable;

        const node = self.nodes.items[@intCast(variable)];
        switch (node) {
            .ident => |name| {
                self.step();

                const value = try self.assignStmt();
                const assign_stmt: AssignStmt = .{
                    .name = name,
                    .value = value,
                };

                variable = try self.push(.{ .assign_stmt = assign_stmt });
                try self.expect(.semicolon);
                self.step();
                return variable;
            },
            else => return Error.ExpectedExpression,
        }
    }

    fn fnDef(self: *Self) !Index {
        try self.expect(.keyword_def);
        self.step();

        try self.expect(.ident);
        const name = self.current.lexeme.?;
        self.step();

        try self.expect(.left_paren);
        self.step();
        var args = Indices.empty;
        while (true) {
            try self.expect(.ident);
            const arg = try self.push(.{ .ident = self.current.lexeme.? });
            try args.append(self.gpa, arg);
            self.step();

            if (self.current.tag == .right_paren) break;
            try self.expect(.comma);
            self.step();
        }
        self.step();

        try self.expect(.left_brace);
        self.step();

        var body_nodes = Indices.empty;
        // TODO: If there is no right brace indeed, the location of the cause
        // would be bugged out. This needs to be fixed.
        while (self.current.tag != .right_brace) {
            const stmt_index = try self.stmt();
            try body_nodes.append(self.gpa, stmt_index);
        }
        self.step();

        const fn_def: FnDef = .{
            .name = name,
            .args = try args.toOwnedSlice(self.gpa),
            .body = try body_nodes.toOwnedSlice(self.gpa),
        };
        const res = self.push(.{ .fn_def = fn_def });
        return res;
    }

    fn returnStmt(self: *Self) !Index {
        self.step();
        const value = try self.expr();
        try self.expect(.semicolon);

        self.step();
        const return_stmt: ReturnStmt = .{ .value = value };
        return self.push(.{ .return_stmt = return_stmt });
    }

    fn forStmt(self: *Self) !Index {
        try self.expect(.keyword_for);
        self.step();

        try self.expect(.ident);
        const var_name = self.current.lexeme.?;
        self.step();

        try self.expect(.keyword_in);
        self.step();

        const iterable = try self.expr();
        try self.expect(.left_brace);
        self.step();

        var body_nodes = Indices.empty;
        while (self.current.tag != .right_brace) {
            const stmt_index = try self.stmt();
            try body_nodes.append(self.gpa, stmt_index);
        }
        self.step();

        const for_stmt: ForStmt = .{
            .var_name = var_name,
            .iterable = iterable,
            .body = try body_nodes.toOwnedSlice(self.gpa),
        };
        return self.push(.{ .for_stmt = for_stmt });
    }

    fn exprStmt(self: *Self) Error!Index {
        const res = self.expr();
        try self.expect(.semicolon);
        self.step();
        return res;
    }

    fn expr(self: *Self) Error!Index {
        return self.condExpr();
    }

    fn condExpr(self: *Self) !Index {
        const then = try self.andOrInExpr();
        if (self.current.tag != .keyword_if) return then;
        self.step();

        const if_cond = try self.andOrInExpr();
        try self.expect(.keyword_else);
        self.step();

        const else_expr = try self.andOrInExpr();
        const cond_expr: CondExpr = .{
            .then = then,
            .if_cond = if_cond,
            .else_expr = else_expr,
        };
        return self.push(.{ .cond_expr = cond_expr });
    }

    fn andOrInExpr(self: *Self) !Index {
        var lhs = try self.comparisonExpr();
        while (true) {
            const op: BinOp = switch (self.current.tag) {
                .keyword_and => .logic_and,
                .keyword_or => .logic_or,
                .keyword_in => .is_in,
                else => break,
            };
            self.step();

            const rhs = try self.comparisonExpr();
            const bin_expr: BinExpr = .{ .lhs = lhs, .op = op, .rhs = rhs };
            lhs = try self.push(.{ .bin_expr = bin_expr });
        }
        return lhs;
    }

    fn comparisonExpr(self: *Self) !Index {
        var lhs = try self.addSubtrExpr();
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

            const rhs = try self.addSubtrExpr();
            const bin_expr: BinExpr = .{ .lhs = lhs, .op = op, .rhs = rhs };
            lhs = try self.push(.{ .bin_expr = bin_expr });
        }
        return lhs;
    }

    fn addSubtrExpr(self: *Self) !Index {
        var lhs = try self.multDivPowExpr();
        while (true) {
            const op: BinOp = switch (self.current.tag) {
                .plus => .add,
                .minus => .subtr,
                else => break,
            };
            self.step();

            const rhs = try self.multDivPowExpr();
            const bin_expr: BinExpr = .{ .lhs = lhs, .op = op, .rhs = rhs };
            lhs = try self.push(.{ .bin_expr = bin_expr });
        }
        return lhs;
    }

    fn multDivPowExpr(self: *Self) !Index {
        var lhs = try self.primaryExpr();
        while (true) {
            const op: BinOp = switch (self.current.tag) {
                .star => .mult,
                .slash => .div,
                .carrot => .power,
                else => break,
            };
            self.step();

            const rhs = try self.multDivPowExpr();
            const bin_expr: BinExpr = .{ .lhs = lhs, .op = op, .rhs = rhs };
            lhs = try self.push(.{ .bin_expr = bin_expr });
        }
        return lhs;
    }

    fn primaryExpr(self: *Self) !Index {
        var primary: Index = try switch (self.current.tag) {
            .ident => self.nameExpr(),
            .int_literal => self.intLiteral(),
            .string_literal => self.stringLiteral(),
            .left_brace => self.mapLiteral(),
            .left_bracket => self.listLiteral(),
            .left_paren => self.boxedExpr(),
            .keyword_true, .keyword_false => self.boolLiteral(),
            else => return Error.ExpectedExpression,
        };
        primary = try self.indexExpr(primary);
        return primary;
    }

    fn indexExpr(self: *Self, target: Index) !Index {
        while (self.current.tag == .left_bracket) {
            self.step();

            const index = try self.expr();
            try self.expect(.right_bracket);
            self.step();

            const index_expr: IndexExpr = .{ .target = target, .index = index };
            return self.push(.{ .index_expr = index_expr });
        }
        return target;
    }

    /// This function is rather specific. Not only this one handles identifiers
    /// and function calls, it also targets the only language built-in, that is
    /// capable of accepting bare comparison predicates as an argument
    /// (`Select` function).
    fn nameExpr(self: *Self) !Index {
        const name = self.current.lexeme.?;
        self.step();

        if (self.current.tag != .left_paren)
            return self.push(.{ .ident = name });
        self.step();

        var args = Indices.empty;
        while (true) {
            const arg = self.expr() catch blk: {
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

                break :blk try self.push(.{ .selector_pred = pred });
            };
            try args.append(self.gpa, arg);
            if (self.current.tag == .right_paren) break;
            try self.expect(.comma);
            self.step();
        }
        self.step();
        const call: FnCall = .{
            .name = name,
            .args = try args.toOwnedSlice(self.gpa),
        };
        return self.push(.{ .fn_call = call });
    }

    fn intLiteral(self: *Self) !Index {
        const int = try fmt.parseInt(i64, self.current.lexeme.?, 10);
        const index = try self.push(.{ .int = int });
        self.step();
        return index;
    }

    fn stringLiteral(self: *Self) !Index {
        const string = self.current.lexeme.?;
        const index = try self.push(.{ .string = string });
        self.step();
        return index;
    }

    fn boolLiteral(self: *Self) !Index {
        const index = switch (self.current.tag) {
            .keyword_true => self.push(.{ .boolean = true }),
            .keyword_false => self.push(.{ .boolean = false }),
            else => unreachable,
        };
        self.step();
        return index;
    }

    fn listLiteral(self: *Self) !Index {
        self.step();
        const expr_ = try self.expr();
        if (self.current.tag != .keyword_for) {
            var elems = Indices.empty;
            try elems.append(self.gpa, expr_);
            while (self.current.tag == .comma) {
                self.step();
                if (self.current.tag == .right_bracket) break;
                const elem = try self.expr();
                try elems.append(self.gpa, elem);
            }
            self.step();

            const list: List = .{ .elems = try elems.toOwnedSlice(self.gpa) };
            return self.push(.{ .list = list });
        }

        self.step();
        try self.expect(.ident);
        const variable = self.current.lexeme.?;
        self.step();

        try self.expect(.keyword_in);
        self.step();

        const iterable = try self.expr();
        try self.expect(.right_bracket);
        self.step();
        const list_comp: ListComp = .{
            .expr = expr_,
            .variable = variable,
            .iterable = iterable,
        };
        return self.push(.{ .list_comp = list_comp });
    }

    fn mapLiteral(self: *Self) !Index {
        self.step();
        var keys, var vals = .{ Indices.empty, Indices.empty };
        while (true) {
            const key = try self.expr();
            try keys.append(self.gpa, key);
            try self.expect(.colon);
            self.step();

            const val = try self.expr();
            try vals.append(self.gpa, val);
            if (self.current.tag == .right_brace) break;
            try self.expect(.comma);
            self.step();
        }
        self.step();
        const map: Map = .{
            .keys = try keys.toOwnedSlice(self.gpa),
            .values = try vals.toOwnedSlice(self.gpa),
        };
        return self.push(.{ .map = map });
    }

    fn push(self: *Self, node: Node) Allocator.Error!Index {
        try self.nodes.append(self.gpa, node);
        const index: Index = @intCast(self.nodes.items.len - 1);
        return index;
    }

    fn boxedExpr(self: *Self) !Index {
        self.step();
        const index = try self.expr();
        try self.expect(.right_paren);

        self.step();
        return index;
    }

    fn step(self: *Self) void {
        self.current = self.tokenizer.next();
    }

    fn peek(self: *Self) void {
        self.peeked = self.tokenizer.peek();
    }

    fn expect(self: *Self, tag: Tag) !void {
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
};

pub const Error = Allocator.Error || fmt.ParseIntError || ParseError;
const ParseError = error{
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
            .map => |map| inline for (.{ "keys", "values" }) |field_name|
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

pub const BinExpr = struct { lhs: Index, op: BinOp, rhs: Index };

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

pub const FnCall = struct { name: []const u8, args: []const Index };

pub const CondExpr = struct {
    then: Index,
    if_cond: Index,
    else_expr: Index,
};
pub const AssignStmt = struct { name: []const u8, value: Index };

pub const FnDef = struct {
    name: []const u8,
    args: []const Index,
    body: []const Index,
};

pub const ReturnStmt = struct { value: Index };

pub const ForStmt = struct {
    var_name: []const u8,
    iterable: Index,
    body: []const Index,
};

pub const List = struct { elems: []const Index };

pub const ListComp = struct {
    expr: Index,
    variable: []const u8,
    iterable: Index,
};

pub const Map = struct { keys: []const Index, values: []const Index };

pub const IndexExpr = struct { target: Index, index: Index };

pub const Indices = ArrayList(Index);
pub const Index = u32;

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
    tree = parser.buildTree() catch |err| {
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
