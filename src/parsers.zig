const StmtTag = enum { fn_def, assign_stmt, expr };
const Stmt = union(StmtTag) {
    fn_def: FnDef,
    assign_stmt: AssignStmt,
    expr: Expr,
};

const ExprTag = enum { tern_expr, bin_expr, atomic };
const Expr = union(ExprTag) {
    tern_expr: TernExpr,
    bin_expr: BinExpr,
    atomic: Atomic,
};

// Functions:
//
// TODO: Figure out function definitions/calls.

const FnDef = struct { name: []const u8, params: [][]const u8, body: []Expr };

// Assignment expression:

const AssignStmt = struct { name: []const u8, value: Atomic };

pub const assign_stmt = mecha.combine(.{
    token(ident),
    token(mecha.utf8.char('=')).discard(),
    token(atomic),
})
    .map(mecha.toStruct(AssignStmt))
    .map(mecha.unionInit(Stmt, StmtTag.assign_stmt));

const ident = mecha.combine(.{
    mecha.oneOf(.{ mecha.ascii.alphabetic, mecha.ascii.char('_') }),
    mecha.oneOf(.{ mecha.ascii.alphanumeric, mecha.ascii.char('_') })
        .many(.{ .collect = false }),
}).asStr();

// Binary expression (evaluates to atomic recursively from left to right):
//
// TODO: Rework binary expression logic. (Store as `Value` first?)

const BinExpr = struct { lhs: Atomic, op: BinOp, rhs: Atomic };

pub const bin_expr = mecha.recursiveRef(struct {
    fn f(comptime lhs: anytype) mecha.Parser(Atomic) {
        return mecha.oneOf(.{
            mecha.combine(.{ lhs, bin_op, part }).map(evalBinExpr),
            part,
        });
    }
}.f);

const part = mecha.recursiveRef(struct {
    fn f(comptime lhs: anytype) mecha.Parser(Atomic) {
        return mecha.oneOf(.{
            mecha.combine(.{ lhs, bin_op, atomic }).map(evalBinExpr),
            atomic,
        });
    }
}.f);

// Function that valuates binary expressions:

pub fn evalBinExpr(parsed: anytype) Atomic {
    const bps = mecha.toStruct(BinExpr)(parsed);

    if (std.meta.activeTag(bps.lhs) != std.meta.activeTag(bps.rhs)) {
        @panic("Operand types mismatch");
    }

    return switch (bps.op) {
        .logic_and => switch (bps.lhs) {
            .boolean => Atomic{ .boolean = bps.lhs.boolean and
                bps.rhs.boolean },
            else => @panic("Invalid types for logical operator (and)"),
        },

        .logic_or => switch (bps.lhs) {
            .boolean => Atomic{ .boolean = bps.lhs.boolean or
                bps.rhs.boolean },
            else => @panic("Invalid types for logical operator or)"),
        },

        .equal => switch (bps.lhs) {
            .float => Atomic{ .boolean = bps.lhs.float == bps.rhs.float },
            .int => Atomic{ .boolean = bps.lhs.int == bps.rhs.int },
            .str => Atomic{ .boolean = std.mem.eql(
                u8,
                bps.lhs.str,
                bps.rhs.str,
            ) },
            .boolean => Atomic{ .boolean = bps.lhs.boolean ==
                bps.rhs.boolean },
            .none => @panic("Cannot compare None type"),
        },

        .not_equal => switch (bps.lhs) {
            .float => Atomic{ .boolean = bps.lhs.float != bps.rhs.float },
            .int => Atomic{ .boolean = bps.lhs.int != bps.rhs.int },
            .str => Atomic{ .boolean = !std.mem.eql(
                u8,
                bps.lhs.str,
                bps.rhs.str,
            ) },
            .boolean => Atomic{ .boolean = bps.lhs.boolean !=
                bps.rhs.boolean },
            .none => @panic("Cannot compare None type"),
        },

        .greater_than => switch (bps.lhs) {
            .float => Atomic{ .boolean = bps.lhs.float > bps.rhs.float },
            .int => Atomic{ .boolean = bps.lhs.int > bps.rhs.int },
            else => @panic("Invalid types for comparison (greater than)"),
        },

        .less_than => switch (bps.lhs) {
            .float => Atomic{ .boolean = bps.lhs.float < bps.rhs.float },
            .int => Atomic{ .boolean = bps.lhs.int < bps.rhs.int },
            else => @panic("Invalid types for comparison (less than)"),
        },

        .greater_equal_than => switch (bps.lhs) {
            .float => Atomic{ .boolean = bps.lhs.float >= bps.rhs.float },
            .int => Atomic{ .boolean = bps.lhs.int >= bps.rhs.int },
            else => @panic(
                "Invalid types for comparison (greater or equal than)",
            ),
        },

        .less_equal_than => switch (bps.lhs) {
            .float => Atomic{ .boolean = bps.lhs.float <= bps.rhs.float },
            .int => Atomic{ .boolean = bps.lhs.int <= bps.rhs.int },
            else => @panic("Invalid types for comparison (less or than)"),
        },

        .add => switch (bps.lhs) {
            .float => Atomic{ .float = bps.lhs.float + bps.rhs.float },
            .int => Atomic{ .int = bps.lhs.int + bps.rhs.int },
            .str => Atomic{ .str = std.mem.concat(
                std.heap.page_allocator,
                u8,
                &.{ bps.lhs.str, bps.rhs.str },
            ) catch @panic("OOM") },
            else => @panic("Invalid types for addition"),
        },

        .subtr => switch (bps.lhs) {
            .float => Atomic{ .float = bps.lhs.float - bps.rhs.float },
            .int => Atomic{ .int = bps.lhs.int - bps.rhs.int },
            else => @panic("Invalid types for subtraction"),
        },

        .mult => switch (bps.lhs) {
            .float => Atomic{ .float = bps.lhs.float * bps.rhs.float },
            .int => Atomic{ .int = bps.lhs.int * bps.rhs.int },
            else => @panic("Invalid types for multiplication"),
        },

        .div => switch (bps.lhs) {
            .float => Atomic{ .float = bps.lhs.float / bps.rhs.float },
            .int => Atomic{ .int = @divTrunc(bps.lhs.int, bps.rhs.int) },
            else => @panic("Invalid types for division"),
        },

        .power => switch (bps.lhs) {
            .float => Atomic{ .float = std.math.pow(
                f32,
                bps.lhs.float,
                bps.rhs.float,
            ) },
            .int => Atomic{ .int = std.math.powi(
                i32,
                bps.lhs.int,
                bps.rhs.int,
            ) catch @panic("Power overflow") },
            else => @panic("Invalid types for power"),
        },

        .residue => switch (bps.lhs) {
            .float => @panic("Modulo not defined for floats"),
            .int => Atomic{ .int = @mod(bps.lhs.int, bps.rhs.int) },
            else => @panic("Invalid types for residue"),
        },
    };
}

// Ternary expression:

const TernExpr = struct { then_br: Expr, cond: Expr, else_br: Expr };

pub fn evalTernExpr(parsed: anytype) Atomic {
    const tps = mecha.toStruct(TernExpr)(parsed);

    if (std.meta.activeTag(tps.lhs) != std.meta.activeTag(tps.rhs)) {
        @panic("Operand types mismatch");
    }
    return Atomic{ .int = 0 };
}

// Value (includes assignment and binary expressions, atomics):
//
// TODO: Add function declarations/calls.

// const ValueTag = enum { assign_expr, bin_expr, atomic };
// const Value = union(ValueTag) {
//     assign_expr: AssignExpr,
//     bin_expr: BinExpr,
//     atomic: Atomic,
// };

// Atomics:

const AtomicTag = enum { float, int, str, boolean, none };
pub const Atomic = union(AtomicTag) {
    float: f32,
    int: i32,
    str: []const u8,
    boolean: bool,
    none: void,
};

pub const atomic = mecha.oneOf(.{
    token(mecha.combine(.{
        mecha.utf8.char('(').discard(),
        bin_expr,
        mecha.utf8.char(')').discard(),
    })),
    token(float),
    token(int),
    token(str),
    token(boolean),
});

const float = mecha.combine(.{ int, mecha.utf8.char('.'), digits })
    .asStr()
    .convert(mecha.toFloat(f32))
    .map(toAtomic(.float));

const int = mecha.combine(.{ mecha.utf8.char('-').opt(), digits })
    .asStr()
    .convert(mecha.toInt(i32, 10))
    .map(toAtomic(.int));

const digits = digit.many(.{ .collect = false, .min = 1 });
const digit = mecha.oneOf(.{ mecha.utf8.char('0'), drange });
const drange = mecha.utf8.range('1', '9');

const str = mecha.combine(.{
    mecha.utf8.char('"').discard(),
    char.many(.{ .collect = false }),
    mecha.utf8.char('"').discard(),
})
    .map(toAtomic(.str));

const char = mecha.oneOf(.{
    mecha.utf8.range(0x0020, '"' - 1),
    mecha.utf8.range('"' + 1, '\\' - 1),
    mecha.utf8.range('\\' + 1, 0x10FFFF),
    mecha.combine(.{
        mecha.utf8.char('\\').discard(),
        escape,
    }),
});

const escape = mecha.oneOf(.{
    mecha.utf8.char('"'),
    mecha.utf8.char('\\'),
    mecha.utf8.char('/'),
    mecha.utf8.char('b'),
    mecha.utf8.char('f'),
    mecha.utf8.char('n'),
    mecha.utf8.char('r'),
    mecha.utf8.char('t'),
});

const boolean = mecha.oneOf(.{
    mecha.string("True")
        .asStr()
        .mapConst(true)
        .map(toAtomic(.boolean)),

    mecha.string("False")
        .asStr()
        .mapConst(false)
        .map(toAtomic(.boolean)),
});

const none = mecha.string("None").asStr().mapConst(void).map(toAtomic(.none));

fn toAtomic(comptime tag: AtomicTag) fn (anytype) Atomic {
    return mecha.unionInit(Atomic, tag);
}

// Binary operaions:

const BinOp = enum {
    logic_and,
    logic_or,

    equal,
    not_equal,
    greater_equal_than,
    less_equal_than,
    greater_than,
    less_than,

    add,
    subtr,
    mult,
    div,
    power,
    residue,
};

const bin_op = mecha.oneOf(.{
    regOp("and", .logic_and),
    regOp("or", .logic_or),

    regOp("==", .equal),
    regOp("!=", .not_equal),
    regOp(">=", .greater_equal_than),
    regOp("<=", .less_equal_than),
    regOp(">", .greater_than),
    regOp("<", .less_than),

    regOp("+", .add),
    regOp("-", .subtr),
    regOp("*", .mult),
    regOp("/", .div),
    regOp("^", .power),
    regOp("%", .residue),
});

fn regOp(symbol: []const u8, op: BinOp) mecha.Parser(BinOp) {
    return token(mecha.mapConst(mecha.string(symbol), op));
}

// Tokens and whitespaces:

fn token(comptime parser: anytype) @TypeOf(parser) {
    return mecha.combine(.{ parser, ws.discard() });
}

const ws = mecha.oneOf(.{
    mecha.utf8.char(0x0009),
    mecha.utf8.char(0x0020),
    mecha.utf8.char(0x000A),
    mecha.utf8.char(0x000D),
}).many(.{ .collect = false });

test "Atomic.int" {
    try isOk(atomic, "-2");
    try isOk(bin_expr, "2 + 2 * 3");
    try isOk(bin_expr, "(2 + 2) * 3");
    try isOk(bin_expr, "2 == 2");
}

test "Atomic.float" {
    try isOk(atomic, "3.14");
    try isOk(atomic, "-3.14");
    try isOk(bin_expr, "2.3 + 4.5 * 6.7");
    try isOk(bin_expr, "(2.3 + 4.5) * 6.7");
    try isOk(bin_expr, "2.3 == 3.4");
}

test "Atomic.boolean" {
    try isOk(atomic, "True");
    try isOk(atomic, "False");
    try isOk(bin_expr, "True == True");
    try isOk(bin_expr, "False != False");
}

test "Atomic.str" {
    try isOk(atomic, "\"String\"");
    try isOk(bin_expr, "\"String\" == \"String\"");
    try isOk(bin_expr, "\"String\" != \"Other string\"");
}

test "bin_expr" {
    try isOk(bin_expr, "1 < ((2 + 2) > (3 + 3)) != True");
}

fn isOk(parser: mecha.Parser(Atomic), in: []const u8) !void {
    const gpa = std.testing.allocator;

    const res = parser.parse(gpa, in) catch @panic("test failure");
    try std.testing.expectEqualStrings("", in[res.index..]);
}

const std = @import("std");

const mecha = @import("mecha");
