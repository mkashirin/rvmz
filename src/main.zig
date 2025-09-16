const std = @import("std");
const mecha = @import("mecha");

const ExprTag = enum { binexpr };
const Expr = union(ExprTag) { binexpr: BinExpr };

const BinExpr = struct { lhs: Atomic, op: Op, rhs: Atomic };

const AtomicTag = enum { binexpr, float, int, str, boolean, none };
const Atomic = union(AtomicTag) {
    binexpr: *BinExpr,
    float: f32,
    int: i32,
    str: []const u8,
    boolean: bool,
    none: void,
};

const Op = enum {
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

const binop = mecha.recursiveRef(struct {
    fn f(comptime _binop: anytype) mecha.Parser(Atomic) {
        return mecha.oneOf(.{
            mecha.combine(.{ _binop, op, part }).map(evalBinExpr),
            part,
        });
    }
}.f);

const part = mecha.recursiveRef(struct {
    fn f(comptime _part: anytype) mecha.Parser(Atomic) {
        return mecha.oneOf(.{
            mecha.combine(.{ _part, op, atomic }).map(evalBinExpr),
            atomic,
        });
    }
}.f);

const atomic = mecha.oneOf(.{
    token(mecha.combine(.{
        mecha.ascii.char('(').discard(),
        binop,
        mecha.ascii.char(')').discard(),
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

const op = mecha.oneOf(.{
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

fn regOp(symbol: []const u8, value: Op) mecha.Parser(Op) {
    return token(mecha.mapConst(mecha.string(symbol), value));
}

fn token(comptime parser: anytype) @TypeOf(parser) {
    return mecha.combine(.{ parser, ws.discard() });
}

const ws = mecha.oneOf(.{
    mecha.utf8.char(0x0009),
    mecha.utf8.char(0x0020),
    mecha.utf8.char(0x000A),
    mecha.utf8.char(0x000D),
}).many(.{ .collect = false });

fn toAtomic(comptime tag: AtomicTag) fn (anytype) Atomic {
    return mecha.unionInit(Atomic, tag);
}

pub fn evalBinExpr(parsed: anytype) Atomic {
    const bps = mecha.toStruct(BinExpr)(parsed);

    if (std.meta.activeTag(bps.lhs) != std.meta.activeTag(bps.rhs)) {
        @panic("Operand types mismatch");
    }

    return switch (bps.op) {
        .equal => switch (bps.lhs) {
            .binexpr => unreachable,
            .float => Atomic{ .boolean = bps.lhs.float == bps.rhs.float },
            .int => Atomic{ .boolean = bps.lhs.int == bps.rhs.int },
            .str => Atomic{ .boolean = std.mem.eql(u8, bps.lhs.str, bps.rhs.str) },
            .boolean => Atomic{ .boolean = bps.lhs.boolean == bps.rhs.boolean },
            .none => @panic("Cannot compare None type"),
        },

        .not_equal => switch (bps.lhs) {
            .binexpr => unreachable,
            .float => Atomic{ .boolean = bps.lhs.float != bps.rhs.float },
            .int => Atomic{ .boolean = bps.lhs.int != bps.rhs.int },
            .str => Atomic{ .boolean = !std.mem.eql(u8, bps.lhs.str, bps.rhs.str) },
            .boolean => Atomic{ .boolean = bps.lhs.boolean != bps.rhs.boolean },
            .none => @panic("Cannot compare None type"),
        },

        .greater_than => switch (bps.lhs) {
            .binexpr => unreachable,
            .float => Atomic{ .boolean = bps.lhs.float > bps.rhs.float },
            .int => Atomic{ .boolean = bps.lhs.int > bps.rhs.int },
            else => @panic("Invalid types for comparison (greater than)"),
        },

        .less_than => switch (bps.lhs) {
            .binexpr => unreachable,
            .float => Atomic{ .boolean = bps.lhs.float < bps.rhs.float },
            .int => Atomic{ .boolean = bps.lhs.int < bps.rhs.int },
            else => @panic("Invalid types for comparison (less than)"),
        },

        .greater_equal_than => switch (bps.lhs) {
            .binexpr => unreachable,
            .float => Atomic{ .boolean = bps.lhs.float >= bps.rhs.float },
            .int => Atomic{ .boolean = bps.lhs.int >= bps.rhs.int },
            else => @panic(
                "Invalid types for comparison (greater or equal than)",
            ),
        },

        .less_equal_than => switch (bps.lhs) {
            .binexpr => unreachable,
            .float => Atomic{ .boolean = bps.lhs.float <= bps.rhs.float },
            .int => Atomic{ .boolean = bps.lhs.int <= bps.rhs.int },
            else => @panic("Invalid types for comparison (less or than)"),
        },

        .add => switch (bps.lhs) {
            .binexpr => unreachable,
            .float => Atomic{ .float = bps.lhs.float + bps.rhs.float },
            .int => Atomic{ .int = bps.lhs.int + bps.rhs.int },
            .str => Atomic{
                .str = std.mem.concat(
                    std.heap.page_allocator,
                    u8,
                    &.{ bps.lhs.str, bps.rhs.str },
                ) catch @panic("OOM"),
            },
            else => @panic("Invalid types for addition"),
        },

        .subtr => switch (bps.lhs) {
            .binexpr => unreachable,
            .float => Atomic{ .float = bps.lhs.float - bps.rhs.float },
            .int => Atomic{ .int = bps.lhs.int - bps.rhs.int },
            else => @panic("Invalid types for subtraction"),
        },

        .mult => switch (bps.lhs) {
            .binexpr => unreachable,
            .float => Atomic{ .float = bps.lhs.float * bps.rhs.float },
            .int => Atomic{ .int = bps.lhs.int * bps.rhs.int },
            else => @panic("Invalid types for multiplication"),
        },

        .div => switch (bps.lhs) {
            .binexpr => unreachable,
            .float => Atomic{ .float = bps.lhs.float / bps.rhs.float },
            .int => Atomic{ .int = @divTrunc(bps.lhs.int, bps.rhs.int) },
            else => @panic("Invalid types for division"),
        },

        .power => switch (bps.lhs) {
            .binexpr => unreachable,
            .float => Atomic{ .float = std.math.pow(
                f32,
                bps.lhs.float,
                bps.rhs.float,
            ) },
            .int => Atomic{ .int = std.math.powi(
                i32,
                bps.lhs.int,
                bps.rhs.int,
            ) catch @panic("pow overflow") },
            else => @panic("Invalid types for power"),
        },

        .residue => switch (bps.lhs) {
            .binexpr => unreachable,
            .float => @panic("Modulo not defined for floats"),
            .int => Atomic{ .int = @mod(bps.lhs.int, bps.rhs.int) },
            else => @panic("Invalid types for residue"),
        },
    };
}

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}).init;
    const allocator = gpa.allocator();

    const res = try binop.parse(allocator, "(2 + 2) < (3 + 3) == True");
    // const res = atomic.parse(allocator, "3.14");
    std.debug.print("{any}\n", .{res});
}

// Tests:

fn isOk(parser: mecha.Parser(Atomic), in: []const u8) !void {
    const gpa = std.testing.allocator;

    const res = parser.parse(gpa, in) catch @panic("test failure");
    try std.testing.expectEqualStrings("", in[res.index..]);
}

test "bin_op_parser" {
    try isOk(binop, "(2 + 2) > (3 + 3)");

    try isOk(atomic, "-2");
    try isOk(binop, "2 + 2 * 3");
    try isOk(binop, "(2 + 2) * 3");
    try isOk(binop, "2 == 2");

    try isOk(atomic, "3.14");
    try isOk(atomic, "-3.14");
    try isOk(binop, "2.3 + 4.5 * 6.7");
    try isOk(binop, "(2.3 + 4.5) * 6.7");
    try isOk(binop, "2.3 == 3.4");

    try isOk(atomic, "True");
    try isOk(atomic, "False");
    try isOk(binop, "True == True");
    try isOk(binop, "False != False");

    try isOk(atomic, "\"String\"");
    try isOk(binop, "\"String\" == \"String\"");
    try isOk(binop, "\"String\" != \"Other string\"");
}
