// TODO: Finish binary equality evaluation.

tree: Tree,
gpa: Allocator,
global: Table,
local: Table,
const Interpreter = @This();

pub fn walkTree(i: *Interpreter) !void {
    _ = i;
}

pub fn init(tree: Tree, gpa: Allocator) Interpreter {
    return .{
        .tree = tree,
        .gpa = gpa,
        .global = .init(gpa),
        .local = .init(gpa),
    };
}

const IValue = union(enum) {
    int: i64,
    string: []const u8,
    boolean: bool,
    list: ast.List,
    map: ast.Map,
};

pub fn visitNode(i: *Interpreter, index: NodeIndex) anyerror!IValue {
    const node: Node = i.tree.nodes[@intCast(index)];
    const res: IValue = switch (node) {
        .bin_expr => |bin_expr| try i.evalBinExpr(bin_expr),
        .index_expr => |index_expr| try i.evalIndexExpr(index_expr),
        .int => |int| .{ .int = int },
        .string => |string| .{ .string = string },
        .list => |list| .{ .list = list },
        else => return error.UnsupportedNodeType,
    };
    return res;
}

fn evalIndexExpr(i: *Interpreter, node: ast.IndexExpr) !IValue {
    const target: Node = i.tree.nodes[@intCast(node.target)];
    switch (target) {
        .map => |map| {
            const key = try i.visitNode(node.index);
            for (0..map.keys.len) |j| {
                const index = try i.visitNode(map.keys[j]);
                const keys_match =
                    try evalEqual(meta.activeTag(key), key, index);
                if (keys_match.boolean) return try i.visitNode(map.vals[j]);
            }
            return error.NoSuchKey;
        },
        .list => |list| {
            const index = try i.visitNode(node.index);
            if (index.int >= list.elems.len) return error.IndexOutOfBounds;
            return try i.visitNode(list.elems[@intCast(index.int)]);
        },
        else => return error.UnsupportedType,
    }
}

fn evalBinExpr(i: *Interpreter, node: ast.BinExpr) anyerror!IValue {
    const lhs = try i.visitNode(node.lhs);
    const lhs_type = meta.activeTag(lhs);
    const rhs = try i.visitNode(node.rhs);

    const f = switch (node.op) {
        .add => return evalAdd(i.gpa, lhs_type, lhs, rhs),

        .subtr => &evalSubtr,
        .mult => &evalMult,
        .power => &evalPower,
        .div => &evalDiv,
        .equal => &evalEqual,
        .not_equal => &evalNotEqual,
        .greater_than => &evalGreaterThan,
        .greater_or_equal_than => &evalGreaterOrEqualThan,
        .less_than => &evalLessThan,
        .less_or_equal_than => &evalLessOrEqualThan,

        else => return error.UnsupportedOperation,
    };

    return f(lhs_type, lhs, rhs);
}

fn evalAdd(
    gpa: Allocator,
    ivalue_type: IValueType,
    lhs: IValue,
    rhs: IValue,
) !IValue {
    const res: IValue = switch (ivalue_type) {
        .int => .{ .int = lhs.int + rhs.int },
        .string => .{ .string = try std.mem.concat(
            gpa,
            u8,
            &.{ lhs.string, rhs.string },
        ) },
        .list => .{ .list = .{ .elems = try std.mem.concat(
            gpa,
            u32,
            &.{ lhs.list.elems, rhs.list.elems },
        ) } },
        else => return error.UnsupportedType,
    };
    return res;
}

fn evalSubtr(
    ivalue_type: IValueType,
    lhs: IValue,
    rhs: IValue,
) !IValue {
    const res: IValue = switch (ivalue_type) {
        .int => .{ .int = lhs.int - rhs.int },
        else => return error.UnsupportedType,
    };
    return res;
}

fn evalMult(ivalue_type: IValueType, lhs: IValue, rhs: IValue) !IValue {
    const res: IValue = switch (ivalue_type) {
        .int => .{ .int = lhs.int * rhs.int },
        else => return error.UnsupportedType,
    };
    return res;
}

fn evalDiv(ivalue_type: IValueType, lhs: IValue, rhs: IValue) !IValue {
    const res: IValue = switch (ivalue_type) {
        .int => .{ .int = @divFloor(lhs.int, rhs.int) },
        else => return error.UnsupportedType,
    };
    return res;
}

fn evalPower(ivalue_type: IValueType, lhs: IValue, rhs: IValue) !IValue {
    const res: IValue = switch (ivalue_type) {
        .int => .{ .int = lhs.int ^ rhs.int },
        else => return error.UnsupportedType,
    };
    return res;
}

fn evalEqual(
    ivalue_type: IValueType,
    lhs: IValue,
    rhs: IValue,
) !IValue {
    return switch (ivalue_type) {
        .boolean => .{ .boolean = lhs.boolean == rhs.boolean },
        .int => .{ .boolean = lhs.int == rhs.int },
        .string => .{ .boolean = std.mem.eql(u8, lhs.string, rhs.string) },

        else => error.UnsupportedType,
    };
}

fn evalNotEqual(
    ivalue_type: IValueType,
    lhs: IValue,
    rhs: IValue,
) !IValue {
    return .{ .boolean = !(try evalEqual(ivalue_type, lhs, rhs)).boolean };
}

fn evalLessThan(ivalue_type: IValueType, lhs: IValue, rhs: IValue) !IValue {
    return switch (ivalue_type) {
        .int => .{ .boolean = lhs.int < rhs.int },
        else => error.UnsupportedType,
    };
}

fn evalLessOrEqualThan(
    ivalue_type: IValueType,
    lhs: IValue,
    rhs: IValue,
) !IValue {
    return switch (ivalue_type) {
        .int => .{ .boolean = lhs.int <= rhs.int },
        else => error.UnsupportedType,
    };
}

fn evalGreaterThan(ivalue_type: IValueType, lhs: IValue, rhs: IValue) !IValue {
    return switch (ivalue_type) {
        .int => .{ .boolean = lhs.int > rhs.int },
        else => error.UnsupportedType,
    };
}

fn evalGreaterOrEqualThan(
    ivalue_type: IValueType,
    lhs: IValue,
    rhs: IValue,
) !IValue {
    return switch (ivalue_type) {
        .int => .{ .boolean = lhs.int >= rhs.int },
        else => error.UnsupportedType,
    };
}

fn evalLogicAnd(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalLogicOr(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalIsIn(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    _ = i;
    _ = lhs;
    _ = rhs;
}

const Table = std.StringHashMap(Member);
const Member = union(enum) {
    int: i64,
    string: []const u8,
    function: u32,
    list: u32,
    dictionary: u32,
};

const IValueType = meta.Tag(IValue);

const std = @import("std");
const meta = std.meta;
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Tree = ast.Tree;
const Node = ast.Node;
const NodeIndex = ast.NodeIndex;
