// TODO: Finish binary equality evaluation.

tree: Tree,
gpa: Allocator,
global: Table,
local: Table,
const Interpreter = @This();

pub fn walkTree(i: *Interpreter) !void {
    _ = i;
}

pub fn init(tree: Tree, gpa: Allocator) !Interpreter {
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
    list: Parser.List,
    map: Parser.Map,
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

fn evalIndexExpr(i: *Interpreter, node: Parser.IndexExpr) !IValue {
    const target: Node = i.tree.nodes[@intCast(node.target)];
    switch (target) {
        .map => |map| {
            const keys_start: usize = @intCast(map.keys_start);
            const keys_end = keys_start + @as(usize, map.keys_len);
            const keys = i.tree.adpb[keys_start..keys_end];

            const vals_start: usize = @intCast(map.vals_start);
            const vals_end = vals_start + @as(usize, map.vals_len);
            const vals = i.tree.adpb[vals_start..vals_end];

            const keys_len = keys_end - keys_start;
            const key = try i.visitNode(node.index);
            var j: u16 = 0;
            while (keys_len > j) {
                const index = try i.visitNode(keys[j]);
                const keys_match =
                    try i.evalEqual(meta.activeTag(key), key, index);
                if (keys_match.boolean) return try i.visitNode(vals[j]);
                j += 1;
            }
            return error.NoSuchKey;
        },
        .list => |list| {
            const index = try i.visitNode(node.index);
            if (index.int >= list.elems_len) return error.IndexOutOfBounds;
            const target_index = i.tree.adpb[
                @intCast(list.elems_start + index.int)
            ];
            return try i.visitNode(target_index);
        },
        else => return error.UnsupportedType,
    }
}

fn evalBinExpr(i: *Interpreter, node: Parser.BinExpr) anyerror!IValue {
    const lhs = try i.visitNode(node.lhs);
    const lhs_type = meta.activeTag(lhs);
    const rhs = try i.visitNode(node.rhs);

    const res: IValue = try switch (node.op) {
        .add => i.evalAdd(lhs_type, lhs, rhs),
        .subtr => evalSubtr(lhs_type, lhs, rhs),
        .mult => evalMult(lhs_type, lhs, rhs),
        .power => evalPower(lhs_type, lhs, rhs),
        .div => evalDiv(lhs_type, lhs, rhs),
        .equal => i.evalEqual(lhs_type, lhs, rhs),
        .not_equal => i.evalNotEqual(lhs_type, lhs, rhs),

        else => return error.UnsupportedOperation,
    };

    return res;
}

fn evalAdd(
    i: *Interpreter,
    ivalue_type: IValueType,
    lhs: IValue,
    rhs: IValue,
) !IValue {
    const res: IValue = switch (ivalue_type) {
        .int => .{ .int = lhs.int + rhs.int },
        .string => .{ .string = try std.mem.concat(
            i.gpa,
            u8,
            &.{ lhs.string, rhs.string },
        ) },
        // Revisit this one, since indices into lists can be separated by other
        // pointers (e.g. into some function body), this is not correct.
        .list => .{ .list = Parser.List{
            .elems_start = lhs.list.elems_start,
            .elems_len = lhs.list.elems_len + rhs.list.elems_len,
        } },
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
    i: *Interpreter,
    ivalue_type: IValueType,
    lhs: IValue,
    rhs: IValue,
) !IValue {
    _ = i;
    return switch (ivalue_type) {
        .boolean => .{ .boolean = lhs.boolean == rhs.boolean },
        .int => .{ .boolean = lhs.int == rhs.int },
        .string => .{ .boolean = std.mem.eql(u8, lhs.string, rhs.string) },
        else => error.UnsupportedType,
    };
}

fn evalNotEqual(
    i: *Interpreter,
    ivalue_type: IValueType,
    lhs: IValue,
    rhs: IValue,
) !IValue {
    return .{ .boolean = !(try i.evalEqual(ivalue_type, lhs, rhs)).boolean };
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

const Parser = @import("Parser.zig");
const Tree = Parser.Tree;
const Node = Parser.Node;
const NodeIndex = Parser.NodeIndex;
