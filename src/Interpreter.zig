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
                if ((try i.evalEqual(key, index)).boolean)
                    return try i.visitNode(vals[j]);
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

fn beql(a: anytype, b: anytype) bool {
    return std.mem.eql(u8, a, b);
}

fn evalBinExpr(i: *Interpreter, node: Parser.BinExpr) anyerror!IValue {
    const lhs = try i.visitNode(node.lhs);
    const lhs_type = meta.activeTag(lhs);
    const rhs = try i.visitNode(node.rhs);

    const res: IValue = try switch (node.op) {
        .add => i.evalAdd(lhs_type, lhs, rhs),
        .subtr => i.evalSubtr(lhs_type, lhs, rhs),
        .mult => i.evalMult(lhs_type, lhs, rhs),
        .div => i.evalDiv(lhs_type, lhs, rhs),
        else => return error.UnsupportedOperation,
    };

    return res;
}

fn evalAdd(
    i: *Interpreter,
    operand_type: OperandType,
    lhs: IValue,
    rhs: IValue,
) !IValue {
    // TODO: Index expressions.
    const res: IValue = switch (operand_type) {
        .int => .{ .int = lhs.int + rhs.int },
        .string => .{ .string = try std.mem.concat(
            i.gpa,
            u8,
            &.{ lhs.string, rhs.string },
        ) },
        .list => .{ .list = Parser.List{
            .elems_start = lhs.list.elems_start,
            .elems_len = lhs.list.elems_len + rhs.list.elems_len,
        } },
        else => return error.UnsupportedType,
    };
    return res;
}

fn evalSubtr(
    i: *Interpreter,
    node_type: OperandType,
    lhs: IValue,
    rhs: IValue,
) !IValue {
    // TODO: Index expressions.
    _ = i;
    const res: IValue = switch (node_type) {
        .int => .{ .int = lhs.int - rhs.int },
        else => return error.UnsupportedType,
    };
    return res;
}

fn evalMult(i: *Interpreter, node_type: OperandType, lhs: IValue, rhs: IValue) !IValue {
    // TODO: Index expressions.
    _ = i;
    const res: IValue = switch (node_type) {
        .int => .{ .int = lhs.int * rhs.int },
        else => return error.UnsupportedType,
    };
    return res;
}

fn evalDiv(i: *Interpreter, node_type: OperandType, lhs: IValue, rhs: IValue) !IValue {
    // TODO: Index expressions.
    _ = i;
    const res: IValue = switch (node_type) {
        .int => .{ .int = @divFloor(lhs.int, rhs.int) },
        else => return error.UnsupportedType,
    };
    return res;
}

fn evalPower(i: *Interpreter, node_type: OperandType, lhs: IValue, rhs: IValue) !IValue {
    // TODO: Index expressions.
    _ = i;
    const res: IValue = switch (node_type) {
        .int => .{ .int = lhs.int ^ rhs.int },
        else => return error.UnsupportedType,
    };
    return res;
}

fn evalEqual(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    _ = i;
    if (meta.activeTag(lhs) != meta.activeTag(rhs))
        return error.OperandsTypeMismatch;
    return switch (lhs) {
        .boolean => .{ .boolean = lhs.boolean == rhs.boolean },
        .int => .{ .boolean = lhs.int == rhs.int },
        .string => .{ .boolean = std.mem.eql(u8, lhs.string, rhs.string) },
        else => error.OperandsTypeUnsupported,
    };
}

fn evalNotEqual(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalLessThan(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalLessOrEqualThan(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalGreaterThan(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalGreaterOrEqualThan(i: *Interpreter, lhs: IValue, rhs: IValue) !IValue {
    _ = i;
    _ = lhs;
    _ = rhs;
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

const OperandType = meta.Tag(IValue);

const std = @import("std");
const meta = std.meta;
const Allocator = std.mem.Allocator;

const Parser = @import("Parser.zig");
const Tree = Parser.Tree;
const Node = Parser.Node;
const NodeIndex = Parser.NodeIndex;
