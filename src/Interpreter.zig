// TODO: Finish index expression evaluation.

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

const Literal = union(enum) {
    int: i64,
    string: []const u8,
    list: Parser.List,
};

pub fn visitNode(i: *Interpreter, index: NodeIndex) anyerror!Literal {
    const node: Node = i.tree.nodes[@intCast(index)];
    const res: Literal = switch (node) {
        .bin_expr => |bin_expr| try i.evalBinExpr(bin_expr),
        .index_expr => |index_expr| try i.evalIndexExpr(index_expr),
        .int => |int| .{ .int = int },
        .string => |string| .{ .string = string },
        .list => |list| .{ .list = list },
        else => return error.UnsupportedNodeType,
    };
    return res;
}

fn evalIndexExpr(i: *Interpreter, node: Parser.IndexExpr) !Literal {
    const target: Node = i.tree.nodes[@intCast(node.target)];
    switch (target) {
        // .dictionary => |dict| {
        //     const keys_start: usize = @intCast(dict.keys_start);
        //     const keys_end = keys_start + @as(usize, dict.keys_len);
        //     const keys = i.tree.adpb[keys_start..keys_end];
        //     const vals_start: usize = @intCast(dict.vals_start);
        //     const vals_end = vals_start + @as(usize, dict.vals_len);
        //     const vals = i.tree.adpb[vals_start..vals_end];
        // },
        .list => |list| {
            const index = try i.visitNode(node.index);
            const target_index = i.tree.adpb[
                @intCast(list.elems_start + index.int)
            ];
            return try i.visitNode(target_index);
        },
        else => return error.UnsupportedType,
    }
}

fn evalBinExpr(i: *Interpreter, node: Parser.BinExpr) anyerror!Literal {
    const lhs = try i.visitNode(node.lhs);
    const lhs_type = meta.activeTag(lhs);
    const rhs = try i.visitNode(node.rhs);

    const res: Literal = try switch (node.op) {
        .add => i.evalAdd(lhs_type, lhs, rhs),
        .subtr => i.evalSubtr(lhs_type, lhs, rhs),
        .mult => i.evalMult(lhs_type, lhs, rhs),
        .div => i.evalDiv(lhs_type, lhs, rhs),
        else => return error.UnsupportedOperation,
    };

    return res;
}

fn evalAdd(i: *Interpreter, operand_type: OperandType, lhs: Literal, rhs: Literal) !Literal {
    // TODO: Index expressions.
    const res: Literal = switch (operand_type) {
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
    };
    return res;
}

fn evalSubtr(
    i: *Interpreter,
    node_type: OperandType,
    lhs: Literal,
    rhs: Literal,
) !Literal {
    // TODO: Index expressions.
    _ = i;
    const res: Literal = switch (node_type) {
        .int => .{ .int = lhs.int - rhs.int },
        else => return error.UnsupportedType,
    };
    return res;
}

fn evalMult(i: *Interpreter, node_type: OperandType, lhs: Literal, rhs: Literal) !Literal {
    // TODO: Index expressions.
    _ = i;
    const res: Literal = switch (node_type) {
        .int => .{ .int = lhs.int * rhs.int },
        else => return error.UnsupportedType,
    };
    return res;
}

fn evalDiv(i: *Interpreter, node_type: OperandType, lhs: Literal, rhs: Literal) !Literal {
    // TODO: Index expressions.
    _ = i;
    const res: Literal = switch (node_type) {
        .int => .{ .int = @divFloor(lhs.int, rhs.int) },
        else => return error.UnsupportedType,
    };
    return res;
}

fn evalEqual(i: *Interpreter, lhs: Literal, rhs: Literal) !Literal {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalNotEqual(i: *Interpreter, lhs: Literal, rhs: Literal) !Literal {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalLessThan(i: *Interpreter, lhs: Literal, rhs: Literal) !Literal {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalLessOrEqualThan(i: *Interpreter, lhs: Literal, rhs: Literal) !Literal {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalGreaterThan(i: *Interpreter, lhs: Literal, rhs: Literal) !Literal {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalGreaterOrEqualThan(i: *Interpreter, lhs: Literal, rhs: Literal) !Literal {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalLogicAnd(i: *Interpreter, lhs: Literal, rhs: Literal) !Literal {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalLogicOr(i: *Interpreter, lhs: Literal, rhs: Literal) !Literal {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalIsIn(i: *Interpreter, lhs: Literal, rhs: Literal) !Literal {
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

const OperandType = meta.Tag(Literal);

const std = @import("std");
const meta = std.meta;
const Allocator = std.mem.Allocator;

const Parser = @import("Parser.zig");
const Tree = Parser.Tree;
const Node = Parser.Node;
const NodeIndex = Parser.NodeIndex;
