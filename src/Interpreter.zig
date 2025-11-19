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

pub fn visitNode(i: *Interpreter, index: NodeIndex) !Node {
    const node: Parser.BinExpr = i.tree.nodes[@intCast(index)].bin_expr;
    return i.evalBinExpr(node);
}

fn evalBinExpr(i: *Interpreter, node: Parser.BinExpr) anyerror!Node {
    const lhs = i.tree.nodes[@intCast(node.lhs)];
    // const lhs_type = std.meta.activeTag(lhs);
    const rhs = i.tree.nodes[@intCast(node.rhs)];
    // if (lhs_type != std.meta.activeTag(rhs))
    //     return error.IncompatableTypes;
    const res: Node = switch (node.op) {
        .add => try i.evalAdd(lhs, rhs),
        .subtr => .{ .int = lhs.int - rhs.int },
        .mult => .{ .int = lhs.int * rhs.int },
        .div => .{ .int = @divFloor(lhs.int, rhs.int) },
        else => return error.UnsupportedOperation,
    };
    return res;
}

fn evalAdd(i: *Interpreter, lhs: Node, rhs: Node) !Node {
    const res: Node = switch (meta.activeTag(lhs)) {
        .int => .{ .int = lhs.int + rhs.int },
        .string => .{ .string = try std.mem.concat(
            i.gpa,
            u8,
            &.{ lhs.string, rhs.string },
        ) },
        .bin_expr => try i.evalAdd(try i.evalBinExpr(lhs.bin_expr), rhs),
        else => return error.UnsupportedType,
    };
    return res;
}

fn evalSubtr(i: *Interpreter, lhs: Node, rhs: Node) !Node {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalMult(i: *Interpreter, lhs: Node, rhs: Node) !Node {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalDiv(i: *Interpreter, lhs: Node, rhs: Node) !Node {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalEqual(i: *Interpreter, lhs: Node, rhs: Node) !Node {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalNotEqual(i: *Interpreter, lhs: Node, rhs: Node) !Node {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalLessThan(i: *Interpreter, lhs: Node, rhs: Node) !Node {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalLessOrEqualThan(i: *Interpreter, lhs: Node, rhs: Node) !Node {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalGreaterThan(i: *Interpreter, lhs: Node, rhs: Node) !Node {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalGreaterOrEqualThan(i: *Interpreter, lhs: Node, rhs: Node) !Node {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalLogicAnd(i: *Interpreter, lhs: Node, rhs: Node) !Node {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalLogicOr(i: *Interpreter, lhs: Node, rhs: Node) !Node {
    _ = i;
    _ = lhs;
    _ = rhs;
}

fn evalIsIn(i: *Interpreter, lhs: Node, rhs: Node) !Node {
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

const NodeType = meta.Tag(Node);

const std = @import("std");
const meta = std.meta;
const Allocator = std.mem.Allocator;

const Parser = @import("Parser.zig");
const Tree = Parser.Tree;
const Node = Parser.Node;
const NodeIndex = Parser.NodeIndex;
