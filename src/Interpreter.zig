tree: Tree,
gpa: Allocator,
global: Table,
local: Table,
const Interpreter = @This();

pub fn init(tree: Tree, gpa: Allocator) !Interpreter {
    return .{
        .tree = tree,
        .gpa = gpa,
        .global = .init(gpa),
        .local = .init(gpa),
    };
}

const Table = std.StringHashMap(Member);
const Member = union(enum) {
    int: i64,
    string: []const u8,
    function: u32,
    list: u32,
    dictionary: u32,
};

const std = @import("std");
const Allocator = std.mem.Allocator;

const Parser = @import("Parser.zig");
const Tree = Parser.Tree;
const Node = Parser.Node;
