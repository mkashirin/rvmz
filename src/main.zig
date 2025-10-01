const std = @import("std");

const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const Node = Parser.Node;
const NodeIndex = Parser.NodeIndex;
const debug = @import("debug.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const gpa = arena.allocator();

    const src =
        \\a = 1 * 2 + 3 >= 0;
        \\b = a if a > 3 else 0;
        \\def add(x, y) {
        \\    sum = x + y;
        \\    sum;
        \\}
        \\c = add(a, b);
        \\print("Success");
    ;

    var tokenizer = Tokenizer.init(src);
    var parser = try Parser.init(&tokenizer, gpa);

    var program_indices: std.ArrayList(u32) = .empty;
    while (parser.current.tag != .eof) {
        const stmt_index = try parser.parseStmt();
        try program_indices.append(gpa, stmt_index);
    }

    std.debug.print("Parsed AST (index-backed):\n", .{});
    const nodes_slice = parser.nodes.items;
    const eib_slice = parser.eib.items;

    for (program_indices.items) |node_index| {
        debug.printNodes(nodes_slice, eib_slice, node_index, 0);
        std.debug.print("\n", .{});
    }
}
