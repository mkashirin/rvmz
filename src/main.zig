const std = @import("std");

const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const Node = Parser.Node;
const NodeIndex = Parser.NodeIndex;
const Renderer = @import("Renderer.zig");

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
    const nodes = parser.nodes.items;
    const eib = parser.eib.items;
    var buffer: [1024]u8 = undefined;
    const writer = std.Progress.lockStderrWriter(&buffer);
    defer std.Progress.unlockStderrWriter();

    var renderer = Renderer.init(writer, nodes, eib);
    for (program_indices.items) |node_index| {
        std.debug.print("\n", .{});
        try renderer.render(node_index);
    }
}
