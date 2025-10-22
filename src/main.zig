const std = @import("std");

const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const Node = Parser.Node;
const NodeIndex = Parser.NodeIndex;
const Renderer = @import("Renderer.zig");

pub fn main() !void {
    var arena: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
    defer arena.deinit();
    const gpa = arena.allocator();

    const source =
        \\a = 1;
        \\b = 23;
        \\def add(x, y) {
        \\    sum = x + y;
        \\    return sum;
        \\}
        \\c = add(a, b);
        \\print("Success") if c > 0 else print(0);
        \\
        \\0 if a - b and b - a else c or "c";
        \\
        \\a_list = [1, 2, 3];
        \\a_dict = {"integer": 1, "list": [2, 3]};
        \\the_list = [0, {"one": 1}, 2 + 3];
    ;

    var tokenizer: Tokenizer = .init(source);
    var parser: Parser = try .init(&tokenizer, gpa);

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
    for (program_indices.items) |node| {
        std.debug.print("\n", .{});
        try renderer.render(node);
    }
}
