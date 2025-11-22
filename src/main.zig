const std = @import("std");

const Tokenizer = @import("Tokenizer.zig");
const Parser = @import("Parser.zig");
const Node = Parser.Node;
const NodeIndex = Parser.NodeIndex;
const Renderer = @import("Renderer.zig");
const Interpreter = @import("Interpreter.zig");

pub fn main() !void {
    var arena: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
    defer arena.deinit();
    const gpa = arena.allocator();

    const source =
        // \\ 16 + 4 / [1, 2, 3][1];
        // \\ [1, 2, 3][1];
        // \\ [1, 2, 3] + [4, 5, 6];
        \\ 1 + {"one": 1, "two": 2}["one"];
        // \\ 1 == 2;
    ;

    var tokenizer: Tokenizer = .init(source);
    var parser: Parser = try .init(&tokenizer, gpa);
    var result = try parser.buildAst();
    var tree: Parser.Tree = undefined;
    result.unwrap(&tree);

    var buffer: [1024]u8 = undefined;
    const writer = std.Progress.lockStderrWriter(&buffer);
    defer std.Progress.unlockStderrWriter();

    var renderer = Renderer.init(writer, tree.nodes, tree.adpb, tree.csapb);
    std.debug.print("Parsed AST (index-backed):\n", .{});
    for (tree.indices) |node| {
        // std.debug.print("index: {d}\n", .{node});
        try renderer.render(node);
    }

    var interpreter: Interpreter = try .init(tree, gpa);
    std.debug.print(
        "\n{any}\n{any}\n",
        .{ tree.nodes[0], try interpreter.visitNode(tree.indices[0]) },
    );
}
