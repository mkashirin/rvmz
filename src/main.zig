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
        \\ {"one": 1, "two": 2}["one"];
        // \\an_int = 4 / 2;
        // \\the_int = 2^3;
        // \\
        // \\
        // \\def add(a, b) {
        // \\    sum = a + b;
        // \\    return sum;
        // \\}
        // \\
        // \\int_sum = add(a_int, the_int);
        // \\print("Success") if c > 0 else print(0);
        // \\
        // \\0 if true and the_int - an_int else int_sum or "Huh?";
        // \\
        // \\a_list = [1, 2, 3];
        // \\a_dict = {"integer": 1, "list": [2, 3]};
        // \\the_list = [0, {"one": 1}, 2 + 3];
        // \\
        // \\zero = the_list[a_list[0]];
        // \\
        // \\for n in a_list {
        // \\    print(n + 1);
        // \\}
        // \\
        // \\zero_in_the_list = 0 in the_list;
        // \\
        // \\selector = Select([1, 2, 3], [1, 2, 3], ==);
        // \\
        // \\list_comp = [i + 1 if i > 0 else i for i in a_list];
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
