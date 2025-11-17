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
        \\ an_int = 1;
        \\ the_int = 23;
        \\
        \\
        \\ def add(a, b) {
        \\     sum = a + b;
        \\     return sum;
        \\ }
        \\
        \\
        \\ int_sum = add(a_int, the_int);
        \\ print("Success") if c > 0 else print(0);
        \\
        \\ 0 if an_int - the_int and the_int - an_int else int_sum or "Huh?";
        \\
        \\ a_list = [1, 2, 3];
        \\ a_dict = {"integer": 1, "list": [2, 3]};
        \\ the_list = [0, {"one": 1}, 2 + 3];
        \\
        \\ zero = the_list[a_list[0]];
        \\
        \\ for n in a_list {
        \\     print(n + 1);
        \\ }
        \\
        \\ zero_in_the_list = 0 in the_list;
        \\
        \\ selector = Select([1, 2, 3], [1, 2, 3], ==);
        \\
        \\ list_comp = [i + 1 for i in a_list if i > 0 else i];
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
    const adbp = parser.adpb.items;
    const csapb = parser.csapb.items;
    var buffer: [1024]u8 = undefined;
    const writer = std.Progress.lockStderrWriter(&buffer);
    defer std.Progress.unlockStderrWriter();

    var renderer = Renderer.init(writer, nodes, adbp, csapb);
    for (program_indices.items) |node| {
        std.debug.print("\n", .{});
        try renderer.render(node);
    }
}
