// TODO: Figure out a more ergonomic way to display nodes as strings.

const std = @import("std");

const Parser = @import("Parser.zig");
const Node = Parser.Node;
const binOpLexeme = Parser.binOpLexeme;
const NodeIndex = Parser.NodeIndex;

const INDENT: usize = 4;

pub fn printNodes(
    nodes: []const Node,
    eib: []const u32,
    index: NodeIndex,
    base_indent: usize,
) void {
    const node = nodes[@intCast(index)];
    printIndent(base_indent);
    const indent = base_indent + INDENT;
    switch (node) {
        .int => |int| std.debug.print("int {d}\n", .{int}),
        .string => |string| std.debug.print("string \"{s}\"\n", .{string}),
        .identifier => |identifier| std.debug.print(
            "identifier \"{s}\"\n",
            .{identifier},
        ),

        .bin_expr => |bin_expr| {
            std.debug.print("binary {s}\n", .{binOpLexeme(bin_expr.op)});
            printNodes(nodes, eib, bin_expr.lhs, indent);

            printNodes(nodes, eib, bin_expr.rhs, indent);
        },

        .cond_expr => |cond_expr| {
            std.debug.print("conditional\n", .{});
            printNodes(nodes, eib, cond_expr.then, indent);
            printNodes(nodes, eib, cond_expr.if_cond, indent);
            printNodes(nodes, eib, cond_expr.else_expr, indent);
        },

        .assign_stmt => |assign_stmt| {
            std.debug.print("assign {s}\n", .{assign_stmt.name});

            printNodes(nodes, eib, assign_stmt.value, indent);
            printIndent(base_indent);
        },

        .fn_call => |fn_call| {
            std.debug.print("call {s}(\n", .{fn_call.fn_name});
            const start: usize = @intCast(fn_call.args_start);
            const end = start + @as(usize, fn_call.args_len);
            var i: usize = start;
            while (i < end) : (i += 1) {
                printNodes(nodes, eib, eib[i], indent);
            }
            printIndent(base_indent);
            std.debug.print(")\n", .{});
        },

        .fn_def => |fn_def| {
            std.debug.print("define function {s}(\n", .{fn_def.name});
            const args_start: usize = @intCast(fn_def.args_start);
            const args_end = args_start + @as(usize, fn_def.args_len);

            var i: usize = args_start;
            while (i < args_end) : (i += 1) {
                const arg_index = eib[i];
                const arg_node = nodes[@as(usize, arg_index)];
                var j: usize = 0;
                while (j < indent) : (j += 1) std.debug.print(" ", .{});
                std.debug.print("arg: {s}\n", .{arg_node.identifier});
            }

            std.debug.print(") {{\n", .{});
            const bstart = @as(usize, fn_def.body_start);
            const bend = bstart + @as(usize, fn_def.body_len);
            i = bstart;
            while (i < bend) : (i += 1) {
                printNodes(nodes, eib, eib[i], indent);
            }
            std.debug.print("}}\n", .{});
        },
    }
}

fn printIndent(indent: usize) void {
    var i: usize = 0;
    while (i < indent) : (i += 1) std.debug.print(" ", .{});
}
