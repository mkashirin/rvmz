/// Renderer is a stateful struct that pretty-prints an AST to a generic
/// writer. It separates the printing logic from the AST data structures.
writer: *std.Io.Writer,
nodes: []const Node,
indent_level: u8,
const Renderer = @This();
const INDENT_SIZE = 4;

pub fn init(writer: *std.Io.Writer, nodes: []const Node) Renderer {
    return .{ .writer = writer, .nodes = nodes, .indent_level = 0 };
}

pub fn render(r: *Renderer, root_index: NodeIndex) !void {
    try r.renderNode(root_index);
}

fn indent(r: *Renderer) void {
    r.indent_level += INDENT_SIZE;
}

fn unindent(r: *Renderer) void {
    r.indent_level -= INDENT_SIZE;
}

fn printIndented(
    r: *Renderer,
    comptime format: []const u8,
    args: anytype,
) !void {
    for (0..r.indent_level) |_| try r.writer.print(" ", .{});
    try r.writer.print(format ++ "\n", args);
}

fn renderNode(r: *Renderer, index: NodeIndex) std.Io.Writer.Error!void {
    try switch (r.nodes[@intCast(index)]) {
        .boolean => |node| r.boolean(node),
        .int => |node| r.int(node),
        .string => |node| r.string(node),
        .ident => |node| r.ident(node),
        .bin_expr => |node| r.binExpr(node),
        .cond_expr => |node| r.condExpr(node),
        .assign_stmt => |node| r.assignStmt(node),
        .fn_def => |node| r.fnDef(node),
        .return_stmt => |node| r.returnStmt(node),
        .fn_call => |node| r.fnCall(node),
        .list => |node| r.list(node),
        .list_comp => |node| r.listComp(node),
        .map => |node| r.map(node),
        .index_expr => |node| r.indexExpr(node),
        .for_stmt => |node| r.forStmt(node),
        .selector_pred => |pnode| r.selectorPred(pnode),
    };
}

fn boolean(r: *Renderer, node: bool) !void {
    try r.printIndented("Bool({any})", .{node});
}

fn int(r: *Renderer, node: i64) !void {
    try r.printIndented("Int({d})", .{node});
}

fn string(r: *Renderer, node: []const u8) !void {
    try r.printIndented("String(\"{s}\")", .{node});
}

fn ident(r: *Renderer, node: []const u8) !void {
    try r.printIndented("Identifier({s})", .{node});
}

fn binExpr(r: *Renderer, node: ast.BinExpr) !void {
    try r.printIndented("BinExpr({s}):", .{binOpLexeme(node.op)});
    r.indent();
    defer r.unindent();
    try r.renderNode(node.lhs);
    try r.renderNode(node.rhs);
}

fn condExpr(r: *Renderer, node: ast.CondExpr) !void {
    try r.printIndented("CondExpr:", .{});
    r.indent();
    defer r.unindent();

    try r.printIndented("Then:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(node.then);
    }

    try r.printIndented("If:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(node.if_cond);
    }

    try r.printIndented("Else:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(node.else_expr);
    }
}

fn assignStmt(r: *Renderer, node: ast.AssignStmt) !void {
    try r.printIndented("AssignStmt(name: {s}):", .{node.name});
    r.indent();
    defer r.unindent();
    try r.renderNode(node.value);
}

fn returnStmt(r: *Renderer, node: ast.ReturnStmt) !void {
    try r.printIndented("ReturnStmt:", .{});
    r.indent();
    defer r.unindent();
    try r.renderNode(node.value);
}

fn fnCall(r: *Renderer, node: ast.FnCall) !void {
    try r.printIndented("FnCall(name: {s}):", .{node.name});
    r.indent();
    defer r.unindent();

    try r.printIndented("Args:", .{});
    r.indent();
    defer r.unindent();
    const end = node.args.len;
    for (0..end) |i| try r.renderNode(node.args[i]);
}

fn fnDef(r: *Renderer, node: ast.FnDef) !void {
    try r.printIndented("FnDef(name: {s})", .{node.name});
    r.indent();
    defer r.unindent();

    try r.printIndented("Args:", .{});
    {
        r.indent();
        defer r.unindent();
        const end = node.args.len;
        // For FnDef args, the ADPB stores an index to a simple identifier node.
        for (0..end) |i| {
            const arg = r.nodes[@intCast(node.args[i])];
            try r.printIndented("Arg: {s}", .{arg.ident});
        }
    }

    try r.printIndented("Body:", .{});
    {
        r.indent();
        defer r.unindent();
        const end = node.body.len;
        for (0..end) |i| try r.renderNode(node.args[i]);
    }
}

fn list(r: *Renderer, node: ast.List) !void {
    try r.printIndented("List:", .{});
    r.indent();
    defer r.unindent();

    const end = node.elems.len;
    for (0..end) |i| try r.renderNode(node.elems[i]);
}

fn listComp(r: *Renderer, node: ast.ListComp) !void {
    try r.printIndented("ListComp:", .{});
    r.indent();
    defer r.unindent();

    try r.printIndented("Expr:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(node.expr);
    }

    try r.printIndented("Variable: {s}", .{node.variable});

    try r.printIndented("Iterable:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(node.iterable);
    }
}

fn map(r: *Renderer, node: ast.Map) !void {
    try r.printIndented("Map:", .{});
    r.indent();
    defer r.unindent();

    const keys_end = node.keys.len;
    const values_end = node.values.len;
    for (0..keys_end, 0..values_end) |i, j| {
        try r.printIndented("Pair:", .{});
        r.indent();
        defer r.unindent();
        try r.renderNode(node.keys[i]);
        try r.renderNode(node.values[j]);
    }
}

fn indexExpr(r: *Renderer, node: ast.IndexExpr) !void {
    try r.printIndented("IndexExpr:", .{});
    r.indent();
    defer r.unindent();

    try r.printIndented("Target:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(node.target);
    }

    try r.printIndented("Index:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(node.index);
    }
}

fn forStmt(r: *Renderer, node: ast.ForStmt) !void {
    try r.printIndented("ForStmt(var: {s}):", .{node.var_name});
    r.indent();
    defer r.unindent();

    try r.printIndented("Iterable:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(node.iterable);
    }

    try r.printIndented("Body:", .{});
    {
        r.indent();
        defer r.unindent();
        const end = node.body.len;
        for (0..end) |i| try r.renderNode(node.body[i]);
    }
}

fn selectorPred(r: *Renderer, node: ast.SelectorPred) !void {
    try r.printIndented("SelectorPred({s})", .{
        selectorPredLexeme(node),
    });
    r.indent();
    defer r.unindent();
}

test {
    _ = @import("Renderer.zig");
}

const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Node = ast.Node;
const NodeIndex = ast.Index;
const binOpLexeme = ast.binOpLexeme;
const selectorPredLexeme = ast.selectorPredLexeme;
