/// Renderer is a stateful struct that pretty-prints an AST to a generic
/// writer. It separates the printing logic from the AST data structures.
writer: *std.Io.Writer,
nodes: []const Node,
indent_level: u8,
const Renderer = @This();
const INDENT_SIZE = 4;

pub fn init(writer: *std.Io.Writer, nodes: []const Node) Renderer {
    return .{
        .writer = writer,
        .nodes = nodes,
        .indent_level = 0,
    };
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

fn printIndentedLine(
    r: *Renderer,
    comptime format: []const u8,
    args: anytype,
) !void {
    for (0..r.indent_level) |_| try r.writer.print(" ", .{});
    try r.writer.print(format ++ "\n", args);
}

fn renderNode(r: *Renderer, index: NodeIndex) std.Io.Writer.Error!void {
    const node = r.nodes[@intCast(index)];
    try switch (node) {
        .boolean => |boolean| r.renderBool(boolean),
        .int => |int| r.renderInt(int),
        .string => |string| r.renderString(string),
        .ident => |idnetfier| r.renderIdentifier(idnetfier),
        .bin_expr => |bin_expr| r.renderBinExpr(bin_expr),
        .cond_expr => |cond_expr| r.renderCondExpr(cond_expr),
        .assign_stmt => |assign_stmt| r.renderAssignStmt(assign_stmt),
        .fn_def => |fn_def| r.renderFnDef(fn_def),
        .return_stmt => |return_stmt| r.renderReturnStmt(return_stmt),
        .fn_call => |fn_call| r.renderFnCall(fn_call),
        .list => |list| r.renderList(list),
        .list_comp => |list_comp| r.renderListComp(list_comp),
        .map => |map| r.renderMap(map),
        .index_expr => |index_expr| r.renderIndexExpr(index_expr),
        .for_stmt => |for_stmt| r.renderForStmt(for_stmt),
        .selector_pred => |pred| r.renderSelectorPred(pred),
    };
}

fn renderBool(r: *Renderer, node: bool) !void {
    try r.printIndentedLine("Bool({any})", .{node});
}

fn renderInt(r: *Renderer, node: i64) !void {
    try r.printIndentedLine("Int({d})", .{node});
}

fn renderString(r: *Renderer, node: []const u8) !void {
    try r.printIndentedLine("String(\"{s}\")", .{node});
}

fn renderIdentifier(r: *Renderer, node: []const u8) !void {
    try r.printIndentedLine("Identifier({s})", .{node});
}

fn renderBinExpr(r: *Renderer, node: ast.BinExpr) !void {
    try r.printIndentedLine("BinExpr({s}):", .{binOpLexeme(node.op)});
    r.indent();
    defer r.unindent();
    try r.renderNode(node.lhs);
    try r.renderNode(node.rhs);
}

fn renderCondExpr(r: *Renderer, node: ast.CondExpr) !void {
    try r.printIndentedLine("CondExpr:", .{});
    r.indent();
    defer r.unindent();

    try r.printIndentedLine("Then:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(node.then);
    }

    try r.printIndentedLine("If:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(node.if_cond);
    }

    try r.printIndentedLine("Else:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(node.else_expr);
    }
}

fn renderAssignStmt(r: *Renderer, node: ast.AssignStmt) !void {
    try r.printIndentedLine("AssignStmt(name: {s}):", .{node.name});
    r.indent();
    defer r.unindent();
    try r.renderNode(node.value);
}

fn renderReturnStmt(r: *Renderer, node: ast.ReturnStmt) !void {
    try r.printIndentedLine("ReturnStmt:", .{});
    r.indent();
    defer r.unindent();
    try r.renderNode(node.value);
}

fn renderFnCall(r: *Renderer, node: ast.FnCall) !void {
    try r.printIndentedLine("FnCall(name: {s}):", .{node.name});
    r.indent();
    defer r.unindent();

    try r.printIndentedLine("Args:", .{});
    r.indent();
    defer r.unindent();
    const end = node.args.len;
    for (0..end) |i| try r.renderNode(node.args[i]);
}

fn renderFnDef(r: *Renderer, node: ast.FnDef) !void {
    try r.printIndentedLine("FnDef(name: {s})", .{node.name});
    r.indent();
    defer r.unindent();

    try r.printIndentedLine("Args:", .{});
    {
        r.indent();
        defer r.unindent();
        const end = node.args.len;
        // For FnDef args, the ADPB stores an index to a simple identifier node.
        for (0..end) |i| {
            const arg = r.nodes[@intCast(node.args[i])];
            try r.printIndentedLine("Arg: {s}", .{arg.ident});
        }
    }

    try r.printIndentedLine("Body:", .{});
    {
        r.indent();
        defer r.unindent();
        const end = node.body.len;
        for (0..end) |i| try r.renderNode(node.args[i]);
    }
}

fn renderList(r: *Renderer, node: ast.List) !void {
    try r.printIndentedLine("List:", .{});
    r.indent();
    defer r.unindent();

    const end = node.elems.len;
    for (0..end) |i| try r.renderNode(node.elems[i]);
}

fn renderListComp(r: *Renderer, node: ast.ListComp) !void {
    try r.printIndentedLine("ListComp:", .{});
    r.indent();
    defer r.unindent();

    try r.printIndentedLine("Expr:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(node.expr);
    }

    try r.printIndentedLine("Variable: {s}", .{node.variable});

    try r.printIndentedLine("Iterable:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(node.iterable);
    }
}

fn renderMap(r: *Renderer, node: ast.Map) !void {
    try r.printIndentedLine("Map:", .{});
    r.indent();
    defer r.unindent();

    const keys_end = node.keys.len;
    const values_end = node.vals.len;
    for (0..keys_end, 0..values_end) |i, j| {
        try r.printIndentedLine("Pair:", .{});
        r.indent();
        defer r.unindent();
        try r.renderNode(node.keys[i]);
        try r.renderNode(node.vals[j]);
    }
}

fn renderIndexExpr(r: *Renderer, node: ast.IndexExpr) !void {
    try r.printIndentedLine("IndexExpr:", .{});
    r.indent();
    defer r.unindent();

    try r.printIndentedLine("Target:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(node.target);
    }

    try r.printIndentedLine("Index:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(node.index);
    }
}

fn renderForStmt(r: *Renderer, node: ast.ForStmt) !void {
    try r.printIndentedLine("ForStmt(var: {s}):", .{node.var_name});
    r.indent();
    defer r.unindent();

    try r.printIndentedLine("Iterable:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(node.iterable);
    }

    try r.printIndentedLine("Body:", .{});
    {
        r.indent();
        defer r.unindent();
        const end = node.body.len;
        for (0..end) |i| try r.renderNode(node.body[i]);
    }
}

fn renderSelectorPred(r: *Renderer, node: ast.SelectorPred) !void {
    try r.printIndentedLine("SelectorPred({s})", .{
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
const NodeIndex = ast.NodeIndex;
const binOpLexeme = ast.binOpLexeme;
const selectorPredLexeme = ast.selectorPredLexeme;
