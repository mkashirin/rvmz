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

fn print(
    r: *Renderer,
    comptime format: []const u8,
    args: anytype,
) !void {
    for (0..r.indent_level) |_| try r.writer.print(" ", .{});
    try r.writer.print(format ++ "\n", args);
}

fn renderNode(r: *Renderer, index: NodeIndex) std.Io.Writer.Error!void {
    try switch (r.nodes[@intCast(index)]) {
        // zig fmt: off
        .boolean    => |boolean_| r.boolean(boolean_),
        .int        => |int_| r.int(int_),
        .string     => |string_| r.string(string_),
        .ident      => |ident_| r.ident(ident_),
        .list       => |list_| r.list(list_),
        .list_comp  => |list_comp_| r.listComp(list_comp_),
        .map        => |map_| r.map(map_),

        .bin_expr   => |bin_expr_| r.binExpr(bin_expr_),
        .cond_expr  => |cond_expr_| r.condExpr(cond_expr_),
        .index_expr => |index_expr_| r.indexExpr(index_expr_),

        .assign_stmt    => |assign_stmt_| r.assignStmt(assign_stmt_),
        .return_stmt    => |return_stmt_| r.returnStmt(return_stmt_),
        .fn_def         => |fn_def_| r.fnDef(fn_def_),
        .fn_call        => |fn_call_| r.fnCall(fn_call_),
        .bin_arg        => |bin_arg_| r.binArg(bin_arg_),
        .for_stmt       => |for_stmt_| r.forStmt(for_stmt_),
        // zig fmt: on
    };
}

fn boolean(r: *Renderer, boolean_: bool) !void {
    try r.print("Bool({any})", .{boolean_});
}

fn int(r: *Renderer, int_: i64) !void {
    try r.print("Int({d})", .{int_});
}

fn string(r: *Renderer, string_: []const u8) !void {
    try r.print("String(\"{s}\")", .{string_});
}

fn ident(r: *Renderer, idnet_: []const u8) !void {
    try r.print("Identifier({s})", .{idnet_});
}

fn binExpr(r: *Renderer, bin_expr: ast.BinExpr) !void {
    try r.print("BinExpr({s}):", .{bol.get(bin_expr.op)});
    r.indent();
    defer r.unindent();
    try r.renderNode(bin_expr.lhs);
    try r.renderNode(bin_expr.rhs);
}

fn condExpr(r: *Renderer, cond_expr: ast.CondExpr) !void {
    try r.print("CondExpr:", .{});
    r.indent();
    defer r.unindent();

    try r.print("Then:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(cond_expr.then);
    }

    try r.print("If:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(cond_expr.if_cond);
    }

    try r.print("Else:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(cond_expr.else_expr);
    }
}

fn assignStmt(r: *Renderer, assign_stmt: ast.AssignStmt) !void {
    try r.print("AssignStmt(name: {s}):", .{assign_stmt.name});
    r.indent();
    defer r.unindent();
    try r.renderNode(assign_stmt.value);
}

fn returnStmt(r: *Renderer, return_stmt: ast.ReturnStmt) !void {
    try r.print("ReturnStmt:", .{});
    r.indent();
    defer r.unindent();
    try r.renderNode(return_stmt.value);
}

fn fnCall(r: *Renderer, fn_call: ast.FnCall) !void {
    try r.print("FnCall(name: {s}):", .{fn_call.name});
    r.indent();
    defer r.unindent();

    try r.print("Args:", .{});
    r.indent();
    defer r.unindent();
    const end = fn_call.call_args.len;
    for (0..end) |i| try r.renderNode(fn_call.call_args[i]);
}

fn fnDef(r: *Renderer, fn_def: ast.FnDef) !void {
    try r.print("FnDef(name: {s})", .{fn_def.name});
    r.indent();
    defer r.unindent();

    try r.print("Args:", .{});
    {
        r.indent();
        defer r.unindent();
        const end = fn_def.def_args.len;
        // For FnDef args, the ADPB stores an index to a simple identifier node.
        for (0..end) |i| {
            const arg = r.nodes[@intCast(fn_def.def_args[i])];
            try r.print("Arg: {s}", .{arg.ident});
        }
    }

    try r.print("Body:", .{});
    {
        r.indent();
        defer r.unindent();
        const end = fn_def.body.len;
        for (0..end) |i| try r.renderNode(fn_def.def_args[i]);
    }
}

fn list(r: *Renderer, list_: ast.List) !void {
    try r.print("List:", .{});
    r.indent();
    defer r.unindent();

    const end = list_.elems.len;
    for (0..end) |i| try r.renderNode(list_.elems[i]);
}

fn listComp(r: *Renderer, list_comp: ast.ListComp) !void {
    try r.print("ListComp:", .{});
    r.indent();
    defer r.unindent();

    try r.print("Expr:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(list_comp.expr);
    }

    try r.print("Variable: {s}", .{list_comp.variable});

    try r.print("Iterable:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(list_comp.iterable);
    }
}

fn map(r: *Renderer, map_: ast.Map) !void {
    try r.print("Map:", .{});
    r.indent();
    defer r.unindent();

    const keys_end = map_.keys.len;
    const values_end = map_.values.len;
    for (0..keys_end, 0..values_end) |i, j| {
        try r.print("Pair:", .{});
        r.indent();
        defer r.unindent();
        try r.renderNode(map_.keys[i]);
        try r.renderNode(map_.values[j]);
    }
}

fn indexExpr(r: *Renderer, index_expr: ast.IndexExpr) !void {
    try r.print("IndexExpr:", .{});
    r.indent();
    defer r.unindent();

    try r.print("Target:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(index_expr.target);
    }

    try r.print("Index:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(index_expr.index);
    }
}

fn forStmt(r: *Renderer, for_stmt: ast.ForStmt) !void {
    try r.print("ForStmt(var: {s}):", .{for_stmt.var_name});
    r.indent();
    defer r.unindent();

    try r.print("Iterable:", .{});
    {
        r.indent();
        defer r.unindent();
        try r.renderNode(for_stmt.iterable);
    }

    try r.print("Body:", .{});
    {
        r.indent();
        defer r.unindent();
        const end = for_stmt.body.len;
        for (0..end) |i| try r.renderNode(for_stmt.body[i]);
    }
}

fn binArg(r: *Renderer, bin_arg: ast.BinOp) !void {
    try r.print("BinOp({s})", .{bol.get(bin_arg)});
    r.indent();
    defer r.unindent();
}

const bol: std.enums.EnumArray(ast.BinOp, []const u8) = .init(.{
    // BOL — Binary Operations' Lexemes.
    // zig fmt: off
    .add    = "+",
    .subtr  = "-",
    .mult   = "*",
    .power  = "^",
    .div    = "/",

    .equal                  = "==",
    .not_equal              = "!=",
    .greater_than           = ">",
    .greater_or_equal_than  = ">=",
    .less_than              = "<",
    .less_or_equal_than     = "<=",

    .logic_and  = "and",
    .logic_or   = "or",
    .is_in      = "in",
    // zig fmt: on
});

test {
    _ = @import("Renderer.zig");
}

const std = @import("std");
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const Node = ast.Node;
const NodeIndex = ast.Index;
