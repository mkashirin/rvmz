/// Renderer is a stateful struct that pretty-prints an AST to a generic
/// writer. It separates the printing logic from the AST data structures.
writer: *std.Io.Writer,
nodes: []const Node,
eib: []const u32,
indent_level: usize,
const Self = @This();
const INDENT_SPACES = 4;

pub fn init(
    writer: *std.Io.Writer,
    nodes: []const Node,
    eib: []const u32,
) Self {
    return .{
        .writer = writer,
        .nodes = nodes,
        .eib = eib,
        .indent_level = 0,
    };
}

pub fn render(self: *Self, root_index: NodeIndex) !void {
    try self.renderNode(root_index);
}

fn indent(self: *Self) void {
    self.indent_level += INDENT_SPACES;
}

fn unindent(self: *Self) void {
    self.indent_level -= INDENT_SPACES;
}

fn printIndentedLine(
    self: *Self,
    comptime format: []const u8,
    args: anytype,
) !void {
    for (0..self.indent_level) |_| try self.writer.print(" ", .{});
    try self.writer.print(format ++ "\n", args);
}

fn renderNode(self: *Self, index: NodeIndex) std.Io.Writer.Error!void {
    const node = self.nodes[@intCast(index)];
    switch (node) {
        .int => |int| try self.renderInt(int),
        .string => |string| try self.renderString(string),
        .identifier => |idnetfier| try self.renderIdentifier(idnetfier),
        .bin_expr => |bin_expr| try self.renderBinExpr(bin_expr),
        .cond_expr => |cond_expr| try self.renderCondExpr(cond_expr),
        .assign_stmt => |assign_stmt| try self.renderAssignStmt(assign_stmt),
        .fn_def => |fn_def| try self.renderFnDef(fn_def),
        .return_stmt => |return_stmt| try self.renderReturnStmt(return_stmt),
        .fn_call => |fn_call| try self.renderFnCall(fn_call),
        .list => |list| try self.renderList(list),
        .dictionary => |dictionary| try self.renderDcitionary(dictionary),
        .index_expr => |index_expr| try self.renderIndexExpr(index_expr),
    }
}

fn renderInt(self: *Self, node: i64) !void {
    try self.printIndentedLine("Int({d})", .{node});
}

fn renderString(self: *Self, node: []const u8) !void {
    try self.printIndentedLine("String(\"{s}\")", .{node});
}

fn renderIdentifier(self: *Self, node: []const u8) !void {
    try self.printIndentedLine("Identifier({s})", .{node});
}

fn renderBinExpr(self: *Self, node: Parser.BinExpr) !void {
    try self.printIndentedLine("Binxpr({s})", .{binOpLexeme(node.op)});
    self.indent();
    defer self.unindent();
    try self.renderNode(node.lhs);
    try self.renderNode(node.rhs);
}

fn renderCondExpr(self: *Self, node: Parser.CondExpr) !void {
    try self.printIndentedLine("CondExpr", .{});
    self.indent();
    defer self.unindent();

    try self.printIndentedLine("Then:", .{});
    {
        self.indent();
        defer self.unindent();
        try self.renderNode(node.then);
    }

    try self.printIndentedLine("If:", .{});
    {
        self.indent();
        defer self.unindent();
        try self.renderNode(node.if_cond);
    }

    try self.printIndentedLine("Else:", .{});
    {
        self.indent();
        defer self.unindent();
        try self.renderNode(node.else_expr);
    }
}

fn renderAssignStmt(self: *Self, node: Parser.AssignStmt) !void {
    try self.printIndentedLine("AssignStmt(name: {s})", .{node.name});
    self.indent();
    defer self.unindent();
    try self.renderNode(node.value);
}

fn renderReturnStmt(self: *Self, node: Parser.ReturnStmt) !void {
    try self.printIndentedLine("ReturnStmt:", .{});
    self.indent();
    defer self.unindent();
    try self.renderNode(node.value);
}

fn renderFnCall(self: *Self, node: Parser.FnCall) !void {
    try self.printIndentedLine("FnCall(name: {s})", .{node.fn_name});
    self.indent();
    defer self.unindent();

    try self.printIndentedLine("Args:", .{});
    self.indent();
    defer self.unindent();
    const start: usize = @intCast(node.args_start);
    const end = start + @as(usize, node.args_len);
    var i: usize = start;
    while (i < end) : (i += 1) {
        try self.renderNode(self.eib[i]);
    }
}

fn renderFnDef(self: *Self, node: Parser.FnDef) !void {
    try self.printIndentedLine("FnDef(name: {s})", .{node.name});
    self.indent();
    defer self.unindent();

    try self.printIndentedLine("Args:", .{});
    {
        self.indent();
        defer self.unindent();
        const args_start: usize = @intCast(node.args_start);
        const args_end = args_start + @as(usize, node.args_len);
        var i: usize = args_start;
        // For FnDef args, the EIB stores an index to a simple identifier node.
        while (i < args_end) : (i += 1) {
            const arg_node_index = self.eib[i];
            const arg_node = self.nodes[@intCast(arg_node_index)];
            try self.printIndentedLine("Arg: {s}", .{arg_node.identifier});
        }
    }

    try self.printIndentedLine("Body:", .{});
    {
        self.indent();
        defer self.unindent();
        const body_start: usize = @intCast(node.body_start);
        const body_end = body_start + @as(usize, node.body_len);
        var i: usize = body_start;
        while (i < body_end) : (i += 1) {
            try self.renderNode(self.eib[i]);
        }
    }
}

fn renderList(self: *Self, node: Parser.List) !void {
    try self.printIndentedLine("List", .{});
    self.indent();
    defer self.unindent();

    const start: usize = @intCast(node.elems_start);
    const end = start + @as(usize, node.elems_len);
    var i: usize = start;
    while (i < end) : (i += 1) {
        try self.renderNode(self.eib[i]);
    }
}

fn renderDcitionary(self: *Self, node: Parser.Dictionary) !void {
    try self.printIndentedLine("Dictionary", .{});
    self.indent();
    defer self.unindent();

    const keys_start: usize = @intCast(node.keys_start);
    const keys_end = keys_start + @as(usize, node.keys_len);
    const vals_start: usize = @intCast(node.vals_start);
    var i, var j = .{ keys_start, vals_start };
    while (i < keys_end) : (i += 1) {
        try self.printIndentedLine("Pair", .{});
        self.indent();
        defer self.unindent();
        try self.renderNode(self.eib[i]);

        try self.renderNode(self.eib[j]);
        j += 1;
    }
}

fn renderIndexExpr(self: *Self, node: Parser.IndexExpr) !void {
    try self.printIndentedLine("IndexExpr", .{});
    self.indent();
    defer self.unindent();

    try self.printIndentedLine("Target:", .{});
    {
        self.indent();
        defer self.unindent();
        try self.renderNode(node.target);
    }

    try self.printIndentedLine("Index:", .{});
    {
        self.indent();
        defer self.unindent();
        try self.renderNode(node.index);
    }
}

const std = @import("std");
const Allocator = std.mem.Allocator;

const Parser = @import("Parser.zig");
const Node = Parser.Node;
const NodeIndex = Parser.NodeIndex;
const binOpLexeme = Parser.binOpLexeme;
