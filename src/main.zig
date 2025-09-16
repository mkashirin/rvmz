const std = @import("std");

const mecha = @import("mecha");

const parsers = @import("parsers.zig");

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}).init;
    const allocator = gpa.allocator();

    const res = try parsers.bin_expr.parse(allocator, "(2 + 2) < (3 + 3) != True");
    // const res = try atomic.parse(allocator, "\"String\"");
    // const res = try assign_expr.parse(allocator, "the_number = 1488");
    std.debug.print("{any}\n", .{res.value.ok});
}

// Tests:

fn isOk(parser: mecha.Parser(parsers.Atomic), in: []const u8) !void {
    const gpa = std.testing.allocator;

    const res = parser.parse(gpa, in) catch @panic("test failure");
    try std.testing.expectEqualStrings("", in[res.index..]);
}

test "bin_op_parser" {
    try isOk(parsers.bin_expr, "(2 + 2) > (3 + 3)");

    try isOk(parsers.atomic, "-2");
    try isOk(parsers.bin_expr, "2 + 2 * 3");
    try isOk(parsers.bin_expr, "(2 + 2) * 3");
    try isOk(parsers.bin_expr, "2 == 2");

    try isOk(parsers.atomic, "3.14");
    try isOk(parsers.atomic, "-3.14");
    try isOk(parsers.bin_expr, "2.3 + 4.5 * 6.7");
    try isOk(parsers.bin_expr, "(2.3 + 4.5) * 6.7");
    try isOk(parsers.bin_expr, "2.3 == 3.4");

    try isOk(parsers.atomic, "True");
    try isOk(parsers.atomic, "False");
    try isOk(parsers.bin_expr, "True == True");
    try isOk(parsers.bin_expr, "False != False");

    try isOk(parsers.atomic, "\"String\"");
    try isOk(parsers.bin_expr, "\"String\" == \"String\"");
    try isOk(parsers.bin_expr, "\"String\" != \"Other string\"");
}
