const std = @import("std");

const parsers = @import("parsers.zig");

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}).init;
    const allocator = gpa.allocator();

    const res = parsers.bin_expr.parse(
        allocator,
        "(2 + 2) < (3 + 3) != True",
    );
    // const res = try parsers.atomic.parse(allocator, "\"String\"");
    // const res = try parsers.assign_stmt.parse(allocator, "the_number = 1488");
    std.debug.print("{any}\n", .{res});
}
