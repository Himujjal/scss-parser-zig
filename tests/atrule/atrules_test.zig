const std = @import("std");
const test_utils = @import("../test_utils.zig");

const Allocator = std.Allocator;
const allocator = std.testing.allocator;
const print = std.debug.print;
const expect = std.testing.expect;

test "'AtRule > Font-Face' Test" {
    try test_utils.testFile("atrule", "font-face", false);
}
