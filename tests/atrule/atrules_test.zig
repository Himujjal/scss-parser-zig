const std = @import("std");
const test_utils = @import("../test_utils.zig");

const Allocator = std.Allocator;
const allocator = std.testing.allocator;
const print = std.debug.print;
const expect = std.testing.expect;

test "'AtRule > Font-Face' Test" {
    // try test_utils.testFile("atrule", "font-face", false);
    // try test_utils.testFile("atrule", "import", false);
    // try test_utils.testFile("atrule", "media", false);

	// TODO: Look more into what this is
	// try test_utils.testFile("atrule", "nest", false);

	try test_utils.testFile("atrule", "supports", false);

}
