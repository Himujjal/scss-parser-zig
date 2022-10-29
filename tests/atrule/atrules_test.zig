const std = @import("std");
const test_utils = @import("../test_utils.zig");

const Allocator = std.Allocator;
const allocator = std.testing.allocator;
const print = std.debug.print;
const expect = std.testing.expect;

test "'AtRule > Font-Face' Test" {
    try test_utils.testFile("atrule", "font-face", false);
    try test_utils.testFile("atrule", "import", false);
    try test_utils.testFile("atrule", "media", false);

	// // TODO: Look more into what this is
	// try test_utils.testFile("atrule", "nest", false);

	try test_utils.testFile("atrule", "supports", false);
	try test_utils.testFile("atrule", "block", false);
	try test_utils.testFile("atrule", "no-block", false);
	try test_utils.testFile("atrule", "stylesheet", false);
	
	// // TODO: Look more into tolerancy of parsers later
	// try test_utils.testFile("atrule", "tolerant", false);
}
