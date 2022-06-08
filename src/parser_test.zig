const std = @import("std");
const parser = @import("./parser.zig");

const expect = std.testing.expect;
const _a = std.testing.allocator;

const Parser = parser.Parser;
const ParserOptions = parser.ParserOptions;

test "Parser Tests" {
    var p = Parser.init(_a, ParserOptions{
        .parse_rule_prelude = true,
        .skip_ws_comments = false,
    });


    defer p.deinit();

    p = p.parse("a{x:y;}", false).*;

    try expect(p.errors.items.len == 0);
}
