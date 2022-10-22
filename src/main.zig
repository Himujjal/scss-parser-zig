const std = @import("std");
const expect = std.testing.expect;
const parser = @import("./parser.zig");
const renderer = @import("./renderer.zig");

const scanner = @import("./scanner.zig");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const ParserOptions = parser.ParserOptions;
pub const Parser = parser.Parser;
pub const Error = parser.Error;
pub const ParserErrorType = parser.ParserErrorType;

pub fn parseText(allocator: Allocator, text: []const u8, options: ParserOptions) Parser {
    var _parser = Parser.init(allocator, options);
    _ = _parser.parse(text, false);
    return _parser;
}

export fn render(text: [*]const u8, len: usize) [*]const u8 {
    var a = std.heap.page_allocator;
    var t: []const u8 = text[0..len];
    var p = parseText(a, t, ParserOptions{});
    var r = renderer.Renderer.init(a);
    var res = r.renderCSS(t, p.tree, p.tokens);
    var output = a.alloc(u8, res.len) catch unreachable;
    std.mem.copy(u8, output, res);
    p.deinit();
    r.deinit();
    return output.ptr;
}

test "Parser" {
    _ = scanner;
    _ = parser;
    _ = renderer;
}
// test "Parse Test" {
//     const str: []const u8 = "p { font-size: 1px; }";
//     var allocator = std.testing.allocator;
//     var p = parseText(allocator, str, .{});

//     const out = p.tree.toString();

//     try expect(
//         std.mem.eql(u8, out,
//             \\Div
//             \\  Children
//             \\      Text
//             \\          Hello Svelte
//         ),
//     );
//     p.deinit();
// }
