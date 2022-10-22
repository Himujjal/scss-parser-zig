const std = @import("std");
const parser = @import("parser.zig");
const renderer = @import("renderer.zig");

const expect = std.testing.expect;
const _a = std.testing.allocator;

const Renderer = renderer.Renderer;
const Parser = parser.Parser;
const ParserOptions = parser.ParserOptions;

const test_strings = [_][]const u8{
    " a { x : y ; }",
    ".a{x:y;}",
    "#a{x:y;}",
    "a[x=y]{a:b;}",
    "a[x='y' g]{a:b;}",
    "a[href$=\".org\"]{font-style:italic;}",
    "a[href*=\"cAsE\" s]{color:pink;}",
    "a[data-lang*=\"zh-TW\" s]{color:pink;}",
    ".a[x=y]{x:y;}",
    ".a#a[x=y]{x:y;}",
    " .a {x:y;}",
    ".a,.a{x:y;}",
    " .a , #b , a[ x = 'y' ] { x : y ; }",
    \\#main {
    \\    color: rgba(10,10,10);
    \\}
    ,
    \\ a{ --a: var(a); }
};

test "Parser Tests" {
    // const MAX = test_strings.len;
    const MAX = 14;
    for (test_strings) |test_str, i| {
        if (i == MAX) {
            std.debug.print("\nTest {d}:______{s}______\n", .{ i, test_str });
            var p = Parser.init(_a, ParserOptions{
                .parse_rule_prelude = true,
                .skip_ws_comments = false,
            });
            // _ = p.parse(test_str, false);

            // var r = Renderer.init(_a);
            // const output = r.renderCSS(test_str, p.tree, p.tokens);

            // expect(std.mem.eql(u8, test_str, output)) catch |err| {
            //     std.debug.print("{d}. EXPECTED: __{s}__ | GOT: __{s}__\n", .{ i + 1, test_str, output });
            //     p.deinit();
            //     r.deinit();
            //     return err;
            // };
            // expect(p.errors.items.len == 0) catch |err| {
            //     for (p.errors.items) |_err, k| {
            //         std.debug.print("{d}. Error: {}\n", .{ k + 1, _err._type });
            //     }
            //     p.deinit();
            //     r.deinit();
            //     return err;
            // };

            p.deinit();
            // r.deinit();
        }
    }
}
