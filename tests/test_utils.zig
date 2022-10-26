const std = @import("std");
const json5 = @import("json5.zig");
const parser = @import("../src/parser.zig");
const renderer = @import("../src/renderer.zig");
const Allocator = std.Allocator;
const allocator = std.testing.allocator;

const CSSParser = parser.Parser;
const CSSParserOptions = parser.ParserOptions;
const CSSRenderer = renderer.Renderer;

const JSONParser = json5.Parser;
const JSONValue = json5.Value;

const print = std.debug.print;
const expect = std.testing.expect;

pub fn testFile(comptime folder: []const u8, comptime file_without_ext: []const u8, to_log: bool) !void {
    const test_name = folder ++ "/" ++ file_without_ext;

    print("\n========= Running '{s}' Test ========\n", .{test_name});

    const file = test_name ++ ".json5";
    const json_str: []const u8 = @embedFile(file);

    var json_parser: JSONParser = JSONParser.init(allocator, false);
    defer json_parser.deinit();

    var tree = try json_parser.parse(json_str);
    defer tree.deinit();

    var keys: [][]const u8 = tree.root.Object.keys();
    for (keys) |key| {
        const sub_test = tree.root.Object.get(key).?;
        switch (sub_test) {
            .Object => |_sub_test| {
                if (_sub_test.get("source")) |src| {
                    print("\t>>>>> '{s} > {s}' Test\n", .{ test_name, key });

                    const source = src.String;
                    if (to_log) print("\nsource: {s}\n\n", .{source});

                    var p = CSSParser.init(allocator, CSSParserOptions{});
                    _ = p.parse(source, false);

                    var r = CSSRenderer.init(allocator);
                    const output = r.renderCSS(source, p.tree, p.tokens);

                    const generate: JSONValue = _sub_test.get("generate") orelse JSONValue{ .String = "" };
                    const generated_str = generate.String;

                    const result = std.mem.eql(u8, source, output) or std.mem.eql(u8, generated_str, output);

                    expect(result) catch |err| {
                        var expected = source;
                        std.debug.print("EXPECTED: __{s}__ | GOT: __{s}__\n", .{ expected, output });
                        if (generated_str.len > 0) std.debug.print("Also Possible: __{s}__", .{generated_str});
                        p.deinit();
                        r.deinit();
                        return err;
                    };

                    expect(p.errors.items.len == 0) catch |err| {
                        for (p.errors.items) |_err, k| {
                            std.debug.print("{d}. Error: {}\n", .{ k + 1, _err._type });
                        }
                        p.deinit();
                        r.deinit();
                        return err;
                    };

                    p.deinit();
                    r.deinit();
                }
            },
            else => {},
        }
    }

    print("~~~~~~~~~ '{s}' Test SUCCESSFUL! ~~~~~~~~~~\n", .{test_name});
}
