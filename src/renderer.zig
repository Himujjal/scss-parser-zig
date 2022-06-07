const std = @import("std");
const parser = @import("./parser.zig");
const token = @import("./token.zig");

const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Token = token.Token;
const Tree = parser.Tree;

/// Takes the CSS AST and outputs the results as a CSS file
/// Renders both the original document as it is given the
pub const Renderer = struct {
    code: []const u8 = undefined,
    tokens: *ArrayList(token.Token),

    allocator: Allocator,
    renderer_arena: *ArenaAllocator,

    output: []const u8 = "",

    tree: Tree = undefined,

    pub fn init(allocator: std.mem.Allocator) Renderer {
        var renderer_arena = allocator.create(std.ArenaAllocator) catch unreachable;
        return Renderer{ .renderer_arena = renderer_arena };
    }

    pub fn render(r: *Renderer, code: []const u8, tree: parser.Tree, tokens: *ArrayList(token.Token)) []const u8 {
        r.tree = tree;
        r.code = code;
        r.tokens = tokens;
        return r.output;
    }

    pub fn deinit(r: *Renderer) void {
        r.renderer_arena.deinit();
        r.allocator.destroy(r.renderer_arena);
    }
};

test "Renderer" {}
