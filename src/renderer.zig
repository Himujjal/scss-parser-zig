const std = @import("std");
const parser = @import("parser.zig");
const token = @import("token.zig");
const utils = @import("utils.zig");
const nodes = @import("nodes.zig");

const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Token = token.Token;

// Nodes
const Tree = nodes.Tree;
const Inline = nodes.Inline;
const Declaration = nodes.Declaration;
const DeclarationList = nodes.DeclarationList;
const CSSNode = nodes.CSSNode;
const Rule = nodes.Rule;
const Prelude = nodes.Prelude;
const SelectorList = nodes.SelectorList;
const Selector = nodes.Selector;
const SingleSelector = nodes.SingleSelector;
const TypeSelector = nodes.TypeSelector;
const PseudoClassSelector = nodes.PseudoClassSelector;
const PseudoElementSelector = nodes.PseudoElementSelector;
const Combinator = nodes.Combinator;
const AttributeSelector = nodes.AttributeSelector;
const TypeSelectorBranch = nodes.TypeSelectorBranch;
const FunctionNode = nodes.FunctionNode;
const StringNode = nodes.StringNode;
const Raw = nodes.Raw;
const Block = nodes.Block;
const StyleSheet = nodes.StyleSheet;
const Location = nodes.CSSLocation;

const concatStrings = utils.concatStrings;

/// Takes the CSS AST and outputs the results as a CSS file
/// Renders both the original document as it is given the
pub const Renderer = struct {
    code: []const u8 = undefined,
    tokens: *ArrayList(token.Token) = undefined,
    allocator: Allocator = std.heap.page_allocator,

    _a: Allocator,
    renderer_arena: *ArenaAllocator,

    pub fn init(allocator: Allocator) Renderer {
        var renderer_arena = allocator.create(std.heap.ArenaAllocator) catch unreachable;
        renderer_arena.* = std.heap.ArenaAllocator.init(allocator);
        const _a = renderer_arena.allocator();
        return Renderer{ .renderer_arena = renderer_arena, ._a = _a, .allocator = allocator };
    }

    pub fn renderCSS(
        r: *Renderer,
        code: []const u8,
        tree: nodes.Tree,
        tokens: *ArrayList(token.Token),
    ) []const u8 {
        r.code = code;
        r.tokens = tokens;

        const str = switch (tree) {
            .Inline => |_i| r.renderInline(_i),
            .StyleSheet => |_s| r.renderStyleSheet(_s),
        };

        return str;
    }

    inline fn renderInline(r: *Renderer, _i: Inline) []const u8 {
        return r.renderDeclarationList(_i);
    }

    fn renderStyleSheet(r: *Renderer, s: StyleSheet) []const u8 {
        var res: []const u8 = "";
        for (s.children.items) |css_node| {
            const cn_str = r.renderCSSNode(css_node);
            res = r.concat(res, cn_str);
        }
        return res;
    }

    fn renderCSSNode(r: *Renderer, css_node: CSSNode) []const u8 {
        // TODO: Populate Node
        return switch (css_node) {
            .an_plus_b => "",
            .at_rule => "",
            .at_rule_prelude => "",
            .attribute_selector => "",
            .block => "",
            .brackets => "",
            .class_selector => "",
            .combinator => "",
            .declaration => |d| r.renderDeclaration(d) ,
            .declaration_list => |dl| r.renderDeclarationList(dl),
            .dimension => "",
            .function_node => |f| r.renderFunctionNode(f),
            .hash => "#",
            .id_selector => "",
            .identifier => |i| r.renderToken(i),
            .media_feature => "",
            .media_query => "",
            .media_query_list => "",
            .nth => "",
            .number_node => "",
            .operator => "",
            .parentheses => "",
            .selector_list => "",
            .selector => "",
            .pseudo_class_selector => "",
            .pseudo_element_selector => "",
            .ratio => "",
            .raw => "",
            .rule => |_r| r.renderRule(_r),
            .variable => "",
            .string_node => "",
            .percentage => "",
            .style_sheet => "",
            .at_keyword_token => "",
            .type_selector => "",
            .unicode_range => "",
            .url => "",
            .value => "",
            .misc_token => |m| r.renderToken(m),
            .comment => |w| r.renderToken(w),
            .whitespace => |w| r.renderToken(w),
            .cdo => "",
            .cdc => "",
            ._error => "",
        };
    }

    inline fn concat(r: *Renderer, a: []const u8, b: []const u8) []const u8 {
        return concatStrings(r._a, a, b);
    }

    fn renderRule(r: *Renderer, rule: Rule) []const u8 {
        return r.concat(r.renderPrelude(rule.prelude), r.renderBlock(rule.block));
    }

    fn renderPrelude(r: *Renderer, prelude: Prelude) []const u8 {
        return switch (prelude) {
            .raw => |_raw| r.renderRaw(_raw),
            .selector_list => |_sl| r.renderSelectorList(_sl),
        };
    }

    fn renderSelectorList(r: *Renderer, selector_list: SelectorList) []const u8 {
        var res: []const u8 = "";
        for (selector_list.children.items) |selector, i| {
            res = r.concat(res, r.renderSelector(selector));
            if (i + 1 != selector_list.children.items.len) res = r.concat(res, ",");
        }
        return res;
    }

    fn renderSelector(r: *Renderer, selector: Selector) []const u8 {
        var res: []const u8 = "";
        for (selector.children.items) |_s| {
            res = r.concat(res, switch (_s) {
                .type_selector => |_t| r.renderTypeSelector(_t),
                .id => |_id| r.renderToken(_id),
                .class => |_class| r.renderClassSelector(_class[1]),
                .psuedo_class => |_psuedo_class| r.renderPsuedoClassSelector(_psuedo_class),
                .pseudo_element => |_pseudo_element| r.renderPseudoElementSelector(_pseudo_element),
                .combinator => |_combinator| r.renderCombinatorSelector(_combinator),
                .attribute_selector => |_as| r.renderAttributeSelector(_as),
            });
        }
        return res;
    }

    fn renderTypeSelector(r: *Renderer, ts: TypeSelector) []const u8 {
        var res: []const u8 = "";
        if (ts.branch1) |branch1| {
            res = r.concat(res, switch (branch1) {
                TypeSelectorBranch.star => "*",
                TypeSelectorBranch.ident => |ident| r.renderToken(ident),
            });
        }
        if (ts.vertical_line) |_| res = r.concat(res, "|");
        if (ts.branch2) |branch2| {
            res = r.concat(res, switch (branch2) {
                TypeSelectorBranch.star => "*",
                TypeSelectorBranch.ident => |ident| r.renderToken(ident),
            });
        }
        return res;
    }

    fn renderIdSelector(r: *Renderer, id: usize) []const u8 {
        std.debug.print("{d}\n", .{id});
        return r.concat("#", r.renderToken(id));
    }

    fn renderClassSelector(r: *Renderer, class: usize) []const u8 {
        return r.concat(".", r.renderToken(class));
    }

    fn renderPsuedoClassSelector(r: *Renderer, pcs: PseudoClassSelector) []const u8 {
        var res: []const u8 = "";
        res = r.concat(res, switch (pcs.branches) {
            .identifier => |i| r.renderToken(i),
            .func_node => |f| r.renderFunctionNode(f),
        });
        return res;
    }

    fn renderPseudoElementSelector(r: *Renderer, pes: PseudoElementSelector) []const u8 {
        return r.concat(":", r.renderPsuedoClassSelector(pes.class_selector)); 
    }

    fn renderCombinatorSelector(r: *Renderer, cs: Combinator) []const u8 {
        var res: []const u8 = "";
        if (cs.ws1) |ws1| res = r.concat(res, r.renderArrayOfTokens(ws1));
        if (cs.tok) |tok| res = r.concat(res, r.renderToken(tok));
        if (cs.ws2) |ws2| res = r.concat(res, r.renderArrayOfTokens(ws2));
        return res;
    }

    fn renderAttributeSelector(r: *Renderer, as: AttributeSelector) []const u8 {
        var res: []const u8 = "";
        res = r.concat(res, "[");
        if (as.ws1) |ws1| res = r.concat(res, r.renderArrayOfTokens(ws1));
        res = r.concat(res, r.renderToken(as.name_index));
        if (as.ws2) |ws2| res = r.concat(res, r.renderArrayOfTokens(ws2));
        if (as.matcher) |matcher| res = r.concat(res, r.renderToken(matcher));
        if (as.ws3) |ws3| res = r.concat(res, r.renderArrayOfTokens(ws3));
        if (as.value) |value| {
            res = r.concat(res, switch(value) {
                .string_node => |string_node| r.renderToken(string_node),
                .identifier => |identifier| r.renderToken(identifier),
            });
        }
        if (as.ws4) |ws4| res = r.concat(res, r.renderArrayOfTokens(ws4));
        if (as.flags) |flags| res = r.concat(res, r.renderToken(flags));
        if (as.ws5) |ws5| res = r.concat(res, r.renderArrayOfTokens(ws5));
        res = r.concat(res, "]");
        return res;
    }

    fn renderFunctionNode(r: *Renderer, f_n: FunctionNode) []const u8 {
        var res: []const u8 = "";
        res = r.concat(res, r.renderToken(f_n.name));
        for (f_n.children.items) |child| res = r.concat(res, r.renderCSSNode(child));
        res = r.concat(res, ")");
        return res;
    }

    fn renderDeclarationList(r: *Renderer, dl: DeclarationList) []const u8 {
        var res: []const u8 = "";
        for (dl.children.items) |decl| res = r.concat(res, r.renderDeclaration(decl));
        return res;
    }

    fn renderDeclaration(r: *Renderer, decl: Declaration) []const u8 {
        var res: []const u8 = "";
        res = r.concat(res, r.renderArrayOfTokens(decl.property.children));
        if (decl.ws1) |ws1| res = r.concat(res, r.renderArrayOfTokens(ws1));
        res = r.concat(res, ":");
        if (decl.ws2) |ws2| res = r.concat(res, r.renderArrayOfTokens(ws2));
        for (decl.value.children.items) |cssnode| res = r.concat(res, r.renderCSSNode(cssnode));
        if (decl.important or decl.value.important) res = r.concat(res, " !important");
        if (decl.ws3) |ws3| res = r.concat(res, r.renderArrayOfTokens(ws3));
        res = r.concat(res, ";");
        return res;
    }


    fn renderRaw(r: *Renderer, raw: Raw) []const u8 {
        var res: []const u8 = "";
        for (raw.children.items) |child| {
            res = r.concat(res, r.renderToken(child));
        }
        return res;
    }

    fn renderArrayOfTokens(r: *Renderer, a: ArrayList(usize)) []const u8 {
        var res: []const u8 = "";
        for (a.items) |_a| {
            res = r.concat(res, r.renderToken(_a));
        }
        return res;
    }

    fn renderLoc(r: *Renderer, loc: Location) []const u8 {
        return r.code[loc.start..loc.end];
    }

    fn renderBlock(r: *Renderer, block: Block) []const u8 {
        var res: []const u8 = "{";
        for (block.children.items) |child| {
            res = r.concat(res, r.renderCSSNode(child));
        }
        return r.concat(res, "}");
    }

    fn renderToken(r: *Renderer, token_index: usize) []const u8 {
        const tok: Token = r.tokens.items[token_index];
        return r.code[tok.start..tok.end];
    }

    pub fn deinit(r: *Renderer) void {
        r.renderer_arena.deinit();
        r.allocator.destroy(r.renderer_arena);
    }
};
