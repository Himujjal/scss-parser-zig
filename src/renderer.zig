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
const Atrule = nodes.Atrule;
const AtrulePrelude = nodes.AtrulePrelude;
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
const URL = nodes.URL;

const MediaQueryList = nodes.MediaQueryList;
const MediaQuery = nodes.MediaQuery;
const MediaFeature = nodes.MediaFeature;

const MediaAndOr = nodes.MediaAndOr;
const MediaCondition = nodes.MediaCondition;
const MediaQueryBranch2 = nodes.MediaQueryBranch2;
const MediaConditionWithoutOr = nodes.MediaConditionWithoutOr;
const MediaNot = nodes.MediaNot;
const MediaInParens = nodes.MediaInParens;
const GeneralEnclosed = nodes.GeneralEnclosed;
const GeneralEnclosedIndent = nodes.GeneralEnclosedIdent;

const MfPlain = nodes.MfPlain;
const MfValue = nodes.MfValue;
const MfBoolean = nodes.MfBoolean;
const MfRange = nodes.MfRange;
const Ratio = nodes.Ratio;
const MfRangeOp = nodes.MfRangeOp;

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
        return r.renderSequenceCSSNode(s.children);
    }

    fn renderSequenceCSSNode(r: *Renderer, seq: ArrayList(CSSNode)) []const u8 {
        var res: []const u8 = "";
        for (seq.items) |css_node| {
            const cn_str = r.renderCSSNode(css_node);
            res = r.concat(res, cn_str);
        }
        return res;
    }

    fn renderCSSNode(r: *Renderer, css_node: CSSNode) []const u8 {
        // TODO: Populate Node
        return switch (css_node) {
            .an_plus_b => "",
            .at_rule => |ar| r.renderAtRule(ar),
            .at_rule_prelude => "",
            .attribute_selector => "",
            .block => "",
            .brackets => "",
            .class_selector => "",
            .combinator => "",
            .declaration => |d| r.renderDeclaration(d),
            .declaration_list => |dl| r.renderDeclarationList(dl),
            .dimension => "",
            .function_node => |f| r.renderFunctionNode(f),
            .hash => "#",
            .id_selector => "",
            .identifier => |i| r.renderToken(i),
            .media_feature => "",
            .media_query => |mq| r.renderMediaQuery(mq),
            .media_query_list => |mql| r.renderMediaQueryList(mql),
            .nth => "",
            .number_node => "",
            .operator => "",
            .parentheses => "",
            .selector_list => "",
            .selector => "",
            .pseudo_class_selector => "",
            .pseudo_element_selector => "",
            .ratio => |ra| r.renderRatio(ra),
            .raw => "",
            .rule => |_r| r.renderRule(_r),
            .variable => "",
            .string_node => |s| r.renderString(s),
            .percentage => "",
            .style_sheet => "",
            .at_keyword_token => "",
            .type_selector => "",
            .unicode_range => "",
            .url => |u| r.renderToken(u),
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

    fn renderAtRule(r: *Renderer, rule: Atrule) []const u8 {
        var res: []const u8 = "";
        res = r.concat(res, utils.toLower(r._a, r.renderToken(rule.rule_index)));

        res = r.concatWS(res, rule.ws1);

        if (rule.prelude) |prelude| {
            res = r.concat(res, switch (prelude) {
                .at_rule_prelude => |at_rule_prelude| r.renderAtRulePrelude(at_rule_prelude),
                .raw => |raw| r.renderRaw(raw),
            });
        }

        res = r.concatWS(res, rule.ws2);
        res = r.concat(res, if (rule.block) |block| r.renderBlock(block) else ";");
        return res;
    }

    fn renderAtRulePrelude(r: *Renderer, at_rule_prelude: AtrulePrelude) []const u8 {
        return r.renderSequenceCSSNode(at_rule_prelude.children);
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

    fn renderUrl(r: *Renderer, url: URL) []const u8 {
        return r.renderToken(url.url);
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
            res = r.concat(res, switch (value) {
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

    fn renderMediaQueryList(r: *Renderer, mql: MediaQueryList) []const u8 {
        var res: []const u8 = "";
        for (mql.children.items) |mq, i| {
            res = r.concat(res, r.renderMediaQuery(mq));
            if (i != mql.children.items.len - 1) res = r.concat(res, ",");
        }
        return res;
    }

    fn renderMediaQuery(r: *Renderer, mq: MediaQuery) []const u8 {
        var res: []const u8 = "";
        res = r.concatWS(res, mq.ws1);
        res = r.concat(res, switch (mq.branches) {
            .media_condition => |mc| r.renderMediaCondition(mc),
            .media_query_branch_2 => |mqb2| r.renderMediaQueryBranch2(mqb2.*),
        });
        res = r.concatWS(res, mq.ws2);
        return res;
    }

    fn renderMediaAndOr(r: *Renderer, rm: MediaAndOr) []const u8 {
        var res: []const u8 = "";

        const len = rm.and_or_list.items.len;
        for (rm.children.items) |media_in_parens, i| {
            if (i != 0 and i != len - 1) {
                const ws1 = rm.ws1_list.items[i - 1];
                res = r.concatWS(res, ws1);

                const and_or_list = rm.and_or_list.items[i - 1];
                res = r.concat(res, r.renderToken(and_or_list));

                const ws2 = rm.ws2_list.items[i - 1];
                res = r.concatWS(res, ws2);
            }
            res = r.concat(res, r.renderMediaInParens(media_in_parens));
        }

        return res;
    }

    fn renderMediaCondition(r: *Renderer, rm: MediaCondition) []const u8 {
        return switch (rm) {
            .media_not => |mn| r.renderMediaNot(mn.*),
            .media_and => |ma| r.renderMediaAndOr(ma.*),
            .media_or => |mo| r.renderMediaAndOr(mo.*),
        };
    }
    fn renderMediaQueryBranch2(r: *Renderer, rm: MediaQueryBranch2) []const u8 {
        var res: []const u8 = "";

        if (rm.is_not) |is_not| {
            res = r.concat(res, if (is_not) "not" else "else");
        }
        res = r.concatWS(res, rm.ws1);
        res = r.concat(res, r.renderToken(rm.media_type));

        if (rm.media_condition_without_or) |media_condition_without_or| {
            res = r.concatWS(res, rm.ws2);
            res = r.concat(res, "and");
            res = r.concatWS(res, rm.ws3);
            res = r.concat(res, r.renderMediaConditionWithoutOr(media_condition_without_or));
        }

        return res;
    }
    fn renderMediaConditionWithoutOr(r: *Renderer, rm: MediaConditionWithoutOr) []const u8 {
		return switch(rm) {
			.media_not => |mn| r.renderMediaNot(mn.*),
			.media_and => |ma| r.renderMediaAndOr(ma.*),
		};
    }
    fn renderMediaNot(r: *Renderer, rm: MediaNot) []const u8 {
        var res: []const u8 = "";
        res = r.concat(res, "not");
        res = r.concatWS(res, rm.ws1);
        res = r.concat(res, r.renderMediaInParens(rm.media_in_parens.*));
        return res;
    }

    fn renderMediaInParens(r: *Renderer, rm: MediaInParens) []const u8 {
        var res: []const u8 = "";
        res = r.concat(res, "(");
        res = r.concatWS(res, rm.ws1);

        res = r.concat(res, switch (rm.content) {
            .media_condition => |mc| r.renderMediaCondition(mc),
            .media_feature => |mf| r.renderMediaFeature(mf),
            .general_enclosed => |ge| r.renderGeneralEnclosed(ge.*),
        });

        res = r.concatWS(res, rm.ws2);
        res = r.concat(res, ")");
        return res;
    }

    fn renderGeneralEnclosed(r: *Renderer, ge: GeneralEnclosed) []const u8 {
		return switch(ge) {
			.function => |func| r.renderFunctionNode(func.*),
			.ident => |ident| r.renderGeneralEnclosedIndent(ident.*),
		};
    }
    fn renderGeneralEnclosedIndent(r: *Renderer, gei: GeneralEnclosedIndent) []const u8 {
		_ = r;
		_ = gei;
		return "";
    }

    fn renderMfPlain(r: *Renderer, rm: MfPlain) []const u8 {
        var res: []const u8 = "";
        res = r.concat(res, r.renderToken(rm.name));
        res = r.concatWS(res, rm.ws1);
		res = r.concat(res, ":");
        res = r.concatWS(res, rm.ws2);
        res = r.concat(res, r.renderMfValue(rm.value));
        return res;
    }

    fn renderMfValue(r: *Renderer, mfv: MfValue) []const u8 {
        return switch (mfv) {
            .number => |n| r.renderToken(n),
            .dimension => |d| r.renderToken(d),
            .ident => |i| r.renderToken(i),
            .ratio => |ra| r.renderRatio(ra.*),
        };
    }

    fn renderMfRange(r: *Renderer, rm: MfRange) []const u8 {
        var res: []const u8 = "";

        res = r.concat(
            res,
            if (rm.mf_name_first) r.renderToken(rm.mf_name) else r.renderMfValue(rm.mf_value1),
        );
        res = r.concatWS(res, rm.ws1);
        if (rm.op1) |op1| res = r.concat(res, r.renderMfRangeOp(op1));
        res = r.concatWS(res, rm.ws2);

        res = r.concat(
            res,
            if (!rm.mf_name_first) r.renderToken(rm.mf_name) else r.renderMfValue(rm.mf_value1),
        );

        if (rm.mf_value2) |mf_value2| {
            res = r.concatWS(res, rm.ws3);
            res = r.concat(res, r.renderMfRangeOp(rm.op2.?));
            res = r.concatWS(res, rm.ws4);
            res = r.concat(res, r.renderMfValue(mf_value2));
        }

        return res;
    }

    fn renderMfRangeOp(r: *Renderer, mfro: MfRangeOp) []const u8 {
        return switch (mfro) {
            .GreaterThan, .LessThan, .Equal => |t| r.renderToken(t),
            .GreaterThanEqual, .LessThanEqual => |gte| r.concat(r.renderToken(gte[0]), r.renderToken(gte[1])),
        };
    }

    fn renderMediaFeature(r: *Renderer, mf: MediaFeature) []const u8 {
        return switch (mf) {
            .mf_plain => |mp| r.renderMfPlain(mp.*),
            .mf_boolean => |mb| r.renderToken(mb),
            .mf_range => |mr| r.renderMfRange(mr.*),
        };
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

    fn renderString(r: *Renderer, string_index: usize) []const u8 {
		const string_tok: Token = r.tokens.items[string_index];
		const string = r.code[string_tok.start + 1..string_tok.end - 1];
        return r.concat("\"", r.concat(string, "\""));
    }

    fn renderRatio(r: *Renderer, ra: Ratio) []const u8 {
        var res: []const u8 = "";
        res = r.concat(res, r.renderToken(ra.left));
        res = r.concatWS(res, ra.ws1);
        res = r.concat(res, r.renderToken(ra.div));
        res = r.concatWS(res, ra.ws2);
        res = r.concat(res, r.renderToken(ra.right));
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

    fn concatWS(r: *Renderer, res: []const u8, ws: ?ArrayList(usize)) []const u8 {
        var new_res = res;
        if (ws) |_ws| new_res = r.concat(res, r.renderArrayOfTokens(_ws));
        return new_res;
    }

    pub fn deinit(r: *Renderer) void {
        r.renderer_arena.deinit();
        r.allocator.destroy(r.renderer_arena);
    }
};
