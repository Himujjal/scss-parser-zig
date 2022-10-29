const std = @import("std");
const scanner = @import("scanner.zig");
const token = @import("token.zig");
const _error = @import("error.zig");
const utils = @import("utils.zig");
const nodes = @import("nodes.zig");
const radix_tree = @import("radix_tree.zig");

const RadixTree = radix_tree.StringRadixTree;
const initAtKeywordRadixTree = radix_tree.initAtKeywordRadixTree;

const expect = std.testing.expect;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const AtKeywordTypes = token.AtKeywordTypes;

const Scanner = scanner.Scanner;

const Token = token.Token;
const TT = token.TokenType;
const WT = TT.WhitespaceToken;

const Error = _error.Error;
const ParserErrorType = _error.ParserErrorType;

const eqlStr = utils.eqlStr;

// ========================================  NODES ========================================
const StyleSheet = nodes.StyleSheet;
const Rule = nodes.Rule;
const CSSNodePlain = nodes.CSSNodePlain;
const CSSNode = nodes.CSSNode;
const Atrule = nodes.Atrule;
const AtRulePrelude = nodes.AtrulePrelude;
const Prelude = nodes.Prelude;
const Raw = nodes.Raw;
const Block = nodes.Block;
const SelectorList = nodes.SelectorList;
const Selector = nodes.Selector;
const SingleSelector = nodes.SingleSelector;
const TypeSelector = nodes.TypeSelector;
const Declaration = nodes.Declaration;
const DeclProperty = nodes.Property;
const DeclValue = nodes.DeclValue;
const Location = nodes.CSSLocation;
const FunctionNode = nodes.FunctionNode;
const Parentheses = nodes.Value;

const MediaQueryList = nodes.MediaQueryList;
const MediaQuery = nodes.MediaQuery;
const MediaCondition = nodes.MediaCondition;
const MediaQueryBranch2 = nodes.MediaQueryBranch2;
const MediaConditionWithoutOr = nodes.MediaConditionWithoutOr;
const MediaInParens = nodes.MediaInParens;
const MediaFeature = nodes.MediaFeature;
const MfPlain = nodes.MfPlain;
const MfRange = nodes.MfRange;
const MfValue = nodes.MfValue;
const Ratio = nodes.Ratio;
const MfRangeOp = nodes.MfRangeOp;
const MediaNot = nodes.MediaNot;
const MediaAndOr = nodes.MediaAndOr;
const GeneralEnclosed = nodes.GeneralEnclosed;

const CSSVariable = nodes.CSSVariable;
const TypeSelectorBranch = nodes.TypeSelectorBranch;
const Combinator = nodes.Combinator;
const AttributeSelector = nodes.AttributeSelector;
const PseudoClassSelector = nodes.PseudoClassSelector;
const PseudoElementSelector = nodes.PseudoElementSelector;

const SupportsCondition = nodes.SupportsCondition;
const NotSupportsInParens = nodes.NotSupportsInParens;
const SupportsInParensAndOr = nodes.SupportsInParensAndOr;
const SupportsConditionWithParens = nodes.SupportsConditionWithParens;
const SupportsFeature = nodes.SupportsFeature;
const SupportsDecl = nodes.SupportsDecl;
const SupportsSelectorFn = nodes.SupportsSelectorFn;
const SupportsInParens = nodes.SupportsInParens;

const PageSelectorList = nodes.PageSelectorList;
const PageSelector = nodes.PageSelector;
const PageSelectorPseudoPage = nodes.PageSelectorPseudoPage;

const URL = nodes.URL;
// =======================================================================================

pub const StyleLanguage = enum { CSS, SCSS, POSTCSS };
pub const ParserOptions = struct {
    is_inline: bool = false,
    parse_rule_prelude: bool = true,
    /// TODO: Implement this
    parse_selectors: bool = false,
    parse_media_prelude: bool = true,
    parse_custom_property: bool = true,
    parse_value: bool = true,
    skip_ws_comments: bool = false,
};
const Tree = nodes.Tree;

/// This error is used to signal that the following parsing for
/// for the node failed and that we are to parse the node as a raw.
/// That's it!
const UnMatchErr = error{UnMatch};

fn initArrayList(comptime t: type, allocator: Allocator) *ArrayList(t) {
    var r = allocator.create(ArrayList(t)) catch unreachable;
    r.* = ArrayList(t).init(allocator);
    return r;
}

pub const Parser = struct {
    const Self = @This();

    allocator: Allocator,
    tree: Tree,
    scanner_instance: Scanner,

    code: []const u8 = undefined,
    errors: *ArrayList(Error),
    warnings: *ArrayList(Error),
    tokens: *ArrayList(Token),
    token_len: usize = 0,
    options: ParserOptions,

    err: []const u8 = "",
    errPos: usize = 0,

    start: usize = 0,
    cursor: usize = 0,

    parser_arena: *ArenaAllocator,
    _a: Allocator,

    prev_end: bool = false,
    keep_ws: bool = false,
    prev_ws: bool = false,
    prevEnd: bool = false,
    prev_comment: bool = false,
    level: u32 = 0,

    radix_tree: RadixTree(AtKeywordTypes),

    tt: TT = TT.EOF,
    data: Token = Token{ .start = 1, .end = 1, .tok_type = .EOF },

    pub fn init(allocator: Allocator, opt: ParserOptions) Self {
        var options = opt;
        options.parse_media_prelude = true;

        var parser_arena = allocator.create(ArenaAllocator) catch unreachable;
        parser_arena.* = ArenaAllocator.init(allocator);

        const _a = parser_arena.allocator();

        var tokens = initArrayList(Token, allocator);
        var errors = initArrayList(Error, allocator);
        var warnings = initArrayList(Error, allocator);

        const scanner_instance = Scanner.init(
            _a,
            tokens,
            errors,
            warnings,
        );

        return Self{
            .allocator = allocator,
            .scanner_instance = scanner_instance,
            .tree = undefined,
            .errors = errors,
            .warnings = warnings,
            .options = options,
            .tokens = tokens,
            .token_len = tokens.items.len,
            .radix_tree = initAtKeywordRadixTree(),

            .parser_arena = parser_arena,
            ._a = _a,
        };
    }

    pub fn parse(p: *Parser, code: []const u8, is_inline: bool) *Self {
        _ = p.scanner_instance.scan(code);

        p.code = code;
        p.options.is_inline = is_inline;
        p.token_len = p.tokens.items.len;

        if (is_inline) {} else {
            var styleSheet = p.parseStylesheet();
            p.tree = Tree{ .StyleSheet = styleSheet };
        }
        return p;
    }

    pub fn deinitInternal(p: *Parser) void {
        p.allocator.destroy(p.errors);
        p.allocator.destroy(p.warnings);
        p.allocator.destroy(p.tokens);

        p.parser_arena.deinit();
        p.allocator.destroy(p.parser_arena);
    }

    pub fn deinit(p: *Parser) void {
        p.tokens.deinit();
        p.errors.deinit();
        p.warnings.deinit();

        p.scanner_instance.deinit();
        p.deinitInternal();
    }

    // --------------------------------------------
    // HasParseError returns true if there is a parse error (and not a read error).
    fn HasParseError(p: *Parser) bool {
        return p.errors.items.len != 0;
    }

    pub fn current(p: *Parser) Token {
        return p.tokens.items[p.cursor];
    }

    fn addExpectedTokenFoundThisError(p: *Parser, expected_token_type: TT) void {
        p.errors.append(Error.init(
            p._a,
            p.tokens.items[p.cursor].start,
            p.tokens.items[p.cursor].end,
            p.code,
            ParserErrorType{ .ExpectedTokenFoundThis = .{
                .expected_token_type = expected_token_type,
                .found_token_pos = p.cursor,
            } },
        )) catch unreachable;
    }

    fn addError(p: *Parser, _type: ParserErrorType) void {
        const _err = Error.init(
            p._a,
            p.tokens.items[p.start].start,
            p.tokens.items[p.cursor - 1].end,
            p.code,
            _type,
        );

        p.errors.append(_err) catch unreachable;
    }

    fn addWarnings(p: *Parser, message: []const u8) void {
        p.warnings.append(
            Error.init(
                p.allocator,
                p.tokens.items[p.start].start,
                p.tokens.items[p.cursor].end,
                p.code,
                message,
            ),
        ) catch unreachable;
    }

    /// only move the cursor once step ahead
    pub fn advance(p: *Parser) void {
        p.cursor += 1;
    }

    /// Get the next token and move the cursor one step ahead
    pub fn nextTok(p: *Parser) Token {
        if (!p.end()) {
            p.cursor += 1;
            return p.tokens.items[p.cursor];
        } else {
            return p.tokens.items[p.cursor - 1];
        }
    }

    fn eatOptional(p: *Parser, tts: []const TT) ?void {
        const tok_type = p.current().tok_type;
        for (tts) |tt| {
            if (tok_type == tt) {
                p.advance();
                return;
            }
        }
        p.addExpectedTokenFoundThisError(tts[0]);
        return null;
    }

    /// Eat the current token and advance cursor. If not. Add an error
    fn eat(p: *Parser, tt: TT) ?void {
        if (p.current().tok_type != tt) {
            p.addExpectedTokenFoundThisError(tt);
            return null;
        }
        p.advance();
    }

    fn end(p: *Parser) bool {
        return (p.token_len - 1) <= p.cursor;
    }

    fn getTokenString(p: *Parser, tok: Token) []const u8 {
        const _start = tok.start;
        const _end = tok.end;
        return p.code[_start.._end];
    }

    fn getTokenStrFromIndex(p: *Parser, index: usize) []const u8 {
        return p.getTokenString(p.tokens.items[index]);
    }

    fn getCurrTokStr(p: *Parser) []const u8 {
        return p.getTokenString(p.current());
    }

    fn getCurrentChar(p: *Parser) u8 {
        return p.code[p.tokens.items[p.cursor].start];
    }

    fn getNextChar(p: *Parser) u8 {
        return p.code[p.tokens.items[p.cursor].start + 1];
    }

    fn isCustomProperty(val: []const u8) bool {
        return val.len >= 2 and (val[0] == '-' and val[1] == '-');
    }

    /// Add Location of the Node. Manual token_end position
    fn addLocationWithEnd(p: *Parser, loc: *Location, token_start: usize, token_end: usize) void {
        const start_tok: Token = p.tokens.items[token_start];
        loc.start = start_tok.start;
        loc.start_line = start_tok.start_line;
        loc.start_col = start_tok.start_col;

        const end_tok: Token = p.tokens.items[token_end];
        loc.end = end_tok.end;
        loc.end_line = end_tok.end_line;
        loc.end_col = end_tok.end_col;

        p.start = p.cursor;
    }

    /// Add Location of Node. Auto-deciphering token_end position
    fn addLocation(p: *Parser, loc: *Location, token_start: usize) void {
        const token_end = p.cursor - 1;
        p.addLocationWithEnd(loc, token_start, token_end);
    }

    fn lookAhead(p: *Parser) Token {
        if (p.token_len <= p.cursor + 1) {
            return p.tokens.items[p.cursor + 1];
        }
        return p.tokens.items[p.token_len - 1];
    }

    fn lookSuperAhead(p: *Parser) Token {
        if (p.token_len <= p.cursor + 2) {
            return p.tokens.items[p.cursor + 2];
        }
        return p.tokens.items[p.token_len - 1];
    }

    fn lookSuperDuperAhead(p: *Parser) Token {
        if (p.token_len <= p.cursor + 3) {
            return p.tokens.items[p.cursor + 3];
        }
        return p.tokens.items[p.token_len - 1];
    }

    // ======================= All Parse Rules ===============================

    fn parseStylesheet(p: *Parser) StyleSheet {
        var style_sheet = StyleSheet.init(p._a);
        const _start = p.start;

        var tok = p.current();
        while (!p.end()) {
            p.start = p.cursor;
            const tt = tok.tok_type;
            var node: ?CSSNode = null;

            switch (tt) {
                .CDOToken => {
                    node = CSSNode{ .cdo = p.cursor };
                    tok = p.nextTok();
                },
                .CDCToken => {
                    node = CSSNode{ .cdc = p.cursor };
                    tok = p.nextTok();
                },
                .WhitespaceToken, .CommentToken => {
                    if (!p.options.skip_ws_comments) {
                        node = CSSNode{ .whitespaces = p.consumeWhiteSpaceComment().? };
                    }
                },
                .ErrorToken => {
                    p.addError(ParserErrorType.ErrorTokenFound);
                    node = CSSNode{ ._error = p.start };
                    tok = p.nextTok();
                },
                .AtKeywordToken => {
                    node = CSSNode{ .at_rule = p.parseAtRule() };
                    tok = p.nextTok();
                },
                else => {
                    const rule = p.parseRule();
                    if (rule == null) {
                        const raw = p.parseRaw(&[_]TT{TT.EOF}, null);
                        node = CSSNode{ .raw = raw };
                    } else {
                        node = CSSNode{ .rule = rule.? };
                    }
                },
            }

            if (node) |n| {
                style_sheet.children.append(n) catch unreachable;
            }
        }
        p.addLocation(&style_sheet.loc, _start);

        return style_sheet;
    }

    fn parseRule(p: *Parser) ?Rule {
        const _start = p.start;

        const prelude = p.parsePrelude();
        if (prelude == null) return null;

        if (p.end()) {
            p.cursor = _start;
            return null;
        }

        const block = p.parseBlock(true, false);

        var rule: Rule = Rule.init(p._a);
        rule.prelude = prelude.?;
        rule.block = block;

        p.addLocation(&rule.loc, _start);
        return rule;
    }

    /// The main At rule parser
    fn parseAtRule(p: *Parser) Atrule {
        var at_rule = Atrule.init();
        const _start = p.start;

        var tok = p.current();

        const at_rule_type_str = utils.toLower(p._a, p.code[tok.start + 1 .. tok.end]);
        at_rule.rule_type = p.radix_tree.get(at_rule_type_str) orelse AtKeywordTypes.Custom;
        at_rule.rule_index = p.cursor;

        tok = p.nextTok();

        at_rule.ws1 = p.consumeWhiteSpaceComment();
        tok = p.current();

        if (!p.end() and tok.tok_type != TT.LeftBraceToken and tok.tok_type != TT.SemicolonToken) {
            if (p.options.parse_rule_prelude) {
                const at_rule_prelude: AtRulePrelude = p.parseAtRulePrelude(at_rule.rule_type);
                at_rule.prelude = .{ .at_rule_prelude = at_rule_prelude };
            } else {
                const raw_tokens: []const TT = &[_]TT{ TT.LeftBraceToken, TT.SemicolonToken };
                at_rule.prelude = .{ .raw = p.parseRaw(raw_tokens, null) };
            }
        }

        at_rule.ws2 = p.consumeWhiteSpaceComment();

        tok = p.current();

        switch (tok.tok_type) {
            // @charset, @import, @namespace
            TT.SemicolonToken => {},
            TT.LeftBraceToken => {
                // TODO: Parse this at-rule
                const block = p.parseBlock(true, true);
                at_rule.block = block;
            },
            else => {
                // Handle fallback here
            },
        }

        p.addLocation(&at_rule.loc, _start);
        return at_rule;
    }

    fn parseAtRulePrelude(p: *Parser, rule_type: AtKeywordTypes) AtRulePrelude {
        var at_rule_prelude = AtRulePrelude.init(p._a);
        const _start = p.start;

        var tok = p.current();

        switch (rule_type) {
            AtKeywordTypes.Media => {
                if (p.options.parse_media_prelude) {
                    if (p.parseMediaQueryList()) |media_query_list| {
                        at_rule_prelude.children.append(CSSNode{ .media_query_list = media_query_list }) catch unreachable;
                    } else {
                        at_rule_prelude.children.append(
                            CSSNode{ .raw = p.parseRaw(&[_]TT{ TT.SemicolonToken, TT.LeftBraceToken, TT.EOF }, null) },
                        ) catch unreachable;
                    }
                } else {
                    const raw: Raw = p.parseRaw(&[_]TT{ TT.LeftBraceToken, TT.SemicolonToken }, null);
                    at_rule_prelude.children.append(CSSNode{ .raw = raw }) catch unreachable;
                }
            },
            AtKeywordTypes.Charset => {
                tok = p.current();
                if (tok.tok_type == TT.WhitespaceToken and p.getCurrentChar() == ' ') {
                    tok = p.nextTok();
                    if (tok.tok_type == TT.StringToken) {
                        at_rule_prelude.children.append(CSSNode{ .string_node = p.cursor }) catch unreachable;
                    } else {
                        p.parseAtRulePreludeRaw(&at_rule_prelude.children, &[_]TT{TT.SemicolonToken});
                    }
                } else {
                    p.parseAtRulePreludeRaw(&at_rule_prelude.children, &[_]TT{TT.SemicolonToken});
                }
            },
            AtKeywordTypes.ColorProfile => {
                tok = p.current();
                var ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                if (ws_comments) |w| at_rule_prelude.children.append(w) catch unreachable;

                // css_var = p.parseCSSVariable() catch CSSNode{ .raw = p.parseRaw([]TT{ TT.LeftBraceToken, TT.SemicolonToken }) };

                ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                if (ws_comments) |w| at_rule_prelude.children.append(w) catch unreachable;
            },
            AtKeywordTypes.CounterStyle => {
                tok = p.current();
                var ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                if (ws_comments) |w| at_rule_prelude.children.append(w) catch unreachable;

                if (p.current().tok_type == TT.IdentToken) {
                    at_rule_prelude.children.append(CSSNode{ .identifier = p.cursor }) catch unreachable;
                }

                var ws_comments2 = p.consumeWhiteSpaceCommentCSSNode();
                if (ws_comments2) |w| at_rule_prelude.children.append(w) catch unreachable;
            },
            AtKeywordTypes.FontFace => {
                if (p.readSequence(&[_]TT{TT.LeftBraceToken}, '{')) |seq| {
                    at_rule_prelude.children = seq;
                }
            },
            AtKeywordTypes.FontFeatureValue => {
                const raw = p.parseRaw(&[_]TT{ TT.SemicolonToken, TT.LeftBraceToken }, null);
                at_rule_prelude.children.append(CSSNode{ .raw = raw }) catch unreachable;
            },
            AtKeywordTypes.Import => {
                switch (tok.tok_type) {
                    TT.URLToken => {
                        at_rule_prelude.children.append(CSSNode{ .url = p.cursor }) catch unreachable;
                        tok = p.nextTok();
                    },
                    TT.StringToken => {
                        at_rule_prelude.children.append(CSSNode{ .string_node = p.cursor }) catch unreachable;
                        tok = p.nextTok();
                    },
                    else => {
                        // error
                    },
                }
                var ws_css_nodep = p.consumeWhiteSpaceCommentCSSNode();
                if (ws_css_nodep) |w| at_rule_prelude.children.append(w) catch unreachable;

                tok = p.current();

                switch (tok.tok_type) {
                    TT.IdentToken => {
                        if (p.parseMediaQueryList()) |media_query_list| {
                            at_rule_prelude.children.append(CSSNode{ .media_query_list = media_query_list }) catch unreachable;
                        } else {
                            at_rule_prelude.children.append(
                                CSSNode{ .raw = p.parseRaw(&[_]TT{ TT.LeftBraceToken, TT.EOF, TT.SemicolonToken }, null) },
                            ) catch unreachable;
                        }
                    },
                    else => {
                        at_rule_prelude.children.append(CSSNode{
                            .raw = p.parseRaw(&[_]TT{TT.SemicolonToken}, null),
                        }) catch unreachable;
                    },
                }
            },
            AtKeywordTypes.Keyframes => {
                var ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                if (ws_comments) |w| at_rule_prelude.children.append(w) catch unreachable;

                tok = p.current();
                if (tok.tok_type == TT.IdentToken) {
                    at_rule_prelude.children.append(CSSNode{ .identifier = p.cursor }) catch unreachable;
                } else if (tok.tok_type == TT.StringToken) {
                    at_rule_prelude.children.append(CSSNode{ .string_node = p.cursor }) catch unreachable;
                }
                var ws_comments_2 = p.consumeWhiteSpaceCommentCSSNode();
                if (ws_comments_2) |w| at_rule_prelude.children.append(w) catch unreachable;
            },
            AtKeywordTypes.Layer => {
                var ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                if (ws_comments) |w| at_rule_prelude.children.append(w) catch unreachable;

                tok = p.current();
                while (tok.tok_type != TT.LeftBraceToken or tok.tok_type != TT.SemicolonToken) {
                    ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                    if (ws_comments) |w| at_rule_prelude.children.append(w) catch unreachable;

                    if (tok.tok_type == TT.IdentToken) {
                        at_rule_prelude.children.append(CSSNode{ .identifier = p.cursor }) catch unreachable;
                        tok = p.nextTok();
                        var ws_comments_2 = p.consumeWhiteSpaceCommentCSSNode();
                        if (ws_comments_2) |w| at_rule_prelude.children.append(w) catch unreachable;

                        tok = p.current();

                        if (tok.tok_type != TT.DelimToken or p.getCurrentChar() != ',') continue;

                        var ws_comments_3 = p.consumeWhiteSpaceCommentCSSNode();
                        if (ws_comments_3) |w| at_rule_prelude.children.append(w) catch unreachable;

                        tok = p.current();

                        if (tok.tok_type == TT.IdentToken) continue;
                    }
                    const raw = p.parseRaw(&[_]TT{ TT.SemicolonToken, TT.LeftBraceToken }, null);
                    at_rule_prelude.children.append(CSSNode{ .raw = raw }) catch unreachable;
                }

                // p.appendWSCommentsCSSNode(&at_rule_prelude);
                // ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                // at_rule_prelude.children.appendslice(ws_comments.toOwnedSlice()) catch unreachable;
            },
            AtKeywordTypes.Namespace => {
                // TODO: Namespace
            },
            AtKeywordTypes.Page => {
                if (p.parsePageSelectorList()) |page_selector_list| {
                    at_rule_prelude.children.append(CSSNode{ .page_selector_list = page_selector_list }) catch unreachable;
                } else {
                    p.cursor = _start;
                    p.start = _start;
                    p.parseAtRulePreludeRaw(&at_rule_prelude.children, &[_]TT{ TT.SemicolonToken, TT.LeftBraceToken });
                }
            },
            AtKeywordTypes.Supports => {
                if (p.parseSupportsCondition()) |supports_condition| {
                    at_rule_prelude.children.append(CSSNode{ .supports = supports_condition }) catch unreachable;
                } else {
                    p.cursor = _start;
                    p.start = _start;
                    p.parseAtRulePreludeRaw(&at_rule_prelude.children, &[_]TT{ TT.SemicolonToken, TT.LeftBraceToken });
                }
            },
            else => {
                p.parseAtRulePreludeRaw(&at_rule_prelude.children, &[_]TT{ TT.LeftBraceToken, TT.SemicolonToken });
            },
        }

        p.addLocation(&at_rule_prelude.loc, _start);
        return at_rule_prelude;
    }

    fn parsePageSelectorList(p: *Parser) ?PageSelectorList {
        var page_selector_list = PageSelectorList.init(p._a);

        var tok = p.current();
        while (!p.end()) {
            const _start = p.start;
            var page_selector: PageSelector = PageSelector.init(p._a);

            if (tok.tok_type == TT.IdentToken) {
                page_selector.ident = p.cursor;
                tok = p.nextTok();
                page_selector.ws1 = p.consumeWhiteSpaceComment();
                tok = p.current();
            }

            while (!p.end()) {
                const ws1 = p.consumeWhiteSpaceComment();
                tok = p.current();
                if (tok.tok_type == TT.ColonToken) {
                    const colon = p.cursor;
                    tok = p.nextTok();
                    if (tok.tok_type == TT.IdentToken) {
                        const ident = p.cursor;
                        tok = p.nextTok();
                        const pseudo_page = [2]usize{ colon, ident };
                        const page_selector_branch = PageSelectorPseudoPage{
                            .ws1 = ws1,
                            .pseudo_page = pseudo_page,
                        };
                        page_selector.ws2 = p.consumeWhiteSpaceComment();
                        page_selector.pseudo_pages.append(page_selector_branch) catch unreachable;
                    } else break;
                } else {
                    break;
                }

                tok = p.current();
            }
            p.addLocation(&page_selector.loc, _start);

            page_selector_list.append(page_selector) catch unreachable;

            if (tok.tok_type != .CommaToken) break;
            tok = p.nextTok();
        }

        return page_selector_list;
    }

    fn parseSupportsCondition(p: *Parser) ?SupportsCondition {
        const tok_type = p.current().tok_type;
        if (tok_type == TT.IdentToken and std.mem.eql(u8, p.getCurrTokStr(), "not")) {
            const not_supports_in_parens = p.parseNotSupportsInParens() orelse return null;
            return SupportsCondition{ .not_supports_in_parens = not_supports_in_parens };
        } else if (tok_type == TT.LeftParenthesisToken or tok_type == TT.FunctionToken) {
            const supports_in_parens_and_or = p.parseSupportsInParensAndOr() orelse return null;
            return SupportsCondition{ .supports_in_parens_and_or = supports_in_parens_and_or };
        }
        return null;
    }

    fn parseNotSupportsInParens(p: *Parser) ?*NotSupportsInParens {
        const _start = p.start;
        var not_supports_in_parens = p.heaped(NotSupportsInParens, NotSupportsInParens{});
        not_supports_in_parens.not_index = p.cursor;
        p.advance();
        not_supports_in_parens.ws1 = p.consumeWhiteSpaceComment();

        const supports_parens = p.parseSupportsInParens() orelse return null;
        not_supports_in_parens.supports_parens = supports_parens;

        p.addLocation(&not_supports_in_parens.loc, _start);
        return not_supports_in_parens;
    }

    fn parseSupportsInParensAndOr(p: *Parser) ?*SupportsInParensAndOr {
        const _start = p.start;
        var supports_in_parens_and_or = p.heaped(SupportsInParensAndOr, SupportsInParensAndOr.init(p._a));

        var tt = p.current().tok_type;
        var is_and: ?bool = null;

        var checkpoint = p.cursor;
        while (!p.end() and tt != TT.LeftBraceToken and tt != TT.SemicolonToken) {
            if (p.parseSupportsInParens()) |supports_in_parens| {
                checkpoint = p.cursor;
                supports_in_parens_and_or.list_supports_in_parens.append(supports_in_parens) catch unreachable;
                supports_in_parens_and_or.ws1_list.append(p.consumeWhiteSpaceComment()) catch unreachable;

                if (p.current().tok_type == TT.IdentToken) {
                    if (eqlStr(p.getCurrTokStr(), "and") and (is_and == null or is_and.?)) {
                        is_and = true;
                        supports_in_parens_and_or.and_or_list.append(p.cursor) catch unreachable;
                    } else if (eqlStr(p.getCurrTokStr(), "or") and (is_and == null or !is_and.?)) {
                        is_and = false;
                        supports_in_parens_and_or.and_or_list.append(p.cursor) catch unreachable;
                    } else {
                        return null;
                    }
                    p.advance();
                }
                supports_in_parens_and_or.ws2_list.append(p.consumeWhiteSpaceComment()) catch unreachable;
            } else {
                return null;
            }
            tt = p.current().tok_type;
        }
        p.cursor = checkpoint;

        p.addLocation(&supports_in_parens_and_or.loc, _start);
        return supports_in_parens_and_or;
    }

    fn parseSupportsInParens(p: *Parser) ?SupportsInParens {
        const _start = p.start;
        switch (p.current().tok_type) {
            .LeftParenthesisToken => {
                _ = p.eat(TT.LeftParenthesisToken);

                const ws1 = p.consumeWhiteSpaceComment();
                var tok = p.current();

                if (tok.tok_type == TT.IdentToken or tok.tok_type == TT.CustomPropertyNameToken) {
                    if (eqlStr("not", p.getCurrTokStr())) {
                        var supports_cond_with_parens = p.heaped(SupportsConditionWithParens, SupportsConditionWithParens{});
                        supports_cond_with_parens.ws1 = ws1;

                        var not_supports_in_parens = p.parseNotSupportsInParens() orelse return null;
                        supports_cond_with_parens.condition = SupportsCondition{ .not_supports_in_parens = not_supports_in_parens };

                        supports_cond_with_parens.ws2 = p.consumeWhiteSpaceComment();
                        p.eat(.RightParenthesisToken) orelse return null;

                        p.addLocation(&supports_cond_with_parens.loc, _start);
                        return SupportsInParens{ .supports_condition = supports_cond_with_parens };
                    } else {
                        const supports_decl: *SupportsDecl = p.heaped(SupportsDecl, SupportsDecl{});
                        supports_decl.ws1 = ws1;
                        supports_decl.declaration = p.parseDeclaration(false) orelse return null;
                        supports_decl.ws2 = p.consumeWhiteSpaceComment();

                        p.eat(.RightParenthesisToken) orelse return null;

                        return SupportsInParens{ .supports_feature = SupportsFeature{
                            .supports_decl = supports_decl,
                        } };
                    }
                } else if (tok.tok_type == TT.LeftParenthesisToken) {
                    var supports_cond_with_parens = p.heaped(SupportsConditionWithParens, SupportsConditionWithParens{});
                    supports_cond_with_parens.ws1 = p.consumeWhiteSpaceComment();
                    supports_cond_with_parens.condition = p.parseSupportsCondition() orelse return null;
                    supports_cond_with_parens.ws2 = p.consumeWhiteSpaceComment();
                    p.addLocation(&supports_cond_with_parens.loc, _start);
                    return SupportsInParens{ .supports_condition = supports_cond_with_parens };
                }
            },
            .FunctionToken => {
                const s = p.getCurrTokStr();
                const is_special_func = eqlStr(s, "selector") or eqlStr(s, "font-tech") or eqlStr(s, "font-format");
                if (is_special_func) {
                    // Is Supports Selector Function
                    const func = p.parseFunction() orelse return null;
                    const f = p.heaped(FunctionNode, func);
                    return SupportsInParens{ .supports_feature = SupportsFeature{ .supports_selector_fn = f } };
                } else {
                    const general_enclosed = p.parseGeneralEnclosed() orelse return null;
                    return SupportsInParens{ .general_enclosed = general_enclosed };
                }
            },
            else => return null,
        }
        return null;
    }

    fn parseCSSVariable(p: *Parser) ?CSSVariable {
        const _start = p.start;
        var tok = p.current();
        if (tok.tok_type == .DelimToken and p.getCurrentChar() == '-' and
            p.lookAhead().tok_type == .DelimToken and p.getNextChar() == '-' and
            p.lookSuperAhead().tok_type == .IdentToken)
        {
            p.cursor += 3;
            var loc = Location{};
            p.addLocation(&loc, _start);
            return p.cursor - 1;
        } else if (tok.tok_type == .CustomPropertyNameToken) {
            const var_name = p.cursor;
            p.advance();
            return var_name;
        }
        return null;
    }

    fn parseAtRulePreludeRaw(p: *Parser, children: *ArrayList(CSSNode), tokens: []const TT) void {
        var raw: Raw = p.parseRaw(tokens, null);
        children.append(CSSNode{ .raw = raw }) catch unreachable;
    }

    fn parseMediaQueryList(p: *Parser) ?MediaQueryList {
        var media_query_list = MediaQueryList.init(p._a);
        const _start = p.start;

        var curr_tt = p.current().tok_type;

        while (!p.end() and (curr_tt != TT.LeftBraceToken and curr_tt != TT.SemicolonToken)) : (curr_tt = p.current().tok_type) {
            var media_query = p.parseMediaQuery();
            if (media_query) |mq| {
                media_query_list.children.append(mq) catch unreachable;
            } else {
                // TODO: error
                return null;
            }
            if (p.current().tok_type == TT.CommaToken) p.advance();
        }

        p.addLocation(&media_query_list.loc, _start);
        return media_query_list;
    }

    fn parseMediaQuery(p: *Parser) ?MediaQuery {
        var media_query = MediaQuery.init();
        const _start = p.start;

        media_query.ws1 = p.consumeWhiteSpaceComment();
        const checkpoint = p.cursor;

        const media_condition = p.parseMediaCondition();
        if (media_condition) |mc| {
            media_query.branches.media_condition = mc;
        } else {
            p.cursor = checkpoint;
            const media_query_branch_2 = p.parseMediaQueryBranch2();
            if (media_query_branch_2) |mqb2| {
                media_query.branches = .{ .media_query_branch_2 = mqb2 };
            } else {
                // to be parsed raw
                return null;
            }
        }

        media_query.ws2 = p.consumeWhiteSpaceComment();

        p.addLocation(&media_query.loc, _start);
        return media_query;
    }

    fn parseMediaNot(p: *Parser) ?*MediaNot {
        const _start = p.cursor;
        var media_not: *MediaNot = p.heaped(MediaNot, MediaNot{});

        var tok = p.current();
        var tok_str = p.getCurrTokStr();

        if (tok.tok_type == .IdentToken and std.mem.eql(u8, tok_str, "not")) {
            media_not.not = p.cursor;
            tok = p.nextTok();
            media_not.ws1 = p.consumeWhiteSpaceComment();
            media_not.media_in_parens = p.parseMediaInParens() orelse return null;
        } else {
            return null;
        }

        p.addLocation(&media_not.loc, _start);
        return media_not;
    }

    // TODO: WOrk on this
    fn parseMediaCondition(p: *Parser) ?MediaCondition {
        const _start = p.cursor;
        var tok = p.current();
        var tok_str = p.getCurrTokStr();

        if (tok.tok_type == .IdentToken and std.mem.eql(u8, tok_str, "not")) {
            const media_not = p.parseMediaNot();
            if (media_not) |mn| {
                return MediaCondition{ .media_not = mn };
            } else {
                p.cursor = _start;
                return null;
            }
        } else if (tok.tok_type == .LeftParenthesisToken) {
            var media_and_or: *MediaAndOr = p.heaped(MediaAndOr, MediaAndOr.init(p._a));
            var is_and: ?bool = null;

            outer_loop: while (true) {
                const media_parens = p.parseMediaInParens() orelse return null;
                media_and_or.children.append(media_parens.*) catch unreachable;

                var fallback_cursor = p.cursor;

                const ws1 = p.consumeWhiteSpaceComment();
                media_and_or.ws1_list.append(ws1) catch unreachable;

                tok = p.current();
                switch (tok.tok_type) {
                    .CommaToken, .LeftBraceToken, .EOF => {
                        p.cursor = fallback_cursor;
                        is_and = false;
                        break :outer_loop;
                    },
                    .IdentToken => {
                        tok_str = p.getCurrTokStr();
                        if (std.mem.eql(u8, tok_str, "and")) {
                            if (is_and) |ia| {
                                if (!ia) return null;
                            }
                            is_and = true;
                        } else if (std.mem.eql(u8, tok_str, "or")) {
                            if (is_and) |ia| {
                                if (ia) return null;
                            }
                            is_and = false;
                        } else {
                            return null;
                        }

                        media_and_or.and_or_list.append(p.cursor) catch unreachable;

                        const ws2 = p.consumeWhiteSpaceComment();
                        media_and_or.ws2_list.append(ws2) catch unreachable;
                    },
                    else => return null,
                }
            }

            media_and_or.is_and = is_and.?;
            if (media_and_or.is_and) {
                return MediaCondition{ .media_and = media_and_or };
            } else {
                return MediaCondition{ .media_or = media_and_or };
            }
        }

        p.cursor = _start;
        return null;
    }

    fn parseMediaQueryBranch2(p: *Parser) ?*MediaQueryBranch2 {
        var media_query_branch_2: *MediaQueryBranch2 = p.heaped(MediaQueryBranch2, MediaQueryBranch2{});
        const _start = p.cursor;

        var tok = p.current();

        if (tok.tok_type == .IdentToken) {
            const curr_str = p.getCurrTokStr();
            if (std.mem.eql(u8, curr_str, "not") or std.mem.eql(u8, curr_str, "only")) {
                media_query_branch_2.is_not = std.mem.eql(u8, curr_str, "not");
                media_query_branch_2.not_only_index = p.cursor;
                tok = p.nextTok();
                media_query_branch_2.ws1 = p.consumeWhiteSpaceComment();
                tok = p.current();
            }
            if (tok.tok_type == .IdentToken) {
                media_query_branch_2.media_type = p.cursor;
                tok = p.nextTok();
            }
        }

        media_query_branch_2.ws2 = p.consumeWhiteSpaceComment();
        tok = p.current();
        var point = p.cursor;

        if (tok.tok_type == .IdentToken and std.mem.eql(u8, p.getCurrTokStr(), "and")) {
            p.advance();
            media_query_branch_2.ws3 = p.consumeWhiteSpaceComment();
            tok = p.current();

            var media_condition_without_or = p.parseMediaConditionWithoutOr();
            if (media_condition_without_or) |mcwo| {
                media_query_branch_2.media_condition_without_or = mcwo;
            } else {
                p.cursor = point;
                return null;
            }
        }
        switch (p.current().tok_type) {
            .CommaToken, .LeftBraceToken, .SemicolonToken => {
                p.addLocation(&media_query_branch_2.loc, _start);
                return media_query_branch_2;
            },
            else => {
                p.cursor = point;
                return null;
            },
        }
    }

    fn parseMediaConditionWithoutOr(p: *Parser) ?MediaConditionWithoutOr {
        const _start = p.cursor;
        var tok = p.current();
        var tok_str = p.getCurrTokStr();

        if (tok.tok_type == .IdentToken and std.mem.eql(u8, tok_str, "not")) {
            const media_not = p.parseMediaNot();
            if (media_not) |mn| {
                return MediaConditionWithoutOr{ .media_not = mn };
            } else {
                p.cursor = _start;
                return null;
            }
        } else if (tok.tok_type == .LeftParenthesisToken) {
            var media_and_or: *MediaAndOr = p.heaped(MediaAndOr, MediaAndOr.init(p._a));
            media_and_or.is_and = true;

            outer_loop: while (true) {
                const media_parens = p.parseMediaInParens() orelse return null;
                media_and_or.children.append(media_parens.*) catch unreachable;

                var fallback_cursor = p.cursor;

                const ws1 = p.consumeWhiteSpaceComment();
                media_and_or.ws1_list.append(ws1) catch unreachable;

                tok = p.current();

                switch (tok.tok_type) {
                    .CommaToken, .LeftBraceToken, .EOF, .SemicolonToken => {
                        p.cursor = fallback_cursor;
                        break :outer_loop;
                    },
                    .IdentToken => {
                        tok_str = p.getCurrTokStr();
                        if (!std.mem.eql(u8, tok_str, "and")) return null;

                        media_and_or.and_or_list.append(p.cursor) catch unreachable;

                        const ws2 = p.consumeWhiteSpaceComment();
                        media_and_or.ws2_list.append(ws2) catch unreachable;
                    },
                    else => return null,
                }
            }

            return MediaConditionWithoutOr{ .media_and = media_and_or };
        }

        return null;
    }

    fn parseMediaInParens(p: *Parser) ?*MediaInParens {
        var media_in_parens: *MediaInParens = p._a.create(MediaInParens) catch unreachable;
        const _start = p.start;
        var tok = p.current();

        if (tok.tok_type == TT.LeftParenthesisToken) {
            p.advance();
            media_in_parens.ws1 = p.consumeWhiteSpaceComment();
            tok = p.current();

            if (tok.tok_type == TT.IdentToken) {
                if (std.mem.eql(u8, p.getCurrTokStr(), "not")) {
                    if (p.parseMediaCondition()) |cdn| {
                        media_in_parens.content.media_condition = cdn;
                    } else {
                        p.cursor = _start;
                        return null;
                    }
                } else {
                    if (p.parseMediaFeature()) |mf| {
                        media_in_parens.content = .{ .media_feature = mf };
                    } else {
                        p.cursor = _start;
                        return null;
                    }
                }
            } else if (tok.tok_type == TT.LeftParenthesisToken) {
                // media_condition
                const condition = p.parseMediaCondition();
                if (condition) |cdn| {
                    media_in_parens.content.media_condition = cdn;
                } else {
                    p.cursor = _start;
                    return null;
                }
            }

            media_in_parens.ws2 = p.consumeWhiteSpaceComment();
            tok = p.current();

            if (tok.tok_type == TT.RightParenthesisToken) {
                p.advance();
            } else {
                p.cursor = _start;
                return null;
            }
        } else if (tok.tok_type == TT.FunctionToken) {
            if (p.parseGeneralEnclosed()) |general_enclosed| {
                media_in_parens.content = .{ .general_enclosed = general_enclosed };
            } else {
                p.cursor = _start;
                return null;
            }
        } else {
            return null;
        }

        p.addLocation(&media_in_parens.loc, _start);
        return media_in_parens;
    }

    fn parseGeneralEnclosed(p: *Parser) ?GeneralEnclosed {
        const f = p.parseFunction() orelse return null;
        return GeneralEnclosed{ .function = p.heaped(FunctionNode, f) };
    }

    fn parseMediaFeature(p: *Parser) ?MediaFeature {
        const _start = p.cursor;

        var tok = p.current();

        const first_name_index = p.cursor;
        tok = p.nextTok();

        const ws1 = p.consumeWhiteSpaceComment();
        tok = p.current();

        if (tok.tok_type == TT.RightParenthesisToken) {
            p.cursor = first_name_index + 1;
            return MediaFeature{ .mf_boolean = first_name_index };
        } else if (tok.tok_type == TT.ColonToken) {
            tok = p.nextTok();

            var mf_plain: *MfPlain = p._a.create(MfPlain) catch unreachable;
            mf_plain.name = first_name_index;
            mf_plain.ws1 = ws1;
            mf_plain.ws2 = p.consumeWhiteSpaceComment();

            tok = p.current();

            if (p.parseMfValue()) |val| {
                mf_plain.value = val;
            } else {
                p.cursor = _start;
                return null;
            }
            p.addLocation(&mf_plain.loc, _start);
            return MediaFeature{ .mf_plain = mf_plain };
        } else if (tok.tok_type == TT.DelimToken) {
            const curr_char = p.getCurrentChar();
            switch (curr_char) {
                '<', '>', '=' => {
                    p.cursor = first_name_index;
                    const mf_range = p.parseMfRange();
                    if (mf_range) |mr| {
                        return MediaFeature{ .mf_range = mr };
                    } else {
                        p.cursor = first_name_index;
                        return null;
                    }
                },
                else => {
                    return null;
                },
            }
        }
        return null;
    }

    fn parseMfValue(p: *Parser) ?MfValue {
        const _start = p.cursor;
        var tok: Token = p.current();
        switch (tok.tok_type) {
            .NumberToken => {
                const first_number_index = p.cursor;
                p.advance();

                const ws1 = p.consumeWhiteSpaceComment();
                tok = p.current();

                if (tok.tok_type == TT.DelimToken and p.getCurrentChar() == '/') {
                    const div_index = p.cursor;
                    const ws2 = p.consumeWhiteSpaceComment();
                    tok = p.current();
                    const second_number_index = p.cursor;
                    tok = p.nextTok();
                    var ratio: *Ratio = p._a.create(Ratio) catch unreachable;
                    ratio.* = Ratio{
                        .left = first_number_index,
                        .ws1 = ws1,
                        .div = div_index,
                        .ws2 = ws2,
                        .right = second_number_index,
                    };
                    p.addLocation(&ratio.loc, _start);
                    return MfValue{ .ratio = ratio };
                } else if (tok.tok_type == TT.RightParenthesisToken) {
                    p.cursor = first_number_index + 1;
                    return MfValue{ .number = first_number_index };
                }
                p.cursor = _start;
                return null;
            },
            .DimensionToken => {
                p.advance();
                return MfValue{ .dimension = p.cursor - 1 };
            },
            .IdentToken => {
                p.advance();
                return MfValue{ .ident = p.cursor - 1 };
            },
            else => return null,
        }
    }

    /// <mf-name> [ '<' | '>' ]? '='? <mf-value> |
    /// <mf-value> [ '<' | '>' ]? '='? <mf-name> |
    /// <mf-value> '<' '='? <mf-name> '<' '='? <mf-value> |
    /// <mf-value> '>' '='? <mf-name> '>' '='? <mf-value>
    fn parseMfRange(p: *Parser) ?*MfRange {
        const _start = p.cursor;
        const mf_range: *MfRange = p._a.create(MfRange) catch unreachable;
        mf_range.* = MfRange{};
        var tok = p.current();

        if (tok.tok_type == TT.IdentToken) {
            mf_range.mf_name_first = true;
            mf_range.mf_name = p.cursor;
            tok = p.nextTok();
        } else {
            if (p.parseMfValue()) |value| {
                mf_range.mf_name_first = false;
                mf_range.mf_value1 = value;
            } else {
                return null;
            }
        }
        mf_range.ws1 = p.consumeWhiteSpaceComment();
        mf_range.op1 = p.parseMfRangeOp() orelse return null;
        mf_range.ws2 = p.consumeWhiteSpaceComment();
        tok = p.current();

        if (mf_range.mf_name_first) {
            mf_range.mf_value1 = p.parseMfValue() orelse return null;
            tok = p.current();
        } else {
            if (tok.tok_type == TT.IdentToken) {
                mf_range.mf_name = p.cursor;
                p.advance();

                mf_range.ws3 = p.consumeWhiteSpaceComment();
                tok = p.current();
                if (tok.tok_type != TT.DelimToken) return null;
                mf_range.op2 = p.parseMfRangeOp() orelse return null;
                mf_range.ws4 = p.consumeWhiteSpaceComment();

                mf_range.mf_value2 = p.parseMfValue() orelse return null;
            } else {
                return null;
            }
        }

        p.addLocation(&mf_range.loc, _start);
        return mf_range;
    }

    fn parseMfRangeOp(p: *Parser) ?MfRangeOp {
        var next_tok: Token = p.tokens.items[p.cursor + 1];

        const next_char = p.getNextChar();
        const op_index = p.cursor;

        switch (p.getCurrentChar()) {
            '<' => {
                if (next_tok.tok_type == .DelimToken) {
                    if (next_char != '=') return null;
                    p.advance();
                    p.advance();
                    return MfRangeOp{ .LessThanEqual = [2]usize{ op_index, op_index + 1 } };
                }
                p.advance();
                return MfRangeOp{ .LessThan = op_index };
            },
            '>' => {
                if (next_tok.tok_type == .DelimToken) {
                    if (next_char != '=') return null;
                    p.advance();
                    p.advance();
                    return MfRangeOp{ .GreaterThanEqual = [2]usize{ op_index, op_index + 1 } };
                }
                p.advance();
                return MfRangeOp{ .GreaterThan = op_index };
            },
            '=' => {
                p.advance();
                return MfRangeOp{ .Equal = op_index };
            },
            else => {
                return null;
            },
        }
    }

    fn parsePrelude(p: *Parser) ?Prelude {
        const _start = p.start;
        if (p.options.parse_rule_prelude == false) {
            const raw = p.parseRaw(&[_]TT{ TT.LeftBraceToken, TT.EOF }, null);
            return Prelude{ .raw = raw };
        }
        // Parse Selector List
        const selector_list = p.parseSelectorList();
        if (selector_list) |l| {
            return Prelude{ .selector_list = l };
        }
        p.cursor = _start;
        const raw = p.parseRaw(&[_]TT{ TT.LeftBraceToken, TT.EOF }, null);
        return Prelude{ .raw = raw };
    }

    fn parseSelectorList(p: *Parser) ?SelectorList {
        var selector_list = SelectorList.init(p._a);
        const _start = p.start;

        var curr = p.current();

        while (curr.tok_type != TT.LeftBraceToken and !p.end()) {
            const selector = p.parseSelector();
            if (selector) |s| {
                selector_list.children.append(s) catch unreachable;

                // handle ',' here
                curr = p.current();
                const curr_tt = curr.tok_type;
                if (curr_tt == TT.CommaToken) {
                    p.advance();
                    continue;
                } else if (p.end()) {
                    break;
                }
            } else {
                return null;
            }
        }

        p.addLocation(&selector_list.loc, _start);
        return selector_list;
    }

    fn parseSelector(p: *Parser) ?Selector {
        var selector = Selector.init(p._a);
        const _start = p.start;

        var curr = p.current();

        while (!p.end() and curr.tok_type != TT.LeftBraceToken) {
            if (curr.tok_type == TT.CommaToken) {
                break;
            }
            const ss = p.parseSingleSelector();
            if (ss) |_s| {
                selector.children.append(_s) catch unreachable;
                curr = p.current();
            } else {
                return null;
            }
        }

        p.addLocation(&selector.loc, _start);
        return selector;
    }

    fn parseSingleSelector(p: *Parser) ?SingleSelector {
        var curr = p.current();

        switch (curr.tok_type) {
            TT.IdentToken, TT.StarToken => {
                const ts = p.parseTypeSelector();
                if (ts) |_ts| {
                    return SingleSelector{ .type_selector = _ts };
                }
                return null;
            },
            TT.DelimToken => {
                const ch = p.getCurrentChar();
                if (ch == '|') {
                    const ts = p.parseTypeSelector();
                    if (ts) |_ts| {
                        return SingleSelector{ .type_selector = _ts };
                    }
                    return null;
                } else if (ch == '.') {
                    const next_token = p.nextTok();
                    if (next_token.tok_type == TT.IdentToken) {
                        p.advance();
                        return SingleSelector{ .class = [_]usize{ p.cursor - 2, p.cursor - 1 } };
                    } else {
                        p.addError(ParserErrorType{ .ExpectedTokenFoundThis = .{ .expected_token_type = TT.IdentToken, .found_token_pos = p.cursor } });
                        return null;
                    }
                } else {
                    var comb = Combinator{};
                    const _start = p.start;

                    const res = p.parseCombinator(&comb);
                    if (res == null) return null;

                    p.addLocation(&comb.loc, _start);
                    return SingleSelector{ .combinator = comb };
                }
            },
            TT.HashToken => {
                p.advance();
                return SingleSelector{ .id = p.cursor };
            },
            TT.LeftBracketToken => {
                curr = p.nextTok();
                const as = p.parseAttributeSelector();
                if (as) |_as| {
                    return SingleSelector{ .attribute_selector = _as };
                }
                return null;
            },
            TT.ColonToken => {
                if (p.lookAhead().tok_type == TT.ColonToken) {
                    const pseudo_element = p.parsePseudoElementSelector();
                    if (pseudo_element) |_pe| return SingleSelector{ .pseudo_element = _pe };
                    return null;
                }
                const psuedo_class = p.parsePseudoClassSelector();
                if (psuedo_class) |_pcs| {
                    return SingleSelector{ .psuedo_class = _pcs };
                }
                return null;
            },
            TT.WhitespaceToken, TT.CommentToken => {
                const _start = p.start;
                var comb = Combinator{};

                comb.ws1 = p.consumeWhiteSpaceComment();

                const res = p.parseCombinator(&comb);
                if (res == null) return null;

                p.addLocation(&comb.loc, _start);
                return SingleSelector{ .combinator = comb };
            },
			TT.PercentageToken => {
				const dim = p.cursor;
				p.advance();
				return SingleSelector{ .percent = dim };
			},
            else => {
                p.addError(ParserErrorType{
                    .ExpectedTokenFoundThis = .{
                        .expected_token_type = TT.IdentToken,
                        .found_token_pos = p.cursor,
                    },
                });
                return null;
            },
        }

        return null;
    }

    fn parseCombinator(p: *Parser, comb: *Combinator) ?*Combinator {
        const t = p.current();
        if (t.tok_type == TT.ColumnToken) {
            comb.tok = p.cursor;
            p.advance();
        } else if (t.tok_type == TT.DelimToken) {
            const ch = p.getCurrentChar();
            if (ch == '>' or ch == '+' or ch == '~') {
                comb.tok = p.cursor;
                p.advance();
            } else if (ch == '/') {
                p.advance();
                if (p.current().tok_type == TT.IdentToken and std.mem.eql(u8, p.getCurrTokStr(), "deep")) {
                    p.advance();
                    if (p.current().tok_type == TT.DelimToken and p.getCurrentChar() == '/') {
                        comb.tok = p.cursor - 1; // deep
                    } else {
                        p.addError(ParserErrorType.comb_op_expected);
                        return null;
                    }
                } else {
                    p.addError(ParserErrorType.comb_op_expected);
                    return null;
                }
            } else {
                p.addError(ParserErrorType.comb_op_expected);
                return null;
            }
        }

        comb.ws2 = p.consumeWhiteSpaceComment();

        return comb;
    }

    fn parseTypeSelector(p: *Parser) ?TypeSelector {
        var type_selector = TypeSelector.init();

        var curr = p.current();
        const _start = p.start;

        switch (curr.tok_type) {
            TT.StarToken => {
                type_selector.branch1 = TypeSelectorBranch{ .star = p.cursor };
                curr = p.nextTok();
            },
            TT.IdentToken => {
                type_selector.branch1 = TypeSelectorBranch{ .ident = p.cursor };
                curr = p.nextTok();
            },
            TT.DelimToken => {
                type_selector.branch1 = null;
                type_selector.vertical_line = p.cursor;
                curr = p.nextTok();
            },
            else => {
                return null;
            },
        }

        if (curr.tok_type == TT.DelimToken) {
            if (p.getCurrentChar() == '|') {
                type_selector.vertical_line = p.cursor;
                curr = p.nextTok();
            } else {
                return null;
            }
        }

        switch (curr.tok_type) {
            TT.StarToken => {
                type_selector.branch2 = TypeSelectorBranch{ .star = p.cursor };
                curr = p.nextTok();
            },
            TT.IdentToken => {
                type_selector.branch2 = TypeSelectorBranch{ .ident = p.cursor };
                curr = p.nextTok();
            },
            else => {},
        }

        p.addLocation(&type_selector.loc, _start);
        return type_selector;
    }

    fn parseAttributeSelector(p: *Parser) ?AttributeSelector {
        var attr = AttributeSelector.init();
        const _start = p.start;

        var curr = p.current();

        attr.ws1 = p.consumeWhiteSpaceComment();

        curr = p.current();

        // TODO: parse attribute name
        if (curr.tok_type == TT.IdentToken) {
            attr.name_index = p.cursor;
            p.advance();
        } else {
            return null;
        }

        attr.ws2 = p.consumeWhiteSpaceComment();

        curr = p.current();

        if (curr.tok_type == TT.RightBracketToken) {
            attr.matcher = null;
            attr.value = null;
            return attr;
        }

        if (curr.tok_type != TT.IdentToken) {
            // parse matcher
            switch (curr.tok_type) {
                TT.EqualToken,
                TT.IncludeMatchToken,
                TT.DashMatchToken,
                TT.PrefixMatchToken,
                TT.SubstringMatchToken,
                TT.SuffixMatchToken,
                => {
                    p.advance();
                    attr.matcher = p.cursor - 1;
                },
                else => {
                    p.addError(ParserErrorType.AttributeSelectorIsExpected);
                    return null;
                },
            }

            // parse whitespace
            attr.ws3 = p.consumeWhiteSpaceComment();
            curr = p.current();

            // parse value
            if (curr.tok_type == TT.IdentToken) {
                attr.value = .{ .identifier = p.cursor };
            } else if (curr.tok_type == TT.StringToken) {
                attr.value = .{ .string_node = p.cursor };
            } else {
                return null;
            }
            p.advance();

            attr.ws4 = p.consumeWhiteSpaceComment();
            curr = p.current();

            // attribute flags
            if (curr.tok_type == TT.IdentToken) {
                attr.flags = p.cursor;
                p.advance();

                attr.ws5 = p.consumeWhiteSpaceComment();
                curr = p.current();
            } else {
                return null;
            }
        } else {
            return null;
        }

        curr = p.current();
        if (curr.tok_type != TT.RightBracketToken) {
            p.addError(
                ParserErrorType{
                    .ExpectedTokenFoundThis = .{
                        .expected_token_type = TT.RightBraceToken,
                        .found_token_pos = p.cursor,
                    },
                },
            );
        }

        p.eat(TT.RightBracketToken) orelse return null;

        p.addLocation(&attr.loc, _start);
        return attr;
    }

    /// Parses PseudoClasses.
    /// TODO: Finish the Nth Child parser
    fn parsePseudoClassSelector(p: *Parser) ?PseudoClassSelector {
        var pseudo_class = PseudoClassSelector.init();
        const _start = p.start;

        p.eat(TT.ColonToken) orelse return null;

        var curr = p.current();
        if (curr.tok_type == TT.FunctionToken) {
            const func = p.parseFunction();
            if (func) |_func| {
                return PseudoClassSelector{ .branches = .{ .func_node = _func } };
            } else return null;
        } else if (curr.tok_type == TT.IdentToken) {
            return PseudoClassSelector{ .branches = .{ .identifier = p.cursor } };
        } else {
            return null;
        }

        p.addLocation(&pseudo_class.loc, _start);
        p.advance();
        return pseudo_class;
    }

    fn parsePseudoElementSelector(p: *Parser) ?PseudoElementSelector {
        var pes = PseudoElementSelector.init();
        const _start = p.start;

        p.eat(TT.ColonToken) orelse return null;

        if (p.parsePseudoClassSelector()) |_pcs| {
            pes.class_selector = _pcs;
        } else {
            return null;
        }

        p.addLocation(&pes.loc, _start);
        return pes;
    }

    // is_declaration is 'false' if its a atrule block
    fn parseBlock(p: *Parser, is_declaration: bool, is_nested: bool) Block {
        var block = Block.init(p._a);
        const _start = p.start;
        _ = p.eat(TT.LeftBraceToken);

        if (is_declaration) {
            // -- Consume Declaration
            while (!p.end()) {
                switch (p.current().tok_type) {
                    TT.RightBraceToken => break,
                    WT, TT.CommentToken => {
                        var ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                        if (ws_comments) |w| block.children.append(w) catch unreachable;
                    },
                    TT.AtKeywordToken => {
                        var at_rule = p.parseAtRule();
                        block.children.append(CSSNode{ .at_rule = at_rule }) catch unreachable;
                    },
                    else => {
                        const checkpoint = p.cursor;
                        var declaration = p.parseDeclaration(true);
                        if (declaration) |d| {
                            block.children.append(CSSNode{ .declaration = d }) catch unreachable;
                            if (p.consumeWhiteSpaceComment()) |ws| block.children.append(CSSNode{ .whitespaces = ws }) catch unreachable;
                        } else {
                            p.cursor = checkpoint;
                            if (is_nested) {
                                if (p.parseRule()) |rule| {
                                    block.children.append(CSSNode{ .rule = rule }) catch unreachable;
                                } else {
                                    p.cursor = _start;
                                    const raw = p.parseRaw(&[_]TT{ TT.EOF, TT.RightBraceToken, TT.SemicolonToken }, null);
                                    block.children.append(CSSNode{ .raw = raw }) catch unreachable;
                                }
                            } else {
                                p.cursor = _start;
                                const raw = p.parseRaw(&[_]TT{ TT.EOF, TT.RightBraceToken, TT.SemicolonToken }, null);
                                block.children.append(CSSNode{ .raw = raw }) catch unreachable;
                            }
                        }
                    },
                }
            }

            const ws = p.consumeWhiteSpaceComment();
            if (ws) |w| block.children.append(CSSNode{ .whitespaces = w }) catch unreachable;

            if (!p.end()) {
                _ = p.eat(TT.RightBraceToken);
            }
        } else {
            // consume rule
            while (!p.end()) {
                switch (p.current().tok_type) {
                    TT.RightBraceToken => break,
                    WT, TT.CommentToken => {
                        var ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                        if (ws_comments) |w| block.children.append(w) catch unreachable;
                    },
                    TT.AtKeywordToken => {
                        const at_rule = p.parseAtRule();
                        block.children.append(CSSNode{ .at_rule = at_rule }) catch unreachable;
                    },
                    else => {
                        const rule = p.parseRule().?;
                        block.children.append(CSSNode{ .rule = rule }) catch unreachable;
                    },
                }
            }

            if (!p.end()) {
                _ = p.eat(TT.RightBraceToken);
            }
        }

        p.addLocation(&block.loc, _start);
        return block;
    }

    fn parseDeclaration(p: *Parser, is_in_block: bool) ?Declaration {
        if (p.current().tok_type == TT.SemicolonToken) {
            return null;
        }

        var declaration = Declaration.init(p._a);
        const _start = p.start;

        declaration.ws0 = p.consumeWhiteSpaceComment();

        var property: ?DeclProperty = p.parseDeclProperty();
        if (property) |_p| declaration.property = _p else return null;

        // consume whitespace before color
        declaration.ws1 = p.consumeWhiteSpaceComment();

        p.eat(TT.ColonToken) orelse return null;

        declaration.ws2 = p.consumeWhiteSpaceComment();

        const value = p.parseDeclValue(property.?.is_custom_property, is_in_block);

        if (value) |_val| {
            declaration.value = _val;
            declaration.important = _val.important;
        } else return null;

        declaration.ws3 = p.consumeWhiteSpaceComment();

        if (is_in_block) {
            const tt = p.current().tok_type;
            if (tt == TT.SemicolonToken) {
                p.advance();
            } else if (tt == TT.RightBraceToken) {
				// remove the whitespace
				if (declaration.ws3) |ws3| {
					const ws3_len = ws3.items.len;
					p.cursor -= ws3_len;
					ws3.deinit();
					declaration.ws3 = null;
				}
			}
        } else {
            if (p.current().tok_type != TT.RightParenthesisToken) return null;
        }

        p.addLocation(&declaration.loc, _start);
        return declaration;
    }

    fn parseDeclValue(p: *Parser, is_custom_property: bool, is_in_block: bool) ?DeclValue {
        var value = DeclValue.init(p._a);
        const _start = p.start;

        if (!is_custom_property) {
            // const arr = p.consumeWhiteSpaceComment();
        }

        const to_parse_value = if (is_custom_property) p.options.parse_custom_property else p.options.parse_value;

        if (to_parse_value) {
            var seq: ?ArrayList(CSSNode) = null;
            if (is_in_block) {
                seq = p.readSequence(&[_]TT{ TT.SemicolonToken, TT.RightBraceToken, TT.EOF }, null);
            } else {
                seq = p.readSequence(&[_]TT{TT.RightParenthesisToken}, null);
            }

            if (seq) |s| {
                var _seq = s;
                // if there is whitespace at the end just remove them
                if (_seq.items.len > 0) {
                    const last = _seq.items[_seq.items.len - 1];
                    switch (last) {
                        .whitespaces => {
                            const popped = _seq.pop();
                            p.cursor -= popped.whitespaces.items.len;
                        },
                        else => {},
                    }
                }
                value.children = _seq;
            } else {
                p.cursor = _start;
                const raw = p.parseRaw(&[_]TT{ TT.SemicolonToken, TT.EOF, TT.DelimToken }, '!');
                value.children.append(CSSNode{ .raw = raw }) catch unreachable;
            }
        } else {
            // const raw = p.parseRaw([_]TT{ TT.SemicolonToken, TT.DelimToken }, '!');
        }

        // TODO: Do this later
        if (is_custom_property and value.children.items.len == 0) {}

        if (p.current().tok_type == TT.DelimToken and p.isDelim(p.cursor, '!')) {
            p.advance();
            _ = p.consumeWhiteSpaceComment();
            const important = p.nextTok();
            if (important.tok_type == TT.IdentToken and std.mem.eql(u8, p.getTokenString(important), "important")) {
                value.important = true;
                p.advance();
            }
        }

        p.addLocation(&value.loc, _start);
        return value;
    }

    fn readSequence(p: *Parser, until: []const TT, delim_char: ?u8) ?ArrayList(CSSNode) {
        var arr = ArrayList(CSSNode).init(p._a);

        var tok = p.current();

        while (!p.end() and !p.matchTokenType(until, tok.tok_type, delim_char)) {
            switch (tok.tok_type) {
                TT.URLToken => {
                    arr.append(CSSNode{ .url = p.cursor }) catch unreachable;
                    tok = p.nextTok();
                },
                TT.CommentToken, TT.WhitespaceToken => {
                    if (!p.options.skip_ws_comments) {
                        if (p.consumeWhiteSpaceComment()) |w| {
                            arr.append(CSSNode{ .whitespaces = w }) catch unreachable;
                            tok = p.current();
                        }
                    }
                },
                TT.FunctionToken => {
                    const function_node = p.parseFunction();
                    if (function_node) |f_n| {
                        arr.append(CSSNode{ .function_node = f_n }) catch unreachable;
                        tok = p.current();
                    } else {
                        p.addError(ParserErrorType.FunctionNodeUnclosedBracket);
                        return null;
                    }
                },
                TT.IdentToken => {
                    const identifier_node = p.cursor;
                    arr.append(CSSNode{ .identifier = identifier_node }) catch unreachable;
                    tok = p.nextTok();
                },
                TT.NumberToken => {
                    const number_node = p.cursor;
                    arr.append(CSSNode{ .misc_token = number_node }) catch unreachable;
                    tok = p.nextTok();
                },
                TT.CommaToken => {
                    const comma_node = p.cursor;
                    arr.append(CSSNode{ .misc_token = comma_node }) catch unreachable;
                    tok = p.nextTok();
                },
                TT.LeftParenthesisToken => {
                    const seq = p.readSequence(&[_]TT{}, null);
                    if (seq) |_seq| {
                        var value: DeclValue = DeclValue.init(p._a);
                        value.children = _seq;
                        arr.append(CSSNode{ .parentheses = value }) catch unreachable;
                        tok = p.nextTok();
                    } else {
                        return null;
                    }
                },
                TT.DimensionToken,
                TT.StringToken,
                TT.DelimToken,
                TT.ColonToken,
                => {
                    const dim_node = p.cursor;
                    arr.append(CSSNode{ .misc_token = dim_node }) catch unreachable;
                    tok = p.nextTok();
                },
                else => {
                    return null;
                },
            }
        }

        return arr;
    }

    fn parseParentheses(p: *Parser) Parentheses {
        var parentheses: Parentheses = Parentheses.init(p._a);
        const _start = p.start;

        // '('
        parentheses.children.append(CSSNode{ .misc_token = p.cursor }) catch unreachable;

        // advance to the next token after `(`
        p.advance();

        parentheses.children = p.readSequence(&[_]TT{ TT.RightParenthesisToken, TT.EOF }, null);

        // ')'
        parentheses.children.append(CSSNode{ .misc_token = p.cursor }) catch unreachable;

        p.addLocation(&parentheses.loc, _start);
        return parentheses;
    }

    fn parseFunction(p: *Parser) ?FunctionNode {
        var func = FunctionNode.init(p._a);
        const _start = p.cursor;
        const name = p.current();

        func.name = p.cursor;
        p.advance();

        if (eqlStr(p.getTokenString(name), "var(")) {
            const css_var: CSSVariable = p.parseCSSVariable() orelse return null;
            if (p.current().tok_type == TT.RightParenthesisToken) {
                func.children.append(CSSNode{ .variable = css_var }) catch unreachable;
                p.advance();

                p.addLocation(&func.loc, _start);
                return func;
            }
        }

        var seq = p.readSequence(&[_]TT{ TT.RightParenthesisToken, TT.EOF }, null);

        if (seq) |_seq| {
            var __s = _seq;
            func.children.appendSlice(__s.toOwnedSlice()) catch unreachable;
        } else {
            return null;
        }

        if (!p.end()) p.eat(TT.RightParenthesisToken) orelse return null;

        p.addLocation(&func.loc, _start);

        return func;
    }

    /// consume_util: Takes a List of TokenType. If the token matches any of them, stop.
    /// delim_token_if_any: If its a delimToken, pass in the delim character
    fn parseRaw(p: *Parser, consume_until: []const TT, delim_token_if_any: ?u8) Raw {
        var raw = Raw.init(p._a);
        const _start = p.start;

        var t = p.current();
        while (!p.end() and !p.matchTokenType(consume_until, t.tok_type, delim_token_if_any)) : (t = p.nextTok()) {
            raw.children.append(p.cursor) catch unreachable;
        }

        p.addLocation(&raw.loc, _start);
        return raw;
    }

    fn matchTokenType(p: *Parser, all_possible_token_types: []const TT, token_type_to_match: TT, delim_token_if_any: ?u8) bool {
        for (all_possible_token_types) |token_type| {
            if (token_type == token_type_to_match) {
                if (token_type_to_match == TT.DelimToken) {
                    if (delim_token_if_any) |delim_tok| {
                        if (p.getTokenString(p.current())[0] == delim_tok) {
                            return true;
                        }
                    }
                    p.printCurrTok();
                    return false;
                }
                return true;
            }
        }
        return false;
    }

    fn isDelim(p: *Parser, token_index: usize, c: u8) bool {
        if (p.tokens.items[token_index].tok_type == .DelimToken) {
            const cc = p.code[p.tokens.items[token_index].start];
            if (cc == c) return true;
        }
        return false;
    }

    fn consumeWhiteSpaceCommentCSSNode(p: *Parser) ?CSSNode {
        const ws = p.consumeWhiteSpaceComment() orelse return null;
        return CSSNode{ .whitespaces = ws };
    }

    // Comsume WhiteSpace and Comment Until the
    fn consumeWhiteSpaceComment(p: *Parser) ?ArrayList(usize) {
        if (p.end()) return null;
        var arr = ArrayList(usize).init(p._a);
        var t = p.current();
        if (p.options.skip_ws_comments) {
            while ((t.tok_type == TT.WhitespaceToken or t.tok_type == TT.CommentToken) and !p.end()) : (t = p.nextTok()) {}
        } else {
            if (!p.end()) {
                t = p.current();
                while ((t.tok_type == TT.WhitespaceToken or t.tok_type == TT.CommentToken) and !p.end()) : (t = p.nextTok()) {
                    arr.append(p.cursor) catch unreachable;
                }
            }
        }
        if (arr.items.len == 0) {
            arr.deinit();
            return null;
        }
        return arr;
    }

    fn parseDeclProperty(p: *Parser) ?DeclProperty {
        var property = DeclProperty.init(p._a);
        const _start = p.start;

        var tok: Token = p.current();
        const tt: TT = tok.tok_type;

        if (tt == TT.DelimToken) {
            switch (p.getCurrentChar()) {
                '-' => {
                    tok = p.nextTok();
                    if (tok.tok_type == TT.DelimToken and p.getCurrentChar() == '-') {
                        property.is_custom_property = true;
                        property.children.append(p.cursor - 1) catch unreachable;
                        property.children.append(p.cursor) catch unreachable;
                        p.advance();
                    }
                },
                '*', '$', '+', 'k', '#', '&' => {
                    property.children.append(p.cursor) catch unreachable;
                    p.advance();
                },
                // TODO: Handle SASS comment here
                '/' => {
                    tok = p.nextTok();
                    if (tok.tok_type == TT.DelimToken) {
                        const c = p.getTokenString(tok);
                        if (c[0] == '/') {
                            property.children.append(p.cursor - 1) catch unreachable;
                            property.children.append(p.cursor) catch unreachable;
                            p.advance();
                        }
                    }
                },
                else => {
                    p.advance();
                },
            }
        }

        if (tok.tok_type == TT.HashToken) {
            p.eat(TT.HashToken) orelse return null;
        } else if (tok.tok_type == TT.IdentToken) {
            property.children.append(p.cursor) catch unreachable;
            p.advance();
        } else if (tok.tok_type == TT.CustomPropertyNameToken) {
            property.is_custom_property = true;
            property.children.append(p.cursor) catch unreachable;
            p.advance();
        } else {
            return null;
        }

        p.addLocation(&property.loc, _start);
        return property;
    }

    fn printLoc(p: *Parser, loc: Location) void {
        std.debug.print("print-loc: __{s}__\n", .{p.code[loc.start..loc.end]});
    }

    fn printCurrTok(p: *Parser) void {
        p.printToken(p.current());
    }

    fn printToken(p: *Parser, tok: Token) void {
        std.debug.print("print-token: type={}, value=__{s}__\n", .{ tok.tok_type, p.code[tok.start..tok.end] });
    }

    fn printTokenFromIndex(p: *Parser, i: usize) void {
        p.printToken(p.tokens.items[i]);
    }

    /// Allocate value to the heap and return the pointer
    fn heaped(p: *Parser, comptime T: type, value: T) *T {
        var r: *T = p._a.create(T) catch unreachable;
        r.* = value;
        return r;
    }
};

test "Parser" {
    _ = @import("parser_test.zig");
}
