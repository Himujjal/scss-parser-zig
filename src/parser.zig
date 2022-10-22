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
const DeclValue = nodes.Value;
const Location = nodes.CSSLocation;
const FunctionNode = nodes.FunctionNode;
const Parentheses = nodes.Value;
const MediaQueryList = nodes.MediaQueryList;
const MediaQuery = nodes.MediaQuery;
const MediaCondition = nodes.MediaCondition;
const CSSVariable = nodes.CSSVariable;
const TypeSelectorBranch = nodes.TypeSelectorBranch;
const Combinator = nodes.Combinator;
const AttributeSelector = nodes.AttributeSelector;
const Value = nodes.Value;
const PseudoClassSelector = nodes.PseudoClassSelector;
const PseudoElementSelector = nodes.PseudoElementSelector;
const URL = nodes.URL;
// =======================================================================================

pub const StyleLanguage = enum { CSS, SCSS, POSTCSS };
pub const ParserOptions = struct {
    is_inline: bool = false,
    parse_rule_prelude: bool = true,
    /// TODO: Implement this 
    parse_selectors: bool = false,
    parse_media_prelude: bool = false,
    parse_custom_property: bool = true,
    parse_value: bool = true,
    skip_ws_comments: bool = true,
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
        // TODO: Remove this after media query parser is complete
        options.parse_media_prelude = false;

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
            .radix_tree = initAtKeywordRadixTree(),

            .parser_arena = parser_arena,
            ._a = _a,
        };
    }

    pub fn parse(p: *Parser, code: []const u8, is_inline: bool) *Self {
        _ = p.scanner_instance.scan(code);

        p.code = code;
        p.options.is_inline = is_inline;

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
        p.cursor += 1;
        return p.tokens.items[p.cursor];
    }

    fn eat(p: *Parser, tt: TT) void {
        if (p.current().tok_type != tt) {
            p.addExpectedTokenFoundThisError(tt);
        }
        p.advance();
    }

    fn end(p: *Parser) bool {
        return (p.tokens.items.len - 1) <= p.cursor;
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
        if (p.tokens.items.len <= p.cursor + 1) {
            return p.tokens.items[p.cursor + 1];
        }
        return p.tokens.items[p.tokens.items.len - 1];
    }

    fn lookSuperAhead(p: *Parser) Token {
        if (p.tokens.items.len <= p.cursor + 2) {
            return p.tokens.items[p.cursor + 2];
        }
        return p.tokens.items[p.tokens.items.len - 1];
    }

    fn lookSuperDuperAhead(p: *Parser) Token {
        if (p.tokens.items.len <= p.cursor + 3) {
            return p.tokens.items[p.cursor + 3];
        }
        return p.tokens.items[p.tokens.items.len - 1];
    }

    fn appendWSCommentsCSSNode(p: *Parser, t: anytype) void {
        var ws_comments = p.consumeWhiteSpaceCommentCSSNode();
        t.children.appendSlice(ws_comments.toOwnedSlice()) catch unreachable;
    }

    fn appendWSComments(p: *Parser, t: anytype) void {
        var ws_comments = p.consumeWhiteSpaceComment();
        t.children.appendSlice(ws_comments.toOwnedSlice()) catch unreachable;
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
                .WhitespaceToken => {
                    if (!p.options.skip_ws_comments) {
                        node = CSSNode{ .whitespace = p.cursor };
                    }
                    tok = p.nextTok();
                },
                .CommentToken => {
                    if (!p.options.skip_ws_comments) {
                        node = CSSNode{ .comment = p.cursor };
                    }
                    tok = p.nextTok();
                },
                .ErrorToken => {
                    p.addError(ParserErrorType.ErrorTokenFound);
                    node = CSSNode{ ._error = p.start };
                    tok = p.nextTok();
                },
                .AtKeywordToken => {
                    node = CSSNode{ .at_rule = p.parseAtRule() };
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

		std.debug.print("{s}\n", .{prelude.?.selector_list.loc});

        if (p.end()) {
            p.cursor = _start;
            return null;
        }

        const block = p.parseBlock(true);

        var rule: Rule = Rule.init(p._a);
        rule.prelude = prelude.?;
        rule.block = block;

        p.addLocation(&rule.loc, _start);
        return rule;
    }

    /// The main At rule parser
    fn parseAtRule(p: *Parser) Atrule {
        var at_rule = Atrule.init(p._a);
        const _start = p.start;

        // get name. remove '@'
        var name = p.current();
        name.start = name.start + 1;
        name.start_col = name.start_col + 1;

        var tok = p.current();

        if (!p.end() and tok.tok_type != TT.LeftBraceToken and tok.tok_type != TT.SemicolonToken) {
            if (p.options.parse_rule_prelude) {
                const at_rule_prelude: AtRulePrelude = p.parseAtRulePrelude();
                at_rule.rule_type = at_rule_prelude.rule_type;
                at_rule.prelude = .{ .at_rule_prelude = at_rule_prelude };
            } else {
                const raw_tokens: []const TT = &[_]TT{ TT.LeftBraceToken, TT.SemicolonToken };
                at_rule.prelude = .{ .raw = p.parseRaw(raw_tokens, null) };
            }

            // whitespace and comments
            switch (at_rule.prelude) {
                .at_rule_prelude => {
                    p.appendWSCommentsCSSNode(&at_rule.prelude.at_rule_prelude);
                    // var ws_comments_2 = p.consumeWhiteSpaceCommentCSSNode();
                    // at_rule.prelude.at_rule_prelude.children.appendSlice(ws_comments_2.toOwnedSlice()) catch unreachable;
                },
                .raw => {
                    p.appendWSComments(&at_rule.prelude.raw);
                    // var ws_comments_2: ArrayList(usize) = p.consumeWhiteSpaceComment();
                    // at_rule.prelude.raw.children.appendSlice(ws_comments_2.toOwnedSlice()) catch unreachable;
                },
            }
        }
        p.advance();

        switch (p.current().tok_type) {
            // @charset, @import, @namespace
            TT.SemicolonToken => p.advance(),
            TT.LeftBraceToken => {
                // TODO: Parse this at-rule
                const block = p.parseBlock(false);
                at_rule.block = block;
            },
            else => {
                // Handle fallback here
            },
        }

        p.addLocation(&at_rule.loc, _start);
        return at_rule;
    }

    fn parseAtRulePrelude(p: *Parser) AtRulePrelude {
        var at_rule_prelude = AtRulePrelude.init(p._a);
        const _start = p.cursor;

        var tok = p.current();

        const rule_type = p.radix_tree.get(p.getTokenString(tok)) orelse AtKeywordTypes.Custom;
        at_rule_prelude.rule_type = rule_type;

        at_rule_prelude.children.append(CSSNode{ .at_keyword_token = p.cursor }) catch unreachable;
        p.advance();

        switch (rule_type) {
            AtKeywordTypes.Media => {
                // whitespaces and comments
                var ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                at_rule_prelude.children.appendSlice(ws_comments.toOwnedSlice()) catch unreachable;

                if (p.options.parse_media_prelude) {
                    const media_query_list = p.parseMediaQueryList();
                    at_rule_prelude.children.append(
                        CSSNode{ .media_query_list = media_query_list },
                    ) catch unreachable;
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
                at_rule_prelude.children.appendSlice(ws_comments.toOwnedSlice()) catch unreachable;

                // css_var = p.parseCSSVariable() catch CSSNode{ .raw = p.parseRaw([]TT{ TT.LeftBraceToken, TT.SemicolonToken }) };

                ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                at_rule_prelude.children.appendSlice(ws_comments.toOwnedSlice()) catch unreachable;
            },
            AtKeywordTypes.CounterStyle => {
                tok = p.current();
                var ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                at_rule_prelude.children.appendSlice(ws_comments.toOwnedSlice()) catch unreachable;

                if (p.current().tok_type == TT.IdentToken) {
                    at_rule_prelude.children.append(CSSNode{ .identifier = p.cursor }) catch unreachable;
                }

                var ws_comments2 = p.consumeWhiteSpaceCommentCSSNode();
                at_rule_prelude.children.appendSlice(ws_comments2.toOwnedSlice()) catch unreachable;
            },
            AtKeywordTypes.FontFace => {
                var ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                at_rule_prelude.children.appendSlice(ws_comments.toOwnedSlice()) catch unreachable;
            },
            AtKeywordTypes.FontFeatureValue => {
                const raw = p.parseRaw(&[_]TT{ TT.SemicolonToken, TT.LeftBraceToken }, null);
                at_rule_prelude.children.append(CSSNode{ .raw = raw }) catch unreachable;
            },
            AtKeywordTypes.Import => {
                var ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                at_rule_prelude.children.appendSlice(ws_comments.toOwnedSlice()) catch unreachable;

                if (p.current().tok_type == TT.URLToken) {
                    const url = p.parseUrl();
                    at_rule_prelude.children.append(CSSNode{ .url = url }) catch unreachable;
                    p.advance();
                } else {
                    const raw = p.parseRaw(&[_]TT{TT.SemicolonToken}, null);
                    at_rule_prelude.children.append(CSSNode{ .raw = raw }) catch unreachable;
                }
            },
            AtKeywordTypes.Keyframes => {
                var ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                at_rule_prelude.children.appendSlice(ws_comments.toOwnedSlice()) catch unreachable;

                tok = p.current();
                if (tok.tok_type == TT.IdentToken) {
                    at_rule_prelude.children.append(CSSNode{ .identifier = p.cursor }) catch unreachable;
                } else if (tok.tok_type == TT.StringToken) {
                    at_rule_prelude.children.append(CSSNode{ .string_node = p.cursor }) catch unreachable;
                }
                var ws_comments2 = p.consumeWhiteSpaceCommentCSSNode();
                at_rule_prelude.children.appendSlice(ws_comments2.toOwnedSlice()) catch unreachable;
            },
            AtKeywordTypes.Layer => {
                var ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                at_rule_prelude.children.appendSlice(ws_comments.toOwnedSlice()) catch unreachable;

                tok = p.current();
                while (tok.tok_type != TT.LeftBraceToken or tok.tok_type != TT.SemicolonToken) {
                    ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                    at_rule_prelude.children.appendSlice(ws_comments.toOwnedSlice()) catch unreachable;
                    if (tok.tok_type == TT.IdentToken) {
                        at_rule_prelude.children.append(CSSNode{ .identifier = p.cursor }) catch unreachable;
                        tok = p.nextTok();
                        var ws_comments_2 = p.consumeWhiteSpaceCommentCSSNode();
                        at_rule_prelude.children.appendSlice(ws_comments_2.toOwnedSlice()) catch unreachable;

                        tok = p.current();

                        if (tok.tok_type != TT.DelimToken or p.getCurrentChar() != ',') continue;

                        var ws_comments_3 = p.consumeWhiteSpaceCommentCSSNode();
                        at_rule_prelude.children.appendSlice(ws_comments_3.toOwnedSlice()) catch unreachable;

                        tok = p.current();

                        if (tok.tok_type == TT.IdentToken) continue;
                    }
                    const raw = p.parseRaw(&[_]TT{ TT.SemicolonToken, TT.LeftBraceToken }, null);
                    at_rule_prelude.children.append(CSSNode{ .raw = raw }) catch unreachable;
                }

                p.appendWSCommentsCSSNode(&at_rule_prelude);
                // ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                // at_rule_prelude.children.appendslice(ws_comments.toOwnedSlice()) catch unreachable;
            },
            AtKeywordTypes.Namespace => {
                // TODO: Namespace
            },
            AtKeywordTypes.Page => {
                // TODO: Pages
            },
            else => {
                p.parseAtRulePreludeRaw(&at_rule_prelude.children, &[_]TT{ TT.LeftBraceToken, TT.SemicolonToken });
            },
        }

        p.addLocation(&at_rule_prelude.loc, _start);
        return at_rule_prelude;
    }

    fn parseUrl(p: *Parser) URL {
        var url = URL.init();
        const _start = p.start;
        p.addLocation(&url.loc, _start);
        return url;
    }

    fn parseCSSVariable(p: *Parser) UnMatchErr!CSSVariable {
        const _start = p.start;
        var tok = p.current();
        if (tok.tok_type == .DelimToken and p.getCurrentChar() == '-' and
            p.lookAhead().tok_type == .DelimToken and p.getNextChar() == '-' and
            p.lookSuperAhead().tok_type == .IdentToken)
        {
            p.cursor += 3;
            var loc = Location{};
            p.addLocation(&loc, _start);
            return CSSVariable{ .loc = loc, .ident = p.cursor - 1 };
        }
        return UnMatchErr.UnMatch;
    }

    fn parseAtRulePreludeRaw(p: *Parser, children: *ArrayList(CSSNode), tokens: []const TT) void {
        var raw: Raw = p.parseRaw(tokens, null);
        children.append(CSSNode{ .raw = raw }) catch unreachable;
    }

    fn parseMediaQuery(p: *Parser) MediaQuery {
        var media_query = MediaQuery.init();
        const _start = p.start;

        p.addLocation(&media_query.loc, _start);
        return media_query;
    }

    fn parseMediaQueryList(p: *Parser) MediaQueryList {
        var media_query_list = MediaQueryList.init(p._a);
        const _start = p.start;

        while (!p.end() and p.current().tok_type != TT.RightBraceToken) {
            var media_query = p.parseMediaQuery();
            media_query_list.children.append(media_query) catch unreachable;
        }

        p.addLocation(&media_query_list.loc, _start);
        return media_query_list;
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
                return SingleSelector{ .id = p.cursor - 1 };
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

                const ws1 = p.consumeWhiteSpaceComment();
                if (ws1.items.len != 0) comb.ws1 = ws1;

                const res = p.parseCombinator(&comb);
                if (res == null) return null;

                p.addLocation(&comb.loc, _start);
                return SingleSelector{ .combinator = comb };
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

        const ws2 = p.consumeWhiteSpaceComment();
        if (ws2.items.len != 0) comb.ws2 = ws2;

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

        const ws1 = p.consumeWhiteSpaceComment();
        if (ws1.items.len != 0) attr.ws1 = ws1;

        curr = p.current();

        // TODO: parse attribute name
        if (curr.tok_type == TT.IdentToken) {
            attr.name_index = p.cursor;
            p.advance();
        } else {
            return null;
        }

        const ws2 = p.consumeWhiteSpaceComment();
        if (ws2.items.len != 0) attr.ws2 = ws2;

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
            const ws3 = p.consumeWhiteSpaceComment();
            if (ws3.items.len != 0) attr.ws3 = ws3;
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

            const ws4 = p.consumeWhiteSpaceComment();
            if (ws4.items.len != 0) attr.ws4 = ws4;
            curr = p.current();

            // attribute flags
            if (curr.tok_type == TT.IdentToken) {
                attr.flags = p.cursor;
                p.advance();

                const ws5 = p.consumeWhiteSpaceComment();
                if (ws5.items.len != 0) attr.ws5 = ws5;
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

        p.eat(TT.RightBracketToken);

        p.addLocation(&attr.loc, _start);
        return attr;
    }

    /// Parses PseudoClasses.
    /// TODO: Finish the Nth Child parser
    fn parsePseudoClassSelector(p: *Parser) ?PseudoClassSelector {
        var pseudo_class = PseudoClassSelector.init();
        const _start = p.start;

        p.eat(TT.ColonToken);

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

        p.eat(TT.ColonToken);

        if (p.parsePseudoClassSelector()) |_pcs| {
            pes.class_selector = _pcs;
        } else {
            return null;
        }

        p.addLocation(&pes.loc, _start);
        return pes;
    }

    // is_declaration is 'false' if its a atrule block
    fn parseBlock(p: *Parser, is_declaration: bool) Block {
        var block = Block.init(p._a);
        const _start = p.start;
        p.eat(TT.LeftBraceToken);

        if (is_declaration) {
            // -- Consume Declaration
            while (!p.end()) {
                switch (p.current().tok_type) {
                    TT.RightBraceToken => break,
                    WT, TT.CommentToken => {
                        var ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                        block.children.appendSlice(ws_comments.toOwnedSlice()) catch unreachable;
                    },
                    TT.AtKeywordToken => {
                        var at_rule = p.parseAtRule();
                        block.children.append(CSSNode{ .at_rule = at_rule }) catch unreachable;
                    },
                    else => {
                        var declaration = p.parseDeclaration();
                        if (declaration) |d| {
                            block.children.append(CSSNode{ .declaration = d }) catch unreachable;
                        } else {
                            p.cursor = _start;
                            const raw = p.parseRaw(&[_]TT{ TT.EOF, TT.RightBraceToken, TT.SemicolonToken }, null);
                            block.children.append(CSSNode{ .raw = raw }) catch unreachable;
                        }
                    },
                }
            }

            if (!p.end()) {
                p.eat(TT.RightBraceToken);
            }
        } else {
            // consume rule
            while (!p.end()) {
                switch (p.current().tok_type) {
                    TT.RightBraceToken => break,
                    WT, TT.CommentToken => {
                        var ws_comments = p.consumeWhiteSpaceCommentCSSNode();
                        block.children.appendSlice(ws_comments.toOwnedSlice()) catch unreachable;
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
                p.eat(TT.RightBraceToken);
            }
        }

        p.addLocation(&block.loc, _start);
        return block;
    }

    fn parseDeclaration(p: *Parser) ?Declaration {
        if (p.current().tok_type == TT.SemicolonToken) {
            return null;
        }

        var declaration = Declaration.init(p._a);
        const _start = p.start;

        var property: ?DeclProperty = p.parseDeclProperty();
        if (property) |_p| declaration.property = _p else return null;

        const ws1 = p.consumeWhiteSpaceComment();
        if (ws1.items.len != 0) declaration.ws1 = ws1;

        p.eat(TT.ColonToken);

        const ws2 = p.consumeWhiteSpaceComment();
        if (ws2.items.len != 0) declaration.ws2 = ws2;

        const value = p.parseDeclValue(property.?.is_custom_property);

        if (value) |_val| {
            declaration.value = _val;
            declaration.important = _val.important;
        } else return null;

        const ws3 = p.consumeWhiteSpaceComment();
        if (ws3.items.len != 0) declaration.ws3 = ws3;

        p.eat(TT.SemicolonToken);

        p.addLocation(&declaration.loc, _start);
        return declaration;
    }

    fn parseDeclValue(p: *Parser, is_custom_property: bool) ?DeclValue {
        var value = DeclValue.init(p._a);
        const _start = p.start;

        if (!is_custom_property) {
            // const arr = p.consumeWhiteSpaceComment();
        }

        const to_parse_value = if (is_custom_property) p.options.parse_custom_property else p.options.parse_value;

        if (to_parse_value) {
            const seq = p.readSequence(&[_]TT{TT.SemicolonToken}, null);
            if (seq) |_seq| {
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

        while (!p.end() and !p.consumeUntilUtil(until, tok.tok_type, delim_char)) {
            switch (tok.tok_type) {
                TT.CommentToken, TT.WhitespaceToken => {
                    if (!p.options.skip_ws_comments) {
                        if (tok.tok_type == TT.CommentToken) {
                            arr.append(CSSNode{ .comment = p.cursor }) catch unreachable;
                        } else {
                            arr.append(CSSNode{ .whitespace = p.cursor }) catch unreachable;
                        }
                        tok = p.nextTok();
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
                        var value: Value = Value.init(p._a);
                        value.children = _seq;
                        arr.append(CSSNode{ .parentheses = value }) catch unreachable;
                        tok = p.nextTok();
                    } else {
                        return null;
                    }
                },
                TT.DimensionToken => {
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

        if (std.mem.eql(u8, p.getTokenString(name), "var(")) att: {
            const css_var: CSSVariable = p.parseCSSVariable() catch {
                break :att;
            };
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

        if (!p.end()) p.eat(TT.RightParenthesisToken);

        p.addLocation(&func.loc, _start);

        return func;
    }

    /// Takes a List of TokenType. If the token matches any of them, stop.
    /// delim_token_if_any: If its a delimToken, pass in the delim character
    fn parseRaw(p: *Parser, consume_until: []const TT, delim_token_if_any: ?u8) Raw {
        var raw = Raw.init(p._a);
        const _start = p.start;

        var t = p.current();
        while (!p.end() and !p.consumeUntilUtil(consume_until, t.tok_type, delim_token_if_any)) : (t = p.nextTok()) {
            raw.children.append(p.cursor) catch unreachable;
        }

        p.addLocation(&raw.loc, _start);
        return raw;
    }

    fn consumeUntilUtil(p: *Parser, consume_until: []const TT, token_type_to_match: TT, delim_token_if_any: ?u8) bool {
        for (consume_until) |consume_until_unit| {
            if (consume_until_unit == token_type_to_match) {
                if (token_type_to_match == TT.DelimToken) {
                    if (delim_token_if_any) |delim_tok| {
                        if (p.getTokenString(p.current())[0] == delim_tok) {
                            return true;
                        }
                    }
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

    fn consumeWhiteSpaceCommentCSSNode(p: *Parser) ArrayList(CSSNode) {
        var arr = ArrayList(CSSNode).init(p._a);
        var t = p.current();
        if (p.options.skip_ws_comments) {
            while ((t.tok_type == TT.WhitespaceToken or t.tok_type == TT.CommentToken) and !p.end()) : (t = p.nextTok()) {}
        } else {
            t = p.current();
            while ((t.tok_type == TT.WhitespaceToken or t.tok_type == TT.CommentToken) and !p.end()) : (t = p.nextTok()) {
                if (!p.options.skip_ws_comments) {
                    if (t.tok_type == TT.WhitespaceToken) {
                        arr.append(CSSNode{ .whitespace = p.cursor }) catch unreachable;
                    } else {
                        arr.append(CSSNode{ .comment = p.cursor }) catch unreachable;
                    }
                }
            }
        }
        return arr;
    }

    // Comsume WhiteSpace and Comment Until the
    fn consumeWhiteSpaceComment(p: *Parser) ArrayList(usize) {
        var arr = ArrayList(usize).init(p._a);
        var t = p.current();
        if (p.options.skip_ws_comments) {
            while ((t.tok_type == TT.WhitespaceToken or t.tok_type == TT.CommentToken) and !p.end()) : (t = p.nextTok()) {}
        } else {
            t = p.current();
            while ((t.tok_type == TT.WhitespaceToken or t.tok_type == TT.CommentToken) and !p.end()) : (t = p.nextTok()) {
                if (!p.options.skip_ws_comments) {
                    arr.append(p.cursor) catch unreachable;
                }
            }
        }

        return arr;
    }

    fn parseDeclProperty(p: *Parser) ?DeclProperty {
        var property = DeclProperty.init(p._a);
        const _start = p.start;

        var tok: Token = p.current();
        const tt: TT = tok.tok_type;

        if (tt == TT.DelimToken) {
            switch (p.getTokenString(tok)[0]) {
                '-' => {
                    tok = p.nextTok();
                    if (tok.tok_type == TT.DelimToken and p.getTokenString(tok)[0] == '-') {
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
            p.eat(TT.HashToken);
        } else if (tok.tok_type == TT.IdentToken) {
            property.children.append(p.cursor) catch unreachable;
            p.advance();
        } else {
            return null;
        }

        p.addLocation(&property.loc, _start);
        return property;
    }
};

test "Parser" {
    _ = @import("./parser_test.zig");
}
