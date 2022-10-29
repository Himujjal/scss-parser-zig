/// Copied from https://github.com/DefinitelyTyped/DefinitelyTyped/blob/master/types/css-tree/index.d.ts
const std = @import("std");
const token = @import("token.zig");

const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Token = token.Token;
const AtKeywordTypes = token.AtKeywordTypes;

/// Struct that stores the information of the location of the nodes
pub const CSSLocation = struct {
    /// location of the start of node in the source
    start: usize = 0,
    /// location of the end of node in the source
    end: usize = 0,
    /// Line where the node starts
    start_line: usize = 0,
    /// Col where the node starts
    start_col: usize = 0,
    /// Line where the node ends
    end_line: usize = 0,
    /// Col where the node ends
    end_col: usize = 0,
};

const default_location: CSSLocation = CSSLocation{};

pub const Tree = union(enum) {
    Inline: Inline,
    StyleSheet: StyleSheet,

    pub fn init(a: Allocator, is_inline: bool) Tree {
        if (is_inline) {
            return Tree{ .Inline = Inline.init(a) };
        } else {
            return Tree{ .StyleSheet = StyleSheet.init(a) };
        }
    }
    pub fn deinit(self: *Tree) void {
        switch (self) {
            .Inline => self.Inline.deinit(),
            .StyleSheet => self.StyleSheet.deinit(),
        }
    }
};

pub const Inline = DeclarationList;

pub const StyleSheet = struct {
    loc: CSSLocation = default_location,
    children: ArrayList(CSSNode),

    pub fn init(a: Allocator) StyleSheet {
        return StyleSheet{ .children = ArrayList(CSSNode).init(a) };
    }

    pub fn deinit(self: *StyleSheet) void {
        self.rules.deinit();
    }
};

pub const CSSNode = union(enum) {
    an_plus_b: AnPlusB,
    at_rule: Atrule,
    at_rule_prelude: AtrulePrelude,
    attribute_selector: AttributeSelector,
    block: Block,
    brackets: Brackets,
    class_selector: ClassSelector, // .class
    combinator: Combinator,
    declaration: Declaration,
    declaration_list: DeclarationList,
    dimension: Dimension,
    function_node: FunctionNode,
    hash: Hash,
    id_selector: IdSelector,
    identifier: Identifier,
    media_feature: MediaFeature,
    media_query: MediaQuery,
    media_query_list: MediaQueryList,
    supports: SupportsCondition,
	page_selector_list: PageSelectorList,
    nth: Nth,
    number_node: Number,
    operator: Operator,
    parentheses: DeclValue,
    percentage: Percentage,

    selector_list: SelectorList,
    selector: Selector,
    pseudo_class_selector: PseudoClassSelector,
    pseudo_element_selector: PseudoElementSelector,

    ratio: Ratio,
    raw: Raw,
    rule: Rule,
    variable: CSSVariable,

    string_node: StringNode,
    style_sheet: *StyleSheet,

    at_keyword_token: usize,

    type_selector: TypeSelector,

    unicode_range: UnicodeRange,
    url: URL,
    value: DeclValue,

    misc_token: usize,

    comment: Comment,
	whitespaces: ArrayList(usize),
    cdo: CDO,
    cdc: CDC,
    _error: _Error,

    pub fn deinit(self: *CSSNode) void {
        switch (self) {
            .at_rule => |a| a.deinit(),
            .at_rule_prelude => |a| a.deinit(),
            .attribute_selector => |a| a.deinit(),
            .block => |a| a.deinit(),
            .brackets => |a| a.deinit(),
            .declaration => |a| a.deinit(),
            .declaration_list => |a| a.deinit(),
            .function_node => |a| a.deinit(),
            .media_feature => |a| a.deinit(),
            .media_query => |a| a.deinit(),
            .media_query_list => |a| a.deinit(),
            .nth => |a| a.deinit(),
            .pseudo_class_selector => |a| a.deinit(),
            .pseudo_element_selector => |a| a.deinit(),
            .rule => |a| a.deinit(),
            .selector => |a| a.deinit(),
            .selector_list => |a| a.deinit(),
            .style_sheet => |a| a.deinit(),
            .type_selector => |a| a.deinit(),
            .unicode_range => |a| a.deinit(),
            .url => |a| a.deinit(),
            .value => |a| a.deinit(),
            else => {},
        }
    }
};

pub const AtrulePrelude = struct {
    loc: CSSLocation = default_location,
    children: ArrayList(CSSNode),

    pub fn init(a: Allocator) AtrulePrelude {
        return AtrulePrelude{ .children = ArrayList(CSSNode).init(a) };
    }
    pub fn deinit(self: *AtrulePrelude) void {
        return self.children.deinit();
    }
};

/// [ <declaration>? ';' ]* <declaration>?
pub const DeclarationList = struct {
    loc: CSSLocation = default_location,
    children: ArrayList(Declaration),
    pub fn init(a: Allocator) DeclarationList {
        return DeclarationList{ .children = ArrayList(Declaration).init(a) };
    }
    pub fn deinit(d: *DeclarationList) void {
        d.children.deinit();
    }
};

/// <ident-token> : <declaration-value>? [ '!' important ]?
pub const Declaration = struct {
    loc: CSSLocation = default_location,
    important: bool = false,
    property: Property,
    ws0: ?ArrayList(usize) = null,
    /// whitespace before colon
    ws1: ?ArrayList(usize) = null,
    /// whitespace after colon
    ws2: ?ArrayList(usize) = null,

    value: DeclValue,

    /// whitespace before semi-colon
    ws3: ?ArrayList(usize) = null,
    pub fn init(a: Allocator) Declaration {
        const property = Property.init(a);
        const value = DeclValue.init(a);
        return Declaration{ .property = property, .value = value };
    }
    pub fn deinit(self: *Declaration) void {
        self.value.deinit();
        self.property.deinit();
    }
};

pub const CSSVariable = usize;

pub const Property = struct {
    loc: CSSLocation = default_location,
    is_custom_property: bool = false,

    children: ArrayList(usize), // locations of tokens in the tokenlist that compose all the property values

    pub fn init(a: Allocator) Property {
        return Property{ .children = ArrayList(usize).init(a) };
    }
    pub fn deinit(self: *Property) void {
        self.children.deinit();
    }
};

pub const AnPlusB = struct {
    loc: CSSLocation,
    a: ?i32,
    b: ?i32,
    n: ?i32,
};

pub const AtRulePlain = struct {
    loc: CSSLocation,
    name_Index: usize,
};

pub const Atrule = struct {
    loc: CSSLocation = default_location,

    /// AtRule Type
    rule_type: AtKeywordTypes,
    /// Token index where the name of the AtRule lies
    rule_index: usize = 0,
    /// WhiteSpaces after the rule_type
    ws1: ?ArrayList(WS) = null,

    /// Prelude is the Prelude of the At Rule between name and body
    prelude: ?union(enum) {
        /// List of the CSS Nodes that fit in the AtRulePrelude
        at_rule_prelude: AtrulePrelude,
        /// Array of raw tokens
        raw: Raw,
    } = null,

    /// Whitespaces after prelude and before block
    ws2: ?ArrayList(WS) = null,

    block: ?Block = null,

    pub fn init() Atrule {
        return Atrule{ .rule_type = AtKeywordTypes.Custom };
    }

    pub fn deinit(self: *Atrule) void {
        self.prelude.deinit();
        self.simple_block.deinit();
    }
};

pub const Block = struct {
    loc: CSSLocation = default_location,
    children: ArrayList(CSSNode),
    pub fn init(a: Allocator) Block {
        return Block{ .children = ArrayList(CSSNode).init(a) };
    }
    pub fn deinit(s: *Block) void {
        s.children.deinit();
    }
};

/// <name>( <CSSNode>* )
pub const FunctionNode = struct {
    loc: CSSLocation = default_location,
    /// location of the token name
    name: usize = 0,
    is_custom_property: bool = false,
    children: ArrayList(CSSNode),
    pub fn init(a: Allocator) FunctionNode {
        return FunctionNode{ .children = ArrayList(CSSNode).init(a) };
    }
    pub fn deinit(self: *FunctionNode) void {
        self.children.deinit();
    }
};

pub const Hash = usize; // token position

/// # <Identifier>
pub const IdSelector = struct {
    loc: CSSLocation = default_location,
    identifier: usize, // token position
};

pub const MediaQueryList = struct {
    loc: CSSLocation = default_location,
    children: ArrayList(MediaQuery),
    pub fn init(a: Allocator) MediaQueryList {
        return MediaQueryList{ .children = ArrayList(MediaQuery).init(a) };
    }
    pub fn deinit(self: *MediaQueryList) void {
        self.children.deinit();
    }
};

/// [ <mf-plain> | <mf-boolean> | <mf-range> ]
/// <mf-plain> = <mf-name> : <mf-value>
/// <mf-boolean> = <mf-name>
/// <mf-range> = <mf-name> [ '<' | '>' ]? '='? <mf-value> | <mf-value> [ '<' | '>' ]? '='? <mf-name> | <mf-value> '<' '='? <mf-name> '<' '='? <mf-value> | <mf-value> '>' '='? <mf-name> '>' '='? <mf-value>
/// <mf-name> = <ident>
/// <mf-value> = <number> | <dimension> | <ident> | <ratio>
/// <ratio> = <integer> / <integer>
/// https://csstree.github.io/docs/syntax/#Type:media-feature
pub const MediaFeature = union(enum) {
    mf_plain: *MfPlain,
    mf_boolean: Identifier,
    mf_range: *MfRange,
};

/// <media-condition> | [ not | only ]? <media-type> [ and <media-condition-without-or> ]?
/// https://csstree.github.io/docs/syntax/#Type:media-query
pub const MediaQuery = struct {
    loc: CSSLocation = default_location,
    /// whitespace before the media query
    ws1: ?ArrayList(WS) = null,
    branches: union(enum) {
        media_condition: MediaCondition,
        media_query_branch_2: *MediaQueryBranch2,
    } = undefined,
    /// whitespace after the media query
    ws2: ?ArrayList(WS) = null,

    pub fn init() MediaQuery {
        return MediaQuery{};
    }
    pub fn deinit(self: *MediaQuery) void {
        self.children.deinit();
    }
};

/// [ not | only ]? <media-type> [ and <media-condition-without-or> ]?
pub const MediaQueryBranch2 = struct {
    loc: CSSLocation = default_location,

    /// if null, not present, false = 'only', true = 'not'
    is_not: ?bool = null,
    not_only_index: usize = 0,

    /// space between ['not' | 'only'] and media_type
    ws1: ?ArrayList(usize) = null,

    media_type: Identifier = 0,

    /// space between media_type and 'and'
    ws2: ?ArrayList(usize) = null,
    /// space between 'and' and media_condition_without_or
    ws3: ?ArrayList(usize) = null,

    media_condition_without_or: ?MediaConditionWithoutOr = null,
};

/// <media-condition> = <media-not> | <media-and> | <media-or> | <media-in-parens>
/// If media_and or media_or has one child, understand its a media_in_parens
pub const MediaCondition = union(enum) {
    media_not: *MediaNot,
    media_and: *MediaAndOr,
    media_or: *MediaAndOr,
};

pub const MediaNot = struct {
    loc: CSSLocation = default_location,
    /// index of the not symbol
    not: usize = 0,
    /// WS between 'not' and media_in_parens
    ws1: ?ArrayList(usize) = null,
    media_in_parens: *MediaInParens = undefined,
};

/// <media-in-parens> [ and <media-in-parens> ]+
pub const MediaAndOr = struct {
    loc: CSSLocation = default_location,
    is_and: bool = true,
    /// list of all ws1 between ')' and 'and'
    ws1_list: ArrayList(?ArrayList(usize)),
    /// list of all ws1 between and and '('
    ws2_list: ArrayList(?ArrayList(usize)),
    /// list of all the 'and' or 'or'
    and_or_list: ArrayList(usize),
    children: ArrayList(MediaInParens),
    pub fn init(a: Allocator) MediaAndOr {
        return MediaAndOr{
            .children = ArrayList(MediaInParens).init(a),
            .ws1_list = ArrayList(?ArrayList(usize)).init(a),
            .ws2_list = ArrayList(?ArrayList(usize)).init(a),
            .and_or_list = ArrayList(usize).init(a),
        };
    }
    pub fn deinit(self: *MediaAndOr) void {
        self.children.deinit();
        self.ws1_list.deinit();
        self.ws2_list.deinit();
        self.and_or_list.deinit();
    }
};

/// '(' <content> ')'
pub const MediaInParens = struct {
    loc: CSSLocation = default_location,
    /// between '(' and media_condition
    ws1: ?ArrayList(WS) = null,
    content: union(enum) {
        media_condition: MediaCondition,
        media_feature: MediaFeature,
        general_enclosed: GeneralEnclosed,
    } = undefined,
    /// between media_condition and ')'
    ws2: ?ArrayList(WS) = null,
};

pub const GeneralEnclosed = union(enum) {
    function: *FunctionNode,
    ident: *GeneralEnclosedIdent,
};

pub const GeneralEnclosedIdent = struct {
    loc: CSSLocation = default_location,
    ident: Identifier = 0,
    children: ArrayList(CSSNode),
    fn init(a: Allocator) GeneralEnclosedIdent {
        return GeneralEnclosedIdent{ .children = ArrayList(CSSNode).init(a) };
    }
    fn deinit(_gei: *GeneralEnclosedIdent) void {
        _gei.children.deinit();
    }
};

/// <media-condition-without-or> = <media-not> | <media-and> | <media-in-parens>
pub const MediaConditionWithoutOr = union(enum) {
    media_not: *MediaNot,
    /// if media_and has only 1 child, then its a media_paren
    media_and: *MediaAndOr,
};

/// <mf-name> [ '<' | '>' ]? '='? <mf-value> |
/// <mf-value> [ '<' | '>' ]? '='? <mf-name> |
/// <mf-value> '<' '='? <mf-name> '<' '='? <mf-value> |
/// <mf-value> '>' '='? <mf-name> '>' '='? <mf-value>
pub const MfRange = struct {
    loc: CSSLocation = default_location,

    /// if (mf_name <= mf_value) and not (mf_value <= mf_name)
    /// basically if mf_name comes first in comparison
    mf_name_first: bool = false,

    /// mf_name
    mf_name: Identifier = undefined,
    /// WS between first value/name and the first op
    ws1: ?ArrayList(usize) = null,
    /// first operator
    op1: ?MfRangeOp = null,
    /// WS between first operator and the first value/name
    ws2: ?ArrayList(usize) = null,
    /// WS between name and second op
    ws3: ?ArrayList(usize) = null,
    /// WS between second op and second value
    ws4: ?ArrayList(usize) = null,
    mf_value1: MfValue = undefined,
    /// second operator
    op2: ?MfRangeOp = null,
    /// second value
    mf_value2: ?MfValue = null,
};

/// operation like for range and its token index
pub const MfRangeOp = union(enum) {
    GreaterThan: usize, // >
    GreaterThanEqual: [2]usize, // >=
    LessThan: usize, // <
    LessThanEqual: [2]usize, // <=
    Equal: usize,
};

// name ':' value
pub const MfPlain = struct {
    loc: CSSLocation,
    name: Identifier,
    /// WS between name and ':'
    ws1: ?ArrayList(WS) = null,
    /// WS between ':' and value
    ws2: ?ArrayList(WS) = null,
    value: MfValue,
};

pub const MfBoolean = Identifier;
pub const MfName = Identifier;
pub const MfValue = union(enum) {
    number: Number, // numberToken
    dimension: Dimension, // dimensionToken
    ident: Identifier, // identifier
    ratio: *Ratio, // ratio
};
const MediaType = Identifier; // identifier

/// ('not' <supports-in-parens>) |
/// (<supports-in-parens> ('and' <supports-in-parens> )\* |
/// (<supports-in-parens> ('or' <supports-in-parens> )\*
pub const SupportsCondition = union(enum) {
    not_supports_in_parens: *NotSupportsInParens,
    supports_in_parens_and_or: *SupportsInParensAndOr,
};

/// "not" <supports_in_parens>
pub const NotSupportsInParens = struct {
    loc: CSSLocation = default_location,
    not_index: usize = 0,
	ws1: ?ArrayList(usize) = null,
    supports_parens: SupportsInParens = undefined,
};

/// <supports-in-parens> (('and' | 'or') <supports-in-parens> )\*
pub const SupportsInParensAndOr = struct {
    loc: CSSLocation = default_location,
    list_supports_in_parens: ArrayList(SupportsInParens),

    /// list of all the ws between <supports_in_parens> and "and" | "or"
    ws1_list: ArrayList(?ArrayList(usize)),

    /// index of all the "and" or "or"
    and_or_list: ArrayList(usize),

    /// list of all the ws between ("and" | "or") & <supports_in_parens>
    ws2_list: ArrayList(?ArrayList(usize)),

    /// is "and". false if "or"
    is_and: bool = false,

    pub fn init(a: Allocator) SupportsInParensAndOr {
        return SupportsInParensAndOr{
			.list_supports_in_parens = ArrayList(SupportsInParens).init(a),
			.ws1_list = ArrayList(?ArrayList(usize)).init(a),
			.ws2_list = ArrayList(?ArrayList(usize)).init(a),
			.and_or_list = ArrayList(usize).init(a),
		};
    }
    pub fn deinit(s: *SupportsInParensAndOr) void {
        s.list_supports_in_parens.deinit();
    }
};

pub const SupportsInParens = union(enum) {
    supports_condition: *SupportsConditionWithParens,
    supports_feature: SupportsFeature,
    general_enclosed: GeneralEnclosed,
};

pub const SupportsConditionWithParens = struct {
    loc: CSSLocation = default_location,
    /// ws between '(' and support_content
    ws1: ?ArrayList(usize) = null,

    condition: SupportsCondition = undefined,

    /// ws between support_content and ')'
    ws2: ?ArrayList(usize) = null,
};

pub const SupportsFeature = union(enum) {
    supports_decl: *SupportsDecl,
    supports_selector_fn: *SupportsSelectorFn,
};

/// '(' <declaration> ')'
pub const SupportsDecl = struct {
    loc: CSSLocation = default_location,
	ws1: ?ArrayList(usize) = null,
    declaration: Declaration = undefined,
	ws2: ?ArrayList(usize) = null,
};

/// ('selector' | 'font-tech' | 'font-format')'(' <complex-selector> ')'
pub const SupportsSelectorFn = FunctionNode;

/// <page-selector> (',' <page-selector>)
pub const PageSelectorList = ArrayList(PageSelector);

/// <pseudo-page>+ | <ident> <pseudo-page>* 
pub const PageSelector = struct {
	loc: CSSLocation = default_location,
	ident: ?usize = null,
	ws1: ?ArrayList(usize) = null,
	pseudo_pages: ArrayList(PageSelectorPseudoPage),
	ws2: ?ArrayList(usize) = null,
	pub fn init(a: Allocator) PageSelector {
		return PageSelector{ .pseudo_pages = ArrayList(PageSelectorPseudoPage).init(a) };
	}
};

/// :('left'|'right'|'first'|'blank')
pub const PageSelectorPseudoPage = struct {
	ws1: ?ArrayList(usize) = null,
	pseudo_page: [2]usize,
};

/// <an-plus-b> | 'even' | 'odd'
pub const Nth = struct {
    loc: CSSLocation = default_location,
    nth: union(enum) { an_plus_b: AnPlusB, odd, even } = undefined,
    selector: ?SelectorList = null,
    pub fn init() Nth {
        return Nth{};
    }
    pub fn deinit(self: *Nth) void {
        if (self.selector) |selector| {
            selector.deinit();
        }
    }
};

pub const Percentage = struct {
    loc: CSSLocation = default_location,
    value: usize, // location of the value
    percentage_tok: usize, // location of the identifier
};

/// <integer> / <integer>
pub const Ratio = struct {
    loc: CSSLocation = default_location,
    left: usize,
    ws1: ?ArrayList(WS) = null,
    div: usize = 0,
    ws2: ?ArrayList(WS) = null,
    right: usize,
};

/// any set of tokens
pub const Raw = struct {
    loc: CSSLocation = default_location,
    children: ArrayList(usize),
    pub fn init(a: Allocator) Raw {
        return Raw{ .children = ArrayList(usize).init(a) };
    }
    pub fn deinit(self: *Raw) void {
        self.children.deinit();
    }
};

/// Rule is a great way to tokenize shit
pub const Rule = struct {
    loc: CSSLocation = default_location,
    prelude: Prelude,
    block: Block,
    pub fn init(a: Allocator) Rule {
        return Rule{
            .prelude = undefined,
            .block = Block.init(a),
        };
    }
    pub fn deinit(self: *Rule) void {
        switch (self.prelude) {
            .raw => |r| r.deinit(),
            .selector_list => |s| s.deinit(),
        }
        self.block.deinit();
    }
};

pub const Prelude = union(enum) { raw: Raw, selector_list: SelectorList };

// (<Selector> [,])*
pub const SelectorList = struct {
    loc: CSSLocation = default_location,
    /// whitespace before the selector
    ws1: ?ArrayList(usize) = null,
    children: ArrayList(Selector),
    /// whitespace after the selector, before the comma
    ws2: ?ArrayList(usize) = null,
    pub fn init(a: Allocator) SelectorList {
        return SelectorList{ .children = ArrayList(Selector).init(a) };
    }
    pub fn deinit(self: *SelectorList) void {
        for (self.children.items) |child| {
            child.deinit();
        }
        self.children.deinit();
    }
};

// SingleSelector*
pub const Selector = struct {
    loc: CSSLocation = default_location,
    children: ArrayList(SingleSelector),
    pub fn init(a: Allocator) Selector {
        return Selector{ .children = ArrayList(SingleSelector).init(a) };
    }
    pub fn deinit(self: *Selector) void {
        self.children.deinit();
    }
};

pub const SingleSelector = union(enum) {
    type_selector: TypeSelector,
    id: usize,
	percent: usize,
    class: [2]usize,
    psuedo_class: PseudoClassSelector,
    pseudo_element: PseudoElementSelector,
    combinator: Combinator, // >, +, ~, '<space>'
    attribute_selector: AttributeSelector,
};

/// ':' <ident-token> | ':' <function-token> <any-value> ')'
/// https://csstree.github.io/docs/syntax/#Type:pseudo-class-selector
pub const PseudoClassSelector = struct {
    loc: CSSLocation = default_location,
    branches: union(enum) {
        identifier: Identifier,
        func_node: FunctionNode,
    },
    pub fn init() PseudoClassSelector {
        return PseudoClassSelector{ .branches = .{ .identifier = 0 } };
    }
    pub fn deinit(self: *PseudoClassSelector) void {
        switch (self.branches) {
            .identifier => {},
            .func_node => |t| t.any_value.deinit(),
        }
    }
};

/// ':' <pseudo-class-selector>
/// https://csstree.github.io/docs/syntax/#Type:pseudo-element-selector
pub const PseudoElementSelector = struct {
    loc: CSSLocation = default_location,
    class_selector: PseudoClassSelector,
    pub fn init() PseudoElementSelector {
        return PseudoElementSelector{ .class_selector = PseudoClassSelector.init() };
    }
    pub fn deinit(self: *PseudoElementSelector) void {
        self.class_selector.deinit();
    }
};

/// [name~=value]
pub const AttributeSelector = struct {
    loc: CSSLocation = default_location,

    ws1: ?ArrayList(usize) = null,

    /// Token index where th identifier lies
    name_index: usize = 0,

    ws2: ?ArrayList(usize) = null,

    /// matcher - array of tokens representing the matcher
    matcher: ?usize = null,

    ws3: ?ArrayList(usize) = null,

    /// value
    value: ?union(enum) { string_node: StringNode, identifier: Identifier } = null,

    ws4: ?ArrayList(usize) = null,

    /// flags - set of tokens that represent the flags
    flags: ?Identifier = null,

    ws5: ?ArrayList(usize) = null,

    pub fn init() AttributeSelector {
        return AttributeSelector{};
    }

    pub fn deinit(self: *AttributeSelector) void {
        if (self.matcher) |matcher| matcher.deinit();
        if (self.flags) |flags| flags.deinit();
    }
};

// <ident> | <ident|ident> | <ident|*> | <*> | <*|ident> | <*|*> <|ident> | <|*>
pub const TypeSelector = struct {
    loc: CSSLocation = default_location,
    branch1: ?TypeSelectorBranch = null,
    vertical_line: ?usize = null,
    branch2: ?TypeSelectorBranch = null,
    pub fn init() TypeSelector {
        return TypeSelector{};
    }
};

pub const TypeSelectorBranch = union(enum) { star: Identifier, ident: Identifier };

// <unicode>*
pub const UnicodeRange = struct {
    loc: CSSLocation = default_location,
    children: ArrayList(usize),
    pub fn init(a: Allocator) UnicodeRange {
        return UnicodeRange{ .children = ArrayList(usize).init(a) };
    }
    pub fn deinit(self: *UnicodeRange) void {
        self.children.deinit();
    }
};

pub const URL = usize;
pub const DeclValue = struct {
    loc: CSSLocation = default_location,

    children: ArrayList(CSSNode),

    important: bool = false,

    pub fn init(a: Allocator) DeclValue {
        return DeclValue{ .children = ArrayList(CSSNode).init(a) };
    }
    pub fn deinit(self: *DeclValue) void {
        self.children.deinit();
    }
};

pub const Identifier = usize;

pub const Combinator = struct {
    loc: CSSLocation = default_location,
    // whitespaces before the
    ws1: ?ArrayList(usize) = null,
    tok: ?usize = null,
    ws2: ?ArrayList(usize) = null,
    pub fn init() Combinator {
        return Combinator{};
    }
}; // '>' | '+' | '~' | '||' | /deep/

pub const ClassSelector = usize;

pub const CDC = usize;
pub const CDO = usize;
pub const Comment = usize;
pub const WS = usize;
pub const _Error = usize;

pub const Brackets = ArrayList(CSSNode);
pub const Number = usize;
pub const Dimension = usize;
pub const Operator = usize;

/// Token with TokenType.StringToken
const StringNode = usize;

const URLToken = usize;
