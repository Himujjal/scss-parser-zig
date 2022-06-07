/// List of all the Nodes that are to be present in the CSS Tree
/// Copied from https://github.com/DefinitelyTyped/DefinitelyTyped/blob/master/types/css-tree/index.d.ts
const std = @import("std");
const token = @import("./token.zig");

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
    Inline: ArrayList(Declaration),
    StyleSheet: StyleSheet,

    pub fn init(a: Allocator, is_inline: bool) Tree {
        if (is_inline) {
            return Tree{ .Inline = ArrayList(Declaration).init(a) };
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
    nth: Nth,
    number_node: Number,
    operator: Operator,
    parentheses: Value,
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
    value: Value,

    misc_token: usize,

    comment: Comment,
    whitespace: WS, // WhitespaceToken
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
    rule_type: AtKeywordTypes = AtKeywordTypes.Custom,

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
    value: Value,
    pub fn init(a: Allocator) Declaration {
        const property = Property.init(a);
        const value = Value.init(a);
        return Declaration{ .property = property, .value = value };
    }
    pub fn deinit(self: *Declaration) void {
        self.value.deinit();
        self.property.deinit();
    }
};

pub const CSSVariable = struct {
    loc: CSSLocation,
    ident: Identifier,
};

pub const Property = struct {
    loc: CSSLocation = default_location,
    is_custom_property: bool = false,
    children: ArrayList(usize),
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

    rule_type: AtKeywordTypes,

    /// Token index where the name of the AtRule lies
    name_index: usize = 0,

    /// Prelude is the Prelude of the At Rule between name and body
    prelude: union(enum) {
        /// List of the CSS Nodes that fit in the AtRulePrelude
        at_rule_prelude: AtrulePrelude,
        /// Array of raw tokens 
        raw: Raw,
    },

    block: Block,

    pub fn init(a: Allocator) Atrule {
        return Atrule{
            .rule_type = AtKeywordTypes.Custom,
            .prelude = .{ .raw = undefined },
            .block = Block.init(a),
        };
    }

    pub fn deinit(self: *Atrule) void {
        self.prelude.deinit();
        self.simple_block.deinit();
    }
};

/// 1px, 1rem 
pub const Dimension = struct {
    loc: CSSLocation = default_location,
    value: usize, // location of the value
    unit: usize, // location of the identifier
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

/// ( [ <mf-plain> | <mf-boolean> | <mf-range> ] )
/// <mf-plain> = <mf-name> : <mf-value>
/// <mf-boolean> = <mf-name>
/// <mf-range> = <mf-name> [ '<' | '>' ]? '='? <mf-value> | <mf-value> [ '<' | '>' ]? '='? <mf-name> | <mf-value> '<' '='? <mf-name> '<' '='? <mf-value> | <mf-value> '>' '='? <mf-name> '>' '='? <mf-value>
/// <mf-name> = <ident>
/// <mf-value> = <number> | <dimension> | <ident> | <ratio>
/// <ratio> = <integer> / <integer>
/// https://csstree.github.io/docs/syntax/#Type:media-feature 
pub const MediaFeature = struct {
    loc: CSSLocation = default_location,
    name: Identifier,
    children: ArrayList(CSSNode),
    pub fn init(a: Allocator) MediaFeature {
        return MediaFeature{ .children = ArrayList(CSSNode).init(a) };
    }
    pub fn deinit(self: *MediaFeature) void {
        self.children.deinit();
    }
};

/// <media-condition> | [ not | only ]? <media-type> [ and <media-condition-without-or> ]?
/// https://csstree.github.io/docs/syntax/#Type:media-query
pub const MediaQuery = struct {
    loc: CSSLocation = default_location,
    media_condition: union(enum) {
        media_condition: *MediaCondition,
        media_query_branch_2: *MediaQueryBranch2,
    } = undefined,

    pub fn init() MediaQuery {
        return MediaQuery{};
    }
    pub fn deinit(self: *MediaQuery) void {
        self.children.deinit();
    }
};

/// [ not | only ]? <media-type> [ and <media-condition-without-or> ]?
const MediaQueryBranch2 = struct {
    loc: CSSLocation = default_location,

    /// if null, not present, false = 'only', true = 'not'
    isNot: ?bool,
    media_type: MediaType,

    media_condition_without_or_branch: ?struct {
        is_and: bool,
        media_condition_without_or: *MediaConditionWithoutOr,
    },
};

pub const MediaCondition = union(enum) {
    media_not: *MediaNot,
    media_and: *MediaAnd,
    media_or: *MediaOr,
    media_in_parens: *MediaInParens,
};

const MediaNot = struct {
    loc: CSSLocation,
    media_in_parens: *MediaInParens,
    a: Allocator,
    pub fn init(a: Allocator) *MediaNot {
        var media_in_parens: *MediaInParens = a.create(MediaInParens);
        media_in_parens.* = MediaInParens.init(a);
        return MediaNot{ .media_in_parens = media_in_parens };
    }
    pub fn deinit(self: *MediaNot) void {
        self.media_in_parens.deinit();
        self.a.destroy(self.media_in_parens);
    }
};

/// <media-in-parens> [ and <media-in-parens> ]+
const MediaAnd = struct {
    loc: CSSLocation,
    children: ArrayList(MediaInParens),
    pub fn init(a: Allocator) MediaAnd {
        return MediaAnd{ .children = ArrayList(MediaInParens).init(a) };
    }
    pub fn deinit(self: *MediaAnd) void {
        self.children.deinit();
    }
};

/// <media-in-parens> [ or <media-in-parens> ]+ 
const MediaOr = struct {
    loc: CSSLocation,
    children: ArrayList(MediaInParens),
    pub fn init(a: Allocator) MediaAnd {
        return MediaOr{ .children = ArrayList(MediaInParens).init(a) };
    }
    pub fn deinit(self: *MediaOr) void {
        self.children.deinit();
    }
};

const MediaInParens = union(enum) {
    media_condition: *MediaCondition,
    media_feature: *MediaFeature,
    general_enclosed: GeneralEnclosed,
};

const GeneralEnclosed = union(enum) {
    function: FunctionNode,
    ident: GeneralEnclosedIdent,
};

pub const GeneralEnclosedIdent = struct {
    loc: CSSLocation = default_location,
    ident: usize = 0,
    children: ArrayList(CSSNode),
    fn init(a: Allocator) GeneralEnclosedIdent {
        return GeneralEnclosedIdent{ .children = ArrayList(CSSNode).init(a) };
    }
    fn deinit(_gei: *GeneralEnclosedIdent) void {
        _gei.children.deinit();
    }
};

const MediaConditionWithoutOr = union(enum) {
    media_not: MediaNot,
    media_and: MediaAnd,
    media_in_parens: MediaInParens,
};

/// <mf-name> [ '<' | '>' ]? '='? <mf-value> |
/// <mf-value> [ '<' | '>' ]? '='? <mf-name> |
/// <mf-value> '<' '='? <mf-name> '<' '='? <mf-value> |
/// <mf-value> '>' '='? <mf-name> '>' '='? <mf-value>
pub const MfRange = struct {
    loc: CSSLocation = default_location,
    node_ranges: ArrayList(CSSNode),
    pub fn init(a: Allocator) MfRange {
        return MfRange{ .node_ranges = ArrayList(CSSNode).init(a) };
    }
    pub fn deinit(mf: *MfRange) void {
        mf.node_ranges.deinit();
    }
};

pub const MfPlain = struct {
    loc: CSSLocation,
    name: MfName,
    value: MfValue,
};
pub const MfBoolean = Identifier;
pub const MfName = Identifier;
pub const MfValue = union(enum) {
    number: Number,
    dimension: Dimension,
    ident: Identifier,
    ratio: Ratio,
};
const MediaType = Identifier; // identifier

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

pub const SelectorList = struct {
    loc: CSSLocation = default_location,
    children: ArrayList(Selector),
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

pub const Selector = struct {
    loc: CSSLocation = default_location,
    children: ArrayList(SingleSelector),
    pub fn init(a: Allocator) Selector {
        return Selector{ .children = ArrayList(SingleSelector).init(a) };
    }
    pub fn deinit(self: *Selector) void {
        switch (self._type) {
            .id, .class, .operator => {},
            .pseudo_class => self._type.psuedo_class.deinit(),
            .pseudo_element => self._type.psuedo_element.deinit(),
            .attribute_selector => self._type.attribute_selector.deinit(),
            .combinator_selector => self._type.combinator_selector.deinit(),
        }
    }
};

pub const SingleSelector = union(enum) {
    type_selector: TypeSelector,
    id: [2]usize,
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

pub const URL = struct {
    loc: CSSLocation = default_location,
    branches: union(enum) {
        url_token: URLToken,
        branch1: URLBranch1,
    } = undefined,
    pub fn init() URL {
        return URL{};
    }
    pub fn deinit(self: *URL) void {
        switch (self.branches) {
            .url_token => |u| u.deinit(),
            .branch1 => |b| b.deinit(),
        }
    }
};

/// url( <string> <url-modifier>* )
pub const URLBranch1 = struct {
    loc: CSSLocation = default_location,
    string: StringNode,
    children: ArrayList(URLModifier),
    pub fn init(a: Allocator) URLBranch1 {
        return URLBranch1{ .children = ArrayList(URLModifier).init(a) };
    }
    pub fn deinit(self: *URLBranch1) void {
        self.children.deinit();
    }
};

pub const URLModifier = struct {
    loc: CSSLocation = default_location,
    branches: union(enum) {
        ident: Identifier,
        branch2: FunctionNode,
    },
    pub fn init() URLModifier {
        return URLModifier{ .brances = .{ .ident = 0 } };
    }
    pub fn deinit(self: *URLModifier) void {
        switch (self.brances) {
            .branch2 => |b| b.deinit(),
            .ident => {},
        }
    }
};

pub const Value = struct {
    loc: CSSLocation = default_location,
    children: ArrayList(CSSNode),
    important: bool = false,
    pub fn init(a: Allocator) Value {
        return Value{ .children = ArrayList(CSSNode).init(a) };
    }
    pub fn deinit(self: *Value) void {
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
pub const Operator = usize;

/// Token with TokenType.StringToken
const StringNode = usize;

const URLToken = usize;
