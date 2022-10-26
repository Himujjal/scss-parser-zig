const std = @import("std");
const utils = @import("utils.zig");
const RadixTree = @import("radix_tree.zig").StringRadixTree;

const Allocator = std.mem.Allocator;

/// Basically an enum of all Possible Token Types
/// Referred from https://github.com/antlr/grammars-v4/blob/master/javascript/typescript/TypeScriptLexer.g4
pub const TokenType = enum {
    ErrorToken, // extra token when errors occur
    IdentToken,
    FunctionToken, // rgb( rgba( ...
    AtKeywordToken, // @abc
    HashToken, // #abc
    StringToken,
    BadStringToken,
    URLToken,
    BadURLToken,
    DelimToken, // any unmatched character
    NumberToken, // 5
    PercentageToken, // 5%
    DimensionToken, // 5em
    UnicodeRangeToken, // U+554A
    EqualToken, // = 
    IncludeMatchToken, // ~=
    DashMatchToken, // |=
    PrefixMatchToken, // ^=
    SuffixMatchToken, // $=
    SubstringMatchToken, // *=
    StarToken, // *
    ColumnToken, // ||
    WhitespaceToken, // space \t \r \n \f
    CDOToken, // <!--
    CDCToken, // -->
    ColonToken, // :
    SemicolonToken, // ;
    CommaToken, // ,
    LeftBracketToken, // [
    RightBracketToken, // ]
    LeftParenthesisToken, // (
    RightParenthesisToken, // )
    LeftBraceToken, // {
    RightBraceToken, // }
    CommentToken, // extra token for comments
    EmptyToken,
    CustomPropertyNameToken,
    CustomPropertyValueToken,
    EOF, // "EOF"
};

pub const AtKeywordTypes = enum {
    Charset,
    Import,
    Namespace,
    Media,
    Supports,
    Document,
    Page,
    Layer,
    FontFace,
    Keyframes,
    Viewport,
    CounterStyle,
    FontFeatureValue,
    Alternates,
    Property,
    ColorProfile,
    Custom,

    const KV = struct { val: []const u8, key_type: AtKeywordTypes };
    const ATKT = AtKeywordTypes;

    pub fn getAtKeyWordTypeFromTok(_a: Allocator, token_string: []const u8, radix_tree: RadixTree(ATKT)) AtKeywordTypes {
        var token_string_lower = utils.toLower(_a, token_string);
		var r = radix_tree;
        return r.get(token_string_lower) orelse AtKeywordTypes.Custom;
    }
};

pub const Token = struct {
    const Self = @This();

    tok_type: TokenType = TokenType.EOF,

    /// Index of the start of the token in the array
    start: usize = 0,

    /// end of the token in the string stream
    end: usize = 0,

    start_line: usize = 0,
    start_col: usize = 0,
    end_line: usize = 0,
    end_col: usize = 0,

    pub fn toString(
        self: *const @This(),
        allocator: Allocator,
        code: []const u8,
    ) []const u8 {
        var res: []const u8 = std.fmt.allocPrint(
            allocator,
            "[\"{s}\", {s}, {d}, {d}]",
            .{
                code[self.start..self.end],
                self.tok_type,
                self.start,
                self.end,
            },
        ) catch "-----------";
        return res;
    }

    pub fn getCodePartOfToken(self: *Self, code: []const u8) []const u8 {
        return code[self.start..self.end];
    }

    pub fn testing(self: *Self, allocator: Allocator) void {
        var res: []const u8 = std.fmt.allocPrint(
            allocator,
            "({d},{d})",
            .{ self.start, self.end },
        ) catch "-----------";
        return res;
    }
};

pub fn getTokenTypeFromString(string: []const u8) TokenType {
    if (std.mem.eql(u8, string, "break")) return TokenType.IdentToken; // 'break';
    return TokenType.Identifier;
}
