/// This module parses the Atrule and the different types
const std = @import("std");

const scanner = @import("./scanner.zig");
const token = @import("./token.zig");
const _error = @import("./error.zig");
const utils = @import("./utils.zig");
const nodes = @import("./nodes.zig");

const radix = @import("./radix_tree.zig");

const expect = std.testing.expect;
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;

const Parser = parser.Parser;

const Token = token.Token;
const TT = token.TokenType;
const AtKeywordTypes = token.AtKeywordTypes;
const WT = TT.WhitespaceToken;

const Error = _error.Error;
const ParserErrorType = _error.ParserErrorType;

const Atrule = nodes.Atrule;
const AtRulePrelude = nodes.AtrulePrelude;

const RadixTree = radix.StringRadixTree(AtKeywordTypes);

// --------------------------------------------------------------------------------

