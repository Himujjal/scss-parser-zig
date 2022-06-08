const std = @import("std");
const _token = @import("./token.zig");

const Allocator = std.mem.Allocator;
const TokenType = _token.TokenType;

/// Instead of using messages, we store the Error as an union of enum with the relevant
/// field taking the necessary information
pub const ParserErrorType = union(enum) {
    /// parser encountered an error token (which cannot be tokenized by the tokenizer)
    ErrorTokenFound,

    /// Parser encountered an invalid attribute selector operator
    /// Valid ones: 
    AttributeSelectorIsExpected,

    /// FunctionNode Unclosed Bracket
    FunctionNodeUnclosedBracket,

    ExpectedTokenFoundThis: struct {
        expected_token_type: TokenType,
        found_token_pos: usize,
    },

    comb_op_expected,
    EOFError,
};

pub const ScannerErrorType = error{EOFError};

pub const Error = struct {
    line: usize,
    col: usize,

    start_position: usize,
    end_position: usize,

    _type: ParserErrorType = ParserErrorType.EOFError,

    a: Allocator,

    pub fn init(
        a: Allocator,
        start_position: usize,
        end_position: usize,
        code: []const u8,
        _type: ParserErrorType,
    ) Error {
        const lineCol = Error.calculateLineAndColPosition(code, start_position);
        return Error{
            .a = a,
            .line = lineCol.line,
            .col = lineCol.col,
            .start_position = start_position,
            .end_position = end_position,
            ._type = _type,
        };
    }

    // pub fn deinit(a: Allocator, self: *Error) void {}

    pub fn calculateLineAndColPosition(code: []const u8, index: usize) struct { line: usize, col: usize } {
        var line: usize = 0;
        var col: usize = 0;
        for (code) |c, i| {
            col += 1;
            if (c == '\n') {
                line += 1;
                col = 1;
            }
            if (i == index) break;
        }
        return .{ .line = line, .col = col };
    }
};
