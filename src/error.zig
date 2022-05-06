const std = @import("std");

pub const ParserErrorType = error{
    EOFError,
};

pub const Error = struct {
    line: usize,
    startPosition: usize,
    endPosition: usize,
    errorMessage: []const u8,
    errorType: ParserErrorType,
};
