const std = @import("std");
const expect = std.testing.expect;

const token = @import("./token.zig");
const parser = @import("./parser.zig");
const _error = @import("./error.zig");

const ArenaAllocator = std.heap.ArenaAllocator;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Token = token.Token;
const TokenType = token.TokenType;
const Error = _error.Error;
const ParserErrorType = _error.ParserErrorType;

pub const Scanner = struct {
    const Self = @This();

    allocator: Allocator,
    code: []const u8 = undefined,

    tokens: *ArrayList(Token),
    errors: *ArrayList(Error),
    warnings: *ArrayList(Error),

    scanner_arena: *ArenaAllocator,
    internal_allocator: Allocator,

    /// start of the cursor that is fixed on conflicts
    start: usize = 0,

    /// cursor that moves on along the source code
    cursor: usize = 0,

    /// line number of the cursor
    line: usize = 0,

    /// column number
    col: usize = 0,

    raw_text_offset: usize = 0,

    pub fn init(
        allocator: Allocator,
        tokens: *ArrayList(Token),
        errors: *ArrayList(Error),
        warnings: *ArrayList(Error),
    ) Self {
        var scanner_arena = allocator.create(ArenaAllocator) catch unreachable;
        scanner_arena.* = ArenaAllocator.init(allocator);

        return Self{
            .allocator = allocator,
            .tokens = tokens,
            .errors = errors,
            .scanner_arena = scanner_arena,
            .internal_allocator = scanner_arena.allocator(),
            .warnings = warnings,
        };
    }

    pub fn scan(self: *Self, code: []const u8) *Self {
        self.code = code;
        while (!self.end()) {
            self.start = self.cursor;
            self.scanToken();
        }
        self.addTok(TokenType.EOF, self.code.len - 1, self.code.len - 1);
        return self;
    }

    pub fn scanToken(self: *Self) void {
        _ = self.advance();
    }

    /// look one character ahead
    fn lookAhead(self: *Self) u8 {
        return if (self.cursor >= self.code.len) 0 else self.code[self.cursor];
    }

    /// look two characters ahead
    fn lookSuperAhead(self: *Self) u8 {
        if (self.cursor >= self.code.len) return 0;
        if (self.cursor + 1 >= self.code.len) return 0;
        return self.code[self.cursor + 1];
    }

    fn lookSuperDuperAhead(self: *Self) u8 {
        if (self.lookSuperAhead() != 0) {
            if (self.cursor + 2 >= self.code.len) return 0;
            return self.code[self.cursor + 2];
        }
        return 0;
    }

    fn match(self: *Self, expectedChar: u8) bool {
        if (self.end()) return false;
        if (self.code[self.cursor] != expectedChar) return false;
        self.*.cursor += 1;
        return true;
    }

    fn advance(self: *Self) u8 {
        self.cursor += 1;
        self.col += 1;
        if (self.code[self.cursor - 1] == '\n') {
            self.line += 1;
            self.col = 1;
        }
        return self.code[self.cursor - 1];
    }

    fn end(self: *Self) bool {
        return self.cursor >= self.code.len;
    }

    pub fn addTokenAdvance(self: *Self, tok_type: TokenType, steps: usize) void {
        self.cursor += steps;
        self.addToken(tok_type);
    }

    fn addTok(self: *Self, tok_type: TokenType, start: usize, endPos: usize) void {
        self.tokens.append(Token{
            .start = start + self.raw_text_offset,
            .end = endPos + self.raw_text_offset,
            .tok_type = tok_type,
        }) catch unreachable;
    }

    pub fn addToken(self: *Self, tok_type: TokenType) void {
        self.addTok(tok_type, self.start, self.cursor);
    }

    pub fn addError(self: *Self, message: []const u8) void {
        var line: usize = 1;
        var col: usize = 1;
        var i: usize = 0;
        while (i < self.start) : (i += 1) {
            if (self.code[i] == '\n') {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        const tok = self.code[self.start..self.cursor];

        const errorMessage = std.fmt.allocPrint(
            self.internal_allocator,
            "[{d}:{d}] {s} at `{s}`",
            .{
                line,
                col,
                message,
                tok,
            },
        ) catch unreachable;

        self.errors.append(Error{
            .line = line,
            .startPosition = self.start,
            .endPosition = self.cursor,
            .errorMessage = errorMessage,
            .errorType = ParserErrorType.TokenizerError,
        }) catch unreachable;
    }

    // Only for debugging purposes
    pub fn printTokens(self: *Self) void {
        std.debug.print("========= TOKENS ===========\nToken length: {d}\n", .{self.tokens.items.len});
        for (self.tokens.items) |tok| {
            std.debug.print("{s}\n", .{
                tok.toString(self.internal_allocator, self.code),
            });
        }
        std.debug.print("====================\n", .{});
    }

    pub fn deinitInternal(self: *Self) void {
        self.scanner_arena.deinit();
        self.allocator.destroy(self.scanner_arena);
    }

    pub fn deinit(self: *Self) void {
        self.deinitInternal();
    }
};
