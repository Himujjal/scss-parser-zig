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
const ParserErrorType = _error.ScannerErrorType;

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
    start_line: usize = 1,
    /// column number
    start_col: usize = 1,
    /// line number of the cursor
    end_line: usize = 1,
    /// column number
    end_col: usize = 1,

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
            self.start_line = self.end_line;
            self.start_col = self.end_col;
            self.scanToken() catch continue;
        }
        self.addTok(TokenType.EOF, code.len, code.len);
        return self;
    }

    pub fn scanToken(self: *Self) ParserErrorType!void {
        const TT = TokenType;
        const c = try self.current();

        switch (c) {
            ' ', '\n', '\r', '\t' => {
                try self.advance();
                while (try self.consumeWhitespace()) {}
                self.addToken(TT.WhitespaceToken);
            },
            ':' => {
                try self.advance();
                self.addToken(TT.ColonToken);
            },
            ';' => {
                try self.advance();
                self.addToken(TT.SemicolonToken);
            },
            ',' => {
                try self.advance();
                self.addToken(TT.CommaToken);
            },
            '(' => {
                try self.advance();
                self.addToken(TT.LeftParenthesisToken);
            },
            ')' => {
                try self.advance();
                self.addToken(TT.RightParenthesisToken);
            },
            '{' => {
                try self.advance();
                self.addToken(TT.LeftBraceToken);
            },
            '}' => {
                try self.advance();
                self.addToken(TT.RightBraceToken);
            },
            '[' => {
                try self.advance();
                self.addToken(TT.LeftBracketToken);
            },
            ']' => {
                try self.advance();
                self.addToken(TT.RightBracketToken);
            },
            '#' => {
                if (try self.consumeHashToken()) {
                    self.addToken(TT.HashToken);
                } else {
                    try self.advance();
                    self.addToken(.DelimToken);
                }
            },
            '"', '\'' => {
                const t = try self.consumeString();
                if (t != TokenType.ErrorToken) {
                    self.addToken(t);
                }
            },
            '+', '.' => {
                const t = try self.consumeNumeric();
                if (t != TokenType.ErrorToken) {
                    self.addToken(t);
                } else {
                    try self.advance();
                    return self.addToken(.DelimToken);
                }
            },
            '=' => {
                try self.advance();
                self.addToken(TT.EqualToken);
            },
            '-' => {
                var t = try self.consumeNumeric();
                if (t != TokenType.ErrorToken) {
                    self.addToken(t);
                    return;
                }
                t = try self.consumeIdentLike();
                if (t != TT.ErrorToken) {
                    self.addToken(t);
                    return;
                }
                if (try self.consumeCDCToken()) {
                    self.addToken(TT.CDCToken);
                    return;
                }
                if (try self.consumeCustomVariableToken()) {
                    self.addToken(TT.CustomPropertyNameToken);
                    return;
                }

                try self.advance();
                return self.addToken(.DelimToken);
            },
            '@' => {
                if (try self.consumeAtKeywordToken()) {
                    self.addToken(TT.AtKeywordToken);
                } else {
                    try self.advance();
                    self.addToken(.DelimToken);
                }
            },
            '*' => {
                const t = try self.lookSuperAhead();
                if (t == '=') {
                    try self.move(2);
                    self.addToken(TT.SubstringMatchToken);
                } else {
                    try self.advance();
                    self.addToken(TT.StarToken);
                }
            },
            '$', '^', '~' => {
                const t = try self.consumeMatch();
                if (t != TT.ErrorToken) {
                    self.addToken(t);
                } else {
                    try self.advance();
                    self.addToken(.DelimToken);
                }
            },
            '/' => {
                if (try self.consumeComment()) {
                    self.addToken(TT.CommentToken);
                } else {
                    try self.advance();
                    self.addToken(.DelimToken);
                }
            },
            '>' => {
                try self.advance();
                self.addToken(.DelimToken);
            },
            '<' => {
                if (try self.consumeCDOToken()) {
                    self.addToken(TT.CDOToken);
                } else {
                    try self.advance();
                    self.addToken(.DelimToken);
                }
            },
            '\\' => {
                const t = try self.consumeIdentLike();
                if (t != TT.ErrorToken) {
                    self.addToken(t);
                } else {
                    try self.advance();
                    self.addToken(.DelimToken);
                }
            },
            'u', 'U' => {
                if (try self.consumeUnicodeRangeToken()) {
                    self.addToken(TT.UnicodeRangeToken);
                    return;
                }
                const t = try self.consumeIdentLike();
                if (t != TT.ErrorToken) {
                    self.addToken(t);
                }
            },
            '|' => {
                const t = try self.consumeMatch();
                if (t != TT.ErrorToken) {
                    return self.addToken(t);
                }
                if (try self.consumeColumnToken()) {
                    return self.addToken(TT.ColumnToken);
                }
                try self.advance();
                return self.addToken(.DelimToken);
            },
            0 => {
                self.addToken(TT.ErrorToken);
                return;
            },
            else => {
                var t = try self.consumeNumeric();
                if (t != TT.ErrorToken) {
                    return self.addToken(t);
                }
                t = try self.consumeIdentLike();
                if (t != TT.ErrorToken) {
                    self.addToken(t);
                } else {
                    try self.advance();
                    self.addToken(.DelimToken);
                }
            },
        }
    }

    fn consumeByte(self: *Self, c: u8) ParserErrorType!bool {
        if ((try self.current()) == c) {
            try self.advance();
            return true;
        }
        return false;
    }

    fn consumeComment(self: *Self) ParserErrorType!bool {
        if ((try self.current()) != '/' or (try self.lookSuperAhead()) != '*') {
            return false;
        }
        try self.move(2);
        while (true) {
            const c = try self.current();
            if (c == 0 and self.end()) {
                break;
            } else if (c == '*' and (try self.lookSuperAhead()) == '/') {
                try self.move(2);
                return true;
            }
            try self.advance();
        }
        return true;
    }

    fn consumeNewline(self: *Self) ParserErrorType!bool {
        const c = try self.current();
        if (c == '\n') {
            try self.advance();
            return true;
        } else if (c == '\r') {
            if ((try self.lookSuperAhead()) == '\n') {
                try self.move(2);
            } else {
                try self.advance();
            }
            return true;
        }
        return false;
    }

    fn consumeWhitespace(self: *Self) ParserErrorType!bool {
        const c = try self.current();
        if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
            try self.advance();
            return true;
        }
        return false;
    }

    fn consumeDigit(self: *Self) ParserErrorType!bool {
        const c = try self.current();
        if (c >= '0' and c <= '9') {
            try self.advance();
            return true;
        }
        return false;
    }

    fn consumeHexDigit(self: *Self) ParserErrorType!bool {
        const c = try self.current();
        if ((c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' and c <= 'F')) {
            try self.advance();
            return true;
        }
        return false;
    }

    fn consumeEscape(self: *Self) ParserErrorType!bool {
        if ((try self.current()) != '\\') {
            return false;
        }
        const mark = self.getMark();
        try self.advance();
        if (try self.consumeNewline()) {
            self.rewind(mark);
            return false;
        } else if (try self.consumeHexDigit()) {
            var k: usize = 1;
            while (k < 6) : (k += 1) {
                if (!(try self.consumeHexDigit())) {
                    break;
                }
            }
            _ = try self.consumeWhitespace();
            return true;
        } else {
            var c = try self.current();
            if (c >= 0xC0) {
                var n = try self.peekRune();
                var i: usize = 0;
                while (i < n) : (i += 1) {
                    try self.advance();
                }
                return true;
            } else if (c == 0 and self.end()) {
                self.rewind(mark);
                return false;
            }
        }
        try self.advance();
        return true;
    }

    fn consumeIdentToken(self: *Self) ParserErrorType!bool {
        const mark = self.getMark();
        if ((try self.current()) == '-') {
            try self.advance();
        }
        var c = try self.current();
        if (!((c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_' or c >= 0x80)) {
            if (c != '\\' or !(try self.consumeEscape())) {
                self.rewind(mark);
                return false;
            }
        } else {
            try self.advance();
        }
        while (true) {
            c = try self.current();
            if (!((c >= 'a' and c <= 'z') or
                (c >= 'A' and c <= 'Z') or
                (c >= '0' and c <= '9') or
                c == '_' or c == '-' or c >= 0x80))
            {
                if (c != '\\' or !(try self.consumeEscape())) {
                    break;
                }
            } else {
                try self.advance();
            }
        }
        return true;
    }

    fn consumeCustomVariableToken(self: *Self) ParserErrorType!bool {
        // expect to be on a '-'
        try self.advance();
        if ((try self.current()) != '-') {
            try self.move(-1);
            return false;
        }
        if (!try self.consumeIdentToken()) {
            try self.move(-1);
            return false;
        }
        return true;
    }

    fn consumeAtKeywordToken(self: *Self) ParserErrorType!bool {
        // expect to be on an '@'
        try self.advance();
        const is_ident_token = try self.consumeIdentToken();

        if (!is_ident_token) {
            try self.move(-1);
            return false;
        }
        return true;
    }

    fn consumeHashToken(self: *Self) ParserErrorType!bool {
        const mark = self.getMark();
        try self.advance();
        var c = try self.current();
        if (!((c >= 'a' and c <= 'z') or
            (c >= 'A' and c <= 'Z') or
            (c >= '0' and c <= '9') or
            c == '_' or
            c == '-' or
            c >= 0x80))
        {
            if (!try self.consumeEscape()) {
                self.rewind(mark);
                return false;
            }
        } else {
            try self.advance();
        }

        while (true) {
            c = try self.current();
            if (!((c >= 'a' and c <= 'z') or
                (c >= 'A' and c <= 'Z') or
                (c >= '0' and c <= '9') or
                c == '_' or
                c == '-' or
                c >= 0x80))
            {
                if (!try self.consumeEscape()) {
                    break;
                }
            } else {
                try self.advance();
            }
        }
        return true;
    }

    fn consumeNumberToken(self: *Self) ParserErrorType!bool {
        var mark = self.getMark();
        var c = try self.current();
        if (c == '+' or c == '-') {
            try self.advance();
        }
        const firstDigit = try self.consumeDigit();
        if (firstDigit) {
            while (try self.consumeDigit()) {}
        }
        if ((try self.current()) == '.') {
            try self.advance();
            if (try self.consumeDigit()) {
                while (try self.consumeDigit()) {}
            } else if (firstDigit) {
                // . could belong to the next token
                try self.move(-1);
                return true;
            } else {
                self.rewind(mark);
                return false;
            }
        } else if (!firstDigit) {
            self.rewind(mark);
            return false;
        }
        mark = self.getMark();
        c = try self.current();
        if (c == 'e' or c == 'E') {
            try self.advance();
            c = try self.current();
            if (c == '+' or c == '-') {
                try self.advance();
            }
            if (!(try self.consumeDigit())) {
                // e could belong to next token
                self.rewind(mark);
                return true;
            }
            while (try self.consumeDigit()) {}
        }
        return true;
    }

    fn consumeUnicodeRangeToken(self: *Self) ParserErrorType!bool {
        const c = try self.current();
        if ((c != 'u' and c != 'U') or try self.lookSuperAhead() != '+') {
            return false;
        }
        const mark = self.getMark();
        try self.move(2);

        // consume up to 6 hexDigits
        var k: usize = 0;
        while (try self.consumeHexDigit()) {
            k += 1;
        }

        // either a minus or a question mark or the end is expected
        if (try self.consumeByte('-')) {
            if (k == 0 or 6 < k) {
                self.rewind(mark);
                return false;
            }

            // consume another up to 6 hexDigits
            if (try self.consumeHexDigit()) {
                k = 1;
                while (try self.consumeHexDigit()) {
                    k += 1;
                }
            } else {
                self.rewind(mark);
                return false;
            }
        } else if (try self.consumeByte('?')) {
            // could be filled up to 6 characters with question marks or else regular hexDigits
            k += 1;
            while (try self.consumeByte('?')) {
                k += 1;
            }
        }
        if (k == 0 or 6 < k) {
            self.rewind(mark);
            return false;
        }
        return true;
    }

    fn consumeColumnToken(self: *Self) ParserErrorType!bool {
        if ((try self.current()) == '|' and (try self.lookSuperAhead()) == '|') {
            try self.move(2);
            return true;
        }
        return false;
    }

    fn consumeCDOToken(self: *Self) ParserErrorType!bool {
        if ((try self.current()) == '<' and
            (try self.lookSuperAhead()) == '!' and
            (try self.lookSuperDuperAhead()) == '-' and
            (try self.peek(3)) == '-')
        {
            try self.move(4);
            return true;
        }
        return false;
    }

    fn consumeCDCToken(self: *Self) ParserErrorType!bool {
        if ((try self.current()) == '-' and (try self.lookSuperAhead()) == '-' and (try self.lookSuperDuperAhead()) == '>') {
            try self.move(3);
            return true;
        }
        return false;
    }

    fn consumeMatch(self: *Self) ParserErrorType!TokenType {
        if ((try self.lookSuperAhead()) == '=') {
            switch (try self.current()) {
                '~' => {
                    try self.move(2);
                    return .IncludeMatchToken;
                },
                '|' => {
                    try self.move(2);
                    return .DashMatchToken;
                },
                '^' => {
                    try self.move(2);
                    return .PrefixMatchToken;
                },
                '$' => {
                    try self.move(2);
                    return .SuffixMatchToken;
                },
                '*' => {
                    try self.move(2);
                    return .SubstringMatchToken;
                },
                else => return .ErrorToken,
            }
        }
        return .ErrorToken;
    }

    fn consumeNumeric(self: *Self) ParserErrorType!TokenType {
        if (try self.consumeNumberToken()) {
            if (try self.consumeByte('%')) {
                return .PercentageToken;
            } else if (try self.consumeIdentToken()) {
                return .DimensionToken;
            }
            return .NumberToken;
        }
        return .ErrorToken;
    }

    fn consumeString(self: *Self) ParserErrorType!TokenType {
        const delim = try self.current();
        try self.advance();
        while (true) {
            const c = try self.current();
            switch (c) {
                0 => {
                    if (self.end()) {
                        break;
                    } else {
                        try self.advance();
                    }
                },
                '\n', '\r' => {
                    try self.advance();
                    return .BadStringToken;
                },
                '\'', '\"' => {
                    if (c == delim) {
                        try self.advance();
                        break;
                    }
                },
                '\\' => {
                    if (!(try self.consumeEscape())) {
                        // either newline or EOF after backslash
                        try self.advance();
                        _ = try self.consumeNewline();
                    }
                },
                else => try self.advance(),
            }
        }
        return .StringToken;
    }

    fn consumeUnquotedURL(self: *Self) ParserErrorType!bool {
        while (true) {
            const c = try self.current();
            if (c == 0 or c == ')') {
                break;
            } else if (c == '"' or c == '\'' or c == '(' or c == '\\' or c == ' ' or
                c <= 0x1F or c == 0x7F)
            {
                if (c != '\\' or !(try self.consumeEscape())) {
                    return false;
                }
                try self.advance();
            } else {
                try self.advance();
            }
        }
        return true;
    }

    // consumeRemnantsBadUrl consumes bytes of a BadUrlToken
    // so that normal tokenization may continue.
    fn consumeRemnantsBadURL(self: *Self) ParserErrorType!void {
        while (true) {
            if ((try self.consumeByte(')')) or self.end()) {
                break;
            } else {
                const b = try self.consumeEscape();
                if (!b) {
                    try self.advance();
                }
            }
        }
    }

    // consumeIdentlike consumes IdentToken, FunctionToken or UrlToken.
    fn consumeIdentLike(self: *Self) ParserErrorType!TokenType {
        if (try self.consumeIdentToken()) {
            if ((try self.current()) != '(') {
                return .IdentToken;
            }
            if (!self.equalFold(self.lexeme(), "url")) {
                try self.advance();
                return .FunctionToken;
            }
            try self.advance();

            // consume url
            while (try self.consumeWhitespace()) {}
            const c = try self.current();
            if (c == '"' or c == '\'') {
                if ((try self.consumeString()) == .BadStringToken) {
                    try self.consumeRemnantsBadURL();
                    return .BadURLToken;
                }
            } else {
                const isUnquotedUrl = try self.consumeUnquotedURL();
                const isWhiteSpace = try self.consumeWhitespace();
                if (!isUnquotedUrl and !isWhiteSpace) { // if unquoted URL fails due to encountering whitespace, continue
                    try self.consumeRemnantsBadURL();
                    return .BadURLToken;
                }
            }
            while (try self.consumeWhitespace()) {}
            if (!(try self.consumeByte(')')) and !self.end()) {
                try self.consumeRemnantsBadURL();
                return .BadURLToken;
            }
            return .URLToken;
        }
        return .ErrorToken;
    }

    fn lexeme(self: *Self) []const u8 {
        return self.code[self.start..self.cursor];
    }

    fn replaceBytes(s: []const u8, old: []const u8, new: []const u8, n: i32) []const u8 {
        _ = old;
        _ = new;
        _ = n;
        return s;
    }

    fn equalFold(self: *Self, s: []const u8, targetLower: []const u8) bool {
        const lxx = self.internal_allocator.alloc(u8, s.len) catch unreachable;
        std.mem.copy(u8, lxx, s);
        for (lxx) |_, i| {
            lxx[i] = std.ascii.toLower(s[i]);
        }
        if (lxx.len != targetLower.len) {
            return false;
        }
        for (targetLower) |c, i| {
            const d = lxx[i];
            if (d != c and (d < 'A' or d > 'Z' or (d + ('a' - 'A')) != c)) {
                return false;
            }
        }
        return true;
    }

    fn hexDigit(self: *Self) bool {
        const c = self.lookAhead();
        return (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') or (c >= 'A' or c <= 'F');
    }

    // returns the size of the rune
    fn peekRune(self: *Self) ParserErrorType!usize {
        // from unicode/utf8
        var c = try self.current();
        if (c < 0xC0 or (try self.lookSuperAhead()) == 0) {
            return 1;
        } else if (c < 0xE0 or (try self.lookSuperDuperAhead()) == 0) {
            return 2;
        } else if (c < 0xF0 or (try self.peek(3)) == 0) {
            return 3;
        }
        return 4;
    }

    /// look one character ahead
    fn current(self: *Self) ParserErrorType!u8 {
        return self.peek(0);
    }

    fn lookSuperAhead(self: *Self) ParserErrorType!u8 {
        return self.peek(1);
    }

    fn lookSuperDuperAhead(self: *Self) ParserErrorType!u8 {
        return self.peek(2);
    }

    fn peek(self: *Self, n: usize) ParserErrorType!u8 {
        const pos = n + self.cursor;
        if (self.code.len <= pos) {
            return 0;
        }
        return self.code[pos];
    }

    fn move(self: *Self, n: i32) ParserErrorType!void {
        const newPos: usize = @intCast(usize, @intCast(i32, self.cursor) + n);
        if (self.code.len < newPos) {
            return error.EOFError;
        }
        var i: usize = 0;
        while (i < n) : (i += 1) {
            self.end_col += 1;
            if (self.code[self.cursor + i] == '\n') {
                self.end_line += 1;
                self.end_col = 1;
            }
        }
        self.cursor = newPos;
    }

    fn advance(self: *Self) ParserErrorType!void {
        try self.move(1);
    }

    fn rewind(self: *Self, mark: usize) void {
        self.cursor = self.start + mark;
    }

    fn getMark(self: *Self) usize {
        return self.cursor - self.start;
    }

    fn end(self: *Self) bool {
        return self.cursor >= self.code.len;
    }

    pub fn addTokenAdvance(self: *Self, tok_type: TokenType, steps: usize) void {
        self.cursor += steps;
        self.addToken(tok_type);
    }

    fn addTok(self: *Self, tok_type: TokenType, start: usize, end_pos: usize) void {
        const tok = self.getTok(tok_type, start, end_pos);
        self.tokens.append(tok) catch unreachable;
    }

    fn getTok(self: *Self, tok_type: TokenType, start: usize, end_pos: usize) Token {
        return Token{
            .start = start + self.raw_text_offset,
            .end = end_pos + self.raw_text_offset,
            .start_line = self.start_line,
            .start_col = self.start_col,
            .end_line = self.end_line,
            .end_col = self.end_col,
            .tok_type = tok_type,
        };
    }

    fn getToken(self: *Self, tok_type: TokenType) Token {
        return self.getTok(tok_type, self.start, self.cursor);
    }

    pub fn addToken(self: *Self, tok_type: TokenType) void {
        self.tokens.append(self.getToken(tok_type)) catch unreachable;
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

const TokenTest = struct {
    css: []const u8,
    ttypes: []const TokenType,
    lexemes: []const ([]const u8),
};

test "Scanner" {
    const TT = TokenType;
    const WS = TT.WhitespaceToken;

    var tokenTests = [_]TokenTest{
        .{
            .css = "",
            .ttypes = &[_]TT{TT.EOF},
            .lexemes = &.{""}, // &[_][]const u8{}
        },
        .{
            .css = "5.2 .4",
            .ttypes = &[_]TT{ .NumberToken, .WhitespaceToken, .NumberToken, .EOF },
            .lexemes = &.{ "5.2", " ", ".4", "" },
        },
        .{
            .css = "color: red;",
            .ttypes = &[_]TT{ .IdentToken, .ColonToken, .WhitespaceToken, .IdentToken, .SemicolonToken, .EOF },
            .lexemes = &.{ "color", ":", " ", "red", ";", "" },
        },
        .{
            .css = "background: url(\"http://x\");",
            .ttypes = &[_]TT{ .IdentToken, .ColonToken, .WhitespaceToken, .URLToken, .SemicolonToken, .EOF },
            .lexemes = &.{ "background", ":", " ", "url(\"http://x\")", ";", "" },
        },
        .{
            .css = "background: URL(x.png);",
            .ttypes = &[_]TT{ .IdentToken, .ColonToken, .WhitespaceToken, .URLToken, .SemicolonToken, .EOF },
            .lexemes = &.{ "background", ":", " ", "URL(x.png)", ";", "" },
        },
        .{
            .css = "color: rgb(4, 0%, 5em);",
            .ttypes = &[_]TT{
                .IdentToken,
                .ColonToken,
                .WhitespaceToken,
                .FunctionToken,
                .NumberToken,
                .CommaToken,
                .WhitespaceToken,
                .PercentageToken,
                .CommaToken,
                .WhitespaceToken,
                .DimensionToken,
                .RightParenthesisToken,
                .SemicolonToken,
                .EOF,
            },
            .lexemes = &.{ "color", ":", " ", "rgb(", "4", ",", " ", "0%", ",", " ", "5em", ")", ";", "" },
        },
        .{
            .css = "body { \"string\" }",
            .ttypes = &[_]TT{ .IdentToken, .WhitespaceToken, .LeftBraceToken, .WhitespaceToken, .StringToken, .WhitespaceToken, .RightBraceToken, .EOF },
            .lexemes = &.{ "body", " ", "{", " ", "\"string\"", " ", "}", "" },
        },
        .{
            .css = "body { \"str\\\"ing\" }",
            .ttypes = &[_]TT{ .IdentToken, .WhitespaceToken, .LeftBraceToken, .WhitespaceToken, .StringToken, .WhitespaceToken, .RightBraceToken, .EOF },
            .lexemes = &.{ "body", " ", "{", " ", "\"str\\\"ing\"", " ", "}" },
        },
        .{
            .css = ".class { }",
            .ttypes = &[_]TT{ .DelimToken, .IdentToken, .WhitespaceToken, .LeftBraceToken, .WhitespaceToken, .RightBraceToken, .EOF },
            .lexemes = &.{ ".", "class", " ", "{", " ", "}", "" },
        },
        .{
            .css = "#class { }",
            .ttypes = &[_]TT{ .HashToken, .WhitespaceToken, .LeftBraceToken, .WhitespaceToken, .RightBraceToken, .EOF },
            .lexemes = &.{ "#class", " ", "{", " ", "}", "" },
        },
        .{
            .css = "#class\\#withhash { }",
            .ttypes = &[_]TT{ .HashToken, .WhitespaceToken, .LeftBraceToken, .WhitespaceToken, .RightBraceToken, .EOF },
            .lexemes = &.{ "#class\\#withhash", " ", "{", " ", "}", "" },
        },
        .{
            .css = "@media print { }",
            .ttypes = &[_]TT{ .AtKeywordToken, .WhitespaceToken, .IdentToken, .WhitespaceToken, .LeftBraceToken, .WhitespaceToken, .RightBraceToken, .EOF },
            .lexemes = &.{ "@media", " ", "print", " ", "{", " ", "}", "" },
        },
        .{
            .css = "/*comment*/",
            .ttypes = &[_]TT{ .CommentToken, .EOF },
            .lexemes = &.{ "/*comment*/", "" },
        },
        .{
            .css = "/*com* /ment*/",
            .ttypes = &[_]TT{ .CommentToken, .EOF },
            .lexemes = &.{ "/*com* /ment*/", "" },
        },
        .{
            .css = "~= |= ^= $= *=",
            .ttypes = &[_]TT{
                .IncludeMatchToken,
                .WhitespaceToken,
                .DashMatchToken,
                .WhitespaceToken,
                .PrefixMatchToken,
                .WhitespaceToken,
                .SuffixMatchToken,
                .WhitespaceToken,
                .SubstringMatchToken,
                .EOF,
            },
            .lexemes = &.{ "~=", " ", "|=", " ", "^=", " ", "$=", " ", "*=", "" },
        },
        .{
            .css = "||",
            .ttypes = &[_]TT{ .ColumnToken, .EOF },
            .lexemes = &.{ "||", "" },
        },
        .{
            .css = "<!-- Hello World -->",
            .ttypes = &[_]TT{ .CDOToken, WS, .IdentToken, WS, .IdentToken, WS, .CDCToken, .EOF },
            .lexemes = &.{ "<!--", " ", "Hello", " ", "World", " ", "-->", "" },
        },
        .{
            .css = "U+1234",
            .ttypes = &[_]TT{ .UnicodeRangeToken, .EOF },
            .lexemes = &.{ "U+1234", "" },
        },
        .{
            .css = "5.2 .4 4e-22",
            .ttypes = &[_]TT{ .NumberToken, .WhitespaceToken, .NumberToken, .WhitespaceToken, .NumberToken, .EOF },
            .lexemes = &.{ "5.2", " ", ".4", " ", "4e-22", "" },
        },
        .{
            .css = "--custom-variable",
            .ttypes = &[_]TT{ .CustomPropertyNameToken, .EOF },
            .lexemes = &.{ "--custom-variable", "" },
        },

        // unexpected ending
        .{
            .css = "ident",
            .ttypes = &[_]TT{ .IdentToken, .EOF },
            .lexemes = &.{ "ident", "" },
        },
        .{
            .css = "123.",
            .ttypes = &[_]TT{ .NumberToken, .DelimToken, .EOF },
            .lexemes = &.{ "123", ".", "" },
        },
        .{
            .css = "\"string",
            .ttypes = &[_]TT{ .StringToken, .EOF },
            .lexemes = &.{ "\"string", "" },
        },
        .{
            .css = "123/*comment",
            .ttypes = &[_]TT{ .NumberToken, .CommentToken, .EOF },
            .lexemes = &.{ "123", "/*comment", "" },
        },
        .{
            .css = "U+1-",
            .ttypes = &[_]TT{ .IdentToken, .NumberToken, .DelimToken, .EOF },
            .lexemes = &.{ "U", "+1", "-", "" },
        },

        // unicode
        .{
            .css = "fooδbar􀀀",
            .ttypes = &[_]TT{ .IdentToken, .EOF },
            .lexemes = &.{ "fooδbar􀀀", "" },
        },
        .{
            .css = "foo\\æ\\†",
            .ttypes = &[_]TT{ .IdentToken, .EOF },
            .lexemes = &.{ "foo\\æ\\†", "" },
        },
        .{
            .css = "'foo\u{554}abar'",
            .ttypes = &[_]TT{ .StringToken, .EOF },
            .lexemes = &.{ "'foo\u{554}abar'", "" },
        },
        .{
            .css = "\\000026B",
            .ttypes = &[_]TT{ .IdentToken, .EOF },
            .lexemes = &.{ "\\000026B", "" },
        },
        .{
            .css = "\\26 B",
            .ttypes = &[_]TT{ .IdentToken, .EOF },
            .lexemes = &.{ "\\26 B", "" },
        },

        // hacks
        // .{.css="\\-\\mo\\z\\-b\\i\\nd\\in\\g:\\url(//business\\i\\nfo.co.uk\\/labs\\/xbl\\/xbl\\.xml\\#xss);",.ttypes=&[_]TT{.IdentToken, .ColonToken, .URLToken, .SemicolonToken}, .lexemes=&.{`\-\mo\z\-b\i\nd\in\g`, ":", `\url(//business\i\nfo.co.uk\/labs\/xbl\/xbl\.xml\#xss)`, ";"}},
        .{
            .css = "width/**/:/**/ 40em;",
            .ttypes = &[_]TT{ .IdentToken, .CommentToken, .ColonToken, .CommentToken, .WhitespaceToken, .DimensionToken, .SemicolonToken, .EOF },
            .lexemes = &.{ "width", "/**/", ":", "/**/", " ", "40em", ";", "" },
        },
        .{
            .css = ":root *> #quince",
            .ttypes = &[_]TT{ .ColonToken, .IdentToken, .WhitespaceToken, .StarToken, .DelimToken, .WhitespaceToken, .HashToken, .EOF },
            .lexemes = &.{ ":", "root", " ", "*", ">", " ", "#quince", "" },
        },
        .{
            .css = "html[xmlns*=\"\"]:root",
            .ttypes = &[_]TT{ .IdentToken, .LeftBracketToken, .IdentToken, .SubstringMatchToken, .StringToken, .RightBracketToken, .ColonToken, .IdentToken, .EOF },
            .lexemes = &.{ "html", "[", "xmlns", "*=", "\"\"", "]", ":", "root", "" },
        },
        .{
            .css = "body:nth-of-type(1)",
            .ttypes = &[_]TT{ .IdentToken, .ColonToken, .FunctionToken, .NumberToken, .RightParenthesisToken, .EOF },
            .lexemes = &.{ "body", ":", "nth-of-type(", "1", ")", "" },
        },
        .{
            .css = "color/*\\**/: blue\\9;",
            .ttypes = &[_]TT{ .IdentToken, .CommentToken, .ColonToken, .WhitespaceToken, .IdentToken, .SemicolonToken, .EOF },
            .lexemes = &.{ "color", "/*\\**/", ":", " ", "blue\\9", ";", "" },
        },
        .{
            .css = "color: blue !ie;",
            .ttypes = &[_]TT{ .IdentToken, .ColonToken, .WhitespaceToken, .IdentToken, .WhitespaceToken, .DelimToken, .IdentToken, .SemicolonToken, .EOF },
            .lexemes = &.{ "color", ":", " ", "blue", " ", "!", "ie", ";", "" },
        },

        // escapes, null and replacement character
        .{
            .css = "c\\\x00olor: white;",
            .ttypes = &[_]TT{ .IdentToken, .ColonToken, .WhitespaceToken, .IdentToken, .SemicolonToken, .EOF },
            .lexemes = &.{ "c\\\x00olor", ":", " ", "white", ";", "" },
        },
        .{
            .css = "null\\0",
            .ttypes = &[_]TT{ .IdentToken, .EOF },
            .lexemes = &.{ "null\\0", "" },
        },
        .{
            .css = "\\",
            .ttypes = &[_]TT{ .DelimToken, .EOF },
            .lexemes = &.{ "\\", "" },
        },
        .{
            .css = "abc\\",
            .ttypes = &[_]TT{ .IdentToken, .DelimToken, .EOF },
            .lexemes = &.{ "abc", "\\", "" },
        },
        .{
            .css = "#\\",
            .ttypes = &[_]TT{ .DelimToken, .DelimToken, .EOF },
            .lexemes = &.{ "#", "\\", "" },
        },
        .{
            .css = "#abc\\",
            .ttypes = &[_]TT{ .HashToken, .DelimToken, .EOF },
            .lexemes = &.{ "#abc", "\\", "" },
        },
        .{
            .css = "\"abc\\",
            .ttypes = &[_]TT{ .StringToken, .EOF },
            .lexemes = &.{ "\"abc\\", "" },
        }, // should officially not include backslash, .lexemes,=&.
        .{
            .css = "url(abc\\",
            .ttypes = &[_]TT{ .BadURLToken, .EOF },
            .lexemes = &.{ "url(abc\\", "" },
        },
        .{
            .css = "\"a\x00b\"",
            .ttypes = &[_]TT{ .StringToken, .EOF },
            .lexemes = &.{ "\"a\x00b\"", "" },
        },
        .{
            .css = "a\\\x00b",
            .ttypes = &[_]TT{ .IdentToken, .EOF },
            .lexemes = &.{ "a\\\x00b", "" },
        },
        .{
            .css = "url(a\x00b)",
            .ttypes = &[_]TT{ .BadURLToken, .EOF },
            .lexemes = &.{ "url(a\x00b)", "" },
        }, // null character .lexemes,=&
        .{
            .css = "/*a\x00b*/",
            .ttypes = &[_]TT{ .CommentToken, .EOF },
            .lexemes = &.{ "/*a\x00b*/", "" },
        },

        // coverage
        .{
            .css = "  \n\r\n\r\"\\\r\n\\\r\"",
            .ttypes = &[_]TT{ .WhitespaceToken, .StringToken, .EOF },
            .lexemes = &.{ "  \n\r\n\r", "\"\\\r\n\\\r\"", "" },
        },
        .{
            .css = "U+?????? U+ABCD?? U+ABC-DEF",
            .ttypes = &[_]TT{ .UnicodeRangeToken, .WhitespaceToken, .UnicodeRangeToken, .WhitespaceToken, .UnicodeRangeToken, .EOF },
            .lexemes = &.{ "U+??????", " ", "U+ABCD??", " ", "U+ABC-DEF", "" },
        },
        .{
            .css = "U+? U+A?",
            .ttypes = &[_]TT{ .UnicodeRangeToken, .WhitespaceToken, .UnicodeRangeToken, .EOF },
            .lexemes = &.{ "U+?", " ", "U+A?", "" },
        },
        .{
            .css = "U+ U+ABCDEF?",
            .ttypes = &[_]TT{ .IdentToken, .DelimToken, .WhitespaceToken, .IdentToken, .DelimToken, .IdentToken, .DelimToken, .EOF },
            .lexemes = &.{ "U", "+", " ", "U", "+", "ABCDEF", "?", "" },
        },
        .{
            .css = "-5.23 -moz",
            .ttypes = &[_]TT{ .NumberToken, .WhitespaceToken, .IdentToken, .EOF },
            .lexemes = &.{ "-5.23", " ", "-moz", "" },
        },
        .{
            .css = "()",
            .ttypes = &[_]TT{ .LeftParenthesisToken, .RightParenthesisToken, .EOF },
            .lexemes = &.{ "(", ")", "" },
        },
        .{
            .css = "url( //url\n  )",
            .ttypes = &[_]TT{ .URLToken, .EOF },
            .lexemes = &.{ "url( //url\n  )", "" },
        },
        .{
            .css = "url( ",
            .ttypes = &[_]TT{ .URLToken, .EOF },
            .lexemes = &.{ "url( ", "" },
        },
        .{
            .css = "url( //url  ",
            .ttypes = &[_]TT{ .URLToken, .EOF },
            .lexemes = &.{ "url( //url  ", "" },
        },
        .{
            .css = "url(\")a",
            .ttypes = &[_]TT{ .URLToken, .EOF },
            .lexemes = &.{ "url(\")a", "" },
        },
        .{
            .css = "url(a'\\\n)a",
            .ttypes = &[_]TT{ .BadURLToken, .IdentToken, .EOF },
            .lexemes = &.{ "url(a'\\\n)", "a", "" },
        },
        .{
            .css = "url(\"\n)a",
            .ttypes = &[_]TT{ .BadURLToken, .IdentToken, .EOF },
            .lexemes = &.{ "url(\"\n)", "a", "" },
        },
        .{
            .css = "url(a h)a",
            .ttypes = &[_]TT{ .BadURLToken, .IdentToken, .EOF },
            .lexemes = &.{ "url(a h)", "a", "" },
        },
        .{
            .css = "a[x=y]{}",
            .ttypes = &[_]TT{
                TT.IdentToken,
                TT.LeftBracketToken,
                TT.IdentToken,
                TT.EqualToken,
                TT.IdentToken,
                TT.RightBracketToken,
                TT.LeftBraceToken,
                TT.RightBraceToken,
                TT.EOF,
            },
            .lexemes = &.{ "a", "[", "x", "=", "y", "]", "{", "}", "" },
        },
        .{
            .css = "<!- | @4 ## /2",
            .ttypes = &[_]TT{
                .DelimToken, // <
                .DelimToken, // !
                .DelimToken, // -
                .WhitespaceToken,
                .DelimToken, // |
                .WhitespaceToken,
                .DelimToken,
                .NumberToken,
                .WhitespaceToken,
                .DelimToken,
                .DelimToken,
                .WhitespaceToken,
                .DelimToken,
                .NumberToken,
                .EOF,
            },
            .lexemes = &.{ "<", "!", "-", " ", "|", " ", "@", "4", " ", "#", "#", " ", "/", "2" },
        },
        .{
            .css = "\"s\\\n\"",
            .ttypes = &[_]TT{ .StringToken, .EOF },
            .lexemes = &.{ "\"s\\\n\"", "" },
        },
        .{
            .css = "\"a\\\"b\"",
            .ttypes = &[_]TT{ .StringToken, .EOF },
            .lexemes = &.{ "\"a\\\"b\"", "" },
        },
        .{
            .css = "\"s\n",
            .ttypes = &[_]TT{ .BadStringToken, .EOF },
            .lexemes = &.{ "\"s\n", "" },
        },

        // small
        .{
            .css = "\"abcd",
            .ttypes = &[_]TT{ .StringToken, .EOF },
            .lexemes = &.{ "\"abcd", "" },
        },
        .{
            .css = "/*comment",
            .ttypes = &[_]TT{ .CommentToken, .EOF },
            .lexemes = &.{ "/*comment", "" },
        },
        .{
            .css = "U+A-B",
            .ttypes = &[_]TT{ .UnicodeRangeToken, .EOF },
            .lexemes = &.{ "U+A-B", "" },
        },
        .{
            .css = "url((",
            .ttypes = &[_]TT{ .BadURLToken, .EOF },
            .lexemes = &.{ "url((", "" },
        },
        .{
            .css = "id\u{554}a",
            .ttypes = &[_]TT{ .IdentToken, .EOF },
            .lexemes = &.{ "id\u{554}a", "" },
        },
    };

    const a = std.testing.allocator;
    var scanner = try a.create(Scanner);
    defer a.destroy(scanner);
    scanner.* = Scanner.init(a, undefined, undefined, undefined);
    defer scanner.deinit();

    const MAX = 61;

    for (tokenTests) |tokenTest, i| {
        if (i == MAX) {
            defer {
                scanner.cursor = 0;
                scanner.start = 0;
            }
            var tokens = std.ArrayList(Token).init(a);
            defer tokens.deinit();
            scanner.tokens = &tokens;
            scanner = Scanner.scan(scanner, tokenTest.css);

            var flag = true;
            for (scanner.tokens.items) |tok, j| {
                if (tok.tok_type != tokenTest.ttypes[j]) {
                    flag = false;
                }
            }

            for (tokenTest.lexemes) |lexeme, j| {
                if (!std.mem.eql(u8, lexeme, scanner.tokens.items[j].getCodePartOfToken(tokenTest.css))) {
                    flag = false;
                }
            }

            try expect(flag == true);
        }
    }
}
