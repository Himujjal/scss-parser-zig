const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("./token.zig").Token;

// ToLower converts all characters in the byte slice from A-Z to a-z.
pub fn toLower(allocator: std.mem.Allocator, src: []const u8) []const u8 {
    var newData: []u8 = allocator.alloc(u8, src.len) catch unreachable;
    for (src) |c, i| {
        newData[i] = std.ascii.toLower(c);
    }
    return src;
}

pub fn indexByte(b: []const u8, c: u8) i32 {
    var index: i32 = -1;
    for (b) |_c, i| {
        if (c == _c) index = @intCast(i32, i);
    }
    return index;
}

pub fn getHexFromTable(h: usize) usize {
    return switch (h) {
        0x1 => 0x2604, // page
        0x2 => 0x2105, // media
        0x3 => 0x809, // font-face
        0x5 => 0x1109, // keyframes
        0x6 => 0x1908, // supports
        0x7 => 0x8, // document
        else => 0x00,
    };
}

const _Hash_hash0 = 0x9acb0442;
const _Hash_maxLen = 9;
const _Hash_text = "documentfont-facekeyframesupportsmediapage";
const _Hash_table_len = 7;

pub fn toHash(s: []const u8) i32 {
    if (s.len == 0 or s.len > _Hash_maxLen) {
        return 0;
    }
    var h = @intCast(u32, _Hash_hash0);
    var i: usize = 0;
    while (i < s.len) : (i += 1) {
        h ^= @intCast(u32, s[i]);
        h *= 16777619;
    }
    var h2 = getHexFromTable(h & @intCast(u32, _Hash_table_len - 1));
    NEXT: {
        if (@intCast(i32, h2 & 0xff) == s.len) {
            const start = h2 >> 8;
            const end = start + h2 & 0xff;
            var t = _Hash_text[start..end];
            var j: usize = 0;
            while (j < s.len) : (j += 1) {
                if (t[j] != s[j]) {
                    break :NEXT;
                }
            }
            return @intCast(i32, j);
        }
    }
    {
        var k = getHexFromTable((h >> 16) & @intCast(i32, _Hash_table_len - 1));
        if (@intCast(i32, k & 0xff) == s.len) {
            const start = k >> 8;
            const end = start + k & 0xff;
            var t = _Hash_text[start..end];
            var l: usize = 0;
            while (l < s.len) : (l += 1) {
                if (t[l] != s[l]) {
                    return 0;
                }
            }
            return @intCast(i32, l);
        }
    }
    return 0;
}

pub fn concatStrings(_a: Allocator, a: []const u8, b: []const u8) []const u8 {
    const result = _a.alloc(u8, a.len + b.len) catch unreachable;
    std.mem.copy(u8, result, a);
    std.mem.copy(u8, result[a.len..], b);
    return result;
}
