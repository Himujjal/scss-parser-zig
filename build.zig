const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    // Standard release options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall.
    const mode = b.standardReleaseOptions();

    const lib = b.addStaticLibrary("css-parser-zig", "src/main.zig");
    lib.setBuildMode(mode);
    lib.install();

    const lib2 = b.addSharedLibrary("css-parser-zig", "src/main.zig", .unversioned);
    lib2.setBuildMode(mode);
    lib2.setTarget(.{ .cpu_arch = .wasm32, .os_tag = .wasi });
    lib2.install();

    const main_tests = b.addTest("src/main.zig");
    main_tests.setBuildMode(mode);

	const json_tests = b.addTest("tests/test_main.zig");
	json_tests.main_pkg_path = ".";
	json_tests.addIncludePath("src");
	json_tests.setBuildMode(mode);

    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&main_tests.step);
	test_step.dependOn(&json_tests.step);
}
