# CSS Parser in Zig

A high-performance CSS Parser in Zig optimized for performance rather than memory.

## Features

- A Parser that parses CSS adhering to full compilance to CSSTree
- Written in Zig for optimum performance and small size (future: WebAssembly)
- Easy to make high level bindings for whoever wishes to make something out of this using a
    serializable AST binary format or the CSSTree JSON format.

## Projects using this:

1. SCSS/SASS compiler in Zig
2. Svelte compiler in Zig

## AST Binary format

To make it easy to transfer the parsed AST to other languages/runtimes (so that we don't have
to worry about their memory management), The Nodes of the AST can be easily serialized in the 
form of a bytecode array (`[]const u8`).

To know more about the bytecode array, follow the guide: [Serializing_Deserializing_AST](./AST_SERIALIZE.md)

## TODO

- [ ] All test cases from [csstree](https://github.com/csstree/csstree)
- [ ] Error Handling - Errors as enum and the generated error strings according to each enum
