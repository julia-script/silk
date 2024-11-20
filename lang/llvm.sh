#!/bin/sh

mkdir -p src/codegen/llvm

curl -o src/codegen/llvm/Builder.zig https://raw.githubusercontent.com/ziglang/zig/refs/heads/master/src/codegen/llvm/Builder.zig
curl -o src/codegen/llvm/BitcodeReader.zig https://raw.githubusercontent.com/ziglang/zig/refs/heads/master/src/codegen/llvm/BitcodeReader.zig
curl -o src/codegen/llvm/bitcode_writer.zig https://raw.githubusercontent.com/ziglang/zig/refs/heads/master/src/codegen/llvm/bitcode_writer.zig
curl -o src/codegen/llvm/ir.zig https://raw.githubusercontent.com/ziglang/zig/refs/heads/master/src/codegen/llvm/ir.zig

echo "Files downloaded and stored in src/codegen/llvm/"