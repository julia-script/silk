"""Opt-in independent oracle check; never runs in the correctness suite.

python3 -m venv /tmp/chacha-oracle
/tmp/chacha-oracle/bin/pip install cryptography==48.0.0
/tmp/chacha-oracle/bin/python packages/compiler/test/fixtures/verify-chacha20-poly1305.py

Recorded with CPython 3.9.6, cryptography 48.0.0, OpenSSL 4.0.0 (14 Apr 2026).
The committed inputs/outputs, rather than generated host results, drive Silk tests.
"""
import hashlib
import json
from pathlib import Path

import cryptography
from cryptography.hazmat.backends.openssl.backend import backend
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms
from cryptography.hazmat.primitives.ciphers.aead import ChaCha20Poly1305
from cryptography.hazmat.primitives.poly1305 import Poly1305

path = Path(__file__).with_name("chacha20-poly1305.json")
fixture = json.loads(path.read_text())
for vector in fixture["aead"]:
    decode = lambda field: bytes.fromhex(vector[field])
    sealed = ChaCha20Poly1305(decode("key")).encrypt(
        decode("nonce"), decode("plaintext"), decode("aad")
    )
    assert sealed == decode("ciphertext") + decode("tag"), vector["name"]
for vector in fixture["poly"]:
    assert Poly1305.generate_tag(bytes.fromhex(vector["key"]), bytes.fromhex(vector["message"])) == bytes.fromhex(vector["tag"]), vector["name"]
vector = fixture["block"]
# Original ChaCha20 and IETF ChaCha20 have identical initial words with this 16-byte
# counter/nonce layout. We request only one block, so counter extension cannot differ.
encryptor = Cipher(algorithms.ChaCha20(bytes.fromhex(vector["key"]), vector["counter"].to_bytes(4, "little") + bytes.fromhex(vector["nonce"])), mode=None).encryptor()
assert encryptor.update(bytes(64)) == bytes.fromhex(vector["output"])
print("cryptography", cryptography.__version__, backend.openssl_version_text())
print("sha256", hashlib.sha256(path.read_bytes()).hexdigest())

# Optional pinned-source Zig cross-check, evaluated at compile time so it does not depend
# on current host runtime interfaces. Pass the root of the pinned Zig checkout as argument 1.
import subprocess
import sys
import tempfile

if len(sys.argv) > 1:
    zig_root = Path(sys.argv[1]).resolve()
    revision = subprocess.check_output(["git", "-C", str(zig_root), "rev-parse", "HEAD"], text=True).strip()
    assert revision == "e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa", revision
    literal = lambda value: "[_]u8{" + ",".join(str(byte) for byte in bytes.fromhex(value)) + "}"
    blocks = ['const std = @import("std");', 'comptime { @setEvalBranchQuota(10000000);']
    for vector in fixture["aead"]:
        blocks.append("{" + f'''
const key = {literal(vector['key'])};
const nonce = {literal(vector['nonce'])};
const aad = {literal(vector['aad'])};
const message = {literal(vector['plaintext'])};
const expected = {literal(vector['ciphertext'])};
const expected_tag = {literal(vector['tag'])};
var ciphertext: [message.len]u8 = undefined;
var tag: [16]u8 = undefined;
std.crypto.aead.chacha_poly.ChaCha20Poly1305.encrypt(&ciphertext, &tag, &message, &aad, nonce, key);
if (!std.mem.eql(u8, &ciphertext, &expected) or !std.mem.eql(u8, &tag, &expected_tag)) @compileError("{vector['name']}");
var opened: [message.len]u8 = undefined;
std.crypto.aead.chacha_poly.ChaCha20Poly1305.decrypt(&opened, &ciphertext, tag, &aad, nonce, key) catch @compileError("decrypt");
if (!std.mem.eql(u8, &opened, &message)) @compileError("plaintext");
''' + "}")
    for vector in fixture["poly"]:
        blocks.append("{" + f'''
const key = {literal(vector['key'])};
const message = {literal(vector['message'])};
const expected = {literal(vector['tag'])};
var tag: [16]u8 = undefined;
std.crypto.onetimeauth.Poly1305.create(&tag, &message, &key);
if (!std.mem.eql(u8, &tag, &expected)) @compileError("{vector['name']}");
''' + "}")
    vector = fixture["block"]
    blocks.append("{" + f'''
const key = {literal(vector['key'])};
const nonce = {literal(vector['nonce'])};
const expected = {literal(vector['output'])};
var output: [64]u8 = undefined;
std.crypto.stream.chacha.ChaCha20IETF.stream(&output, {vector['counter']}, key, nonce);
if (!std.mem.eql(u8, &output, &expected)) @compileError("block");
''' + "}")
    blocks.append("}")
    with tempfile.TemporaryDirectory(prefix="chacha-oracle-") as temporary:
        source = Path(temporary) / "oracle.zig"
        source.write_text("\n".join(blocks))
        subprocess.run(["zig", "build-obj", "--zig-lib-dir", str(zig_root / "lib"), str(source), "-fno-emit-bin"], check=True)
    print("Zig source", revision, "compiler", subprocess.check_output(["zig", "version"], text=True).strip())
