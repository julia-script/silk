"""Opt-in independent fixture verification; see inflate-vectors.md."""

import json
import pathlib
import struct
import subprocess
import sys
import tempfile
import zlib

REVISION = "e2214d401a59e91537838cc16eba82454044044f"
checkout = pathlib.Path(sys.argv[1]).resolve()
revision = subprocess.check_output(
    ["git", "-C", str(checkout), "rev-parse", "HEAD"], text=True
).strip()
assert revision == REVISION, (revision, REVISION)
assert not subprocess.check_output(
    ["git", "-C", str(checkout), "status", "--porcelain"], text=True
).strip(), "Oracle checkout must be clean"
vectors = json.loads(pathlib.Path(__file__).with_name("inflate-vectors.json").read_text())["vectors"]


def gzip_payloads(encoded):
    """Check every wrapper field/checksum; yield each independently decoded payload."""
    while encoded:
        assert encoded[:3] == b"\x1f\x8b\x08"
        flags = encoded[3]
        assert flags & 224 == 0
        offset = 10
        if flags & 4:
            size = int.from_bytes(encoded[offset : offset + 2], "little")
            offset += 2 + size
        for flag in [8, 16]:
            if flags & flag:
                offset = encoded.index(0, offset) + 1
        if flags & 2:
            assert int.from_bytes(encoded[offset : offset + 2], "little") == zlib.crc32(encoded[:offset]) & 65535
            offset += 2
        decoder = zlib.decompressobj(-15)
        output = decoder.decompress(encoded[offset:])
        assert decoder.eof
        payload_size = len(encoded) - offset - len(decoder.unused_data)
        trailer = decoder.unused_data
        assert len(trailer) >= 8
        assert struct.unpack("<II", trailer[:8]) == (zlib.crc32(output), len(output) & 0xFFFFFFFF)
        yield encoded[offset : offset + payload_size], output
        encoded = trailer[8:]


with tempfile.TemporaryDirectory(prefix="silk-inflate-oracle-") as temporary:
    project = pathlib.Path(temporary)
    (project / "src").mkdir()
    (project / "Cargo.toml").write_text(
        '[package]\nname = "silk-inflate-oracle"\nversion = "0.0.0"\nedition = "2021"\n'
        '[dependencies]\nminiz_oxide = { path = '
        + json.dumps(str(checkout / "miniz_oxide"))
        + ' }\nadler2 = "=2.0.1"\n'
    )
    (project / "src/main.rs").write_text('''
use std::{env, fs};
use miniz_oxide::inflate::{decompress_to_vec, decompress_to_vec_zlib};
fn main() {
    let args: Vec<String> = env::args().collect();
    let input = fs::read(&args[2]).unwrap();
    let output = if args[1] == "Zlib" {
        decompress_to_vec_zlib(&input).unwrap()
    } else {
        decompress_to_vec(&input).unwrap()
    };
    assert_eq!(output, fs::read(&args[3]).unwrap());
}
''')
    subprocess.run(["cargo", "build", "--quiet", "--manifest-path", str(project / "Cargo.toml")], check=True)
    executable = project / "target/debug/silk-inflate-oracle"
    for vector in vectors:
        encoded = bytes(vector["compressed"])
        if "expected" in vector:
            expected = bytes(vector["expected"])
        else:
            pattern = bytes(vector["expectedPattern"])
            length = vector["expectedLength"]
            expected = (pattern * ((length + len(pattern) - 1) // len(pattern)))[:length]
        fmt = vector["format"]
        if fmt == "Gzip":
            payloads = list(gzip_payloads(encoded))
            assert b"".join(output for _, output in payloads) == expected
        else:
            assert zlib.decompress(encoded, 15 if fmt == "Zlib" else -15) == expected
            payloads = [(encoded, expected)]
        for payload, output in payloads:
            (project / "input").write_bytes(payload)
            (project / "expected").write_bytes(output)
            subprocess.run([str(executable), fmt, str(project / "input"), str(project / "expected")], check=True)
        print(f"verified {vector['name']}: {len(encoded)} compressed -> {len(expected)} decoded")
print(f"All {len(vectors)} vectors verified with miniz_oxide {REVISION} and zlib {zlib.ZLIB_RUNTIME_VERSION}")
