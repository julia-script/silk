"""Opt-in reproduction against an exact Zig arithmetic source revision; no network access."""
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

revision = 'e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa'
checkout = Path(sys.argv[1]).resolve()
zig = sys.argv[2] if len(sys.argv) > 2 else 'zig'
actual = subprocess.check_output(['git', '-C', str(checkout), 'rev-parse', 'HEAD'], text=True).strip()
if actual != revision:
    raise SystemExit(f'Expected Zig {revision}, found {actual}')
fixture = json.loads((Path(__file__).parent.parent / 'fixtures.json').read_text())
source = checkout / 'lib/std/crypto/pcurves'
# Copy exact arithmetic source; use the installed compiler's standard runtime. The whole pinned
# stdlib cannot be mixed with a different compiler's builtin calling-convention enum.
paths = ['p256.zig', 'common.zig', 'p256/field.zig', 'p256/scalar.zig',
         'p256/p256_64.zig', 'p256/p256_scalar_64.zig', 'tests/p256.zig']
for name in paths:
    expected = fixture['oracle']['sourceSha256'][name]
    if hashlib.sha256((source / name).read_bytes()).hexdigest() != expected:
        raise SystemExit(f'Uncommitted source mismatch: {name}')
with tempfile.TemporaryDirectory(prefix='silk-p256-oracle-') as temporary:
    root = Path(temporary)
    for name in paths:
        target = root / 'pcurves' / name
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(source / name, target)
    program = '''const std = @import("std");
const P = @import("pcurves/p256.zig").P256;
fn bytes(comptime n: usize, comptime hex: []const u8) [n]u8 {
    var out: [n]u8 = undefined;
    _ = std.fmt.hexToBytes(&out, hex) catch unreachable;
    return out;
}
'''
    for vector in fixture['vectors']:
        program += f'''test "{vector['id']}" {{
    const scalar = bytes(32, "{vector['scalar']}");
    const public = (try P.basePoint.mul(scalar, .big)).toUncompressedSec1();
    try std.testing.expectEqualSlices(u8, &bytes(65, "{vector['publicKey']}"), &public);
    const peer = try P.fromSec1(&bytes(65, "{vector['peer']}"));
    const shared = (try peer.mul(scalar, .big)).toUncompressedSec1();
    try std.testing.expectEqualSlices(u8, &bytes(32, "{vector['shared']}"), shared[1..33]);
}}
'''
    (root / 'oracle.zig').write_text(program)
    subprocess.run([zig, 'version'], check=True)
    subprocess.run([zig, 'test', str(root / 'oracle.zig'), '-O', 'ReleaseFast',
                    '--test-filter', 'oracle.test'], check=True)
