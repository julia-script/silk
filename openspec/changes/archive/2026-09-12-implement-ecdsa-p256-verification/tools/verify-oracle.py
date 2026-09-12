"""Opt-in reproduction against an exact Zig arithmetic source revision; no network access."""
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import re
import zipfile

revision = 'e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa'
checkout = Path(sys.argv[1]).resolve()
zig = sys.argv[2] if len(sys.argv) > 2 else 'zig'
actual = subprocess.check_output(['git', '-C', str(checkout), 'rev-parse', 'HEAD'], text=True).strip()
if actual != revision:
    raise SystemExit(f'Expected Zig {revision}, found {actual}')
fixture = json.loads((Path(__file__).parent.parent / 'fixtures.json').read_text())
if len(sys.argv) > 3:
    archive = Path(sys.argv[3])
    assert hashlib.sha256(archive.read_bytes()).hexdigest() == fixture['archiveSha256']
    with zipfile.ZipFile(archive) as zipped:
        member = zipped.read(fixture['member'])
    assert hashlib.sha256(member).hexdigest() == fixture['memberSha256']
    section = member.decode().split('[' + fixture['section'] + ']')[1].split('\n[')[0]
    records = []
    for block in re.split(r'\n\s*\n', section.strip()):
        record = dict(re.findall(r'^(Msg|Qx|Qy|R|S|Result) = (.+)\r?$', block, re.M))
        if record:
            records.append({key: value.strip() for key, value in record.items()})
    for selected in fixture['cases']:
        assert all(records[selected['index']][key] == selected[key] for key in records[selected['index']])
source = checkout / 'lib/std/crypto/pcurves'
# Copy exact arithmetic source; use the installed compiler's standard runtime. The whole pinned
# stdlib cannot be mixed with a different compiler's builtin calling-convention enum.
paths = ['p256.zig', 'common.zig', 'p256/field.zig', 'p256/scalar.zig',
         'p256/p256_64.zig', 'p256/p256_scalar_64.zig', 'tests/p256.zig']
for name in paths:
    expected = fixture['oracle']['sourceSha256'][name]
    if hashlib.sha256((source / name).read_bytes()).hexdigest() != expected:
        raise SystemExit(f'Uncommitted source mismatch: {name}')
ecdsa = checkout / 'lib/std/crypto/ecdsa.zig'
if hashlib.sha256(ecdsa.read_bytes()).hexdigest() != fixture['oracle']['ecdsaSha256']:
    raise SystemExit('Uncommitted ecdsa.zig source mismatch')
# Independently check the synthetic equation construction without implementing ECDSA here.
prime = int('ffffffff00000001000000000000000000000000ffffffffffffffffffffffff', 16)
order = int('ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551', 16)
curve_b = int('5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b', 16)
edge = fixture['equationCases'][0]
point = bytes.fromhex(edge['publicKey'])
x, y = int.from_bytes(point[1:33], 'big'), int.from_bytes(point[33:], 'big')
assert point[0] == 4 and order <= x < prime and y < prime
assert (y*y - x*x*x + 3*x - curve_b) % prime == 0
assert int(edge['digest'], 16) == 0
# Fixture r=s=x-n is one octet: u1=0, u2=1, so R=Q and r=x(R) mod n.
r = x - order
assert 0 < r < 128 and edge['signature'] == bytes([48, 6, 2, 1, r, 2, 1, r]).hex()
identity = fixture['equationCases'][1]
assert int(identity['digest'], 16) == order - 1
assert identity['signature'] == '3006020101020101'
with tempfile.TemporaryDirectory(prefix='silk-ecdsa-oracle-') as temporary:
    root = Path(temporary)
    for name in paths:
        target = root / 'pcurves' / name
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(source / name, target)
    shutil.copyfile(ecdsa, root / 'ecdsa.zig')
    program = '''const std = @import("std");
const P = @import("pcurves/p256.zig").P256;
const E = @import("ecdsa.zig").Ecdsa(P, std.crypto.hash.sha2.Sha256);
fn bytes(comptime n: usize, comptime hex: []const u8) [n]u8 {
    var out: [n]u8 = undefined;
    _ = std.fmt.hexToBytes(&out, hex) catch unreachable;
    return out;
}
'''
    alternate = dict(fixture['cases'][0], signature=fixture['highSAlternative'], index='3 alternate s')
    for vector in [*fixture['cases'], alternate]:
        call = 'try signature.verify(&message, key);' if vector['Result'].startswith('P') else 'try std.testing.expectError(error.SignatureVerificationFailed, signature.verify(&message, key));'
        program += f'''test "NIST case {vector['index']}" {{
    const key = try E.PublicKey.fromSec1(&bytes(65, "{vector['publicKey']}"));
    const signature = try E.Signature.fromDer(&bytes({len(vector['signature'])//2}, "{vector['signature']}"));
    const message = bytes({len(vector['Msg'])//2}, "{vector['Msg']}");
    {call}
}}
'''
    program += f'''test "identity fixture uses generator" {{
    try std.testing.expectEqualSlices(u8, &P.basePoint.toUncompressedSec1(), &bytes(65, "{identity['publicKey']}"));
}}
'''
    (root / 'oracle.zig').write_text(program)
    subprocess.run([zig, 'version'], check=True)
    subprocess.run([zig, 'test', str(root / 'oracle.zig'), '-O', 'ReleaseFast',
                    '--test-filter', 'oracle.test'], check=True)
