"""Opt-in fixture verification. Run with the exact Zig checkout and a compatible Zig binary on PATH.

python3 openspec/changes/archive/2026-09-12-implement-rsa-sha256-verification/verify-oracle.py /path/to/zig-source
Expected values are committed. No network or private key is used by this verifier.
"""
import hashlib
import json
from pathlib import Path
import subprocess
import sys
import tempfile

from cryptography.exceptions import InvalidSignature
from cryptography.hazmat.primitives.asymmetric import rsa, padding
from cryptography.hazmat.primitives import hashes

here = Path(__file__).resolve().parent
vectors = json.loads((here / 'fixtures.json').read_text())
nist = json.loads((here / 'nist-encoding-fixtures.json').read_text())
for vector in vectors + nist:
    decode = lambda name: bytes.fromhex(vector[name])
    n, e = int(vector['modulus'], 16), int(vector['exponent'], 16)
    assert n.bit_length() == vector['bits'], vector['name']
    assert pow(int(vector['signature'], 16), e, n).to_bytes((n.bit_length()+7)//8, 'big') == decode('encoded')
    key = rsa.RSAPublicNumbers(e, n).public_key()
    pad = padding.PSS(mgf=padding.MGF1(hashes.SHA256()), salt_length=32) if vector['mode'] == 'pss' else padding.PKCS1v15()
    valid = True
    try:
        key.verify(decode('signature'), decode('message'), pad, hashes.SHA256())
    except InvalidSignature:
        valid = False
    assert valid == vector['valid'], vector['name']
print('Independent OpenSSL verification and recovered encodings passed')
for name in ['fixtures.json', 'nist-encoding-fixtures.json']:
    print(name, hashlib.sha256((here / name).read_bytes()).hexdigest())

if len(sys.argv) > 1:
    root = Path(sys.argv[1]).resolve()
    revision = subprocess.check_output(['git', '-C', str(root), 'rev-parse', 'HEAD'], text=True).strip()
    assert revision == 'e78ea8f2cb3677c0a104319b8aa5e37ea64d9cfa'
    crypto = root / 'lib/std/crypto'
    certificate = (crypto / 'Certificate.zig').read_text()
    source = certificate[certificate.index('pub const rsa = struct {'):]
    # Only the module import changes: the RSA and ff algorithms are the pinned source.
    source = source.replace('std.crypto.ff', '@import("ff.zig")')
    source = 'const std=@import("std"); const crypto=std.crypto; const der=std.crypto.Certificate.der;\n' + source
    literal = lambda value: '[_]u8{' + ','.join(str(b) for b in bytes.fromhex(value)) + '}'
    program = ['const std=@import("std"); test "selected RSA known answers" {']
    for vector in vectors:
        # Pinned PSS assumes emLen==k; its assertion excludes the valid 2049-bit boundary.
        if vector['bits'] == 2049 and vector['mode'] == 'pss':
            continue
        op = 'PSSSignature' if vector['mode'] == 'pss' else 'PKCS1v1_5Signature'
        program.append('{')
        for var, field in [('n','modulus'), ('e','exponent'), ('sig','signature'), ('msg','message')]:
            program.append('const '+var+'='+literal(vector[field])+';')
        program.append('const key=try @import("rsa.zig").rsa.PublicKey.fromBytes(&e,&n);')
        # concatVerify avoids the pinned PSS convenience wrapper's value/pointer mismatch.
        program.append('try @import("rsa.zig").rsa.'+op+'.concatVerify(sig.len,&sig,&.{&msg},key,std.crypto.hash.sha2.Sha256);')
        program.append('}')
    program.append('}')
    with tempfile.TemporaryDirectory(prefix='rsa-oracle-') as temporary:
        path = Path(temporary)
        (path/'rsa.zig').write_text(source)
        (path/'ff.zig').write_bytes((crypto/'ff.zig').read_bytes())
        (path/'main.zig').write_text('\n'.join(program))
        subprocess.run(['zig', 'test', str(path/'main.zig'), '-O', 'ReleaseSafe'], check=True)
