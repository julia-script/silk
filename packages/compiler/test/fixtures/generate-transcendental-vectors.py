"""Regenerate transcendental-vectors.json from an independent arbitrary-precision oracle."""

import json
import struct
import subprocess

INPUTS = {
    32: [
        0x3F000000,
        0x3F800000,
        0xBF800000,
        0x3F490FDB,
        0x3FC90FDB,
        0x40490FDB,
        0x447A0000,
        0x49742400,
        0x00000001,
    ],
    64: [
        0x3FE0000000000000,
        0x3FF0000000000000,
        0xBFF0000000000000,
        0x3FE921FB54442D18,
        0x3FF921FB54442D18,
        0x400921FB54442D18,
        0x408F400000000000,
        0x412E848000000000,
        0x426D1A94A2000000,
        0x0000000000000001,
    ],
}


def decode(width, bits):
    return struct.unpack("<f" if width == 32 else "<d", struct.pack("<I" if width == 32 else "<Q", bits))[0]


def encode(width, value):
    return struct.unpack("<I" if width == 32 else "<Q", struct.pack("<f" if width == 32 else "<d", value))[0]


vectors = []
for width, inputs in INPUTS.items():
    for input_bits in inputs:
        numerator, denominator = decode(width, input_bits).as_integer_ratio()
        for operation, function in (("Sin", "s"), ("Cos", "c")):
            expression = f"scale=1200; {function}(({numerator})/({denominator}))\n"
            completed = subprocess.run(
                ["/usr/bin/bc", "-l"],
                input=expression,
                text=True,
                capture_output=True,
                check=True,
            )
            decimal = completed.stdout.replace("\\\n", "").strip()
            reference_bits = encode(width, float(decimal))
            vectors.append(
                {
                    "width": width,
                    "inputBits": f"0x{input_bits:0{width // 4}x}",
                    "operation": operation,
                    "referenceBits": f"0x{reference_bits:0{width // 4}x}",
                }
            )

print(
    json.dumps(
        {
            "generator": "generate-transcendental-vectors.py using bc -l at scale 1200 from each exact IEEE input ratio",
            "vectors": vectors,
        },
        indent=2,
    )
)
