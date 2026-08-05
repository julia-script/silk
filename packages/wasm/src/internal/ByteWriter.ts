/**
 * Growable little-endian byte writer with the LEB128 and IEEE 754 encodings used by the
 * WebAssembly binary format.
 *
 * Raw imperative code: this is the per-byte hot loop of `Binary.encode`, kept behind the
 * effectful emitter API.
 *
 * @internal
 */
import * as Utf8 from './Utf8.js'

export class ByteWriter {
  private chunks: Array<number> = []

  get length(): number {
    return this.chunks.length
  }

  byte(value: number): this {
    this.chunks.push(value & 0xff)
    return this
  }

  raw(bytes: Uint8Array | ReadonlyArray<number>): this {
    for (const value of bytes) this.chunks.push(value & 0xff)
    return this
  }

  /** Unsigned LEB128. */
  u32(value: number): this {
    let remaining = value >>> 0
    for (;;) {
      const byte = remaining & 0x7f
      remaining >>>= 7
      if (remaining === 0) {
        this.chunks.push(byte)
        return this
      }
      this.chunks.push(byte | 0x80)
    }
  }

  /** Signed LEB128 over a JavaScript integer (fits 32-bit signed or unsigned inputs). */
  s32(value: number): this {
    return this.s64(BigInt(value | 0))
  }

  /** Signed LEB128 over a 33-bit value, used by block types. */
  s33(value: number): this {
    return this.s64(BigInt(value))
  }

  /** Unsigned LEB128 over a 64-bit value. */
  u64(value: bigint): this {
    let remaining = BigInt.asUintN(64, value)
    for (;;) {
      const byte = Number(remaining & 0x7fn)
      remaining >>= 7n
      if (remaining === 0n) {
        this.chunks.push(byte)
        return this
      }
      this.chunks.push(byte | 0x80)
    }
  }

  /** Signed LEB128 over a 64-bit value. */
  s64(value: bigint): this {
    let remaining = BigInt.asIntN(64, value)
    for (;;) {
      const byte = Number(remaining & 0x7fn)
      remaining >>= 7n
      const done =
        (remaining === 0n && (byte & 0x40) === 0) || (remaining === -1n && (byte & 0x40) !== 0)
      if (done) {
        this.chunks.push(byte)
        return this
      }
      this.chunks.push(byte | 0x80)
    }
  }

  f32(value: number): this {
    const view = new DataView(new ArrayBuffer(4))
    view.setFloat32(0, Math.fround(value), true)
    for (let index = 0; index < 4; index += 1) this.chunks.push(view.getUint8(index))
    return this
  }

  f64(value: number): this {
    const view = new DataView(new ArrayBuffer(8))
    view.setFloat64(0, value, true)
    for (let index = 0; index < 8; index += 1) this.chunks.push(view.getUint8(index))
    return this
  }

  /** A UTF-8 name prefixed with its byte length. */
  name(value: string): this {
    const bytes = Utf8.encode(value)
    this.u32(bytes.length)
    return this.raw(bytes)
  }

  /** The contents of another writer prefixed with their byte length. */
  sized(contents: ByteWriter): this {
    this.u32(contents.length)
    return this.raw(contents.chunks)
  }

  toUint8Array(): Uint8Array {
    return Uint8Array.from(this.chunks)
  }
}
