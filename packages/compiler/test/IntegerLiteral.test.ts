import { assert, it } from '@effect/vitest'
import * as DigitSeparator from '../src/internal/DigitSeparator.js'
import * as IntegerLiteral from '../src/internal/IntegerLiteral.js'

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

it('selects a base only from a committed prefix', () => {
  assert.deepEqual(
    ['0xff', '0XFF', '0b1', '0B1', '0o7', '0O7', '0', '00', '0.5', '42', '0e2'].map(
      (spelling) => IntegerLiteral.recognize(spelling).radix,
    ),
    [16, 16, 2, 2, 8, 8, 10, 10, 10, 10, 10],
  )
  assert.strictEqual(IntegerLiteral.recognize(ascii('0xff')).width, 2)
  assert.strictEqual(IntegerLiteral.recognize(ascii('42')).width, 0)
})

it('accepts only the digits belonging to each base', () => {
  const digits = (base: IntegerLiteral.Base, candidates: string): string =>
    Array.from(candidates)
      .filter((character) => IntegerLiteral.isDigit(base, character.charCodeAt(0)))
      .join('')

  assert.strictEqual(digits(IntegerLiteral.binary, '0123456789abcdefABCDEF'), '01')
  assert.strictEqual(digits(IntegerLiteral.octal, '0123456789abcdefABCDEF'), '01234567')
  assert.strictEqual(digits(IntegerLiteral.decimal, '0123456789abcdefABCDEF'), '0123456789')
  assert.strictEqual(
    digits(IntegerLiteral.hexadecimal, '0123456789abcdefgABCDEFG'),
    '0123456789abcdefABCDEF',
  )
  assert.isFalse(IntegerLiteral.isDigit(IntegerLiteral.decimal, undefined))
})

it('reads the exact magnitude of every base without losing precision', () => {
  assert.strictEqual(IntegerLiteral.magnitude('0xff'), 255n)
  assert.strictEqual(IntegerLiteral.magnitude('0XFF'), 255n)
  assert.strictEqual(IntegerLiteral.magnitude(ascii('0b1010')), 10n)
  assert.strictEqual(IntegerLiteral.magnitude('0o777'), 511n)
  assert.strictEqual(IntegerLiteral.magnitude('0o644'), 420n)
  assert.strictEqual(IntegerLiteral.magnitude('255'), 255n)
  assert.strictEqual(IntegerLiteral.magnitude('0'), 0n)
  assert.strictEqual(IntegerLiteral.magnitude('0x0'), 0n)
  assert.strictEqual(IntegerLiteral.magnitude('0xffffffffffffffff'), 18446744073709551615n)
  assert.strictEqual(
    IntegerLiteral.magnitude('340282366920938463463374607431768211455'),
    340282366920938463463374607431768211455n,
  )
})

it('reads a separated literal as the value of its separator-free spelling', () => {
  assert.strictEqual(IntegerLiteral.magnitude('1_000'), IntegerLiteral.magnitude('1000'))
  assert.strictEqual(IntegerLiteral.magnitude('1_048_576'), 1048576n)
  assert.strictEqual(IntegerLiteral.magnitude(ascii('0b1010_0000')), 160n)
  assert.strictEqual(IntegerLiteral.magnitude('0xff_ff'), 65535n)
  assert.strictEqual(IntegerLiteral.magnitude('0o1_7'), 15n)
})

it('removes every separator from one literal spelling', () => {
  assert.strictEqual(DigitSeparator.strip('1_048_576'), '1048576')
  assert.strictEqual(DigitSeparator.strip(ascii('3.141_592')), '3.141592')
  assert.strictEqual(DigitSeparator.strip('1000'), '1000')
  assert.isTrue(DigitSeparator.isSeparator('_'.charCodeAt(0)))
  assert.isFalse(DigitSeparator.isSeparator(undefined))
})
