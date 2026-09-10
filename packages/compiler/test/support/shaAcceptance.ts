interface ShaVector {
  readonly actor: string
  readonly inputLength: number
  readonly expected: string
}

const knownAnswerVectors: ReadonlyArray<ShaVector> = [
  { actor: 'Sha1', inputLength: 0, expected: 'da39a3ee5e6b4b0d3255bfef95601890afd80709' },
  { actor: 'Sha1', inputLength: 3, expected: 'a9993e364706816aba3e25717850c26c9cd0d89d' },
  {
    actor: 'Sha224',
    inputLength: 0,
    expected: 'd14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f',
  },
  {
    actor: 'Sha224',
    inputLength: 3,
    expected: '23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7',
  },
  {
    actor: 'Sha256',
    inputLength: 0,
    expected: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
  },
  {
    actor: 'Sha256',
    inputLength: 3,
    expected: 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad',
  },
  {
    actor: 'Sha384',
    inputLength: 0,
    expected:
      '38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b',
  },
  {
    actor: 'Sha384',
    inputLength: 3,
    expected:
      'cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7',
  },
  {
    actor: 'Sha512',
    inputLength: 0,
    expected:
      'cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e',
  },
  {
    actor: 'Sha512',
    inputLength: 3,
    expected:
      'ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f',
  },
  {
    actor: 'Sha512_224',
    inputLength: 0,
    expected: '6ed0dd02806fa89e25de060c19d3ac86cabb87d6a0ddd05c333b84f4',
  },
  {
    actor: 'Sha512_224',
    inputLength: 3,
    expected: '4634270f707b6a54daae7530460842e20e37ed265ceee9a43e8924aa',
  },
  {
    actor: 'Sha512_256',
    inputLength: 0,
    expected: 'c672b8d1ef56ed28ab87c3622c5114069bdd3ad7b8f9737498d0c01ecef0967a',
  },
  {
    actor: 'Sha512_256',
    inputLength: 3,
    expected: '53048e2681941ef99b2e29b76b4c7dabe4c2d0c634fc6d46e0e2f13107e7af23',
  },
  {
    actor: 'Sha3_224',
    inputLength: 0,
    expected: '6b4e03423667dbb73b6e15454f0eb1abd4597f9a1b078e3f5b5a6bc7',
  },
  {
    actor: 'Sha3_224',
    inputLength: 3,
    expected: 'e642824c3f8cf24ad09234ee7d3c766fc9a3a5168d0c94ad73b46fdf',
  },
  {
    actor: 'Sha3_256',
    inputLength: 0,
    expected: 'a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a',
  },
  {
    actor: 'Sha3_256',
    inputLength: 3,
    expected: '3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532',
  },
  {
    actor: 'Sha3_384',
    inputLength: 0,
    expected:
      '0c63a75b845e4f7d01107d852e4c2485c51a50aaaa94fc61995e71bbee983a2ac3713831264adb47fb6bd1e058d5f004',
  },
  {
    actor: 'Sha3_384',
    inputLength: 3,
    expected:
      'ec01498288516fc926459f58e2c6ad8df9b473cb0fc08c2596da7cf0e49be4b298d88cea927ac7f539f1edf228376d25',
  },
  {
    actor: 'Sha3_512',
    inputLength: 0,
    expected:
      'a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a615b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26',
  },
  {
    actor: 'Sha3_512',
    inputLength: 3,
    expected:
      'b751850b1a57168a5693cd924b6b096e08f621827444f70d884f5d0240d2712e10e116e9192af3c91a7ec57647e3934057340b4cf408d5a56592f8274eec53f0',
  },
]

const boundaryVectors: ReadonlyArray<ShaVector> = [
  { actor: 'Sha1', inputLength: 55, expected: 'c1c8bbdc22796e28c0e15163d20899b65621d65a' },
  { actor: 'Sha1', inputLength: 56, expected: 'c2db330f6083854c99d4b5bfb6e8f29f201be699' },
  { actor: 'Sha1', inputLength: 57, expected: 'f08f24908d682555111be7ff6f004e78283d989a' },
  { actor: 'Sha1', inputLength: 63, expected: '03f09f5b158a7a8cdad920bddc29b81c18a551f5' },
  { actor: 'Sha1', inputLength: 64, expected: '0098ba824b5c16427bd7a1122a5a442a25ec644d' },
  { actor: 'Sha1', inputLength: 65, expected: '11655326c708d70319be2610e8a57d9a5b959d3b' },
  {
    actor: 'Sha256',
    inputLength: 55,
    expected: '9f4390f8d30c2dd92ec9f095b65e2b9ae9b0a925a5258e241c9f1e910f734318',
  },
  {
    actor: 'Sha256',
    inputLength: 56,
    expected: 'b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a',
  },
  {
    actor: 'Sha256',
    inputLength: 57,
    expected: 'f13b2d724659eb3bf47f2dd6af1accc87b81f09f59f2b75e5c0bed6589dfe8c6',
  },
  {
    actor: 'Sha256',
    inputLength: 63,
    expected: '7d3e74a05d7db15bce4ad9ec0658ea98e3f06eeecf16b4c6fff2da457ddc2f34',
  },
  {
    actor: 'Sha256',
    inputLength: 64,
    expected: 'ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb',
  },
  {
    actor: 'Sha256',
    inputLength: 65,
    expected: '635361c48bb9eab14198e76ea8ab7f1a41685d6ad62aa9146d301d4f17eb0ae0',
  },
  {
    actor: 'Sha512',
    inputLength: 111,
    expected:
      'fa9121c7b32b9e01733d034cfc78cbf67f926c7ed83e82200ef86818196921760b4beff48404df811b953828274461673c68d04e297b0eb7b2b4d60fc6b566a2',
  },
  {
    actor: 'Sha512',
    inputLength: 112,
    expected:
      'c01d080efd492776a1c43bd23dd99d0a2e626d481e16782e75d54c2503b5dc32bd05f0f1ba33e568b88fd2d970929b719ecbb152f58f130a407c8830604b70ca',
  },
  {
    actor: 'Sha512',
    inputLength: 113,
    expected:
      '55ddd8ac210a6e18ba1ee055af84c966e0dbff091c43580ae1be703bdb85da31acf6948cf5bd90c55a20e5450f22fb89bd8d0085e39f85a86cc46abbca75e24d',
  },
  {
    actor: 'Sha512',
    inputLength: 127,
    expected:
      '828613968b501dc00a97e08c73b118aa8876c26b8aac93df128502ab360f91bab50a51e088769a5c1eff4782ace147dce3642554199876374291f5d921629502',
  },
  {
    actor: 'Sha512',
    inputLength: 128,
    expected:
      'b73d1929aa615934e61a871596b3f3b33359f42b8175602e89f7e06e5f658a243667807ed300314b95cacdd579f3e33abdfbe351909519a846d465c59582f321',
  },
  {
    actor: 'Sha512',
    inputLength: 129,
    expected:
      '4f681e0bd53cda4b5a2041cc8a06f2eabde44fb16c951fbd5b87702f07aeab611565b19c47fde30587177ebb852e3971bbd8d3fd30da18d71037dfbd98420429',
  },
  {
    actor: 'Sha3_224',
    inputLength: 143,
    expected: '73b1b22b54f515f626a6abdde6af25cd4801dc6e9dc7fa3f77e1c122',
  },
  {
    actor: 'Sha3_224',
    inputLength: 144,
    expected: 'f9019111996dcf160e284e320fd6d8825cabcd41a5ffdc4c5e9d64b6',
  },
  {
    actor: 'Sha3_224',
    inputLength: 145,
    expected: '7f0521c84aeacc8a46aba17171acbdd22522509a71c663257fbdee0e',
  },
  {
    actor: 'Sha3_256',
    inputLength: 135,
    expected: '8094bb53c44cfb1e67b7c30447f9a1c33696d2463ecc1d9c92538913392843c9',
  },
  {
    actor: 'Sha3_256',
    inputLength: 136,
    expected: '3fc5559f14db8e453a0a3091edbd2bc25e11528d81c66fa570a4efdcc2695ee1',
  },
  {
    actor: 'Sha3_256',
    inputLength: 137,
    expected: 'f8d6846cedd2ccfadf15c5879ef95af724d799eed7391fb1c91f95344e738614',
  },
  {
    actor: 'Sha3_384',
    inputLength: 103,
    expected:
      'af61fb4fd1c6afe80857fcba888318a0a1426635b4509f09707e3787630bdb621655ffa54f5884088ccc000f81436414',
  },
  {
    actor: 'Sha3_384',
    inputLength: 104,
    expected:
      '3a4f3b6284e571238884e95655e8c8a60e068e4059a9734abc08823a900d161592860243f00619ae699a29092ed91a16',
  },
  {
    actor: 'Sha3_384',
    inputLength: 105,
    expected:
      'cb73ab2f8f5fbb13f0e115a7062ba1644aa16534aa80d076ef27f8550deb900d89bdfa169b45073223acadb6001204d3',
  },
  {
    actor: 'Sha3_512',
    inputLength: 71,
    expected:
      '070faf98d2a8fddf8ed886408744dc06456096c2e045f26f3c7b010530e6bbb3db535a54d636856f4e0e1e982461cb9a7e8e57ff8895cff1619af9f0e486e28c',
  },
  {
    actor: 'Sha3_512',
    inputLength: 72,
    expected:
      'a8ae722a78e10cbbc413886c02eb5b369a03f6560084aff566bd597bb7ad8c1ccd86e81296852359bf2faddb5153c0a7445722987875e74287adac21adebe952',
  },
  {
    actor: 'Sha3_512',
    inputLength: 73,
    expected:
      '23e6a8815f8201dbbf6a5463be8dcadb1acea9df5f8998954e59ac9565cf6d29b17aa27a5e8b0fc06343db6122d6e544d27583ddc78504d08203217e7e65b6bd',
  },
]

const inputBytes = (length: number): ReadonlyArray<number> =>
  length === 3 ? [97, 98, 99] : Array.from({ length }, () => 97)

const hexBytes = (value: string): ReadonlyArray<number> =>
  Array.from({ length: value.length / 2 }, (_, index) =>
    Number.parseInt(value.slice(index * 2, index * 2 + 2), 16),
  )

const silkArray = (values: ReadonlyArray<number>): string => `[${values.join(', ')}]`

const vectorChecks = [...knownAnswerVectors, ...boundaryVectors]
  .map((vector, index) => {
    const input = inputBytes(vector.inputLength)
    const expected = hexBytes(vector.expected)
    return `fn vector${index}() -> bool {
  let input: [u8; ${input.length}] = ${silkArray(input)}
  let expected: [u8; ${expected.length}] = ${silkArray(expected)}
  let actual = ${vector.actor}.hash(&input)
  return equalBytes(&actual, &expected)
}`
  })
  .join('\n')

const vectorCalls =
  vectorChecks.length === 0
    ? ''
    : [...knownAnswerVectors, ...boundaryVectors]
        .map((_, index) => `  if !vector${index}() { return ${index + 1} }`)
        .join('\n')

/**
 * One source program that verifies all fixed-output SHA variants and distinct streaming boundaries.
 * Expected digests are pinned from Node/OpenSSL independently of the Silk implementation.
 */
export const shaAcceptanceSource = `import silk.sha1 { Sha1 }
import silk.sha2 { Sha224, Sha256, Sha384, Sha512, Sha512_224, Sha512_256 }
import silk.sha3 { Sha3_224, Sha3_256, Sha3_384, Sha3_512 }

fn equalBytes(actual: &[u8], expected: &[u8]) -> bool {
  if actual.length != expected.length { return false }
  let mut index: usize = 0
  while index < actual.length {
    if actual[index] != expected[index] { return false }
    index = index + 1
  }
  return true
}

${vectorChecks}

fn streaming() -> bool {
  let empty: [u8; 0] = []
  let a: [u8; 1] = [97]
  let b: [u8; 1] = [98]
  let c: [u8; 1] = [99]
  let abc: [u8; 3] = [97, 98, 99]

  let mut sha1 = Sha1.make()
  sha1.update(&empty)
  sha1.update(&a)
  sha1.update(&b)
  sha1.update(&c)
  let sha1Segmented = sha1.finish()
  let sha1Whole = Sha1.hash(&abc)
  if !equalBytes(&sha1Segmented, &sha1Whole) { return false }

  let mut sha256 = Sha256.make()
  sha256.update(&empty)
  sha256.update(&a)
  sha256.update(&b)
  sha256.update(&c)
  let sha256Segmented = sha256.finish()
  let sha256Whole = Sha256.hash(&abc)
  if !equalBytes(&sha256Segmented, &sha256Whole) { return false }

  let mut sha512 = Sha512.make()
  sha512.update(&empty)
  sha512.update(&a)
  sha512.update(&b)
  sha512.update(&c)
  let sha512Segmented = sha512.finish()
  let sha512Whole = Sha512.hash(&abc)
  if !equalBytes(&sha512Segmented, &sha512Whole) { return false }

  let mut sha3 = Sha3_256.make()
  sha3.update(&empty)
  sha3.update(&a)
  sha3.update(&b)
  sha3.update(&c)
  let sha3Segmented = sha3.finish()
  let sha3Whole = Sha3_256.hash(&abc)
  return equalBytes(&sha3Segmented, &sha3Whole)
}

fn repeatedSmallUpdates() -> bool {
  let a: [u8; 1] = [97]
  let expectedInput: [u8; 200] = ${silkArray(inputBytes(200))}
  let expectedSha1 = Sha1.hash(&expectedInput)
  let expectedSha256 = Sha256.hash(&expectedInput)
  let expectedSha512 = Sha512.hash(&expectedInput)
  let expectedSha3 = Sha3_256.hash(&expectedInput)
  let mut sha1 = Sha1.make()
  let mut sha256 = Sha256.make()
  let mut sha512 = Sha512.make()
  let mut sha3 = Sha3_256.make()
  let mut index: usize = 0
  while index < 200 {
    sha1.update(&a)
    sha256.update(&a)
    sha512.update(&a)
    sha3.update(&a)
    index = index + 1
  }
  let actualSha1 = sha1.finish()
  let actualSha256 = sha256.finish()
  let actualSha512 = sha512.finish()
  let actualSha3 = sha3.finish()
  return equalBytes(&actualSha1, &expectedSha1)
    && equalBytes(&actualSha256, &expectedSha256)
    && equalBytes(&actualSha512, &expectedSha512)
    && equalBytes(&actualSha3, &expectedSha3)
}

pub fn main() -> i32 {
${vectorCalls}
  if !streaming() { return 100 }
  if !repeatedSmallUpdates() { return 101 }
  return 42
}`
