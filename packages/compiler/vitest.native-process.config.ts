import { defineSilkConfig } from '../../vitest.shared.js'

export default defineSilkConfig({
  test: {
    include: ['conformance/native-process/NativeProcessConformance.test.mjs'],
    maxWorkers: 1,
  },
})
