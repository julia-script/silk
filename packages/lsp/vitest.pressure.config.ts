import { defineSilkConfig } from '../../vitest.shared.js'

export default defineSilkConfig({
  test: {
    include: ['test/ProjectSessionPressure.bench.ts'],
  },
})
