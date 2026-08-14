import { defineSilkConfig } from '../../vitest.shared.js'
import * as Timeouts from './test/timeouts.js'

export default defineSilkConfig({
  test: {
    // Above the workspace floor, and measured rather than felt: tests in this package shell out to
    // Clang to compile and link real executables. See the derivation in ./test/timeouts.ts.
    testTimeout: Timeouts.nativeBuild,
    hookTimeout: Timeouts.nativeBuild,
  },
})
