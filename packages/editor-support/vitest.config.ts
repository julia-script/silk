import { defineSilkConfig } from '../../vitest.shared.js'

export default defineSilkConfig({
  test: {
    // The element and hover renderer are DOM code; jsdom supplies custom elements and shadow DOM.
    environment: 'jsdom',
  },
})
