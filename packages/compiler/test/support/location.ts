import * as AuthoredIdentity from '../../src/AuthoredIdentity.js'
import * as Location from '../../src/Location.js'

/** A location for a test that needs one but lowers no source: node `occurrence` of `module`. */
export const locationAt = (module: string, occurrence = 0): Location.Location =>
  Location.at({
    _tag: 'AuthoredAnchor',
    owner: AuthoredIdentity.module('memory', module),
    path: [{ _tag: 'LocalSegment', role: 'fixture', occurrence }],
  })
