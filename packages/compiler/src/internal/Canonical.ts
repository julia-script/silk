/** Length-frames one canonical field so concatenation cannot create ambiguous identities. */
export const frame = (value: string): string => `${value.length}:${value}`

/** Encodes one tagged canonical record from already canonical fields. */
export const record = (tag: string, fields: ReadonlyArray<string> = []): string =>
  `${frame(tag)}${fields.map(frame).join('')}`

/** Encodes one ordered canonical sequence. */
export const array = (values: ReadonlyArray<string>): string => record('Array', values)
