/** Normalize only source-span fields in encoded MIR for golden assertions. */
export const normalizeSourceSpans = (mir: string): string =>
  mir
    .replace(/(?<=\bavailable )\[\d+, \d+\)/g, '[source span]')
    .replace(/ \[\d+, \d+\)(?=(?: generated)?$)/gm, ' [source span]')
