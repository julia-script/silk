'use client'

/**
 * Searchable preset picker.
 *
 * There are ~70 presets and the set grows with every phase, so the button bar the labs used does
 * not survive the consolidation. A filter box plus keyboard navigation keeps the ones you know by
 * name one query away, and keeps the rest browsable by phase.
 */

import { useEffect, useMemo, useRef, useState } from 'react'
import { type Preset, presets } from './presets'
import styles from './preset-palette.module.css'

/**
 * Subsequence match, the usual command-palette behaviour: "nfc" finds "Nested flow · complete".
 * Returns the matched indices so the caller can highlight them, or undefined for no match.
 */
const fuzzyMatch = (haystack: string, needle: string): ReadonlyArray<number> | undefined => {
  if (needle.length === 0) return []
  const lowerHaystack = haystack.toLowerCase()
  const lowerNeedle = needle.toLowerCase()
  const hits: Array<number> = []
  let cursor = 0
  for (const character of lowerNeedle) {
    const found = lowerHaystack.indexOf(character, cursor)
    if (found === -1) return undefined
    hits.push(found)
    cursor = found + 1
  }
  return hits
}

function Highlighted({
  text,
  hits,
}: {
  readonly text: string
  readonly hits: ReadonlyArray<number>
}) {
  if (hits.length === 0) return <>{text}</>
  const marked = new Set(hits)
  return (
    <>
      {[...text].map((character, index) =>
        marked.has(index) ? (
          // biome-ignore lint/suspicious/noArrayIndexKey: index *is* the identity of a character
          <mark key={index} className={styles.hit}>
            {character}
          </mark>
        ) : (
          // biome-ignore lint/suspicious/noArrayIndexKey: same
          <span key={index}>{character}</span>
        ),
      )}
    </>
  )
}

export function PresetPalette({
  open,
  onClose,
  onPick,
}: {
  readonly open: boolean
  readonly onClose: () => void
  readonly onPick: (preset: Preset) => void
}) {
  const [query, setQuery] = useState('')
  const [active, setActive] = useState(0)
  const inputRef = useRef<HTMLInputElement>(null)
  const listRef = useRef<HTMLDivElement>(null)

  // A stale query from last time hides most of the catalog and reads as an empty palette.
  useEffect(() => {
    if (open) {
      setQuery('')
      setActive(0)
      inputRef.current?.focus()
    }
  }, [open])

  const matches = useMemo(() => {
    const scored: Array<{ preset: Preset; hits: ReadonlyArray<number> }> = []
    for (const preset of presets) {
      // Matching against "group label" lets "backend nested" narrow by phase as well as name.
      const hits = fuzzyMatch(preset.label, query)
      if (hits !== undefined) {
        scored.push({ preset, hits })
        continue
      }
      if (fuzzyMatch(`${preset.group} ${preset.label}`, query) !== undefined) {
        scored.push({ preset, hits: [] })
      }
    }
    return scored
  }, [query])

  // Keep the highlighted row in range as the result set shrinks under a longer query.
  useEffect(() => {
    setActive((current) => (current >= matches.length ? Math.max(0, matches.length - 1) : current))
  }, [matches.length])

  useEffect(() => {
    listRef.current?.querySelector('[data-active="true"]')?.scrollIntoView({ block: 'nearest' })
  }, [active])

  if (!open) return null

  const choose = (index: number): void => {
    const match = matches[index]
    if (match === undefined) return
    onPick(match.preset)
    onClose()
  }

  return (
    // biome-ignore lint/a11y/noStaticElementInteractions: backdrop click-to-dismiss; the dialog
    // itself is keyboard-operable and Escape closes it.
    <div className={styles.backdrop} onClick={onClose}>
      <div
        className={styles.palette}
        role="dialog"
        aria-modal="true"
        aria-label="Load a preset program"
        onClick={(event) => event.stopPropagation()}
      >
        <input
          ref={inputRef}
          className={styles.search}
          value={query}
          placeholder={`Filter ${presets.length} presets…`}
          aria-label="Filter presets"
          spellCheck={false}
          autoComplete="off"
          onChange={(event) => {
            setQuery(event.target.value)
            setActive(0)
          }}
          onKeyDown={(event) => {
            if (event.key === 'Escape') onClose()
            if (event.key === 'Enter') choose(active)
            if (event.key === 'ArrowDown') {
              event.preventDefault()
              setActive((current) => Math.min(matches.length - 1, current + 1))
            }
            if (event.key === 'ArrowUp') {
              event.preventDefault()
              setActive((current) => Math.max(0, current - 1))
            }
          }}
        />

        <div className={styles.results} ref={listRef}>
          {matches.length === 0 ? (
            <p className={styles.empty}>No preset matches “{query}”</p>
          ) : (
            matches.map((match, index) => {
              const previous = index === 0 ? undefined : matches[index - 1]?.preset.group
              return (
                <div key={`${match.preset.group}-${match.preset.label}`}>
                  {match.preset.group === previous ? null : (
                    <p className={styles.group}>{match.preset.group}</p>
                  )}
                  <button
                    type="button"
                    className={styles.row}
                    data-active={index === active}
                    onMouseEnter={() => setActive(index)}
                    onClick={() => choose(index)}
                  >
                    <span className={styles.rowLabel}>
                      <Highlighted text={match.preset.label} hits={match.hits} />
                    </span>
                    {Object.keys(match.preset.modules).length > 1 ? (
                      <span className={styles.badge}>
                        {Object.keys(match.preset.modules).length} modules
                      </span>
                    ) : null}
                  </button>
                </div>
              )
            })
          )}
        </div>
      </div>
    </div>
  )
}
