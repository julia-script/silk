import { initialState, reduce, selectFrame, variants } from './model.mjs'

const bold = '\u001b[1m'
const dim = '\u001b[2m'
const reset = '\u001b[0m'
const cyan = '\u001b[36m'
const green = '\u001b[32m'
const yellow = '\u001b[33m'
const keyword = '\u001b[1;35m'
const operator = '\u001b[1;34m'
const comment = '\u001b[2;90m'

const keywords = new Set([
  'alias',
  'as',
  'body',
  'callback',
  'catch',
  'conform',
  'conformance',
  'const',
  'copy',
  'do',
  'edit',
  'else',
  'exclusive',
  'fail',
  'fails',
  'failures',
  'fn',
  'flow',
  'for',
  'from',
  'function',
  'handle',
  'if',
  'impl',
  'import',
  'in',
  'interface',
  'let',
  'match',
  'move',
  'mut',
  'needs',
  'own',
  'owned',
  'parameters',
  'provide',
  'pub',
  'public',
  'requirements',
  'return',
  'returns',
  'role',
  'run',
  'scope',
  'scoped',
  'shared',
  'struct',
  'take',
  'to',
  'type',
  'unsafe',
  'using',
  'value',
  'view',
  'when',
  'with',
  'yield',
])

const isIdentifierStart = (character) => /[A-Za-z_]/.test(character)
const isIdentifierPart = (character) => /[A-Za-z0-9_]/.test(character)

const highlight = (source) => {
  let result = ''
  let index = 0

  while (index < source.length) {
    const character = source[index]
    const next = source[index + 1]

    if (character === '/' && next === '/') {
      const end = source.indexOf('\n', index)
      const stop = end === -1 ? source.length : end
      result += `${comment}${source.slice(index, stop)}${reset}`
      index = stop
      continue
    }

    if (character === '"') {
      let end = index + 1
      while (end < source.length) {
        if (source[end] === '\\') end += 1
        else if (source[end] === '"') {
          end += 1
          break
        }
        end += 1
      }
      result += `${green}${source.slice(index, end)}${reset}`
      index = end
      continue
    }

    if (character === '|' && next === '>') {
      result += `${operator}|>${reset}`
      index += 2
      continue
    }

    if (character === "'" && isIdentifierStart(next)) {
      let end = index + 2
      while (end < source.length && isIdentifierPart(source[end])) end += 1
      result += `${yellow}${source.slice(index, end)}${reset}`
      index = end
      continue
    }

    const pair = `${character}${next}`
    if (pair === '->' || pair === '=>' || pair === '::') {
      result += `${operator}${pair}${reset}`
      index += 2
      continue
    }

    if ('!?&@|'.includes(character)) {
      result += `${operator}${character}${reset}`
      index += 1
      continue
    }

    if (/[0-9]/.test(character)) {
      let end = index + 1
      while (end < source.length && /[0-9._]/.test(source[end])) end += 1
      result += `${yellow}${source.slice(index, end)}${reset}`
      index = end
      continue
    }

    if (isIdentifierStart(character)) {
      let end = index + 1
      while (end < source.length && isIdentifierPart(source[end])) end += 1
      const token = source.slice(index, end)

      if (keywords.has(token)) result += `${keyword}${token}${reset}`
      else if (/^[A-Z]/.test(token)) result += `${cyan}${token}${reset}`
      else result += token

      index = end
      continue
    }

    result += character
    index += 1
  }

  return result
}

let state = initialState

const render = () => {
  const { variant, example } = selectFrame(state)
  const variantNumber = state.variantIndex + 1
  const exampleNumber = state.exampleIndex + 1

  console.clear()
  process.stdout.write(`${bold}Silk Effect bootstrap syntax — throwaway prototype${reset}\n`)
  process.stdout.write(
    `${dim}Same semantics, different surface language. Nothing shown here is settled.${reset}\n\n`,
  )
  process.stdout.write(
    `${bold}Variant${reset} ${cyan}${variantNumber}/${variants.length} ${variant.name}${reset}\n`,
  )
  process.stdout.write(`${bold}Thesis${reset}  ${variant.thesis}\n`)
  if (state.showNotes) {
    process.stdout.write(`${bold}Tension${reset} ${variant.tension}\n`)
  }
  process.stdout.write(`\n${bold}Example ${exampleNumber}${reset}  ${example.title}\n\n`)
  process.stdout.write(`${highlight(example.source)}\n\n`)
  if (state.showNotes && example.note !== undefined) {
    process.stdout.write(`${bold}Consequence${reset} ${example.note}\n\n`)
  }
  process.stdout.write(
    `${bold}[W/S · ↑/↓]${reset} ${dim}change variant${reset}  ` +
      `${bold}[A/D · ←/→]${reset} ${dim}change example${reset}\n` +
      `${bold}[t]${reset} ${dim}toggle design note${reset}  ` +
      `${bold}[q]${reset} ${dim}quit${reset}\n`,
  )
}

const actions = new Map([
  ['w', 'previousVariant'],
  ['W', 'previousVariant'],
  ['\u001b[A', 'previousVariant'],
  ['s', 'nextVariant'],
  ['S', 'nextVariant'],
  ['\u001b[B', 'nextVariant'],
  ['a', 'previousExample'],
  ['A', 'previousExample'],
  ['\u001b[D', 'previousExample'],
  ['d', 'nextExample'],
  ['D', 'nextExample'],
  ['\u001b[C', 'nextExample'],
  ['t', 'toggleNotes'],
  ['T', 'toggleNotes'],
])

const quit = () => {
  if (process.stdin.isTTY) process.stdin.setRawMode(false)
  process.stdin.pause()
  process.stdout.write('\n')
}

if (!process.stdin.isTTY) {
  process.stderr.write('This prototype needs an interactive terminal.\n')
  process.exitCode = 1
} else {
  process.stdin.setRawMode(true)
  process.stdin.setEncoding('utf8')
  process.stdin.resume()
  process.stdin.on('data', (key) => {
    if (key === 'q' || key === '\u0003') {
      quit()
      return
    }

    const action = actions.get(key)
    if (action !== undefined) {
      state = reduce(state, action)
      render()
    }
  })
  render()
}
