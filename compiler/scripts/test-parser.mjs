// One prebuilt native parser, many syntax-only inputs. No per-fixture compiler invocations.
import assert from 'node:assert/strict'
import { execFileSync } from 'node:child_process'
import { readFileSync, readdirSync } from 'node:fs'
import { resolve, relative } from 'node:path'
import * as Lexer from '../../packages/compiler/dist/Lexer.js'
import * as Parser from '../../packages/compiler/dist/Parser.js'
import * as SourceFile from '../../packages/compiler/dist/SourceFile.js'
import * as SyntaxTree from '../../packages/compiler/dist/SyntaxTree.js'

assert.ok(
  process.argv[2],
  'Usage: node compiler/scripts/test-parser.mjs <built-executable> [source-files...]',
)
const executable = resolve(process.argv[2])
const trivia = new Set(['Whitespace', 'LineComment', 'DocComment', 'ModuleDocComment'])
const filesUnder = (directory) =>
  readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const path = `${directory}/${entry.name}`
    if (entry.isDirectory()) return filesUnder(path)
    return path.endsWith('.silk') ? [path] : []
  })

function decode(output, sourceLength) {
  const nodes = []
  let root
  let diagnosticCount
  let current
  for (const line of output.split('\n')) {
    let match
    if ((match = /^root #(\d+)$/.exec(line))) root = Number(match[1])
    else if ((match = /^diagnostics (\d+)$/.exec(line))) diagnosticCount = Number(match[1])
    else if (
      diagnosticCount === undefined &&
      (match = /^#(\d+) (\w+) (\d+)\.\.(\d+)$/.exec(line))
    ) {
      assert.equal(Number(match[1]), nodes.length, 'IDs must be array indices')
      current = { kind: match[2], start: Number(match[3]), end: Number(match[4]), elements: [] }
      assert.ok(current.start <= current.end && current.end <= sourceLength, line)
      nodes.push(current)
    } else if ((match = /^  node #(\d+)$/.exec(line))) {
      assert.ok(current)
      const id = Number(match[1])
      assert.ok(id < nodes.length - 1, 'children must precede parents')
      current.elements.push({ node: id })
    } else if ((match = /^  token #(\d+) (\w+) (\d+)\.\.(\d+)$/.exec(line))) {
      assert.ok(current)
      current.elements.push({
        token: Number(match[1]),
        kind: match[2],
        start: Number(match[3]),
        end: Number(match[4]),
      })
    } else if ((match = /^  missing (\w+) (\d+)\.\.(\d+)$/.exec(line))) {
      assert.ok(current)
      assert.equal(match[2], match[3], 'missing tokens must have zero width')
      current.elements.push({ missing: match[1], start: Number(match[2]), end: Number(match[3]) })
    }
  }
  assert.equal(root, nodes.length - 1)
  assert.equal(nodes[root]?.kind, 'SourceFile')
  assert.equal(nodes[root]?.start, 0)
  assert.equal(nodes[root]?.end, sourceLength)
  assert.notEqual(diagnosticCount, undefined)
  const reached = new Set()
  const tokens = new Set()
  const visit = (id) => {
    assert.ok(!reached.has(id), 'a node must have exactly one parent')
    reached.add(id)
    const node = nodes[id]
    let cursor = node.start
    for (const element of node.elements) {
      const span = element.node === undefined ? element : nodes[element.node]
      assert.ok(span.start >= cursor && span.end <= node.end, `ordered elements in ${node.kind}`)
      cursor = span.end
      if (element.node !== undefined) visit(element.node)
      if (element.token !== undefined) {
        assert.ok(!tokens.has(element.token), 'significant tokens must have exactly one owner')
        tokens.add(element.token)
      }
    }
  }
  visit(root)
  assert.equal(reached.size, nodes.length, 'no orphan nodes')
  const shape = (id) => ({
    kind: nodes[id].kind,
    children: nodes[id].elements.map((element) => {
      if (element.node !== undefined) return shape(element.node)
      if (element.missing) return `Missing(${element.missing})`
      return element.kind
    }),
  })
  return { nodes, diagnosticCount, shape: shape(root), tokens }
}

function bootstrapShape(node) {
  return {
    kind: node.kind,
    children: node.children
      .filter((child) => !SyntaxTree.isToken(child) || !trivia.has(child.kind))
      .map((child) => {
        if (SyntaxTree.isNode(child)) return bootstrapShape(child)
        if (SyntaxTree.isToken(child)) return child.kind
        return `Missing(${child.expected})`
      }),
  }
}

function shapeDifferences(actual, expected, path) {
  if (typeof actual !== typeof expected) return [`${path}: node/token mismatch`]
  if (typeof expected === 'string')
    return actual === expected ? [] : [`${path}: ${actual} instead of ${expected}`]
  if (actual.kind !== expected.kind) return [`${path}: ${actual.kind} instead of ${expected.kind}`]
  if (actual.children.length !== expected.children.length)
    return [
      `${path}.${expected.kind}: ${actual.children.length} children instead of ${expected.children.length}`,
    ]
  return expected.children.flatMap((child, index) =>
    shapeDifferences(actual.children[index], child, `${path}.${expected.kind}[${index}]`),
  )
}

let checked = 0
let failed = 0
const files =
  process.argv.length > 3
    ? process.argv.slice(3)
    : [
        ...filesUnder('compiler/fixtures/parser'),
        ...filesUnder('compiler/src'),
        ...filesUnder('packages/compiler/stdlib/silk'),
      ]
function checkFile(file) {
  const bytes = readFileSync(file)
  const lexical = Lexer.lex(SourceFile.make(file, bytes))
  const bootstrap = Parser.parse(lexical)
  const output = execFileSync(executable, [relative(process.cwd(), resolve(file))], {
    encoding: 'utf8',
    timeout: 15000,
    maxBuffer: 32 * 1024 * 1024,
  })
  const actual = decode(output, bytes.length)
  for (const [id, token] of lexical.tokens.entries()) {
    if (!trivia.has(token.kind)) assert.ok(actual.tokens.has(id), `${file}: missing token #${id}`)
  }
  if (file.endsWith('/recovery.silk')) {
    assert.ok(actual.diagnosticCount > 0)
    assert.ok(
      actual.nodes.filter((node) => node.kind === 'FunctionDeclaration').length >= 2,
      'preserve following valid declarations',
    )
  } else {
    assert.equal(
      bootstrap.parserDiagnostics.length,
      0,
      `${file}: fixture must be valid bootstrap syntax`,
    )
    assert.equal(
      actual.diagnosticCount,
      0,
      `${file}: ${output.slice(output.lastIndexOf('\ndiagnostics'))}`,
    )
    assert.deepEqual(
      shapeDifferences(actual.shape, bootstrapShape(bootstrap.root), file),
      [],
      `${file}: significant AST structure must agree`,
    )
  }
}
for (const file of files) {
  try {
    checkFile(file)
    checked++
    process.stdout.write(`ok ${file}\n`)
  } catch (error) {
    failed++
    process.stderr.write(`FAIL ${file}: ${error.message}\n`)
  }
}
process.stdout.write(
  `Verified ${checked} source files; ${failed} failed: flat-tree invariants, token ownership, grammar, and recovery.\n`,
)
if (failed > 0) process.exitCode = 1
