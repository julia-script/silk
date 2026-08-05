import { execFileSync } from 'node:child_process'
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from 'node:fs'
import { tmpdir } from 'node:os'
import { dirname, resolve } from 'node:path'
import { expect, test } from 'vitest'

const workspaceRoot = resolve(dirname(new URL(import.meta.url).pathname), '..')
const packageRoot = resolve(workspaceRoot, 'packages/llvm')
const compilerPackageRoot = resolve(workspaceRoot, 'packages/compiler')

test('the llvm release candidate is a self-contained ESM package', () => {
  const temporary = mkdtempSync(resolve(tmpdir(), 'silk-effect-release-candidate-'))

  try {
    const archiveRoot = resolve(temporary, 'archives')
    const unpackRoot = resolve(temporary, 'unpacked')
    mkdirSync(archiveRoot)
    mkdirSync(unpackRoot)

    execFileSync('pnpm', ['pack', '--pack-destination', archiveRoot], {
      cwd: packageRoot,
      stdio: 'pipe',
    })

    const archive = readdirSync(archiveRoot).find((file) => file.endsWith('.tgz'))
    expect(archive).toBeDefined()
    execFileSync('tar', ['-xzf', resolve(archiveRoot, archive ?? ''), '-C', unpackRoot])

    const packedRoot = resolve(unpackRoot, 'package')
    const manifest = JSON.parse(readFileSync(resolve(packedRoot, 'package.json'), 'utf8'))

    expect(manifest.name).toBe('@silk-effect/llvm')
    expect(manifest.private).not.toBe(true)
    expect(existsSync(resolve(packedRoot, 'dist/index.js'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'dist/index.d.ts'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'README.md'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'docs/reference/actors.md'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'docs/explanation/effect-native-builder.md'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'LICENSE'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'THIRD_PARTY_NOTICES.md'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'UPSTREAM.md'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'src'))).toBe(false)
    expect(existsSync(resolve(packedRoot, 'dist/LlvmError.js'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'dist/SilkError.js'))).toBe(false)
    expect(readFileSync(resolve(packedRoot, 'README.md'), 'utf8')).toContain('LlvmError')
    expect(readFileSync(resolve(packedRoot, 'docs/reference/actors.md'), 'utf8')).toContain(
      'WrappedFailure',
    )
    expect(Object.keys(manifest.dependencies ?? {})).toEqual(['effect'])
    const packedFiles = (directory: string): ReadonlyArray<string> =>
      readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
        const path = resolve(directory, entry.name)
        return entry.isDirectory() ? packedFiles(path) : [path]
      })
    expect(
      packedFiles(packedRoot).filter(
        (file) =>
          file.endsWith('.zig') ||
          (file.endsWith('.ts') && !file.endsWith('.d.ts')) ||
          file.endsWith('/llvm-as') ||
          file.endsWith('/zig'),
      ),
    ).toEqual([])

    expect(Object.keys(manifest.exports).sort()).toEqual([
      '.',
      './AddrSpace',
      './Alias',
      './Alignment',
      './Attribute',
      './Bitcode',
      './Block',
      './Builder',
      './ByteString',
      './Constant',
      './DIFlags',
      './DISPFlags',
      './DataLayout',
      './FastMath',
      './Function',
      './FunctionBody',
      './Global',
      './IntegerMath',
      './Intrinsic',
      './IrText',
      './LlvmError',
      './MemoryAccess',
      './Metadata',
      './Type',
      './Value',
      './Variable',
    ])
    expect(manifest.exports).not.toHaveProperty('./SilkError')

    const consumerRoot = resolve(temporary, 'consumer')
    mkdirSync(consumerRoot)
    writeFileSync(
      resolve(consumerRoot, 'package.json'),
      JSON.stringify({
        private: true,
        type: 'module',
        dependencies: { '@silk-effect/llvm': `file:${resolve(archiveRoot, archive ?? '')}` },
      }),
    )
    execFileSync('pnpm', ['install', '--offline', '--ignore-workspace'], {
      cwd: consumerRoot,
      stdio: 'pipe',
    })

    const deepPaths = Object.keys(manifest.exports)
      .filter((path) => path !== '.')
      .sort()
    const inspectApi = () =>
      execFileSync(
        process.execPath,
        [
          '--input-type=module',
          '--eval',
          `import * as api from '@silk-effect/llvm'; const paths = ${JSON.stringify(deepPaths)}; const modules = await Promise.all(paths.map((path) => import(\`@silk-effect/llvm/\${path.slice(2)}\`))); console.log(JSON.stringify({ root: Object.keys(api).sort(), rootNamespaces: Object.fromEntries(paths.filter((path) => path !== './LlvmError').map((path) => [path, Object.keys(api[path.slice(2)]).sort()])), deep: Object.fromEntries(paths.map((path, index) => [path, Object.keys(modules[index]).sort()])) }))`,
        ],
        {
          cwd: consumerRoot,
          encoding: 'utf8',
          env: {
            ...process.env,
            PATH: dirname(process.execPath),
            ZIG_EXE: '/unavailable/zig',
            LLVM_AS: '/unavailable/llvm-as',
            LLVM_DIS: '/unavailable/llvm-dis',
            LLVM_OPT: '/unavailable/opt',
            LLVM_BCANALYZER: '/unavailable/llvm-bcanalyzer',
          },
        },
      )

    const first = inspectApi()
    const second = inspectApi()
    expect(first).toBe(second)
    const api = JSON.parse(first)
    expect(api.root).toEqual([
      'AddrSpace',
      'Alias',
      'Alignment',
      'Attribute',
      'Bitcode',
      'Block',
      'Builder',
      'ByteString',
      'Constant',
      'DIFlags',
      'DISPFlags',
      'DataLayout',
      'FastMath',
      'Function',
      'FunctionBody',
      'Global',
      'IntegerMath',
      'Intrinsic',
      'IrText',
      'LlvmError',
      'MemoryAccess',
      'Metadata',
      'Type',
      'Value',
      'Variable',
    ])
    for (const [path, exports] of Object.entries(api.deep) as ReadonlyArray<
      readonly [string, ReadonlyArray<string>]
    >) {
      expect(exports.length, `${path} has no exports`).toBeGreaterThan(0)
      const rootName = path.slice(2)
      if (rootName === 'LlvmError') expect(exports).toContain('LlvmError')
      else expect(api.rootNamespaces[path]).toEqual(exports)
    }
    expect(api.deep['./Builder']).toContain('make')
  } finally {
    rmSync(temporary, { recursive: true, force: true })
  }
})

test('the compiler release candidate exposes only its bootstrap ESM actors', () => {
  const temporary = mkdtempSync(resolve(tmpdir(), 'silk-effect-compiler-release-candidate-'))

  try {
    const archiveRoot = resolve(temporary, 'archives')
    const unpackRoot = resolve(temporary, 'unpacked')
    mkdirSync(archiveRoot)
    mkdirSync(unpackRoot)

    execFileSync('pnpm', ['pack', '--pack-destination', archiveRoot], {
      cwd: compilerPackageRoot,
      stdio: 'pipe',
    })
    execFileSync('pnpm', ['pack', '--pack-destination', archiveRoot], {
      cwd: packageRoot,
      stdio: 'pipe',
    })

    const archive = readdirSync(archiveRoot).find(
      (file) => file.startsWith('silk-effect-compiler-') && file.endsWith('.tgz'),
    )
    const llvmArchive = readdirSync(archiveRoot).find(
      (file) => file.startsWith('silk-effect-llvm-') && file.endsWith('.tgz'),
    )
    expect(archive).toBeDefined()
    expect(llvmArchive).toBeDefined()
    execFileSync('tar', ['-xzf', resolve(archiveRoot, archive ?? ''), '-C', unpackRoot])

    const packedRoot = resolve(unpackRoot, 'package')
    const manifest = JSON.parse(readFileSync(resolve(packedRoot, 'package.json'), 'utf8'))

    expect(manifest.name).toBe('@silk-effect/compiler')
    expect(manifest.private).not.toBe(true)
    expect(Object.keys(manifest.dependencies ?? {}).sort()).toEqual(['@silk-effect/llvm', 'effect'])
    expect(Object.keys(manifest.exports).sort()).toEqual([
      '.',
      './Analysis',
      './Backend',
      './BootstrapEvaluation',
      './DeclarationIndex',
      './Diagnostic',
      './Elaboration',
      './Hir',
      './Instances',
      './Lexer',
      './Lower',
      './Mir',
      './ModuleClosure',
      './Ownership',
      './Parser',
      './SourceFile',
      './SourceSpan',
      './SyntaxFile',
      './SyntaxTree',
      './Token',
    ])
    expect(existsSync(resolve(packedRoot, 'dist/index.js'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'dist/index.d.ts'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'README.md'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'LICENSE'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'src'))).toBe(false)

    const packedFiles = (directory: string): ReadonlyArray<string> =>
      readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
        const path = resolve(directory, entry.name)
        return entry.isDirectory() ? packedFiles(path) : [path]
      })
    const compilerFiles = packedFiles(packedRoot)
    expect(compilerFiles.filter((file) => file.endsWith('.ts') && !file.endsWith('.d.ts'))).toEqual(
      [],
    )
    expect(compilerFiles.filter((file) => file.includes('syntax-inspector'))).toEqual([])

    const consumerRoot = resolve(temporary, 'consumer')
    mkdirSync(consumerRoot)
    writeFileSync(
      resolve(consumerRoot, 'package.json'),
      JSON.stringify({
        private: true,
        type: 'module',
        dependencies: { '@silk-effect/compiler': `file:${resolve(archiveRoot, archive ?? '')}` },
      }),
    )
    writeFileSync(
      resolve(consumerRoot, 'pnpm-workspace.yaml'),
      `overrides:\n  '@silk-effect/llvm': file:${resolve(archiveRoot, llvmArchive ?? '')}\n`,
    )
    execFileSync('pnpm', ['install', '--offline'], {
      cwd: consumerRoot,
      stdio: 'pipe',
    })

    const deepPaths = Object.keys(manifest.exports)
      .filter((path) => path !== '.')
      .sort()
    const inspected = execFileSync(
      process.execPath,
      [
        '--input-type=module',
        '--eval',
        `import * as api from '@silk-effect/compiler';
import * as evaluationModule from '@silk-effect/compiler/BootstrapEvaluation';
import * as parserModule from '@silk-effect/compiler/Parser';
import * as semanticModule from '@silk-effect/compiler/Elaboration';
import * as syntaxTreeModule from '@silk-effect/compiler/SyntaxTree';
const paths = ${JSON.stringify(deepPaths)};
const modules = await Promise.all(
  paths.map((path) => import(\`@silk-effect/compiler/\${path.slice(2)}\`)),
);
const source = api.SourceFile.make(
  'memory://packed.silk',
  new TextEncoder().encode(
    'pub fn identity(value: I32) -> I32 { return value }\\npub fn main() -> I32 { return identity(42) }',
  ),
);
const parse = api.Parser.parse(api.Lexer.lex(source));
const concreteFunctions = parse.root.children.filter(
  (element) => api.SyntaxTree.isNode(element) && element.kind === 'FunctionDeclaration',
);
const concreteCalls = [];
const visit = (element) => {
  if (!api.SyntaxTree.isNode(element)) return;
  if (element.kind === 'CallExpression') concreteCalls.push(element);
  for (const child of element.children) visit(child);
};
visit(parse.root);
const analysis = api.Elaboration.elaborateModule(parse);
const deepAnalysis = semanticModule.elaborateModule(parse);
const rootSnapshot = api.Analysis.ofSource(
  'memory://packed.silk',
  new TextEncoder().encode(
    'pub fn identity(value: I32) -> I32 { return value }\\npub fn main() -> I32 { return identity(42) }',
  ),
);
const evaluation = api.Analysis.evaluate(rootSnapshot);
const deepEvaluation = evaluationModule.evaluate(
  api.Analysis.instancesOf(rootSnapshot),
  api.Analysis.loweredMir(rootSnapshot),
);
const call = analysis.functions[1]?.returnedExpression;
const unknownSource = api.SourceFile.make(
  'memory://packed-unknown.silk',
  new TextEncoder().encode('pub fn main() -> I32 { return missing() }'),
);
const unknownAnalysis = semanticModule.elaborateModule(api.Parser.parse(api.Lexer.lex(unknownSource)));
const unknownLocalSource = api.SourceFile.make(
  'memory://packed-unknown-local.silk',
  new TextEncoder().encode('pub fn main() -> I32 { return missing }'),
);
const unknownLocalAnalysis = semanticModule.elaborateModule(
  api.Parser.parse(api.Lexer.lex(unknownLocalSource)),
);
const wrongAritySource = api.SourceFile.make(
  'memory://packed-wrong-arity.silk',
  new TextEncoder().encode(
    'pub fn identity(value: I32) -> I32 { return value }\\npub fn main() -> I32 { return identity() }',
  ),
);
const wrongArityAnalysis = semanticModule.elaborateModule(
  api.Parser.parse(api.Lexer.lex(wrongAritySource)),
);
const cycleSource = api.SourceFile.make(
  'memory://packed-cycle.silk',
  new TextEncoder().encode('pub fn main() -> I32 { return main() }'),
);
const cycleEvaluation = api.Analysis.evaluate(
  api.Analysis.ofSource(
    'memory://packed-cycle.silk',
    new TextEncoder().encode('pub fn main() -> I32 { return main() }'),
  ),
);
const nestedSource = api.SourceFile.make(
  'memory://packed-nested.silk',
  new TextEncoder().encode(
    'pub fn identity(value: I32) -> I32 { return value }\\npub fn main() -> I32 { return identity(identity(42)) }',
  ),
);
const nestedParse = parserModule.parse(api.Lexer.lex(nestedSource));
const nestedCalls = [];
const visitNested = (element) => {
  if (!syntaxTreeModule.isNode(element)) return;
  if (element.kind === 'CallExpression') nestedCalls.push(element);
  for (const child of element.children) visitNested(child);
};
visitNested(nestedParse.root);
const nestedAnalysis = semanticModule.elaborateModule(nestedParse);
const nestedOuter = nestedAnalysis.functions[1]?.returnedExpression;
const nestedInner = nestedOuter?._tag === 'Call' ? nestedOuter.arguments[0]?.expression : null;
const nestedEvaluation = api.Analysis.evaluate(
  api.Analysis.ofSource(
    'memory://packed-nested.silk',
    new TextEncoder().encode(
      'pub fn identity(value: I32) -> I32 { return value }\\npub fn main() -> I32 { return identity(identity(42)) }',
    ),
  ),
);
const names = analysis.functions.map((fact) =>
  fact.declaration.name._tag === 'Present' ? fact.declaration.name.spelling : null,
);
console.log(
  JSON.stringify({
    root: Object.keys(api).sort(),
    rootNamespaces: Object.fromEntries(
      paths.map((path) => [path, Object.keys(api[path.slice(2)]).sort()]),
    ),
    deep: Object.fromEntries(
      paths.map((path, index) => [path, Object.keys(modules[index]).sort()]),
    ),
    functionCount: concreteFunctions.length,
    callCount: concreteCalls.length,
    semantic: {
      names,
      ordinals: analysis.functions.map((fact) => fact.declaration.id.ordinal),
      returnedExpressionTags: analysis.functions.map((fact) => fact.returnedExpression._tag),
      parameterCounts: analysis.functions.map((fact) => fact.declaration.parameterCount),
      parameterOrdinals: analysis.functions.map((fact) =>
        fact.declaration.parameters.map((parameter) => parameter.id.ordinal),
      ),
      parameterLookup:
        analysis.functions[0] === undefined
          ? null
          : api.Elaboration.parameterByName(
              analysis.functions[0].declaration,
              'value',
            )._tag,
      identifierReference:
        analysis.functions[0]?.returnedExpression._tag === 'Identifier'
          ? analysis.functions[0].returnedExpression.reference._tag
          : null,
      identifierType:
        analysis.functions[0]?.returnedExpression._tag === 'Identifier'
          ? analysis.functions[0].returnedExpression.type
          : null,
      callReference: call?.reference._tag,
      argumentExpressionTags:
        call?._tag === 'Call'
          ? call.arguments.map((argument) => argument.expression._tag)
          : [],
      argumentOrdinals:
        call?._tag === 'Call' ? call.arguments.map((argument) => argument.id.ordinal) : [],
      mappingOrdinals:
        call?._tag === 'Call'
          ? call.mappings.map((mapping) => [
              mapping.argument.id.ordinal,
              mapping.parameter.id.ordinal,
            ])
          : [],
      callContract: call?._tag === 'Call' ? call.contract : null,
      callType: call?.type,
      callTargetOrdinal:
        call?.reference._tag === 'Resolved' ? call.reference.declaration.id.ordinal : null,
      callCompatibility: analysis.functions[1]?.returnCompatibility._tag,
      unknownDiagnosticCodes: unknownAnalysis.diagnostics.map((diagnostic) => diagnostic.code),
      unknownLocalDiagnosticCodes: unknownLocalAnalysis.diagnostics.map(
        (diagnostic) => diagnostic.code,
      ),
      wrongArityDiagnosticCodes: wrongArityAnalysis.diagnostics.map(
        (diagnostic) => diagnostic.code,
      ),
      rootLookup: api.Elaboration.declarationByName(analysis, 'identity')._tag,
      deepLookup: semanticModule.declarationByName(deepAnalysis, 'missing')._tag,
      legacyResultFields: ['declaration', 'integerExpression', 'returnCompatibility'].filter(
        (key) => key in analysis,
      ),
      legacyFunctionFields: analysis.functions.map((fact) =>
        ['integerExpression'].filter((key) => key in fact),
      ),
    },
    evaluation: {
      rootTag: evaluation._tag,
      rootResult: evaluation._tag === 'Completed' ? evaluation.result : null,
      rootTrace: evaluation.trace.map((event) => event._tag),
      deepTag: deepEvaluation._tag,
      deepResult: deepEvaluation._tag === 'Completed' ? deepEvaluation.result : null,
      cycleTag: cycleEvaluation._tag,
      cycleReason: cycleEvaluation._tag === 'Blocked' ? cycleEvaluation.reason._tag : null,
      cycleNames:
        cycleEvaluation._tag === 'Blocked' && cycleEvaluation.reason._tag === 'RecursiveCycle'
          ? cycleEvaluation.reason.cycle.map((id) => id.name)
          : [],
      cycleTrace: cycleEvaluation.trace.map((event) => event._tag),
    },
    nested: {
      callCount: nestedCalls.length,
      parserDiagnostics: nestedParse.parserDiagnostics.map((diagnostic) => diagnostic.code),
      semanticDiagnostics: nestedAnalysis.diagnostics.map((diagnostic) => diagnostic.code),
      argumentTag:
        nestedOuter?._tag === 'Call'
          ? nestedOuter.arguments[0]?.expression._tag
          : null,
      innerReference: nestedInner?._tag === 'Call' ? nestedInner.reference._tag : null,
      innerArgumentTag:
        nestedInner?._tag === 'Call' ? nestedInner.arguments[0]?.expression._tag : null,
      innerContract: nestedInner?._tag === 'Call' ? nestedInner.contract._tag : null,
      outerContract: nestedOuter?._tag === 'Call' ? nestedOuter.contract._tag : null,
      mappingCount: nestedOuter?._tag === 'Call' ? nestedOuter.mappings.length : null,
      type: nestedOuter?._tag === 'Call' ? nestedOuter.type : null,
      evaluationReason:
        nestedEvaluation._tag === 'Blocked' ? nestedEvaluation.reason._tag : null,
      evaluationResult:
        nestedEvaluation._tag === 'Completed' ? nestedEvaluation.result : null,
      evaluationTrace: nestedEvaluation.trace.map((event) => event._tag),
    },
    parserDiagnostics: parse.parserDiagnostics.map((diagnostic) => diagnostic.code),
  }),
);`,
      ],
      {
        cwd: consumerRoot,
        encoding: 'utf8',
        env: { ...process.env, PATH: dirname(process.execPath) },
      },
    )
    const api = JSON.parse(inspected)
    expect(api.root).toEqual([
      'Analysis',
      'Backend',
      'BootstrapEvaluation',
      'DeclarationIndex',
      'Diagnostic',
      'Elaboration',
      'Hir',
      'Instances',
      'Lexer',
      'Lower',
      'Mir',
      'ModuleClosure',
      'Ownership',
      'Parser',
      'SourceFile',
      'SourceSpan',
      'SyntaxFile',
      'SyntaxTree',
      'Token',
    ])
    for (const [path, exports] of Object.entries(api.deep) as ReadonlyArray<
      readonly [string, ReadonlyArray<string>]
    >) {
      expect(exports.length, `${path} has no exports`).toBeGreaterThan(0)
      expect(api.rootNamespaces[path]).toEqual(exports)
    }
    expect(api.deep['./Lexer']).toContain('lex')
    expect(api.deep['./BootstrapEvaluation']).toContain('evaluate')
    expect(api.deep['./Parser']).toContain('parse')
    expect(api.deep['./Elaboration']).toContain('elaborateModule')
    expect(api.deep['./Elaboration']).toContain('parameterByName')
    expect(api.deep['./Hir']).toContain('encode')
    expect(api.deep['./Diagnostic']).toContain('unknownType')
    expect(api.deep['./Diagnostic']).toContain('unknownFunction')
    expect(api.deep['./Diagnostic']).toContain('duplicateParameterName')
    expect(api.deep['./Diagnostic']).toContain('unknownParameterReference')
    expect(api.deep['./Diagnostic']).toContain('wrongCallArity')
    expect(api.deep['./Diagnostic']).toContain('merge')
    expect(api.deep['./SourceFile']).toContain('make')
    expect(api.deep['./SyntaxTree']).toContain('tokens')
    expect(api.functionCount).toBe(2)
    expect(api.callCount).toBe(1)
    expect(api.semantic).toEqual({
      names: ['identity', 'main'],
      ordinals: [0, 1],
      returnedExpressionTags: ['Identifier', 'Call'],
      parameterCounts: [1, 0],
      parameterOrdinals: [[0], []],
      parameterLookup: 'Resolved',
      identifierReference: 'Resolved',
      identifierType: { _tag: 'Available', type: 'I32' },
      callReference: 'Resolved',
      argumentExpressionTags: ['Integer'],
      argumentOrdinals: [0],
      mappingOrdinals: [[0, 0]],
      callContract: { _tag: 'Compatible', expectedCount: 1, actualCount: 1 },
      callType: { _tag: 'Available', type: 'I32' },
      callTargetOrdinal: 0,
      callCompatibility: 'Compatible',
      unknownDiagnosticCodes: ['SEM0004'],
      unknownLocalDiagnosticCodes: ['SEM0006'],
      wrongArityDiagnosticCodes: ['SEM0007'],
      rootLookup: 'Resolved',
      deepLookup: 'Missing',
      legacyResultFields: [],
      legacyFunctionFields: [[], []],
    })
    expect(api.evaluation).toEqual({
      rootTag: 'Completed',
      rootResult: { _tag: 'I32Value', value: 42 },
      rootTrace: ['Entry', 'Call', 'Binding', 'Return', 'Return'],
      deepTag: 'Completed',
      deepResult: { _tag: 'I32Value', value: 42 },
      cycleTag: 'Blocked',
      cycleReason: 'RecursiveCycle',
      cycleNames: ['main', 'main'],
      cycleTrace: ['Entry', 'Call'],
    })
    expect(api.nested).toEqual({
      callCount: 2,
      parserDiagnostics: [],
      semanticDiagnostics: [],
      argumentTag: 'Call',
      innerReference: 'Resolved',
      innerArgumentTag: 'Integer',
      innerContract: 'Compatible',
      outerContract: 'Compatible',
      mappingCount: 1,
      type: { _tag: 'Available', type: 'I32' },
      evaluationReason: null,
      evaluationResult: { _tag: 'I32Value', value: 42 },
      evaluationTrace: [
        'Entry',
        'Call',
        'Binding',
        'Return',
        'Call',
        'Binding',
        'Return',
        'Return',
      ],
    })
    expect(api.parserDiagnostics).toEqual([])
  } finally {
    rmSync(temporary, { recursive: true, force: true })
  }
})
