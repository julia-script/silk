import { execFileSync, spawn } from 'node:child_process'
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
const compilerCliPackageRoot = resolve(workspaceRoot, 'packages/compiler-cli')
const wasmPackageRoot = resolve(workspaceRoot, 'packages/wasm')
const lspPackageRoot = resolve(workspaceRoot, 'packages/lsp')

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
    execFileSync('pnpm', ['pack', '--pack-destination', archiveRoot], {
      cwd: wasmPackageRoot,
      stdio: 'pipe',
    })

    const archive = readdirSync(archiveRoot).find(
      (file) => file.startsWith('silk-effect-compiler-') && file.endsWith('.tgz'),
    )
    const llvmArchive = readdirSync(archiveRoot).find(
      (file) => file.startsWith('silk-effect-llvm-') && file.endsWith('.tgz'),
    )
    const wasmArchive = readdirSync(archiveRoot).find(
      (file) => file.startsWith('silk-effect-wasm-') && file.endsWith('.tgz'),
    )
    expect(archive).toBeDefined()
    expect(llvmArchive).toBeDefined()
    expect(wasmArchive).toBeDefined()
    execFileSync('tar', ['-xzf', resolve(archiveRoot, archive ?? ''), '-C', unpackRoot])

    const packedRoot = resolve(unpackRoot, 'package')
    const manifest = JSON.parse(readFileSync(resolve(packedRoot, 'package.json'), 'utf8'))

    expect(manifest.name).toBe('@silk-effect/compiler')
    expect(manifest.private).not.toBe(true)
    expect(Object.keys(manifest.dependencies ?? {}).sort()).toEqual([
      '@silk-effect/llvm',
      '@silk-effect/wasm',
      'effect',
    ])
    expect(Object.keys(manifest.exports).sort()).toEqual([
      '.',
      './Analysis',
      './Backend',
      './BootstrapEvaluation',
      './DeclarationIndex',
      './Diagnostic',
      './Driver',
      './Elaboration',
      './FormattedDocument',
      './Formatter',
      './Hir',
      './Instances',
      './Layout',
      './Lexer',
      './Lower',
      './Mir',
      './ModuleClosure',
      './NameResolution',
      './NativeToolchain',
      './Operator',
      './Ownership',
      './Parser',
      './SourceFile',
      './SourceResolver',
      './SourceSpan',
      './SyntaxFile',
      './SyntaxTree',
      './Target',
      './Token',
      './ToolchainPlan',
      './Type',
      './TypeCompatibility',
      './WasmBackend',
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
        dependencies: {
          '@silk-effect/compiler': `file:${resolve(archiveRoot, archive ?? '')}`,
          effect: manifest.dependencies.effect,
        },
      }),
    )
    writeFileSync(
      resolve(consumerRoot, 'pnpm-workspace.yaml'),
      `overrides:\n  '@silk-effect/llvm': file:${resolve(archiveRoot, llvmArchive ?? '')}\n  '@silk-effect/wasm': file:${resolve(archiveRoot, wasmArchive ?? '')}\n`,
    )
    execFileSync('pnpm', ['install', '--offline'], {
      cwd: consumerRoot,
      stdio: 'pipe',
    })

    const deepPaths = Object.keys(manifest.exports)
      .filter((path) => path !== '.')
      .sort()
    const inspectCompiler = () =>
      execFileSync(
        process.execPath,
        [
          '--input-type=module',
          '--eval',
          `import * as api from '@silk-effect/compiler';
import * as Effect from 'effect/Effect';
import * as evaluationModule from '@silk-effect/compiler/BootstrapEvaluation';
import * as parserModule from '@silk-effect/compiler/Parser';
import * as semanticModule from '@silk-effect/compiler/Elaboration';
import * as syntaxTreeModule from '@silk-effect/compiler/SyntaxTree';
const paths = ${JSON.stringify(deepPaths)};
const modules = await Promise.all(
  paths.map((path) => import(\`@silk-effect/compiler/\${path.slice(2)}\`)),
);
const source = api.SourceFile.make(
  'memory/packed',
  new TextEncoder().encode(
    'pub fn identity(value: I32) -> I32 { return value }\\npub fn main() -> I32 { return identity(42) }',
  ),
);
const snapshotOfSource = (id, bytes) => Effect.runSync(api.Analysis.ofSource(id, bytes));
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
const rootSnapshot = snapshotOfSource(
  'memory/packed',
  new TextEncoder().encode(
    'pub fn identity(value: I32) -> I32 { return value }\\npub fn main() -> I32 { return identity(42) }',
  ),
);
const inputOf = (snapshot) => ({
  syntax: api.Analysis.rootAnalysis(snapshot).syntax,
  headers: api.Analysis.declarationIndex(snapshot).modules[0],
  scope: api.Analysis.moduleScope(snapshot, snapshot.closure.rootModule),
  index: api.Analysis.declarationIndex(snapshot),
});
const analysis = api.Analysis.rootAnalysis(rootSnapshot);
const deepAnalysis = semanticModule.elaborateModule(inputOf(rootSnapshot));
const evaluation = api.Analysis.evaluate(rootSnapshot);
const deepEvaluation = evaluationModule.evaluate(
  api.Analysis.instancesOf(rootSnapshot),
  api.Analysis.loweredMir(rootSnapshot),
);
const call = analysis.functions[1]?.returnedExpression;
const unknownAnalysis = api.Analysis.rootAnalysis(snapshotOfSource(
  'memory/packed-unknown',
  new TextEncoder().encode('pub fn main() -> I32 { return missing() }'),
));
const unknownLocalAnalysis = api.Analysis.rootAnalysis(snapshotOfSource(
  'memory/packed-unknown-local',
  new TextEncoder().encode('pub fn main() -> I32 { return missing }'),
));
const wrongArityAnalysis = api.Analysis.rootAnalysis(snapshotOfSource(
  'memory/packed-wrong-arity',
  new TextEncoder().encode(
    'pub fn identity(value: I32) -> I32 { return value }\\npub fn main() -> I32 { return identity() }',
  ),
));
const cycleEvaluation = api.Analysis.evaluate(
  snapshotOfSource(
    'memory/packed-cycle',
    new TextEncoder().encode('pub fn main() -> I32 { return main() }'),
  ),
);
const nestedSource = api.SourceFile.make(
  'memory/packed-nested',
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
const nestedSnapshot = snapshotOfSource(
  'memory/packed-nested',
  Uint8Array.from(nestedParse.source.bytes),
);
const nestedAnalysis = semanticModule.elaborateModule(inputOf(nestedSnapshot));
const nestedOuter = nestedAnalysis.functions[1]?.returnedExpression;
const nestedInner = nestedOuter?._tag === 'Call' ? nestedOuter.arguments[0]?.expression : null;
const nestedEvaluation = api.Analysis.evaluate(nestedSnapshot);
const arrayText = 'struct Pair { left: I32 right: I32 }\\nfn choose(values: [Pair; 2], index: I32) -> I32 { return values[index].left }\\npub fn main() -> I32 { return choose([Pair { left: 10, right: 11 }, Pair { left: 42, right: 43 }], 1) }';
const arrayBytes = new TextEncoder().encode(arrayText);
const nativeArraySnapshot = Effect.runSync(
  api.Analysis.ofSource('memory/packed-array', arrayBytes, 'aarch64-apple-darwin'),
);
const wasmArraySnapshot = Effect.runSync(
  api.Analysis.ofSource('memory/packed-array', arrayBytes, 'wasm32-unknown-unknown'),
);
const nativeArrayArtifact = Effect.runSync(
  api.Analysis.codegen(nativeArraySnapshot, { mode: 'release' }),
);
const wasmArrayArtifact = Effect.runSync(
  api.Analysis.codegen(wasmArraySnapshot, { mode: 'release' }),
);
const wasmArrayInstance = new WebAssembly.Instance(
  new WebAssembly.Module(wasmArrayArtifact.bitcode.slice()),
  {},
);
const unionText = 'struct A {}\\nstruct B { value: I32 }\\nstruct C { left: I32 right: I32 }\\nfn accept(value: A | B | C) -> I32 { return 42 }\\nfn widen(value: A | B) -> I32 { return accept(move value) }\\npub fn main() -> I32 { return widen(A {}) }';
const unionBytes = new TextEncoder().encode(unionText);
const nativeUnionSnapshot = Effect.runSync(
  api.Analysis.ofSource('memory/packed-union', unionBytes, 'aarch64-apple-darwin'),
);
const wasmUnionSnapshot = Effect.runSync(
  api.Analysis.ofSource('memory/packed-union', unionBytes, 'wasm32-unknown-unknown'),
);
const nativeUnionArtifact = Effect.runSync(
  api.Analysis.codegen(nativeUnionSnapshot, { mode: 'release' }),
);
const wasmUnionArtifact = Effect.runSync(
  api.Analysis.codegen(wasmUnionSnapshot, { mode: 'release' }),
);
const wasmUnionInstance = new WebAssembly.Instance(
  new WebAssembly.Module(wasmUnionArtifact.bitcode.slice()),
  {},
);
const names = analysis.functions.map((fact) =>
  fact.declaration.name._tag === 'Present' ? fact.declaration.name.spelling : null,
);
console.log(
  JSON.stringify({
    root: Object.keys(api).sort(),
    rootNamespaces: Object.fromEntries(
      paths
        .filter((path) => path !== './NativeToolchain' && path !== './Driver')
        .map((path) => [path, Object.keys(api[path.slice(2)]).sort()]),
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
          ? cycleEvaluation.reason.cycle.map((instance) => instance.declaration.name)
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
    arrays: {
      diagnostics: api.Analysis.diagnostics(nativeArraySnapshot),
      types: api.Analysis.fixedArrayTypesOf(nativeArraySnapshot, 'memory/packed-array').map(api.Type.encode),
      literals: api.Analysis.arrayLiteralsOf(nativeArraySnapshot, 'memory/packed-array').map((literal) => ({
        state: literal.state._tag,
        length: literal.length,
        elements: literal.elements.map((element) => element.compatibility._tag),
      })),
      indexes: api.Analysis.indexProjectionsOf(nativeArraySnapshot, 'memory/packed-array').map((projection) => projection.bounds),
      hir: api.Hir.encode(api.Analysis.rootAnalysis(nativeArraySnapshot).hir),
      ownership: api.Ownership.encode(api.Analysis.ownershipOf(nativeArraySnapshot, 'memory/packed-array')),
      layout: api.Layout.encode(api.Analysis.layoutOf(nativeArraySnapshot).value),
      mir: api.Mir.encode(api.Analysis.loweredMir(nativeArraySnapshot)),
      trace: api.Analysis.evaluate(nativeArraySnapshot).trace,
      nativeSymbols: nativeArrayArtifact.symbols,
      nativeIr: nativeArrayArtifact.ir,
      nativeBitcode: Array.from(nativeArrayArtifact.bitcode),
      wasmSymbols: wasmArrayArtifact.symbols,
      wasmWat: wasmArrayArtifact.ir,
      wasmBytes: Array.from(wasmArrayArtifact.bitcode),
      wasmResult: wasmArrayInstance.exports.silk_main(),
    },
    unions: {
      diagnostics: api.Analysis.diagnostics(nativeUnionSnapshot),
      types: api.Analysis.unionLayoutsOf(nativeUnionSnapshot).map((entry) => api.Type.encode(entry.type)),
      hir: api.Hir.encode(api.Analysis.rootAnalysis(nativeUnionSnapshot).hir),
      layout: api.Layout.encode(api.Analysis.layoutOf(nativeUnionSnapshot).value),
      mir: api.Mir.encode(api.Analysis.loweredMir(nativeUnionSnapshot)),
      trace: api.Analysis.evaluate(nativeUnionSnapshot).trace,
      nativeIr: nativeUnionArtifact.ir,
      wasmWat: wasmUnionArtifact.ir,
      wasmResult: wasmUnionInstance.exports.silk_main(),
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
    const inspected = inspectCompiler()
    expect(inspectCompiler()).toBe(inspected)
    const api = JSON.parse(inspected)
    expect(api.root).toEqual([
      'Analysis',
      'Backend',
      'BackendRegistry',
      'BootstrapEvaluation',
      'DeclarationIndex',
      'Diagnostic',
      'Elaboration',
      'FormattedDocument',
      'Formatter',
      'Hir',
      'Instances',
      'Layout',
      'Lexer',
      'Lower',
      'Match',
      'Mir',
      'ModuleClosure',
      'NameResolution',
      'Operator',
      'Ownership',
      'Parser',
      'SourceFile',
      'SourceResolver',
      'SourceSpan',
      'SyntaxFile',
      'SyntaxTree',
      'Target',
      'Token',
      'ToolchainPlan',
      'Type',
      'TypeCompatibility',
      'WasmBackend',
    ])
    for (const [path, exports] of Object.entries(api.deep) as ReadonlyArray<
      readonly [string, ReadonlyArray<string>]
    >) {
      expect(exports.length, `${path} has no exports`).toBeGreaterThan(0)
      if (path !== './NativeToolchain' && path !== './Driver')
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
    expect(api.deep['./Formatter']).toContain('format')
    expect(api.deep['./SyntaxTree']).toContain('tokens')
    expect(api.deep['./Operator']).toContain('infix')
    expect(api.deep['./Type']).toContain('nominal')
    expect(api.deep['./Type']).toContain('fixedArray')
    expect(api.deep['./TypeCompatibility']).toContain('check')
    expect(api.functionCount).toBe(2)
    expect(api.callCount).toBe(1)
    expect(api.arrays.diagnostics).toEqual([])
    expect(api.arrays.types).toEqual(['Array<memory/packed-array.Pair, 2>'])
    expect(api.arrays.literals).toEqual([
      {
        state: 'Complete',
        length: 2,
        elements: ['Compatible', 'Compatible'],
      },
    ])
    expect(api.arrays.indexes).toEqual([{ _tag: 'Runtime', length: 2 }])
    expect(api.arrays.hir).toContain('construct-array')
    expect(api.arrays.ownership).toContain('cleanup array:')
    expect(api.arrays.layout).toContain('repr=repeated')
    expect(api.arrays.mir).toContain('read-place')
    expect(api.arrays.trace.map((event: { _tag: string }) => event._tag)).toContain('PlaceRead')
    expect(api.arrays.nativeIr).toContain('icmp ult')
    expect(api.arrays.wasmWat).toContain('i32.lt_u')
    expect(api.arrays.wasmResult).toBe(42)
    expect(api.unions.diagnostics).toEqual([])
    expect(api.unions.types).toEqual([
      'memory/packed-union.A | memory/packed-union.B',
      'memory/packed-union.A | memory/packed-union.B | memory/packed-union.C',
    ])
    expect(api.unions.hir).toContain('union-inject')
    expect(api.unions.hir).toContain('union-widen')
    expect(api.unions.layout).toContain('repr=union tag=i32')
    expect(api.unions.layout).toContain('I32[tag],I32[payload[0]]')
    expect(api.unions.mir).toContain('union-inject')
    expect(api.unions.mir).toContain('union-widen')
    expect(api.unions.trace.map((event: { _tag: string }) => event._tag)).toEqual(
      expect.arrayContaining(['UnionConversion']),
    )
    expect(api.unions.nativeIr).toContain('union')
    expect(api.unions.wasmWat).toContain('select')
    expect(api.unions.wasmResult).toBe(42)
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
      callContract: {
        _tag: 'Compatible',
        expectedCount: 1,
        actualCount: 1,
        typeArguments: [],
        substitution: {},
      },
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
      rootTrace: ['Entry', 'RegionEntry', 'Call', 'Binding', 'RegionEntry', 'Return', 'Return'],
      deepTag: 'Completed',
      deepResult: { _tag: 'I32Value', value: 42 },
      cycleTag: 'Blocked',
      cycleReason: 'RecursiveCycle',
      cycleNames: ['main', 'main'],
      cycleTrace: ['Entry', 'RegionEntry', 'Call'],
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
        'RegionEntry',
        'Call',
        'Binding',
        'RegionEntry',
        'Return',
        'Call',
        'Binding',
        'RegionEntry',
        'Return',
        'Return',
      ],
    })
    expect(api.parserDiagnostics).toEqual([])
  } finally {
    rmSync(temporary, { recursive: true, force: true })
  }
}, 15_000)

test('the compiler CLI release candidate installs with its project-first command surface', () => {
  const temporary = mkdtempSync(resolve(tmpdir(), 'silk-effect-compiler-cli-release-candidate-'))

  try {
    const archiveRoot = resolve(temporary, 'archives')
    const unpackRoot = resolve(temporary, 'unpacked')
    mkdirSync(archiveRoot)
    mkdirSync(unpackRoot)

    for (const root of [
      compilerCliPackageRoot,
      compilerPackageRoot,
      packageRoot,
      wasmPackageRoot,
    ]) {
      execFileSync('pnpm', ['pack', '--pack-destination', archiveRoot], {
        cwd: root,
        stdio: 'pipe',
      })
    }

    const archives = readdirSync(archiveRoot)
    const archive = archives.find(
      (file) => file.startsWith('silk-effect-compiler-cli-') && file.endsWith('.tgz'),
    )
    const compilerArchive = archives.find(
      (file) => file.startsWith('silk-effect-compiler-') && !file.includes('-cli-'),
    )
    const llvmArchive = archives.find((file) => file.startsWith('silk-effect-llvm-'))
    const wasmArchive = archives.find((file) => file.startsWith('silk-effect-wasm-'))
    expect(archive).toBeDefined()
    expect(compilerArchive).toBeDefined()
    expect(llvmArchive).toBeDefined()
    expect(wasmArchive).toBeDefined()
    execFileSync('tar', ['-xzf', resolve(archiveRoot, archive ?? ''), '-C', unpackRoot])

    const packedRoot = resolve(unpackRoot, 'package')
    const manifest = JSON.parse(readFileSync(resolve(packedRoot, 'package.json'), 'utf8'))
    expect(manifest.name).toBe('@silk-effect/compiler-cli')
    expect(manifest.private).not.toBe(true)
    expect(Object.keys(manifest.dependencies ?? {}).sort()).toEqual([
      '@effect/platform-node',
      '@silk-effect/compiler',
      'effect',
      'smol-toml',
    ])
    expect(Object.keys(manifest.exports).sort()).toEqual([
      '.',
      './BuildCommand',
      './BuildExeCommand',
      './BuildPlan',
      './CheckCommand',
      './Cli',
      './FileSourceResolver',
      './FormatCommand',
      './FormatWorkflow',
      './Program',
      './Project',
      './ProjectOptions',
      './Report',
      './RunCommand',
      './SourceEntry',
      './Workflow',
    ])
    expect(manifest.exports).not.toHaveProperty('./CompileCommand')
    expect(manifest.bin).toEqual({ silk: './dist/bin.js' })
    expect(existsSync(resolve(packedRoot, 'dist/bin.js'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'dist/index.d.ts'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'README.md'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'LICENSE'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'src'))).toBe(false)
    expect(existsSync(resolve(packedRoot, 'test'))).toBe(false)

    const consumerRoot = resolve(temporary, 'consumer')
    mkdirSync(consumerRoot)
    writeFileSync(
      resolve(consumerRoot, 'package.json'),
      JSON.stringify({
        private: true,
        type: 'module',
        dependencies: {
          '@silk-effect/compiler-cli': `file:${resolve(archiveRoot, archive ?? '')}`,
        },
      }),
    )
    writeFileSync(
      resolve(consumerRoot, 'pnpm-workspace.yaml'),
      `overrides:\n  '@silk-effect/compiler': file:${resolve(archiveRoot, compilerArchive ?? '')}\n  '@silk-effect/llvm': file:${resolve(archiveRoot, llvmArchive ?? '')}\n  '@silk-effect/wasm': file:${resolve(archiveRoot, wasmArchive ?? '')}\n`,
    )
    execFileSync('pnpm', ['install', '--offline'], { cwd: consumerRoot, stdio: 'pipe' })

    const executable = resolve(consumerRoot, 'node_modules/.bin/silk')
    const help = execFileSync(executable, ['--help'], { cwd: consumerRoot, encoding: 'utf8' })
    expect(help).toContain('build        Build the nearest Silk project.')
    expect(help).toContain('check        Analyze the nearest Silk project')
    expect(help).toContain(
      'format       Format Silk project source into its canonical representation.',
    )
    expect(help).toContain('run          Build and run the nearest Silk project.')
    expect(help).toContain('build-exe    Build one rooted Silk source graph')
    expect(help).not.toContain('\n  compile ')

    writeFileSync(
      resolve(consumerRoot, 'silk.toml'),
      '[package]\nname = "packed-cli"\nroot = "Main.silk"\n',
    )
    writeFileSync(resolve(consumerRoot, 'Main.silk'), 'pub fn main() -> I32 { return 42 }')
    execFileSync(executable, ['check'], { cwd: consumerRoot, stdio: 'pipe' })
    expect(existsSync(resolve(consumerRoot, '.silk'))).toBe(false)

    const api = JSON.parse(
      execFileSync(
        process.execPath,
        [
          '--input-type=module',
          '--eval',
          `import * as api from '@silk-effect/compiler-cli'; console.log(JSON.stringify(Object.keys(api).sort()))`,
        ],
        { cwd: consumerRoot, encoding: 'utf8' },
      ),
    )
    expect(api).toEqual([
      'BuildCommand',
      'BuildExeCommand',
      'BuildPlan',
      'CheckCommand',
      'Cli',
      'FileSourceResolver',
      'FormatCommand',
      'FormatWorkflow',
      'Program',
      'Project',
      'ProjectOptions',
      'Report',
      'RunCommand',
      'SourceEntry',
      'Workflow',
    ])
  } finally {
    rmSync(temporary, { recursive: true, force: true })
  }
}, 30_000)

test('the wasm release candidate is a self-contained ESM package', () => {
  const temporary = mkdtempSync(resolve(tmpdir(), 'silk-effect-wasm-release-candidate-'))

  try {
    const archiveRoot = resolve(temporary, 'archives')
    const unpackRoot = resolve(temporary, 'unpacked')
    mkdirSync(archiveRoot)
    mkdirSync(unpackRoot)

    execFileSync('pnpm', ['pack', '--pack-destination', archiveRoot], {
      cwd: wasmPackageRoot,
      stdio: 'pipe',
    })

    const archive = readdirSync(archiveRoot).find((file) => file.endsWith('.tgz'))
    expect(archive).toBeDefined()
    execFileSync('tar', ['-xzf', resolve(archiveRoot, archive ?? ''), '-C', unpackRoot])

    const packedRoot = resolve(unpackRoot, 'package')
    const manifest = JSON.parse(readFileSync(resolve(packedRoot, 'package.json'), 'utf8'))

    expect(manifest.name).toBe('@silk-effect/wasm')
    expect(manifest.private).not.toBe(true)
    expect(Object.keys(manifest.dependencies ?? {})).toEqual(['effect'])
    expect(Object.keys(manifest.exports).sort()).toEqual([
      '.',
      './Binary',
      './Builder',
      './ConstExpr',
      './Data',
      './Elem',
      './Export',
      './Func',
      './Global',
      './Import',
      './Instr',
      './Limits',
      './Memory',
      './Table',
      './Tag',
      './Type',
      './ValType',
      './WasmError',
      './WatText',
    ])
    expect(existsSync(resolve(packedRoot, 'dist/index.js'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'dist/index.d.ts'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'README.md'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'LICENSE'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'UPSTREAM.md'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'src'))).toBe(false)
    expect(existsSync(resolve(packedRoot, 'fixtures'))).toBe(false)
    expect(existsSync(resolve(packedRoot, 'scripts'))).toBe(false)

    const packedFiles = (directory: string): ReadonlyArray<string> =>
      readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
        const path = resolve(directory, entry.name)
        return entry.isDirectory() ? packedFiles(path) : [path]
      })
    expect(
      packedFiles(packedRoot).filter((file) => file.endsWith('.ts') && !file.endsWith('.d.ts')),
    ).toEqual([])

    const consumerRoot = resolve(temporary, 'consumer')
    mkdirSync(consumerRoot)
    writeFileSync(
      resolve(consumerRoot, 'package.json'),
      JSON.stringify({
        private: true,
        type: 'module',
        dependencies: { '@silk-effect/wasm': `file:${resolve(archiveRoot, archive ?? '')}` },
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
          `import * as api from '@silk-effect/wasm'; const paths = ${JSON.stringify(deepPaths)}; const modules = await Promise.all(paths.map((path) => import(\`@silk-effect/wasm/\${path.slice(2)}\`))); console.log(JSON.stringify({ root: Object.keys(api).sort(), rootNamespaces: Object.fromEntries(paths.filter((path) => path !== './WasmError').map((path) => [path, Object.keys(api[path.slice(2)]).sort()])), deep: Object.fromEntries(paths.map((path, index) => [path, Object.keys(modules[index]).sort()])) }))`,
        ],
        {
          cwd: consumerRoot,
          encoding: 'utf8',
          env: {
            ...process.env,
            PATH: dirname(process.execPath),
            WASM_TOOLS: '/unavailable/wasm-tools',
          },
        },
      )

    const first = inspectApi()
    const second = inspectApi()
    expect(first).toBe(second)
    const api = JSON.parse(first)
    expect(api.root).toEqual([
      'Binary',
      'Builder',
      'ConstExpr',
      'Data',
      'Elem',
      'Export',
      'Func',
      'Global',
      'Import',
      'Instr',
      'Limits',
      'Memory',
      'Table',
      'Tag',
      'Type',
      'ValType',
      'WasmError',
      'WatText',
    ])
    for (const [path, exports] of Object.entries(api.deep) as ReadonlyArray<
      readonly [string, ReadonlyArray<string>]
    >) {
      const rootName = path.slice(2)
      if (rootName === 'ConstExpr' || rootName === 'Limits') continue
      expect(exports.length, `${path} has no exports`).toBeGreaterThan(0)
      if (rootName === 'WasmError') expect(exports).toContain('WasmError')
      else expect(api.rootNamespaces[path]).toEqual(exports)
    }
    expect(api.deep['./Builder']).toContain('make')
    expect(api.deep['./Binary']).toContain('encode')
    expect(api.deep['./WatText']).toContain('render')
    expect(api.deep['./Instr']).toContain('i32Const')
  } finally {
    rmSync(temporary, { recursive: true, force: true })
  }
})

test('the lsp release candidate installs and answers an initialize request', async () => {
  const temporary = mkdtempSync(resolve(tmpdir(), 'silk-effect-lsp-release-candidate-'))

  try {
    const archiveRoot = resolve(temporary, 'archives')
    const unpackRoot = resolve(temporary, 'unpacked')
    mkdirSync(archiveRoot)
    mkdirSync(unpackRoot)

    for (const root of [
      lspPackageRoot,
      compilerCliPackageRoot,
      compilerPackageRoot,
      packageRoot,
      wasmPackageRoot,
    ]) {
      execFileSync('pnpm', ['pack', '--pack-destination', archiveRoot], {
        cwd: root,
        stdio: 'pipe',
      })
    }

    const archives = readdirSync(archiveRoot)
    const archive = archives.find((file) => file.startsWith('silk-effect-lsp-'))
    const cliArchive = archives.find((file) => file.startsWith('silk-effect-compiler-cli-'))
    const compilerArchive = archives.find(
      (file) => file.startsWith('silk-effect-compiler-') && !file.includes('-cli-'),
    )
    const llvmArchive = archives.find((file) => file.startsWith('silk-effect-llvm-'))
    const wasmArchive = archives.find((file) => file.startsWith('silk-effect-wasm-'))
    expect(archive).toBeDefined()
    execFileSync('tar', ['-xzf', resolve(archiveRoot, archive ?? ''), '-C', unpackRoot])

    const packedRoot = resolve(unpackRoot, 'package')
    const manifest = JSON.parse(readFileSync(resolve(packedRoot, 'package.json'), 'utf8'))
    expect(manifest.name).toBe('@silk-effect/lsp')
    expect(manifest.private).not.toBe(true)
    expect(manifest.bin).toEqual({ 'silk-lsp': './dist/bin.js' })
    expect(Object.keys(manifest.exports).sort()).toEqual([
      '.',
      './Document',
      './LineIndex',
      './Server',
      './Workspace',
    ])
    expect(existsSync(resolve(packedRoot, 'dist/bin.js'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'dist/index.d.ts'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'README.md'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'LICENSE'))).toBe(true)
    expect(existsSync(resolve(packedRoot, 'src'))).toBe(false)
    expect(existsSync(resolve(packedRoot, 'test'))).toBe(false)

    const consumerRoot = resolve(temporary, 'consumer')
    mkdirSync(consumerRoot)
    writeFileSync(
      resolve(consumerRoot, 'package.json'),
      JSON.stringify({
        private: true,
        type: 'module',
        dependencies: {
          '@silk-effect/lsp': `file:${resolve(archiveRoot, archive ?? '')}`,
        },
      }),
    )
    writeFileSync(
      resolve(consumerRoot, 'pnpm-workspace.yaml'),
      `overrides:\n  '@silk-effect/compiler-cli': file:${resolve(archiveRoot, cliArchive ?? '')}\n  '@silk-effect/compiler': file:${resolve(archiveRoot, compilerArchive ?? '')}\n  '@silk-effect/llvm': file:${resolve(archiveRoot, llvmArchive ?? '')}\n  '@silk-effect/wasm': file:${resolve(archiveRoot, wasmArchive ?? '')}\n`,
    )
    execFileSync('pnpm', ['install', '--offline'], { cwd: consumerRoot, stdio: 'pipe' })

    const executable = resolve(consumerRoot, 'node_modules/.bin/silk-lsp')
    const initialize = JSON.stringify({
      jsonrpc: '2.0',
      id: 1,
      method: 'initialize',
      params: { processId: null, rootUri: null, capabilities: {} },
    })
    const response = await new Promise<string>((resolvePromise, rejectPromise) => {
      const server = spawn(executable, [], { cwd: consumerRoot })
      let output = ''
      const finish = (): void => {
        server.kill()
        resolvePromise(output)
      }
      const timer = setTimeout(() => {
        server.kill()
        rejectPromise(new Error(`silk-lsp answered nothing; saw: ${output}`))
      }, 15_000)
      server.stdout.on('data', (chunk: Buffer) => {
        output += chunk.toString('utf8')
        if (output.includes('"capabilities"')) {
          clearTimeout(timer)
          finish()
        }
      })
      server.on('error', (error) => {
        clearTimeout(timer)
        rejectPromise(error)
      })
      server.stdin.write(`Content-Length: ${Buffer.byteLength(initialize)}\r\n\r\n${initialize}`)
    })
    expect(response).toContain('"hoverProvider":true')
    expect(response).toContain('"documentFormattingProvider":true')
  } finally {
    rmSync(temporary, { recursive: true, force: true })
  }
}, 60_000)
