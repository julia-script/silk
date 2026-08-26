import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'

const encoder = new TextEncoder()
const exampleSource = readFileSync(
  new URL('../../../examples/file-system/main.silk', import.meta.url),
  'utf8',
)

const portableProvider = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.u8 as u8
import silk.usize as usize
import silk.filesystem {
  Path,
  DirectoryEntry,
  DirectoryInfo,
  FileError,
  FileInfo,
  FileSystem,
  view as pathView,
  createDirectoriesRecursively,
  createDirectoryOperation,
  createTemporaryDirectoryOperation,
  directory,
  directoryEntry,
  directoryInfo,
  error as fsError,
  file,
  fileInfo,
  make as pathMake,
  alreadyExists,
  notEmpty,
  notFound,
  parent,
  statOperation,
  tooLarge,
  readFileOperation,
  removeDirectoryOperation,
  removeFileOperation,
  wrongType,
  writeFileOperation,
  writeFileWithParents,
  exists,
  unsupported
}
import silk.bytes { Bytes, asSlice as bytesSlice, copy as bytesCopy }
import silk.result { Failure, Result, Success }
import silk.vector {
  Vector,
  append as vectorAppend,
  asSlice as vectorSlice,
  length as vectorLength,
  make as vectorMake
}

struct MemoryFileSystem {
  contents: [u8; 4]
  fileExists: bool
  bExists: bool
  cExists: bool
  calls: i32
  creates: i32
}

fn emptyContents() -> [u8; 4] { return [u8.toU8(0), u8.toU8(0), u8.toU8(0), u8.toU8(0)] }

effect fn memoryRead(
  self: &mut MemoryFileSystem,
  path: &Path
) -> Bytes ! FileError | OutOfMemoryError ? &mut Allocator {
  self.calls = self.calls + 1
  if self.fileExists == false { fail fsError(readFileOperation(), notFound()) }
  let contents = self.contents
  return run bytesCopy(&contents)
}

effect fn memoryWrite(
  self: &mut MemoryFileSystem,
  path: &Path,
  bytes: &[u8]
) -> () ! FileError {
  self.calls = self.calls + 1
  if bytes.length != usize.add(0, 4) {
    fail fsError(writeFileOperation(), tooLarge())
  }
  let mut contents = Intrinsic.replace(self.contents, emptyContents())
  let mut index = usize.add(0, 0)
  while index < bytes.length {
    contents[index] = bytes[index]
    index = index + usize.add(0, 1)
  }
  self.contents = move contents
  self.fileExists = true
  return ()
}

effect fn memoryStat(
  self: &mut MemoryFileSystem,
  path: &Path
) -> FileInfo | DirectoryInfo ! FileError {
  self.calls = self.calls + 1
  let value = pathView(path)
  if value == "/" { return directoryInfo() }
  if value == "/data" { return directoryInfo() }
  if value == "/a" { return directoryInfo() }
  if value == "/a/b" {
    if self.bExists { return directoryInfo() }
    fail fsError(statOperation(), notFound())
  }
  if value == "/a/b/c" {
    if self.cExists { return directoryInfo() }
    fail fsError(statOperation(), notFound())
  }
  if value == "/data/file" {
    if self.fileExists { return fileInfo(usize.add(0, 4)) }
  }
  fail fsError(statOperation(), notFound())
}

effect fn memoryList(
  self: &mut MemoryFileSystem,
  path: &Path
) -> Vector<DirectoryEntry> ! FileError | OutOfMemoryError ? &mut Allocator {
  self.calls = self.calls + 1
  return vectorMake<DirectoryEntry>()
}

effect fn memoryCreate(self: &mut MemoryFileSystem, path: &Path) -> () ! FileError {
  self.calls = self.calls + 1
  let value = pathView(path)
  if value == "/a/b" {
    if self.bExists { fail fsError(createDirectoryOperation(), alreadyExists()) }
    self.bExists = true
    self.creates = self.creates + 1
    return ()
  }
  if value == "/a/b/c" {
    if self.cExists { fail fsError(createDirectoryOperation(), alreadyExists()) }
    self.cExists = true
    self.creates = self.creates + 1
    return ()
  }
  fail fsError(createDirectoryOperation(), wrongType())
}

effect fn memoryRemoveFile(self: &mut MemoryFileSystem, path: &Path) -> () ! FileError {
  self.calls = self.calls + 1
  if self.fileExists == false { fail fsError(removeFileOperation(), notFound()) }
  self.fileExists = false
  return ()
}

effect fn memoryRemoveDirectory(self: &mut MemoryFileSystem, path: &Path) -> () ! FileError {
  self.calls = self.calls + 1
  if self.bExists { fail fsError(removeDirectoryOperation(), notEmpty()) }
  return ()
}

/// The in-memory provider has no namespace to make a unique name in.
effect fn memoryCreateTemporary(
  self: &mut MemoryFileSystem,
  within: &Path,
  prefix: &[u8]
) -> Path ! FileError | OutOfMemoryError ? &mut Allocator {
  fail fsError(createTemporaryDirectoryOperation(), unsupported())
}

impl FileSystem for MemoryFileSystem {
  readFile: MemoryFileSystem.memoryRead
  writeFile: MemoryFileSystem.memoryWrite
  stat: MemoryFileSystem.memoryStat
  listDirectory: MemoryFileSystem.memoryList
  createDirectory: MemoryFileSystem.memoryCreate
  removeFile: MemoryFileSystem.memoryRemoveFile
  removeDirectory: MemoryFileSystem.memoryRemoveDirectory
  createTemporaryDirectory: MemoryFileSystem.memoryCreateTemporary
}

fn checksum(values: &[u8]) -> i32 {
  let mut total = 0
  let mut index = usize.add(0, 0)
  while index < values.length {
    total = total + u8.toI32(values[index])
    index = index + usize.add(0, 1)
  }
  return total
}

effect fn program() -> i32 ! FileError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut fs = MemoryFileSystem {
    contents: emptyContents(),
    fileExists: false,
    bExists: false,
    cExists: false,
    calls: 0,
    creates: 0,
  }
  let filePath = run pathMake("/data/file") |> Effect.provideMut(&mut allocator)
  let input = [u8.toU8(1), u8.toU8(2), u8.toU8(3), u8.toU8(4)]
  let written = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(writeFileWithParents(&filePath, &input), &mut fs),
    &mut allocator
  )
  let owned = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(FileSystem.readFile(&filePath), &mut fs),
    &mut allocator
  )
  if checksum(bytesSlice(&owned)) != 10 { return 1 }

  if run Effect.provideMut(exists(&filePath), &mut fs) {} else { return 2 }
  let nested = run pathMake("/a/b/c") |> Effect.provideMut(&mut allocator)
  let created = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(createDirectoriesRecursively(&nested), &mut fs),
    &mut allocator
  )
  if fs.creates != 2 { return 3 }
  if fs.bExists == false { return 4 }
  if fs.cExists == false { return 5 }
  let dataPath = run pathMake("/data") |> Effect.provideMut(&mut allocator)
  let entries = run Intrinsic.bindRequirementMut(
    Intrinsic.bindRequirementMut(FileSystem.listDirectory(&dataPath), &mut fs),
    &mut allocator
  )
  if vectorLength<DirectoryEntry>(&entries) != usize.add(0, 0) { return 6 }
  let child = run pathMake("/data/child") |> Effect.provideMut(&mut allocator)
  let entry = directoryEntry(move child, file())
  let removed = run Effect.provideMut(FileSystem.removeFile(&filePath), &mut fs)
  if run Effect.provideMut(exists(&filePath), &mut fs) { return 9 }
  return 42
}

pub fn main() -> i32 {
  let completed = run Intrinsic.effectResult(program())
  return match move completed {
    Result<i32, FileError | OutOfMemoryError> { value: outcome } => match move outcome {
      Success<i32> { value } => value
      Failure<FileError | OutOfMemoryError> { error } => 10
    }
  }
}`

it.effect('constructs and resolves normalized provider-absolute Paths', () =>
  Effect.gen(function* () {
    const source = `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.filesystem { FileError }
import silk.filesystem { Path, isRoot, make, name, parent, resolve, root, view }
import silk.option { None, Option, Some }
import silk.result { Failure, Result, Success }
fn matchesParent(possible: Option<Path>, expected: string) -> bool {
  return match move possible {
    None {} => false
    Some<Path> { value } => view(&value) == expected
  }
}

effect fn check() -> i32 ! FileError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let canonicalRoot = run root() |> Effect.provideMut(&mut allocator)
  if isRoot(&canonicalRoot) == false { return 3 }
  if name(&canonicalRoot) != "" { return 4 }
  let rootParent = run parent(&canonicalRoot) |> Effect.provideMut(&mut allocator)
  if matchesParent(move rootParent, "/") { return 5 }
  let base = run make("/project/src") |> Effect.provideMut(&mut allocator)
  let resolved = run resolve(&base, "../assets/./logo.bin")
    |> Effect.provideMut(&mut allocator)
  if view(&resolved) != "/project/assets/logo.bin" { return 1 }
  if name(&resolved) != "logo.bin" { return 6 }
  let ownedParent = run parent(&resolved) |> Effect.provideMut(&mut allocator)
  if matchesParent(move ownedParent, "/project/assets") { return 42 }
  return 7
}

pub fn main() -> i32 {
  let completed = run Intrinsic.effectResult(check())
  return match move completed {
    Result<i32, FileError | OutOfMemoryError> { value: outcome } => match move outcome {
      Success<i32> { value } => value
      Failure<FileError | OutOfMemoryError> { error } => 2
    }
  }
}`
    const snapshot = yield* Analysis.ofSourceRealized(
      'file-system-acceptance/path',
      encoder.encode(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const mir = Analysis.loweredMir(snapshot)
    assert.deepEqual(MirVerification.verify(mir), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      JSON.stringify({
        reason: evaluated._tag === 'Blocked' ? evaluated.reason : evaluated._tag,
        ensureDirectoryFunctions: mir.functions
          .map((fn) => fn.id.name)
          .filter((name) => name.includes('ensureDirectory')),
      }),
    )
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)

    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const module = new WebAssembly.Module(artifact.bytes.slice())
    assert.deepEqual(WebAssembly.Module.imports(module), [])
    const instance = new WebAssembly.Instance(module, {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect('rejects malformed paths and lexical root escape before service provision', () => {
  const source = `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.filesystem { FileError, joinUtf8, make, resolve }

effect fn construct(value: string) -> bool ! FileError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let path = run make(value) |> Effect.provideMut(&mut allocator)
  return false
}

effect fn resolveEscape() -> bool ! FileError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let base = run make("/a") |> Effect.provideMut(&mut allocator)
  let escaped = run resolve(&base, "../../b") |> Effect.provideMut(&mut allocator)
  return false
}

effect fn rejectProviderBytes() -> bool ! FileError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let base = run make("/a") |> Effect.provideMut(&mut allocator)
  let invalid = run joinUtf8(&base, b"\\xff") |> Effect.provideMut(&mut allocator)
  return false
}

effect fn recovered(error: FileError | OutOfMemoryError) -> bool { return true }

pub fn main() -> i32 {
  if run Effect.catchAll(construct("relative"), recovered) {} else { return 1 }
  if run Effect.catchAll(construct("/trailing/"), recovered) {} else { return 2 }
  if run Effect.catchAll(construct("/double//slash"), recovered) {} else { return 3 }
  if run Effect.catchAll(construct("/./dot"), recovered) {} else { return 4 }
  if run Effect.catchAll(construct("/../parent"), recovered) {} else { return 5 }
  if run Effect.catchAll(construct("/\\u{0}"), recovered) {} else { return 6 }
  if run Effect.catchAll(resolveEscape(), recovered) {} else { return 7 }
  if run Effect.catchAll(rejectProviderBytes(), recovered) {} else { return 8 }
  return 42
}`
  return Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'file-system-acceptance/rejected-paths',
      encoder.encode(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(
      evaluated._tag,
      'Completed',
      JSON.stringify(evaluated._tag === 'Blocked' ? evaluated.reason : evaluated._tag),
    )
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const module = new WebAssembly.Module(artifact.bytes.slice())
    assert.deepEqual(WebAssembly.Module.imports(module), [])
    const instance = new WebAssembly.Instance(module, {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  })
})

it.effect('preserves Path allocation failure without publishing partial owned storage', () => {
  const source = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.filesystem { FileError }
import silk.layout { Layout }
import silk.filesystem { make }

struct QuotaAllocator { remaining: i32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  if self.remaining == 0 { fail OutOfMemoryError {} }
  self.remaining = self.remaining - 1
  let mut inner = Allocator.systemAllocatorProvider()
  return run Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }

effect fn build() -> i32 ! FileError | OutOfMemoryError {
  let mut allocator = QuotaAllocator { remaining: 0 }
  let path = run make("/never-owned") |> Effect.provideMut(&mut allocator)
  return 1
}

effect fn recover(error: FileError | OutOfMemoryError) -> i32 { return 42 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`
  return Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'file-system-acceptance/allocation-failure',
      encoder.encode(source),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.deepEqual(
      evaluated.trace.filter((event) => event._tag === 'AllocationAcquire'),
      [],
    )
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  })
})

it.effect(
  'keeps programs with no FileSystem use free of host imports and filesystem machinery',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'file-system-acceptance/no-use',
        encoder.encode('pub fn main() -> i32 { return 42 }'),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const module = new WebAssembly.Module(artifact.bytes.slice())
      assert.deepEqual(WebAssembly.Module.imports(module), [])
      assert.notInclude(artifact.wat, 'FileSystem')
      assert.notInclude(artifact.wat, 'filesystem')
    }),
)

it.effect(
  'retains unprovided FileSystem and Allocator requirements instead of selecting ambient services',
  () =>
    Effect.gen(function* () {
      const source = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.filesystem { FileError, FileSystem, root }
pub effect fn main() -> i32 ! FileError | OutOfMemoryError ? &mut FileSystem | &mut Allocator {
  let path = run root()
  let info = run FileSystem.stat(&path)
  return 42
}`
      const snapshot = yield* Analysis.ofSourceRealized(
        'file-system-acceptance/unprovided',
        encoder.encode(source),
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const mir = Analysis.loweredMir(snapshot)
      assert.strictEqual(mir.entry._tag, 'UnavailableEntry')
    }),
)

it.effect('keeps the documented portable FileSystem example executable', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'examples/file-system/main',
      encoder.encode(exampleSource),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(Analysis.diagnostics(snapshot), [])
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed')
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(artifact.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect(
  'runs a pure mutable FileSystem provider with helpers on evaluation and direct Wasm',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'file-system-acceptance/provider',
        encoder.encode(portableProvider),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const mir = Analysis.loweredMir(snapshot)
      assert.deepEqual(MirVerification.verify(mir), [])
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(
        evaluated._tag,
        'Completed',
        JSON.stringify({
          reason: evaluated._tag === 'Blocked' ? evaluated.reason : evaluated._tag,
          ensureDirectoryFunctions: mir.functions
            .map((fn) => fn.id.name)
            .filter((name) => name.includes('ensureDirectory')),
        }),
      )
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42n)
      const acquires = evaluated.trace.filter((event) => event._tag === 'AllocationAcquire')
      const releases = evaluated.trace.filter((event) => event._tag === 'AllocationRelease')
      assert.strictEqual(releases.length, acquires.length)
      assert.isTrue(
        evaluated.trace.some(
          (event) => event._tag === 'Call' && event.target.name.startsWith('drop@impl'),
        ),
      )

      const artifact = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const module = new WebAssembly.Module(artifact.bytes.slice())
      assert.deepEqual(WebAssembly.Module.imports(module), [])
      const instance = new WebAssembly.Instance(module, {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
    }),
)
