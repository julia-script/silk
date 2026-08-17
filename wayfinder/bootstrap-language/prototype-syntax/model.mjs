const imperativeExamples = [
  {
    title: 'Automatic callable sections',
    source: `pub fn sum(self: i32, that: i32) -> i32 {
  return self + that
}

let direct = Math.sum(2, 3)
let plusThree = Math.sum(3)
let applied = plusThree(2)
let piped = 2 |> plusThree`,
  },
  {
    title: 'Construct now, run later',
    note:
      'Calling compile packages its owned input but does not enter the body. run evaluates exactly one Effect layer.',
    source: `pub effect fn compile(request: own Request) -> Artifact
  ! FileError | ProcessError | OutOfMemory
  ? &FileSystem | &mut Allocator@Scratch
{
  let source = run FileSystem.read(&request.sourcePath)
  let syntax = run Parser.parse(source)
  return run Backend.emit(move syntax)
}

let computation = compile(move request)
let artifact = run computation
return artifact`,
  },
  {
    title: 'Specialize an open Effect twice',
    note:
      'Each owned request first constructs its Effect; borrowed providers then specialize that concrete lazy value.',
    source: `let mut scratch = SystemAllocator.make()
let fileSystem = NativeFileSystem.make()
let virtualFileSystem = MemoryFileSystem.make()

let fsCompilation = FileSystem.provide(
  Effect.provideMut<&mut Allocator@Scratch>(
    Compiler.compile(move diskRequest),
    &mut scratch,
  ),
  &fileSystem,
)
let memoryCompilation = FileSystem.provide(
  Effect.provideMut<&mut Allocator@Scratch>(
    Compiler.compile(move memoryRequest),
    &mut scratch,
  ),
  &virtualFileSystem,
)

let diskArtifact = run fsCompilation
return run memoryCompilation`,
  },
  {
    title: 'Captured ownership determines reuse',
    note:
      'There is no separate single-shot Effect type. The closed environment permits view twice but take only once.',
    source: `pub effect fn inspect(payload: &Payload) -> Report {
  return Inspector.inspect(payload)
}

pub effect fn consume(payload: own Payload) -> Digest {
  return Hasher.digest(move payload)
}

let reusable = inspect(&payload)
let first = run reusable
let second = run reusable

let once = consume(move payload)
let digest = run once
let again = run once // error: captured payload was taken`,
  },
  {
    title: 'Preserve or flatten one Effect layer',
    note:
      'return Effect preserves nesting; run evaluates one layer; Effect.flatten explicitly removes one layer.',
    source: `let planned = Planner.plan(move request)
let nested = Effect.map(planned, Compiler.compile)

// Effect<Effect<Artifact>> remains nested until requested.
let compilation = Effect.flatten(nested)
return run compilation`,
  },
  {
    title: 'Acquire a fresh provider for every run',
    note:
      'provideWith acquires and drops a fresh provider owner for each execution. Allocation results remain self-contained.',
    source: `let promoted = Effect.map(Compiler.compile, Artifact.promote)
let withScratch = Effect.provideWith<&mut Allocator@Scratch>(
  promoted,
  Scratch.acquire,
)
let compilation = withScratch

let first = run compilation(move firstRequest)
return run compilation(move secondRequest)`,
  },
  {
    title: 'Guard non-tail recursion explicitly',
    note:
      'The compiler accepts a recursive cycle only when it is tail-recursive or crosses Effect.suspend.',
    source: `pub effect fn depth(node: &Node) -> U32 {
  if node.isLeaf {
    return 1
  }

  let child = Effect.suspend(depth(&node.child))
  let childDepth = run child
  return 1 + childDepth
}`,
  },
  {
    title: 'Affine callable environment',
    note:
      'A moved allocation is owned by the section. Calling transfers it once; dropping the uncalled section releases it.',
    source: `fn encode(input: &Syntax, storage: Allocation) -> Artifact
  return Encoder.write(input, move storage)

let storage = run Allocator.allocate(layout)
let encodeIntoStorage: once fn(&Syntax) -> Artifact = encode(move storage)
let artifact = encodeIntoStorage(&syntax)`,
  },
]

const pipedExamples = [
  {
    title: 'Automatic callable sections',
    source: `pub fn sum(self: i32, that: i32) -> i32
  return self + that

let direct = Math.sum(2, 3)
let plusThree = Math.sum(3)
let applied = plusThree(2)
let piped = 2 |> plusThree`,
  },
  {
    title: 'Construct now, run later',
    note:
      'The body is still lazy, but flatMap makes the dependency between Effect steps visible without nested blocks.',
    source: `pub effect fn compile(request: own Request) -> Artifact
  ! FileError | ProcessError | OutOfMemory
  ? &FileSystem | &mut Allocator@Scratch
  return run request.sourcePath
    |> FileSystem.read
    |> Effect.flatMap(Parser.parse)
    |> Effect.flatMap(Backend.emit)

let computation = compile(move request)
return run computation`,
  },
  {
    title: 'Specialize an open Effect twice',
    note:
      'The requests construct two Effects; the following callable sections specialize each value without pipeline-only argument insertion.',
    source: `let fsCompilation = Compiler.compile(move diskRequest)
  |> Effect.provideMut<&mut Allocator@Scratch>(&mut scratch)
  |> FileSystem.provide(&fileSystem)

let memoryCompilation = Compiler.compile(move memoryRequest)
  |> Effect.provideMut<&mut Allocator@Scratch>(&mut scratch)
  |> FileSystem.provide(&virtualFileSystem)

let diskArtifact = run fsCompilation
return run memoryCompilation`,
  },
  {
    title: 'Captured ownership determines reuse',
    note:
      'Piping does not change access. A borrowed capture remains reusable; a moved capture remains take-only.',
    source: `pub effect fn inspect(payload: &Payload) -> Report
  return payload |> Inspector.inspect

pub effect fn consume(payload: own Payload) -> Digest
  return move payload |> Hasher.digest

let reusable = payload |> inspect
let first = run reusable
let second = run reusable

let once = move payload |> consume
let digest = run once
let again = run once // error: captured payload was taken`,
  },
  {
    title: 'flatMap flattens while composing',
    note:
      'map would preserve Effect<Effect<Artifact>>. flatMap deliberately composes and removes that one layer.',
    source: `let compilation = move request
  |> Planner.plan
  |> Effect.flatMap(Compiler.compile)

return run compilation`,
  },
  {
    title: 'Acquire a fresh provider for every run',
    note:
      'This chain is a reusable recipe. Provider acquisition and Drop occur independently on every run.',
    source: `let compilation = Compiler.compile(move request)
  |> Effect.map(Artifact.promote)
  |> Effect.provideWith<&mut Allocator@Scratch>(Scratch.acquire)

return run compilation`,
  },
  {
    title: 'Guard non-tail recursion explicitly',
    note:
      'Effect.suspend is an ordinary pipeable operation. It lowers the recursive edge through a trampoline frame.',
    source: `pub effect fn depth(node: &Node) -> U32 {
  if node.isLeaf {
    return 1
  }

  return run depth(&node.child)
    |> Effect.suspend
    |> Effect.map(Math.add(1))
}`,
  },
  {
    title: 'Affine callable environment',
    note:
      'The automatic section is an ordinary once callable whose moved Allocation is visible to ownership and cleanup.',
    source: `fn encode(input: &Syntax, storage: Allocation) -> Artifact
  return Encoder.write(input, move storage)

let storage = run Allocator.allocate(layout)
let encodeIntoStorage = encode(move storage)
return syntax |> encodeIntoStorage`,
  },
]

const effectExamples = [
  {
    title: 'Effect reference: Function.dual',
    source: `// effect/packages/effect/src/Function.ts
const sum = Function.dual(
  2,
  (self: number, that: number) => self + that
)

sum(2, 3)
pipe(2, sum(3))`,
  },
  {
    title: 'Effect reference: suspend and run',
    note:
      'Effect needs a thunk because an ordinary JavaScript function constructs its Effect eagerly. Silk effect calls are lazy already.',
    source: `// effect/packages/effect/src/Effect.ts
let value = 0
const computation = Effect.suspend(() =>
  Effect.succeed(value++)
)

Effect.runSync(computation) // 0
Effect.runSync(computation) // 1`,
  },
  {
    title: 'Effect reference: specialize with provide',
    note:
      'Effect can derive multiple programs from one value because the request and providers live in garbage-collected closures.',
    source: `const compilation = compile(request)

const fsCompilation = compilation.pipe(
  Effect.provideService(FileSystem, fileSystem)
)

const memoryCompilation = compilation.pipe(
  Effect.provideService(FileSystem, virtualFileSystem)
)`,
  },
  {
    title: 'Effect reference: captures have no access mode',
    note:
      'JavaScript permits both executions. Silk derives view, edit, or take from the captured value instead of inventing Effect categories.',
    source: `const computation = Effect.suspend(() =>
  Effect.succeed(use(someValue))
)

Effect.runSync(computation)
Effect.runSync(computation)`,
  },
  {
    title: 'Effect reference: flatten and flatMap',
    note:
      'Silk keeps the same explicit one-layer composition rule.',
    source: `const nested = Effect.succeed(Effect.succeed(42))
const flattened = Effect.flatten(nested)

const compilation = plan(request).pipe(
  Effect.flatMap(compile)
)`,
  },
  {
    title: 'Effect reference: provide and scope',
    note:
      'Effect acquires the layer and closes its resources for every execution of the scoped program.',
    source: `const compilation = compile(request).pipe(
  Effect.provide(Scratch.layer),
  Effect.scoped
)

Effect.runSync(compilation)
Effect.runSync(compilation)`,
  },
  {
    title: 'Effect reference: suspended recursion',
    note:
      'Effect.suspend delays the recursive construction and returns control to the fiber run loop.',
    source: `const depth = (node: Node): Effect.Effect<number> =>
  node.isLeaf
    ? Effect.succeed(1)
    : Effect.suspend(() => depth(node.child)).pipe(
        Effect.map((value) => value + 1)
      )`,
  },
]

export const variants = [
  {
    id: 'imperative',
    name: 'Reified imperative',
    thesis:
      'effect fn builds a lazy computation; run sequences one layer while ordinary actor calls stay data-first.',
    tension:
      'Sequencing is explicit and familiar, but repeated run expressions may feel heavier in orchestration code.',
    examples: imperativeExamples,
  },
  {
    id: 'piped',
    name: 'Reified fully piped',
    thesis:
      'Effect operations compose recipes before run; providers, flattening, and suspension remain ordinary actor calls.',
    tension:
      'The grammar must make run over a multiline pipeline unambiguous without turning run into another block form.',
    examples: pipedExamples,
  },
  {
    id: 'effect',
    name: 'Actual Effect references',
    thesis: 'Upstream examples grounding laziness, specialization, flattening, scoping, and recursive suspension.',
    tension:
      'Effect always uses runtime values and an interpreter; Silk must preserve ownership and erase static composition during lowering.',
    examples: effectExamples,
  },
]

export const initialState = Object.freeze({
  variantIndex: 0,
  exampleIndex: 0,
  showNotes: true,
})

const wrap = (value, length) => (value + length) % length

export const reduce = (state, action) => {
  switch (action) {
    case 'nextVariant':
      return {
        ...state,
        variantIndex: wrap(state.variantIndex + 1, variants.length),
      }
    case 'previousVariant':
      return {
        ...state,
        variantIndex: wrap(state.variantIndex - 1, variants.length),
      }
    case 'nextExample':
      return {
        ...state,
        exampleIndex: wrap(
          state.exampleIndex + 1,
          variants[state.variantIndex].examples.length,
        ),
      }
    case 'previousExample':
      return {
        ...state,
        exampleIndex: wrap(
          state.exampleIndex - 1,
          variants[state.variantIndex].examples.length,
        ),
      }
    case 'toggleNotes':
      return { ...state, showNotes: !state.showNotes }
    default:
      return state
  }
}

export const selectFrame = (state) => {
  const variant = variants[state.variantIndex]
  const example = variant.examples[state.exampleIndex % variant.examples.length]
  return { variant, example }
}
