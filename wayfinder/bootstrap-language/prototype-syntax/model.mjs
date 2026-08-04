const imperativeExamples = [
  {
    title: 'Built-in dual actor calls',
    source: `pub fn sum(self: I32, that: I32) -> I32 {
  return self + that
}

let direct = Math.sum(2, 3)
let piped = 2 |> Math.sum(3)`,
  },
  {
    title: 'Construct now, run later',
    note:
      'Calling compile packages its owned input but does not enter the body. run evaluates exactly one flow layer.',
    source: `pub flow fn compile(request: own Request) -> Artifact
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
    title: 'Specialize an open flow twice',
    note:
      'Providers are attached before the owned requests. Borrowed providers constrain the specialized flow lifetime.',
    source: `let mut scratch = ArenaAllocator.make()
let fileSystem = NativeFileSystem.make()
let virtualFileSystem = MemoryFileSystem.make()

let withScratch = Allocator.provide(
  Compiler.compile,
  &mut scratch,
  @Scratch,
)

let fsCompilation = FileSystem.provide(withScratch, &fileSystem)
let memoryCompilation = FileSystem.provide(withScratch, &virtualFileSystem)

let diskArtifact = run fsCompilation(move diskRequest)
return run memoryCompilation(move memoryRequest)`,
  },
  {
    title: 'Captured ownership determines reuse',
    note:
      'There is no separate single-shot Flow type. The closed environment permits view twice but take only once.',
    source: `pub flow fn inspect(payload: &Payload) -> Report {
  return Inspector.inspect(payload)
}

pub flow fn consume(payload: own Payload) -> Digest {
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
    title: 'Preserve or flatten one flow layer',
    note:
      'return flow preserves nesting; run evaluates one layer; Flow.flatten explicitly removes one layer.',
    source: `let planned = Planner.plan(move request)
let nested = Flow.map(planned, Compiler.compile)

// Flow<Flow<Artifact>> remains nested until requested.
let compilation = Flow.flatten(nested)
return run compilation`,
  },
  {
    title: 'Acquire a fresh provider for every run',
    note:
      'provideWith brackets each execution. Artifact.promote runs before the build scope closes, so no Scratch borrow escapes.',
    source: `let promoted = Flow.map(Compiler.compile, Artifact.promote)
let withScratch = Allocator.provideWith(
  promoted,
  Scratch.acquire,
  @Scratch,
)
let compilation = Scope.scoped(withScratch, 'build)

let first = run compilation(move firstRequest)
return run compilation(move secondRequest)`,
  },
  {
    title: 'Guard non-tail recursion explicitly',
    note:
      'The compiler accepts a recursive cycle only when it is tail-recursive or crosses Flow.suspend.',
    source: `pub flow fn depth(node: &Node) -> U32 {
  if node.isLeaf {
    return 1
  }

  let child = Flow.suspend(depth(&node.child))
  let childDepth = run child
  return 1 + childDepth
}`,
  },
]

const pipedExamples = [
  {
    title: 'Built-in dual actor calls',
    source: `pub fn sum(self: I32, that: I32) -> I32
  return self + that

let direct = Math.sum(2, 3)
let piped = 2 |> Math.sum(3)`,
  },
  {
    title: 'Construct now, run later',
    note:
      'The body is still lazy, but flatMap makes the dependency between flow steps visible without nested blocks.',
    source: `pub flow fn compile(request: own Request) -> Artifact
  ! FileError | ProcessError | OutOfMemory
  ? &FileSystem | &mut Allocator@Scratch
  return run (
    request.sourcePath
      |> FileSystem.read
      |> Flow.flatMap(Parser.parse)
      |> Flow.flatMap(Backend.emit)
  )

let computation = compile(move request)
return run computation`,
  },
  {
    title: 'Specialize an open flow twice',
    note:
      'The pipe specializes the open function value. Owned inputs arrive only when each specialized function is called.',
    source: `let withScratch = Compiler.compile
  |> Allocator.provide(&mut scratch, @Scratch)

let fsCompilation = withScratch
  |> FileSystem.provide(&fileSystem)

let memoryCompilation = withScratch
  |> FileSystem.provide(&virtualFileSystem)

let diskArtifact = run fsCompilation(move diskRequest)
return run memoryCompilation(move memoryRequest)`,
  },
  {
    title: 'Captured ownership determines reuse',
    note:
      'Piping does not change access. A borrowed capture remains reusable; a moved capture remains take-only.',
    source: `pub flow fn inspect(payload: &Payload) -> Report
  return payload |> Inspector.inspect

pub flow fn consume(payload: own Payload) -> Digest
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
      'map would preserve Flow<Flow<Artifact>>. flatMap deliberately composes and removes that one layer.',
    source: `let compilation = move request
  |> Planner.plan
  |> Flow.flatMap(Compiler.compile)

return run compilation`,
  },
  {
    title: 'Acquire a fresh provider for every run',
    note:
      'This chain is a reusable recipe. Acquisition, LIFO cleanup, and the build scope occur independently on every run.',
    source: `let compilation = Compiler.compile
  |> Flow.map(Artifact.promote)
  |> Allocator.provideWith(Scratch.acquire, @Scratch)
  |> Scope.scoped('build)

let first = run compilation(move firstRequest)
return run compilation(move secondRequest)`,
  },
  {
    title: 'Guard non-tail recursion explicitly',
    note:
      'Flow.suspend is an ordinary pipeable operation. It lowers the recursive edge through a trampoline frame.',
    source: `pub flow fn depth(node: &Node) -> U32 {
  if node.isLeaf {
    return 1
  }

  return run (
    depth(&node.child)
      |> Flow.suspend
      |> Flow.map(Math.add(1))
  )
}`,
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
      'Effect needs a thunk because an ordinary JavaScript function constructs its Effect eagerly. Silk flow calls are lazy already.',
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
      'flow fn builds a lazy computation; run sequences one layer while ordinary actor calls stay data-first.',
    tension:
      'Sequencing is explicit and familiar, but repeated run expressions may feel heavier in orchestration code.',
    examples: imperativeExamples,
  },
  {
    id: 'piped',
    name: 'Reified fully piped',
    thesis:
      'Flow operations compose recipes before run; providers, scopes, flattening, and suspension remain ordinary actor calls.',
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
