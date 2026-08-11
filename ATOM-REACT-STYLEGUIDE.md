# Atom + @effect/atom-react Styleguide

Rules for using Effect v4's reactivity primitives (`effect/unstable/reactivity`) with the React
bindings (`@effect/atom-react`). Every rule here is grounded in the actual implementation —
file references point at the v4 source.

```ts
// Core primitives (v4)
import { Atom, AtomRegistry, AsyncResult, Reactivity, Hydration } from "effect/unstable/reactivity"
import { AtomHttpApi, AtomRpc } from "effect/unstable/reactivity"

// React bindings
import {
  useAtom, useAtomValue, useAtomSet, useAtomRefresh, useAtomMount,
  useAtomSuspense, useAtomSubscribe, useAtomInitialValues,
  RegistryProvider, RegistryContext, HydrationBoundary
} from "@effect/atom-react"
import * as ScopedAtom from "@effect/atom-react/ScopedAtom"
```

> Note: the barrel re-exports `ScopedAtom.make` as a top-level `make`. Always import ScopedAtom
> through its subpath (as above) so `make` in your code is never ambiguous.

---

## 1. The mental model (read this before the rules)

1. **An `Atom<A>` is a description, not state.** State lives in an `AtomRegistry`. The same atom
   object can hold different values in different registries.
2. **The atom's object identity is the registry cache key.** `Atom.make(0) !== Atom.make(0)` — two
   calls are two different pieces of state. Everything downstream (hook subscriptions, Suspense
   promise caches, family memoization) keys off that identity.
3. **Atoms are garbage-collected by default.** `keepAlive` is `false`: when the last subscriber
   leaves, the node is disposed (React's default registry keeps it for 400 ms of idle time, then
   drops it). Finalizers run, fibers are interrupted, state is gone.
4. **Effectful atoms produce `AsyncResult<A, E>`**, a three-state union (`Initial` | `Success` |
   `Failure`) where every variant carries a `waiting` flag and failures keep the previous success.
   There is no `null`-while-loading, no throwing from reads.
5. **Reads are tracked.** Inside a derived atom, `get(other)` registers a dependency; the atom
   rebuilds when a dependency changes. Dependencies are re-registered on every build, so
   conditional dependencies work.

If a piece of code fights any of these five facts, the code is wrong, not the library.

---

## 2. Defining atoms

### Rule 2.1 — Atoms live at module scope. Never create one inside a component.

Every hook, the store cache, and the Suspense promise cache key off the atom's object identity.
An atom created during render is a new identity every render: fresh state, resubscription,
and for `useAtomSuspense` a promise cache entry that never resolves.

```ts
// ✅ DO — module scope
const countAtom = Atom.make(0)

function Counter() {
  const [count, setCount] = useAtom(countAtom)
  return <button onClick={() => setCount((n) => n + 1)}>{count}</button>
}
```

```ts
// ❌ DON'T — new atom every render; state resets, subscriptions churn
function Counter() {
  const countAtom = Atom.make(0)          // new identity each render!
  const [count, setCount] = useAtom(countAtom)
  ...
}
```

```ts
// ❌ DON'T — useMemo is not a fix; it's per-component-instance state with extra steps
// and it silently diverges from every other component using "the same" atom.
const countAtom = React.useMemo(() => Atom.make(0), [])
```

For parameterized atoms use `Atom.family` (Rule 2.3). For deliberate per-subtree instances use
`ScopedAtom` (Rule 6.4). Those are the only two sanctioned exceptions.

### Rule 2.2 — Apply combinators once, at the definition site.

`keepAlive`, `setIdleTTL`, `withEquality`, `withLabel`, `map`, `debounce`, `swr`, … all return a
**new atom object**. A combinator applied at a use site creates a second, unrelated piece of state.

```ts
// ✅ DO — the combinator is part of the definition
const todosAtom = Atom.make(fetchTodos).pipe(Atom.keepAlive)
```

```ts
// ❌ DON'T — todosAtom and the keepAlive copy are DIFFERENT atoms with different state
const todosAtom = Atom.make(fetchTodos)

function Todos() {
  const todos = useAtomValue(todosAtom.pipe(Atom.keepAlive))  // new atom every render, never
  ...                                                          // shares state with todosAtom
}
```

```ts
// ❌ DON'T — mapping inline in the component body
const name = useAtomValue(Atom.map(userAtom, (u) => u.name))  // new derived atom per render
// ✅ DO — either define the derived atom at module scope…
const userNameAtom = Atom.map(userAtom, (u) => u.name)
// …or use the selector overload with a stable function (Rule 4.2)
const name = useAtomValue(userAtom, selectName)
```

### Rule 2.3 — Parameterize with `Atom.family`, never with ad-hoc creation.

`Atom.family` memoizes by argument (structural `Equal`/`Hash`, so struct keys work) and holds
entries weakly — unused atoms are GC'd.

```ts
// ✅ DO
const todoAtom = Atom.family((id: number) =>
  runtime.atom(TodoApi.use((api) => api.getTodo(id)))
)
todoAtom(1) === todoAtom(1)  // true — stable identity, stable state
```

```ts
// ❌ DON'T — a "family" hand-rolled with a Map and no weak refs: unbounded leak
const cache = new Map<number, Atom.Atom<any>>()
const todoAtom = (id: number) => {
  if (!cache.has(id)) cache.set(id, Atom.make(...))
  return cache.get(id)!
}
```

```ts
// ❌ DON'T — keepAlive inside a family with unbounded keys.
// A keepAlive atom that has been read is retained by the registry forever, so the
// family entry can never be collected. Unbounded args + keepAlive = memory leak.
const searchAtom = Atom.family((query: string) =>
  Atom.make(search(query)).pipe(Atom.keepAlive)   // leaks one atom per distinct query
)
// ✅ DO — use a TTL so hot entries stay warm and cold ones die
const searchAtom = Atom.family((query: string) =>
  Atom.make(search(query)).pipe(Atom.setIdleTTL("1 minute"))
)
```

### Rule 2.4 — Decide lifetime explicitly: default, `setIdleTTL`, or `keepAlive`.

Default atoms evaporate when the last subscriber leaves. That is the feature — but it means any
state that must survive navigation needs an explicit lifetime.

```ts
// ✅ DO — session-ish state that must survive unmounts
const sessionAtom = Atom.make<Session | null>(null).pipe(Atom.keepAlive)

// ✅ DO — cached server data: warm for a while, then collected
const productsAtom = runtime.atom(fetchProducts).pipe(Atom.setIdleTTL("5 minutes"))

// ✅ DO — ephemeral UI state (dialog open, hover): default is correct, no combinator
const dialogOpenAtom = Atom.make(false)
```

```ts
// ❌ DON'T — writing to an atom nobody subscribes to and expecting it to stick
registry.set(counterAtom, 1)
// …microtask queue drains…
registry.get(counterAtom)  // 0 again — the node was disposed and rebuilt
```

```ts
// ❌ DON'T — keepAlive on everything "to be safe". It disables the GC that makes
// atoms cheap and turns every atom into a global that lives until registry disposal.
```

### Rule 2.5 — Derive, don't duplicate.

If a value can be computed from other atoms, it must be a derived atom — not a second writable
atom that someone remembers to keep in sync, and not a `useMemo` chain in components.

```ts
// ✅ DO
const cartAtom = Atom.make<ReadonlyArray<Item>>([])
const cartTotalAtom = Atom.make((get) =>
  get(cartAtom).reduce((sum, item) => sum + item.price, 0)
)
```

```ts
// ❌ DON'T — two writables that must be updated together
const cartAtom = Atom.make<ReadonlyArray<Item>>([])
const cartTotalAtom = Atom.make(0)   // someone will forget to update this
```

```ts
// ❌ DON'T — deriving in the component; every consumer re-implements it,
// and the computation runs per subscribed component instead of once per registry
function CartBadge() {
  const items = useAtomValue(cartAtom)
  const total = React.useMemo(() => items.reduce(...), [items])
}
```

### Rule 2.6 — Object-valued atoms that rebuild need `withEquality`.

Change detection defaults to `Object.is`. A derived atom that returns a fresh object every build
notifies all subscribers every time, even when the content didn't change.

```ts
// ✅ DO
const visibleIdsAtom = Atom.make((get) =>
  get(todosAtom).filter((t) => !t.done).map((t) => t.id)
).pipe(Atom.withEquality(Equal.equals))
```

### Rule 2.7 — Label atoms you'll want to see in devtools/errors.

```ts
const checkoutAtom = Atom.make(...).pipe(Atom.withLabel("checkout"))
```

---

## 3. Effectful atoms, services, and runtimes

### Rule 3.1 — Services come from `Atom.runtime(Layer)`. One runtime per bounded context, at module scope.

```ts
// ✅ DO
const runtime = Atom.runtime(Layer.mergeAll(TodoApi.layer, Analytics.layer))

const todosAtom = runtime.atom(TodoApi.use((api) => api.list))
const addTodo = runtime.fn(Effect.fnUntraced(function*(text: string) {
  const api = yield* TodoApi
  return yield* api.add(text)
}))
```

```ts
// ❌ DON'T — building layers by hand inside an atom, or ManagedRuntime in React state.
// The runtime atom memoizes layer builds per registry and tears them down with the
// registry; hand-rolled runtimes leak or rebuild per component.
```

Notes that matter:

- Runtime layer memoization is **registry-scoped** (since v4 commit `9591f0099`): two registries
  get independent service instances, disposed with their registry. Cross-registry sharing is
  opt-in via `Atom.context({ memoMap: Layer.makeMemoMapUnsafe() })` — don't reach for it unless
  you actually have multiple registries that must share infrastructure.
- Multiple runtimes in one registry share layer builds through the registry's memo map, so
  splitting runtimes by domain is free — services provided by both are built once.

### Rule 3.2 — Swap implementations with `Atom.initialValue(runtime.layer, TestLayer)`, never by editing atoms.

This is the test-injection story and the storybook/preview story.

```ts
// ✅ DO — tests
const registry = AtomRegistry.make({
  initialValues: [Atom.initialValue(runtime.layer, TodoApiTest)]
})

// ✅ DO — React
<RegistryProvider initialValues={[Atom.initialValue(runtime.layer, TodoApiTest)]}>
  <App />
</RegistryProvider>
```

```ts
// ❌ DON'T — `if (process.env.NODE_ENV === "test")` inside atom definitions
// ❌ DON'T — module-mocking the atom file; inject a layer instead
```

### Rule 3.3 — Inside effectful atoms, read other atoms through `get`, and know the tracking rules.

```ts
// ✅ DO — tracked read of a plain atom, awaited read of an AsyncResult atom
const summaryAtom = runtime.atom((get) => Effect.gen(function*() {
  const filter = get(filterAtom)                  // tracked: rebuilds when filter changes
  const todos = yield* get.result(todosAtom)      // suspends until todosAtom has a value
  return summarize(todos, filter)
}))
```

The rules, from the implementation:

| Call | Tracked? | Behavior |
|---|---|---|
| `get(atom)` | yes | rebuild when it changes |
| `get.once(atom)` | no | snapshot read |
| `get.result(atom)` | yes | `Effect` that stays pending (`Effect.never`) while `Initial` |
| `get.resultOnce(atom)` | no | resolves once, no dependency |
| inside `Atom.fn` bodies | **no** | all reads are untracked; fn re-runs only when called |

```ts
// ❌ DON'T — expecting a fn atom to react to its dependencies.
// Function-atom bodies read atoms UNTRACKED; this only sees filterAtom when invoked.
const exportCsv = runtime.fn((_: void, get) =>
  Effect.sync(() => toCsv(get(todosAtom), get(filterAtom))) // snapshot at call time — fine,
)                                                           // but it will NOT re-run on change
```

```ts
// ❌ DON'T — get.once when you meant get: the derived value silently freezes
const badAtom = Atom.make((get) => get.once(countAtom) * 2)  // never updates
```

### Rule 3.4 — Long-running or push-based data is a Stream atom, not a polling effect.

`Atom.make(stream)` yields `AsyncResult` that updates on every emission (`waiting: true` until the
stream ends). Unmount interrupts the fiber — cleanup is automatic.

```ts
// ✅ DO
const priceAtom = runtime.atom(
  Stream.fromEventListener(socket, "message").pipe(Stream.map(parsePrice))
)
```

```ts
// ❌ DON'T — setInterval + registry.set from a useEffect. You've re-implemented
// the lifetime management the registry already does, minus the interruption.
```

For "refresh every N seconds" semantics use `Atom.withRefresh("30 seconds")`; for
refetch-on-focus use `Atom.swr({ staleTime, revalidateOnFocus: true })` — don't hand-roll either.

---

## 4. Consuming atoms in React

### Rule 4.1 — Pick the narrowest hook. This is the #1 render-performance lever.

| You need | Use | Re-renders on change? |
|---|---|---|
| value only | `useAtomValue(atom)` | yes |
| value + setter | `useAtom(atom)` | yes |
| setter only | `useAtomSet(atom)` | **no** |
| refresh trigger | `useAtomRefresh(atom)` | **no** |
| keep alive, no read | `useAtomMount(atom)` | no |
| side-effect on change | `useAtomSubscribe(atom, f)` | no |
| suspend until ready | `useAtomSuspense(atom)` | yes |

```ts
// ✅ DO — the submit button never re-renders while the form value changes
function SubmitButton() {
  const submit = useAtomSet(submitFormAtom)
  return <button onClick={() => submit()}>Save</button>
}
```

```ts
// ❌ DON'T — useAtom for write-only access; every keystroke re-renders the button
function SubmitButton() {
  const [, submit] = useAtom(submitFormAtom)   // subscribed for nothing
  ...
}
```

### Rule 4.2 — Selectors must be referentially stable.

`useAtomValue(atom, f)` memoizes `Atom.map(atom, f)` on `[atom, f]`. An inline lambda defeats the
memo and creates a new derived atom every render. Same for `useAtomSubscribe`'s callback — it's in
the effect deps, so an inline callback resubscribes every render (and re-fires `immediate`).

```ts
// ✅ DO — module-level selector
const selectName = (u: User) => u.name
const name = useAtomValue(userAtom, selectName)

// ✅ DO — or a module-level derived atom (preferred when several components need it)
const userNameAtom = Atom.map(userAtom, (u) => u.name)
```

```ts
// ❌ DON'T
const name = useAtomValue(userAtom, (u) => u.name)          // new mapped atom every render
useAtomSubscribe(priceAtom, (p) => console.log(p))          // resubscribes every render
```

Also: never toggle the selector's presence at one call site
(`f ? useAtomValue(a, f) : useAtomValue(a)` across renders) — the hook implementation branches on
the selector argument, so a given call site must always or never pass one.

### Rule 4.3 — Render `AsyncResult` by matching. Never unwrap blindly.

`AsyncResult` is `Initial | Success | Failure`, each with `waiting`. Handle all of them —
`matchWithWaiting` or the builder makes the states impossible to forget.

```tsx
// ✅ DO — matchWithWaiting for typical fetch UI
function Todos() {
  const result = useAtomValue(todosAtom)
  return AsyncResult.matchWithWaiting(result, {
    onWaiting: () => <Spinner />,
    onError: (error) => <ErrorBanner error={error} />,
    onDefect: (defect) => <Crash defect={defect} />,
    onSuccess: (todos) => <TodoList todos={todos} />
  })
}
```

```tsx
// ✅ DO — the builder when you need per-error-tag branches; exhaustive() enforces coverage
AsyncResult.builder(result)
  .onWaiting(() => <Spinner />)
  .onErrorTag("TodoNotFound", () => <NotFound />)
  .onError((e) => <ErrorBanner error={e} />)
  .onDefect((d) => <Crash defect={d} />)
  .onSuccess((todos) => <TodoList todos={todos} />)
  .exhaustive()
```

```tsx
// ✅ DO — stale-while-refreshing: previous data + waiting flag
const result = useAtomValue(todosAtom)
const todos = AsyncResult.getOrElse(result, () => [])
return <TodoList todos={todos} dimmed={AsyncResult.isWaiting(result)} />
```

```ts
// ❌ DON'T — getOrThrow in render: throws NoSuchElementError during every initial load
const todos = AsyncResult.getOrThrow(useAtomValue(todosAtom))

// ❌ DON'T — treating waiting/initial as "no data" and dropping stale content.
// Failure carries previousSuccess and refreshes carry the previous value with
// waiting: true precisely so the UI doesn't blank out. AsyncResult.value(result)
// gives you Option<A> including the failure's previous success — use it.

// ❌ DON'T — mirroring an AsyncResult into local state to "simplify" it
const [todos, setTodos] = React.useState<Todo[]>([])
useAtomSubscribe(todosAtom, (r) => { if (AsyncResult.isSuccess(r)) setTodos(r.value) })
// you now have two sources of truth and lost error/waiting states
```

### Rule 4.4 — Suspense: opt in deliberately, with an ErrorBoundary.

`useAtomSuspense` throws a cached promise while `Initial`, and **throws the squashed cause on
`Failure`** unless `includeFailure: true`. No ErrorBoundary → white screen.

```tsx
// ✅ DO
<ErrorBoundary fallback={<ErrorPage />}>
  <React.Suspense fallback={<Spinner />}>
    <TodoView />
  </React.Suspense>
</ErrorBoundary>

function TodoView() {
  const todos = useAtomSuspense(todosAtom).value   // note: returns Success — read .value
  return <TodoList todos={todos} />
}
```

```ts
// ❌ DON'T — useAtomSuspense without an ErrorBoundary above it
// ❌ DON'T — suspendOnWaiting: true on data that refreshes often: every refresh
//            re-suspends and flashes the Suspense fallback
```

### Rule 4.5 — Never write to atoms during render.

Setters are for event handlers and effects. The only sanctioned render-phase writes are the
library's own guarded ones (`useAtomInitialValues`, `HydrationBoundary`). If you need a value
present on first render, seed it via `initialValues` on the provider — don't `set` in render.

```ts
// ❌ DON'T
function Component({ user }) {
  const setUser = useAtomSet(userAtom)
  setUser(user)              // render-phase write: tearing, loops, StrictMode surprises
  ...
}

// ✅ DO — props-to-atom seeding, once
useAtomInitialValues([[userAtom, user]])
// (later calls for the same atom are ignored by design — it's for initial values only)
```

### Rule 4.6 — Beware the updater-function overload.

In value mode a setter treats a function argument as an updater `(current: R) => W`. You cannot
store a function value directly through it — wrap it.

```ts
const setCount = useAtomSet(countAtom)
setCount((n) => n + 1)          // ✅ updater
setCallback(() => myCallback)   // ✅ storing a function value: wrap it
setCallback(myCallback)         // ❌ myCallback gets CALLED with the current value
```

---

## 5. Writes, mutations, and invalidation

### Rule 5.1 — Mutations are `Atom.fn` / `runtime.fn` atoms, not loose `Effect.runPromise` calls.

Fn atoms give you: an `AsyncResult` state for free (pending/error UI), interruption on unmount,
`Atom.Reset` / `Atom.Interrupt` controls, and reactivity-key invalidation.

```ts
// ✅ DO
const addTodo = runtime.fn(
  Effect.fnUntraced(function*(text: string) {
    const api = yield* TodoApi
    return yield* api.add(text)
  }),
  { reactivityKeys: ["todos"] }          // invalidates dependent queries on SUCCESS
)

function AddTodo() {
  const add = useAtomSet(addTodo)
  const state = useAtomValue(addTodo)    // AsyncResult of the last invocation
  return <button disabled={AsyncResult.isWaiting(state)} onClick={() => add("buy milk")} />
}
```

```ts
// ❌ DON'T — running effects imperatively from components; no state, no interruption,
// no invalidation, and the runtime/services get re-resolved ad hoc
onClick={() => Effect.runPromise(api.add(text)).then(() => refetchSomehow())}
```

### Rule 5.2 — Query/mutation pairs communicate through reactivity keys, not manual refresh choreography.

```ts
// ✅ DO — the query declares what it depends on; mutations declare what they touch
const todosAtom = runtime.atom(fetchTodos).pipe(Atom.withReactivity(["todos"]))
const todoAtom = Atom.family((id: number) =>
  runtime.atom(fetchTodo(id)).pipe(Atom.withReactivity({ todos: [id] }))
)
const renameTodo = runtime.fn(renameEffect, { reactivityKeys: (arg) => ({ todos: [arg.id] }) })
```

Key shapes: an array `["todos"]` is a broad key; a record `{ todos: [1, 2] }` invalidates the
broad `"todos"` key **and** the scoped `todos:1`, `todos:2` keys — this is how you get
list-vs-detail granularity for free.

```ts
// ❌ DON'T — imperative refresh fan-out after each mutation
const refreshList = useAtomRefresh(todosAtom)
const refreshDetail = useAtomRefresh(todoAtom(id))
onClick={async () => { await rename(...); refreshList(); refreshDetail() }}  // will drift
```

Note: `reactivityKeys` invalidate **only on success** — a failed mutation does not refetch.
That is the desired default; don't work around it by invalidating in a `catch`.

### Rule 5.3 — Imperative flows use promise mode; don't poll the atom.

When an event handler needs the mutation's outcome (navigate after save, toast on error):

```ts
// ✅ DO
const save = useAtomSet(saveAtom, { mode: "promiseExit" })
const onClick = async () => {
  const exit = await save(form)
  if (Exit.isSuccess(exit)) navigate(`/todo/${exit.value.id}`)
  else showToast(Cause.squash(exit.cause))
}
```

- `mode: "promise"` resolves with the success value and **rejects** with the squashed cause —
  use it only when a surrounding try/catch is genuinely the right shape.
- `mode: "promiseExit"` never rejects for typed failures — prefer it.

```ts
// ❌ DON'T — set then watch the atom with useEffect to detect completion
```

### Rule 5.4 — Multi-atom writes that must be observed atomically go through `Atom.batch`.

```ts
// ✅ DO — listeners fire once, after both writes commit
Atom.batch(() => {
  registry.set(userAtom, user)
  registry.set(permissionsAtom, perms)
})
```

```ts
// ❌ DON'T — sequential sets when derived atoms/subscribers must not see the
// intermediate state (e.g. user set but permissions still the old user's)
```

### Rule 5.5 — Optimistic UI uses `Atom.optimistic` / `Atom.optimisticFn`, not manual set-and-rollback.

The library variant shows the provisional value with `waiting: true`, refreshes the source on
success, and rolls back on failure — including failures you forgot to handle.

```ts
// ✅ DO
const todosOptimistic = Atom.optimistic(todosAtom)
const addTodo = todosOptimistic.pipe(Atom.optimisticFn({
  reducer: (current, todo: Todo) => [...current, todo],
  fn: runtime.fn((todo: Todo) => api.addTodo(todo), { reactivityKeys: ["todos"] })
}))
// components read todosOptimistic; mutations go through addTodo
```

```ts
// ❌ DON'T
onClick={() => {
  setTodos([...todos, newTodo])          // manual optimism
  save(newTodo).catch(() => setTodos(todos))  // stale closure rollback, races on retry
}}
```

### Rule 5.6 — Pagination is `Atom.pull` / `runtime.pull`, and a write means "next page".

```ts
const feedAtom = runtime.pull(feedStream)   // Writable<PullResult<Post>, void>

function Feed() {
  const result = useAtomValue(feedAtom)
  const loadMore = useAtomSet(feedAtom)
  // result: AsyncResult<{ done: boolean; items: NonEmptyArray<Post> }>
  return <>
    {...}
    <button onClick={() => loadMore()} disabled={/* done */}>Load more</button>
  </>
}
```

Refresh restarts from page one; `disableAccumulation` gives you only the newest chunk.

---

## 6. Registries, scoping, and app structure

### Rule 6.1 — One `RegistryProvider` at the app root. Do not rely on the implicit global registry in apps, tests, or SSR.

Without a provider, all hooks use a module-level global registry created at import time. Fine for
a demo; in tests it bleeds state between cases, and on a server it bleeds state between requests.

```tsx
// ✅ DO — app root
<RegistryProvider>
  <App />
</RegistryProvider>

// ✅ DO — per test
beforeEach(() => { registry = AtomRegistry.make() })
render(<RegistryContext.Provider value={registry}>...</RegistryContext.Provider>)

// ✅ DO — per SSR request: a fresh registry per request, always
```

```ts
// ❌ DON'T — nested RegistryProviders to "scope" state. Registries don't share anything:
// not values, not runtimes/services, not reactivity invalidations. A nested registry
// forks your entire world. Use ScopedAtom (6.4) for per-subtree state instead.
```

Also: `RegistryProvider` options (`defaultIdleTTL`, `scheduleTask`, …) are read **once at
creation** — changing props later does nothing. Disposal on unmount is deferred 500 ms (this is
what makes StrictMode's mount/unmount/remount cycle safe).

### Rule 6.2 — Driving atoms from Effect code goes through the `AtomRegistry` service.

```ts
// ✅ DO — inside an Effect with AtomRegistry available (e.g. a runtime.fn body)
const program = Effect.gen(function*() {
  const todos = yield* Atom.getResult(todosAtom)    // Effect<Todos, E, AtomRegistry>
  yield* Atom.set(filterAtom, "done")
  yield* Atom.refresh(todosAtom)
})
```

Remember Rule 2.4: `registry.get`/`set` on a default atom without any subscriber gives you a node
that is disposed when the microtask queue drains. Long-lived Effect-side interaction with an atom
should `Atom.mount` it (scoped) or the atom should be `keepAlive`.

### Rule 6.3 — Ephemeral, registry-free cells are `AtomRef`, not `Atom`.

`AtomRef` is the lightweight primitive: a mutable cell with subscriptions, no registry, no
dependency graph, `prop()` lenses for immutable nested updates. Use it for local editing state
(form rows, drag state) passed down a subtree; use `Atom` for anything shared or derived.

```ts
const form = AtomRef.make({ name: "", email: "" })
const name = useAtomRefPropValue(form, "name")   // subscribes to just .name
```

### Rule 6.4 — Per-subtree instances of the same atom shape use `ScopedAtom`.

```tsx
// ✅ DO — each Provider subtree gets its own counter; state still lives in the one registry
const Counter = ScopedAtom.make(() => Atom.make(0))

function View() {
  const atom = Counter.use()
  const value = useAtomValue(atom)
  ...
}

<Counter.Provider><View /></Counter.Provider>
<Counter.Provider><View /></Counter.Provider>   // independent state
```

Gotchas: `use()` outside the Provider throws; the factory runs **once per Provider instance** —
a changed `value` prop after mount does not recreate the atom.

```ts
// ❌ DON'T — threading atoms through props/context by hand, or worse, nesting
// registries, to achieve per-subtree state
```

---

## 7. API clients: AtomHttpApi and AtomRpc

### Rule 7.1 — If you have an `HttpApi` or `RpcGroup`, derive the atoms. Don't hand-roll fetch atoms.

```ts
// ✅ DO
class Client extends AtomHttpApi.Service<Client>()("Client", {
  api: Api,
  httpClient: FetchHttpClient.layer,
  baseUrl: "https://api.example.com"
}) {}

// Queries: family-memoized — identical requests are the SAME atom everywhere
const userAtom = (id: number) =>
  Client.query("users", "get", {
    params: { id },
    reactivityKeys: { users: [id] },
    timeToLive: "1 minute"
  })

// Mutations
const updateUser = Client.mutation("users", "update")
```

What you get for free (and would otherwise re-implement badly): family memoization on the full
request, reactivity wiring, TTLs, `Atom.serializable` for SSR (via `serializationKey`), typed
errors in the `AsyncResult` error channel, and transport/decode errors converted to **defects** —
so your error channel only contains errors your API actually declares.

```ts
// ❌ DON'T — Atom.make((get) => HttpClient...) per endpoint: no memoization
// (every call site makes a distinct atom), no invalidation, hand-typed errors
```

`AtomRpc.Service` is the same shape for RPC groups; stream RPCs come back as pull atoms
(pagination semantics, Rule 5.6). Its `makeEffect` option is the sanctioned test seam for
stubbing the client.

### Rule 7.2 — Persistent client state uses `Atom.kvs`; URL state uses `Atom.searchParam`.

```ts
const theme = Atom.kvs({
  runtime: kvsRuntime,               // runtime providing a KeyValueStore layer
  key: "theme",
  schema: Schema.Literals(["light", "dark"]),
  defaultValue: () => "light"
})

const page = Atom.searchParam("page", { schema: Schema.NumberFromString })
```

```ts
// ❌ DON'T — localStorage.getItem/setItem inside components or effects; you lose
// schema validation, SSR safety, and cross-tab/cross-component consistency.
// ❌ DON'T — expect searchParam writes to hit the URL synchronously: they are
// debounced 500 ms and coalesced into a single pushState. The schema must be
// synchronous and context-free.
```

---

## 8. SSR and hydration

### Rule 8.1 — Only `Atom.serializable` atoms cross the wire.

```ts
const todosAtom = runtime.atom(fetchTodos).pipe(
  Atom.serializable({ key: "todos", schema: AsyncResult.Schema({ success: TodoList }) })
)
```

The `key` becomes the registry identity for that atom — it must be globally unique and stable
across server and client builds. (AtomHttpApi/AtomRpc do this for you via `serializationKey`.)
Note `debounce`/`swr`/`withRefresh` wrappers drop serializability — serialize the source.

### Rule 8.2 — The flow is: per-request registry → dehydrate → `HydrationBoundary`.

```tsx
// server
const registry = AtomRegistry.make()
// ...render, mounting serializable atoms...
const state = Hydration.dehydrate(registry, { encodeInitialAs: "promise" })  // streaming SSR

// client
<RegistryProvider>
  <HydrationBoundary state={state}>
    <App />
  </HydrationBoundary>
</RegistryProvider>
```

`encodeInitialAs: "promise"` gives streaming SSR: the effect runs once on the server; the client
resolves the same result without re-running it.

### Rule 8.3 — Client-only effects must not run on the server.

Atoms **do execute during SSR by default**. Anything touching browser APIs or per-user secrets
needs a server-value override:

```ts
const analyticsAtom = Atom.make(browserOnlyEffect).pipe(Atom.withServerValueInitial)
// serves AsyncResult.initial on the server; runs on the client after hydration
```

```ts
// ❌ DON'T — `typeof window !== "undefined"` branches inside the read function
// ❌ DON'T — one module-level registry on the server: cross-request state bleed
```

---

## 9. Testing

### Rule 9.1 — Test atoms through a registry, not through React, whenever possible.

```ts
it.effect("computes totals", () =>
  Effect.gen(function*() {
    const registry = AtomRegistry.make({
      initialValues: [Atom.initialValue(runtime.layer, TodoApiTest)]
    })
    registry.set(addTodo, "buy milk")
    const todos = yield* AtomRegistry.getResult(registry, todosAtom)
    assert.deepStrictEqual(todos.length, 1)
  }))
```

### Rule 9.2 — Fresh registry per test; subscribe or keepAlive before asserting on state.

A `registry.set` without a subscriber can be GC'd before your assertion (Rule 2.4). The core
test suite mounts or subscribes first — do the same.

```ts
// ❌ DON'T
registry.set(count, 5)
await somethingAsync()
expect(registry.get(count)).toBe(5)   // may be 0: node was collected and rebuilt

// ✅ DO
const cancel = registry.subscribe(count, () => {})
registry.set(count, 5)
...
cancel()
```

---

## 10. Migrating common React patterns

How the usual hook idioms translate. The left column is not "wrong React" — it's wrong *once this
library is in the codebase*, because each pattern re-implements something the registry already does
(identity, caching, cancellation, sharing) with weaker guarantees.

| Classic pattern | Replacement |
|---|---|
| `useState` lifted up + prop drilling | module-level atom + `useAtomValue`/`useAtomSet` |
| `useState` + `useEffect` fetch | `runtime.atom(effect)` |
| `useEffect` + subscription + cleanup | stream atom (cleanup = fiber interruption) |
| `useMemo` over shared data | derived atom |
| `useReducer` | `Atom.writable` with a reducing write |
| `useContext` for app state | plain atom (no provider needed); `ScopedAtom` for per-subtree |
| `useEffect` → localStorage sync | `Atom.kvs` |
| `useSearchParams` sync | `Atom.searchParam` |
| `setInterval` polling in `useEffect` | `Atom.withRefresh` |
| debounce via `setTimeout` in `useEffect` | `Atom.debounce` |
| react-query `useQuery`/`useMutation` | `runtime.atom` + `reactivityKeys`, or `AtomHttpApi`/`AtomRpc` |
| `useSyncExternalStore` on a custom store | `Atom.subscriptionRef` / `Atom.make(stream)` |

### 10.1 `useState` + lifting state up → one atom

```tsx
// ❌ BEFORE — state lifted to the nearest common ancestor, drilled through props;
// every intermediate component re-renders on change
function App() {
  const [filter, setFilter] = React.useState<Filter>("all")
  return <Layout><Sidebar filter={filter} onChange={setFilter} /><Main filter={filter} /></Layout>
}
```

```tsx
// ✅ AFTER — no lifting, no drilling; only the two subscribers re-render
const filterAtom = Atom.make<Filter>("all")

function Sidebar() {
  const [filter, setFilter] = useAtom(filterAtom)
  ...
}
function Main() {
  const filter = useAtomValue(filterAtom)
  ...
}
```

### 10.2 `useState` + `useEffect` fetch → effectful atom

```tsx
// ❌ BEFORE — the canonical 30 lines: race conditions on param change, no cancellation
// of the fetch itself, error and loading tracked by hand, refetch requires a "tick" state,
// and every mounted copy of the component fetches independently
function Todos({ userId }: { userId: number }) {
  const [todos, setTodos] = React.useState<Todo[]>([])
  const [loading, setLoading] = React.useState(true)
  const [error, setError] = React.useState<Error | null>(null)
  React.useEffect(() => {
    let cancelled = false
    setLoading(true)
    fetchTodos(userId)
      .then((t) => { if (!cancelled) setTodos(t) })
      .catch((e) => { if (!cancelled) setError(e) })
      .finally(() => { if (!cancelled) setLoading(false) })
    return () => { cancelled = true }
  }, [userId])
  ...
}
```

```tsx
// ✅ AFTER — one atom per userId (family), shared by all subscribers, fiber interrupted
// on unmount/param change, loading/error/stale states modeled by AsyncResult
const todosAtom = Atom.family((userId: number) =>
  runtime.atom(TodoApi.use((api) => api.list(userId)))
)

function Todos({ userId }: { userId: number }) {
  const result = useAtomValue(todosAtom(userId))
  return AsyncResult.matchWithWaiting(result, { ... })
}
```

### 10.3 `useEffect` subscription + cleanup → stream atom

```tsx
// ❌ BEFORE — per-component socket, manual cleanup, updates even when unrelated
// state changes remount the effect
React.useEffect(() => {
  const ws = new WebSocket(url)
  ws.onmessage = (e) => setPrice(parse(e.data))
  return () => ws.close()
}, [url])
```

```tsx
// ✅ AFTER — one connection per registry while subscribed; closed automatically
// (scope finalization) when the last subscriber unmounts
const priceAtom = runtime.atom(
  Stream.asyncScoped<Price>((emit) =>
    Effect.acquireRelease(
      Effect.sync(() => {
        const ws = new WebSocket(url)
        ws.onmessage = (e) => emit.single(parse(e.data))
        return ws
      }),
      (ws) => Effect.sync(() => ws.close())
    )
  )
)
```

Multiple components reading `priceAtom` share the single connection — the `useEffect` version
opens one per mounted component.

### 10.4 `useMemo` over shared data → derived atom

```tsx
// ❌ BEFORE — computed per component instance, per render commit
const visible = React.useMemo(() => todos.filter((t) => !t.done), [todos])
```

```tsx
// ✅ AFTER — computed once per registry, cached until dependencies change,
// and components that only need the derived value don't subscribe to the source
const visibleTodosAtom = Atom.make((get) => get(todosAtom).filter((t) => !t.done))
```

Keep `useMemo` for values derived from *props/local state only*. The rule of thumb: if the input
is an atom, the derivation belongs in an atom.

### 10.5 `useReducer` → writable atom with a reducing write

```tsx
// ❌ BEFORE — reducer state is trapped in one component; sharing it means
// context + dispatch drilling
const [state, dispatch] = React.useReducer(reducer, initial)
```

```ts
// ✅ AFTER — same reducer, app-wide state, dispatch from anywhere via useAtomSet
const cartAtom = Atom.writable<Cart, CartAction>(
  () => initialCart,
  (ctx, action) => ctx.setSelf(reducer(ctx.get(cartAtom), action))
)

const dispatch = useAtomSet(cartAtom)   // (action: CartAction) => void
dispatch({ _tag: "AddItem", item })
```

For most cases you don't need the reducer shape at all — a plain writable atom plus updater
functions (`setCount((n) => n + 1)`) or a few `Atom.fn` mutations is simpler.

### 10.6 `useContext` for app state → nothing (or `ScopedAtom`)

```tsx
// ❌ BEFORE — provider pyramid, and every consumer re-renders when ANY field
// of the context value changes
<ThemeContext.Provider value={theme}>
  <UserContext.Provider value={user}>
    <CartContext.Provider value={cart}>...</CartContext.Provider>
  </UserContext.Provider>
</ThemeContext.Provider>
```

```tsx
// ✅ AFTER — atoms are globally addressable; subscriptions are per-atom, so
// a theme change doesn't touch cart consumers. No providers beyond the one
// RegistryProvider at the root.
const themeAtom = Atom.make<Theme>("light").pipe(Atom.keepAlive)
const userAtom = Atom.make<User | null>(null).pipe(Atom.keepAlive)
```

React context remains the right tool for *composition-scoped* values (which subtree am I in,
component-library theming overrides). When you want context-like scoping *of atom state*, that's
exactly `ScopedAtom` (Rule 6.4) — not a nested registry, not a hand-rolled context of atoms.

### 10.7 `useEffect` → localStorage / URL sync → `Atom.kvs` / `Atom.searchParam`

```tsx
// ❌ BEFORE — read-on-mount + write-on-change; no validation, no cross-tab story,
// breaks under SSR (window access during render), duplicated per usage
const [theme, setTheme] = React.useState(() => localStorage.getItem("theme") ?? "light")
React.useEffect(() => { localStorage.setItem("theme", theme) }, [theme])
```

```ts
// ✅ AFTER — schema-validated, SSR-safe, one definition
const themeAtom = Atom.kvs({
  runtime: kvsRuntime,
  key: "theme",
  schema: Schema.Literals(["light", "dark"]),
  defaultValue: () => "light"
})
```

Same shape for URL state: replace the `useSearchParams` + `useEffect` dance with
`Atom.searchParam("page", { schema: Schema.NumberFromString })` (remember the 500 ms write
debounce, Rule 7.2).

### 10.8 Polling and debouncing → `Atom.withRefresh` / `Atom.debounce`

```tsx
// ❌ BEFORE — setInterval in useEffect: keeps polling while the tab is hidden,
// interval per mounted component, stale-closure hazards
React.useEffect(() => {
  const id = setInterval(() => refetch(), 30_000)
  return () => clearInterval(id)
}, [])
```

```ts
// ✅ AFTER — polls only while someone subscribes; timer disposed with the atom
const statusAtom = runtime.atom(fetchStatus).pipe(Atom.withRefresh("30 seconds"))
// or refetch-on-focus with staleness instead of blind polling:
const statusAtom = runtime.atom(fetchStatus).pipe(Atom.swr({ staleTime: "30 seconds", revalidateOnFocus: true }))
```

```tsx
// ❌ BEFORE — debounce via setTimeout/cleanup in useEffect
React.useEffect(() => {
  const id = setTimeout(() => setDebounced(query), 300)
  return () => clearTimeout(id)
}, [query])
```

```ts
// ✅ AFTER
const queryAtom = Atom.make("")
const debouncedQueryAtom = queryAtom.pipe(Atom.debounce(300))
const resultsAtom = Atom.make((get) => searchEffect(get(debouncedQueryAtom)))
```

### 10.9 react-query → atoms

The `useQuery`/`useMutation`/`invalidateQueries` triple maps directly:

| react-query | atoms |
|---|---|
| `useQuery({ queryKey, queryFn })` | `Atom.family` + `runtime.atom(effect)`, or `Client.query(...)` |
| `queryKey` | the atom's identity (family arg / HttpApi request) |
| `staleTime` / `refetchOnWindowFocus` | `Atom.swr({ staleTime, revalidateOnFocus })` |
| `cacheTime` / `gcTime` | `Atom.setIdleTTL` |
| `useMutation` | `runtime.fn` / `Client.mutation(...)` + `useAtomSet` |
| `onSuccess` → `invalidateQueries(key)` | `reactivityKeys` on the mutation (automatic, success-only) |
| `useInfiniteQuery` | `Atom.pull` / `runtime.pull` |
| optimistic `onMutate`/`onError` rollback | `Atom.optimistic` + `Atom.optimisticFn` |
| `<HydrationBoundary state>` | same name here: `Hydration.dehydrate` + `<HydrationBoundary>` |

```ts
// ❌ BEFORE
useQuery({ queryKey: ["todos", userId], queryFn: () => fetchTodos(userId), staleTime: 60_000 })
useMutation({ mutationFn: addTodo, onSuccess: () => queryClient.invalidateQueries({ queryKey: ["todos"] }) })
```

```ts
// ✅ AFTER — and the errors are typed end to end
const todosAtom = Atom.family((userId: number) =>
  runtime.atom(TodoApi.use((api) => api.list(userId))).pipe(
    Atom.withReactivity({ todos: [userId] }),
    Atom.swr({ staleTime: "1 minute" })
  )
)
const addTodo = runtime.fn(addTodoEffect, { reactivityKeys: ["todos"] })
```

Don't run both systems over the same data. If the app already uses react-query, migrate a
domain at a time — one source of truth per endpoint, never two caches for one resource.

### 10.10 `useSyncExternalStore` → `Atom.subscriptionRef` / bridge atoms

If you own an external store (Effect `SubscriptionRef`, an event emitter, a third-party SDK),
don't write `useSyncExternalStore` adapters per component — bridge it once:

```ts
// ✅ SubscriptionRef → two-way writable atom
const settingsAtom = runtime.subscriptionRef(makeSettingsRef)

// ✅ arbitrary emitter → read-only stream atom
const connectionAtom = runtime.atom(Stream.fromEventListener(sdk, "connectionChange"))
```

(The hooks already use `useSyncExternalStore` against the registry internally — that layer is
done for you.)

---

## 11. The condensed DON'T list

State & identity

1. **Don't create atoms in components, hooks, or render paths** — module scope, `Atom.family`, or `ScopedAtom` only.
2. **Don't apply combinators (`keepAlive`, `map`, `swr`, …) at use sites** — they return new atoms; apply once at the definition.
3. **Don't hand-roll atom caches with `Map`** — that's `Atom.family`, with GC.
4. **Don't combine `Atom.family` + `Atom.keepAlive` with unbounded keys** — guaranteed leak; use `setIdleTTL`.
5. **Don't mark everything `keepAlive`** — you're disabling the GC that makes atoms cheap.
6. **Don't store derivable data in a second writable atom** — derive it.
7. **Don't mirror atom state into `useState`/`useReducer`** — one source of truth.

React usage

8. **Don't use `useAtom` when you only write** — `useAtomSet` avoids the subscription entirely.
9. **Don't pass inline selectors or inline subscribe callbacks** — stable references or module-level derived atoms.
10. **Don't unwrap `AsyncResult` with `getOrThrow` in render** — match on it; every load starts at `Initial`.
11. **Don't blank the UI while `waiting`** — previous values are preserved across refreshes and failures on purpose.
12. **Don't use `useAtomSuspense` without an ErrorBoundary** — failures are thrown.
13. **Don't write atoms during render** — seed with `initialValues`/`useAtomInitialValues`.
14. **Don't pass a raw function to a value-mode setter expecting it to be stored** — it's called as an updater.

Effects & data flow

15. **Don't run mutations with `Effect.runPromise` from event handlers** — `runtime.fn` + `useAtomSet` (promise mode if you need the outcome).
16. **Don't choreograph refreshes manually after mutations** — declare `reactivityKeys` on both sides.
17. **Don't expect `Atom.fn` bodies to be reactive** — reads inside fn atoms are untracked.
18. **Don't use `get.once` unless you specifically want a frozen snapshot.**
19. **Don't hand-roll optimistic updates, SWR, polling, debounce, or pagination** — `Atom.optimistic(Fn)`, `Atom.swr`, `Atom.withRefresh`, `Atom.debounce`, `Atom.pull` exist and handle rollback/cancellation.
20. **Don't hand-roll fetch atoms for an `HttpApi`/`RpcGroup`** — `AtomHttpApi.Service` / `AtomRpc.Service`.

Architecture

21. **Don't nest registries to scope state** — registries share nothing; use `ScopedAtom`.
22. **Don't rely on the implicit global registry in tests or SSR** — fresh registry per test / per request.
23. **Don't build service runtimes outside `Atom.runtime`** — layer lifetimes are tied to the registry for a reason.
24. **Don't branch on `NODE_ENV` or module-mock atom files for tests** — inject layers via `Atom.initialValue(runtime.layer, TestLayer)`.
25. **Don't touch `localStorage`/URL imperatively next to atoms** — `Atom.kvs` / `Atom.searchParam`.
26. **Don't let client-only effects run during SSR** — `Atom.withServerValueInitial`.

---

## Appendix: quick reference

| Task | API |
|---|---|
| local writable state | `Atom.make(value)` |
| derived state | `Atom.make((get) => ...)` / `Atom.map` |
| async data | `runtime.atom(effect)` → `AsyncResult` |
| live/push data | `runtime.atom(stream)` |
| mutation | `runtime.fn(effect, { reactivityKeys })` |
| parameterized | `Atom.family((arg) => ...)` |
| pagination | `runtime.pull(stream)` |
| survive unmount | `Atom.keepAlive` / `Atom.setIdleTTL(duration)` |
| cache policy | `Atom.swr({ staleTime, revalidateOnFocus })` |
| polling | `Atom.withRefresh(duration)` |
| optimistic UI | `Atom.optimistic` + `Atom.optimisticFn` |
| persisted | `Atom.kvs({ runtime, key, schema, defaultValue })` |
| URL param | `Atom.searchParam(name, { schema })` |
| atomic writes | `Atom.batch(() => ...)` |
| HTTP client | `AtomHttpApi.Service()(...)` — `.query` / `.mutation` |
| RPC client | `AtomRpc.Service()(...)` — `.query` / `.mutation` |
| per-subtree state | `ScopedAtom.make(() => ...)` |
| registry-free cell | `AtomRef.make(value)` |
| SSR | `Atom.serializable` + `Hydration.dehydrate` + `<HydrationBoundary>` |
| test injection | `Atom.initialValue(runtime.layer, TestLayer)` |
