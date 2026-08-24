const storageAcquire = 'Intrinsic.systemAllocationAcquire'

/**
 * Keeps low-level allocation-origin fixtures focused on provenance while adapting the sealed
 * storage refusal into the ordinary source allocation error. The compiler never recognizes the
 * adapter names; this is the same policy boundary implemented by `silk/core.SystemAllocator`.
 */
export const ordinaryStorageSource = (source: string): string => {
  if (!source.includes(storageAcquire)) return source
  return `import silk.core { OutOfMemoryError as TestStorageFailure }
import silk.layout { Layout as TestStorageLayout }
${source.replaceAll(storageAcquire, 'testStorageAcquire')}
effect fn testStorageRefused(
  error: Intrinsic.StorageFailure
) -> Allocation ! TestStorageFailure {
  drop error
  fail TestStorageFailure {}
}
effect fn testRawStorageAcquire(
  layout: TestStorageLayout
) -> Allocation ! Intrinsic.StorageFailure {
  return run Intrinsic.systemAllocationAcquire(move layout)
}
effect fn testStorageAcquire(
  layout: TestStorageLayout
) -> Allocation ! TestStorageFailure {
  return run Intrinsic.catchFailure<Intrinsic.StorageFailure>(
    testRawStorageAcquire(move layout),
    testStorageRefused
  )
}`
}
