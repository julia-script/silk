const independentPolicyActors = [
  ['Controller', 'Owner'],
  ['EventLoop', 'TimerReactor'],
  ['DelayToken', 'TimerGuard'],
  ['DeniedAllocator', 'ExhaustedAllocator'],
  ['ChannelState', 'PortState'],
  ['ChannelToken', 'PortGuard'],
  ['PendingCondition', 'Condition'],
  ['DispatchQueue', 'Inbox'],
  ['WorkSet', 'Tasks'],
  ['activateIdentity', 'driveIdentity'],
  ['tickLocal', 'poll'],
  ['advancePort', 'resume'],
] as const

/** Renames every ordinary policy actor while preserving the sealed Intrinsic vocabulary. */
export const renameIndependentPolicy = (source: string): string => {
  const policyRenamed = independentPolicyActors.reduce(
    (renamed, [replacement, original]) =>
      renamed.replace(new RegExp(`\\b${original}\\b`, 'g'), replacement),
    source,
  )
  return policyRenamed
    .replaceAll('import silk.execution as Execution', 'import silk.execution as ExecutionFacade')
    .replace(/\bExecution\./g, 'ExecutionFacade.')
    .replace(/import silk\.allocator \{([^}]*)\}/g, (declaration) =>
      declaration
        .replace(/\bAllocator\b/g, '__SOURCE_ALLOCATOR__ as StoragePolicy')
        .replace(/\bSystemAllocator\b/g, '__SOURCE_SYSTEM_ALLOCATOR__ as HeapProvider'),
    )
    .replace(/(?<!\.)\bAllocator\b/g, 'StoragePolicy')
    .replace(/(?<!\.)\bSystemAllocator\b/g, 'HeapProvider')
    .replaceAll('__SOURCE_ALLOCATOR__', 'Allocator')
    .replaceAll('__SOURCE_SYSTEM_ALLOCATOR__', 'SystemAllocator')
}
