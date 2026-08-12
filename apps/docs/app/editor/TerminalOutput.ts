import { AsyncResult } from 'effect/unstable/reactivity'

export interface Handlers {
  readonly onFailure: (failure: unknown) => void
  readonly onSuccess: (value: string) => void
}

export const consume = (
  result: AsyncResult.AsyncResult<string, unknown>,
  handlers: Handlers,
): void => {
  AsyncResult.matchWithError(result, {
    onInitial: () => undefined,
    onError: handlers.onFailure,
    onDefect: handlers.onFailure,
    onSuccess: ({ value }) => handlers.onSuccess(value),
  })
}
