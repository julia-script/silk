export const raise = (message: string): never => {
  throw new Error(message)
}

export const unreachable = (message = 'Reached unreachable code'): never => raise(message)
