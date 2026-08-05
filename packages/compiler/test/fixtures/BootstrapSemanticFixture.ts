export const acceptedSource = 'pub fn main() -> I32 { return 42 }'
export const missingNameSource = 'pub fn () -> I32 { return 42 }'
export const unknownTypeSource = 'pub fn main() -> Mystery { return 42 }'
export const damagedTypeSource = 'pub fn main() -> @ { return 42 }'
export const i32BoundarySource = 'pub fn main() -> I32 { return 2147483647 }'
export const overflowSource = 'pub fn main() -> I32 { return 2147483648 }'
export const missingIntegerSource = 'pub fn main() -> I32 { return }'
export const mixedDamageSource = 'pub fn () -> Mystery { return 42 }'
export const beyondSafeIntegerSource = 'pub fn main() -> I32 { return 90071992547409931234567890 }'
export const laterSemanticDamageSource = `${acceptedSource}
pub fn ignored() -> Mystery { return 2147483648 }`
export const laterSyntaxDamageSource = `${acceptedSource}
pub fn ignored() -> @ { return }`
export const twoFunctionSource = `pub fn answer() -> I32 { return 42 }
pub fn main() -> I32 { return 0 }`
export const threeFunctionSource = `pub fn one() -> I32 { return 1 }
pub fn two() -> I32 { return 2 }
pub fn three() -> I32 { return 3 }`
export const missingSecondNameSource = `${acceptedSource}
pub fn () -> I32 { return 0 }`
export const duplicateNameSource = `pub fn same() -> I32 { return 1 }
pub fn same() -> I32 { return 2 }`
export const tripleDuplicateNameSource = `${duplicateNameSource}
pub fn same() -> I32 { return 3 }`
export const mixedFunctionDamageSource = `${acceptedSource}
pub fn damaged() -> Mystery { return 2147483648 }`
export const parserAndSemanticDamageSource = `pub fn same() -> I32 { return 1 }
pub fn () -> Mystery { return 2 }
pub fn same() -> I32 { return 2147483648 }`
export const validCallSource = `pub fn answer() -> I32 { return 42 }
pub fn main() -> I32 { return answer() }`
export const missingCallCalleeSource = 'pub fn main() -> I32 { return () }'
export const missingCallRightParenthesisSource = `pub fn answer() -> I32 { return 42 }
pub fn main() -> I32 { return answer( }`
export const identityCallSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(42) }`
export const twoArgumentCallSource = `pub fn choose(left: I32, right: I32) -> I32 { return left }
pub fn main() -> I32 { return choose(1, 2) }`
export const tooFewArgumentsSource = `pub fn choose(left: I32, right: I32) -> I32 { return left }
pub fn main() -> I32 { return choose(1) }`
export const tooManyArgumentsSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(1, 2) }`
export const unavailableParameterContractSource = `pub fn identity(value: Mystery) -> I32 { return 0 }
pub fn main() -> I32 { return identity(42) }`
export const unavailableArgumentContractSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(missing) }`
export const recoveredArgumentSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(@) }`
export const nestedCallSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42)) }`
export const damagedNestedCallSource = `pub fn identity(value: I32) -> I32 { return value }
pub fn main() -> I32 { return identity(identity(42) }`
export const twoParameterSource = 'pub fn choose(left: I32, right: I32) -> I32 { return left }'
export const duplicateParameterSource =
  'pub fn choose(value: I32, value: I32) -> I32 { return value }'
export const tripleDuplicateParameterSource =
  'pub fn choose(value: I32, value: I32, value: I32) -> I32 { return value }'
export const unknownParameterReferenceSource = 'pub fn main() -> I32 { return missing }'
export const crossFunctionParameterSource = `pub fn owner(value: I32) -> I32 { return value }
pub fn other() -> I32 { return value }`
export const sameParameterNamesSource = `pub fn first(value: I32) -> I32 { return value }
pub fn second(value: I32) -> I32 { return value }`
export const unknownParameterTypeSource = 'pub fn identity(value: Mystery) -> I32 { return value }'
export const missingParameterNameSource = 'pub fn identity(: I32) -> I32 { return 0 }'
export const missingParameterTypeSource = 'pub fn identity(value:) -> I32 { return 0 }'
export const damagedIdentifierSource = 'pub fn main(value: I32) -> I32 { return @ value }'
export const forwardCallSource = `pub fn main() -> I32 { return answer() }
pub fn answer() -> I32 { return 42 }`
export const selfCallSource = 'pub fn main() -> I32 { return main() }'
export const unknownCallSource = 'pub fn main() -> I32 { return missing() }'
export const ambiguousCallSource = `pub fn same() -> I32 { return 1 }
pub fn same() -> I32 { return 2 }
pub fn main() -> I32 { return same() }`
export const unresolvedTargetTypeCallSource = `pub fn answer() -> Mystery { return 42 }
pub fn main() -> I32 { return answer() }`
export const damagedTargetBodyCallSource = `pub fn answer() -> I32 { return 2147483648 }
pub fn main() -> I32 { return answer() }`
export const mixedResolutionDamageSource = `pub fn same() -> Mystery { return 2147483648 }
pub fn same() -> I32 { return missing() }`
