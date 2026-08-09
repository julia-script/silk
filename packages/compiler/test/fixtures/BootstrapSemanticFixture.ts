export const acceptedSource = 'pub fn main() -> i32 { return 42 }'
export const missingNameSource = 'pub fn () -> i32 { return 42 }'
export const unknownTypeSource = 'pub fn main() -> Mystery { return 42 }'
export const damagedTypeSource = 'pub fn main() -> @ { return 42 }'
export const i32BoundarySource = 'pub fn main() -> i32 { return 2147483647 }'
export const overflowSource = 'pub fn main() -> i32 { return 2147483648 }'
export const missingIntegerSource = 'pub fn main() -> i32 { return }'
export const mixedDamageSource = 'pub fn () -> Mystery { return 42 }'
export const beyondSafeIntegerSource = 'pub fn main() -> i32 { return 90071992547409931234567890 }'
export const laterSemanticDamageSource = `${acceptedSource}
pub fn ignored() -> Mystery { return 2147483648 }`
export const laterSyntaxDamageSource = `${acceptedSource}
pub fn ignored() -> @ { return }`
export const twoFunctionSource = `pub fn answer() -> i32 { return 42 }
pub fn main() -> i32 { return 0 }`
export const threeFunctionSource = `pub fn one() -> i32 { return 1 }
pub fn two() -> i32 { return 2 }
pub fn three() -> i32 { return 3 }`
export const missingSecondNameSource = `${acceptedSource}
pub fn () -> i32 { return 0 }`
export const duplicateNameSource = `pub fn same() -> i32 { return 1 }
pub fn same() -> i32 { return 2 }`
export const tripleDuplicateNameSource = `${duplicateNameSource}
pub fn same() -> i32 { return 3 }`
export const mixedFunctionDamageSource = `${acceptedSource}
pub fn damaged() -> Mystery { return 2147483648 }`
export const parserAndSemanticDamageSource = `pub fn same() -> i32 { return 1 }
pub fn () -> Mystery { return 2 }
pub fn same() -> i32 { return 2147483648 }`
export const validCallSource = `pub fn answer() -> i32 { return 42 }
pub fn main() -> i32 { return answer() }`
export const missingCallCalleeSource = 'pub fn main() -> i32 { return () }'
export const missingCallRightParenthesisSource = `pub fn answer() -> i32 { return 42 }
pub fn main() -> i32 { return answer( }`
export const identityCallSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(42) }`
export const twoArgumentCallSource = `pub fn choose(left: i32, right: i32) -> i32 { return left }
pub fn main() -> i32 { return choose(1, 2) }`
export const tooFewArgumentsSource = `pub fn choose(left: i32, right: i32) -> i32 { return left }
pub fn main() -> i32 { return choose(1) }`
export const tooManyArgumentsSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(1, 2) }`
export const unavailableParameterContractSource = `pub fn identity(value: Mystery) -> i32 { return 0 }
pub fn main() -> i32 { return identity(42) }`
export const unavailableArgumentContractSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(missing) }`
export const recoveredArgumentSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(:) }`
export const nestedCallSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(identity(42)) }`
export const nestedSiblingCallsSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn choose(left: i32, right: i32) -> i32 { return left }
pub fn main() -> i32 { return choose(identity(1), identity(2)) }`
export const unresolvedNestedCallSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(missing(42)) }`
export const incompatibleNestedCallSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(identity()) }`
export const damagedNestedCallSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(identity(:)) }`
export const twoParameterSource = 'pub fn choose(left: i32, right: i32) -> i32 { return left }'
export const duplicateParameterSource =
  'pub fn choose(value: i32, value: i32) -> i32 { return value }'
export const tripleDuplicateParameterSource =
  'pub fn choose(value: i32, value: i32, value: i32) -> i32 { return value }'
export const unknownParameterReferenceSource = 'pub fn main() -> i32 { return missing }'
export const crossFunctionParameterSource = `pub fn owner(value: i32) -> i32 { return value }
pub fn other() -> i32 { return value }`
export const sameParameterNamesSource = `pub fn first(value: i32) -> i32 { return value }
pub fn second(value: i32) -> i32 { return value }`
export const unknownParameterTypeSource = 'pub fn identity(value: Mystery) -> i32 { return value }'
export const missingParameterNameSource = 'pub fn identity(: i32) -> i32 { return 0 }'
export const missingParameterTypeSource = 'pub fn identity(value:) -> i32 { return 0 }'
export const damagedIdentifierSource = 'pub fn main(value: i32) -> i32 { return @ value }'
export const forwardCallSource = `pub fn main() -> i32 { return answer() }
pub fn answer() -> i32 { return 42 }`
export const selfCallSource = 'pub fn main() -> i32 { return main() }'
export const unknownCallSource = 'pub fn main() -> i32 { return missing() }'
export const ambiguousCallSource = `pub fn same() -> i32 { return 1 }
pub fn same() -> i32 { return 2 }
pub fn main() -> i32 { return same() }`
export const unresolvedTargetTypeCallSource = `pub fn answer() -> Mystery { return 42 }
pub fn main() -> i32 { return answer() }`
export const damagedTargetBodyCallSource = `pub fn answer() -> i32 { return 2147483648 }
pub fn main() -> i32 { return answer() }`
export const mixedResolutionDamageSource = `pub fn same() -> Mystery { return 2147483648 }
pub fn same() -> i32 { return missing() }`
