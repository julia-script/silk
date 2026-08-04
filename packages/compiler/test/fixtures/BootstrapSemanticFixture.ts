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
