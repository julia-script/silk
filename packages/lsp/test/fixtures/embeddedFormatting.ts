export const source = `/// \`\`\`silk
/// pub fn example()->i32{return 7}
/// \`\`\`
pub fn main() -> i32 { return 42 }
`

export const canonical = `/// \`\`\`silk
/// pub fn example() -> i32 {
///   return 7
/// }
/// \`\`\`
pub fn main() -> i32 {
  return 42
}
`
