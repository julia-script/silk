export const selectedForeignDollarSource = `
static if Intrinsic.targetOperatingSystem() == "darwin" {
  unsafe extern "C" fn closeSocket(fd: i32) -> i32 as "close$NOCANCEL"
  unsafe extern "C" fn helperVersion() -> i32 as "helper$version"
  pub fn main() -> i32 { return unsafe closeSocket(2) + unsafe helperVersion() }
} else static if Intrinsic.targetOperatingSystem() == "linux" {
  unsafe extern "C" fn helperVersion() -> i32 as "helper$version"
  pub fn main() -> i32 { return unsafe helperVersion() + 2 }
} else {
  pub fn main() -> i32 { return 42 }
}`

export const linkedForeignDollarSource = `
unsafe extern "C" fn closeSocket(fd: i32) -> i32 as "close$NOCANCEL"
unsafe extern "C" fn helperVersion() -> i32 as "helper$version"
export "C" fn entry() -> i32 as "main" {
  return unsafe closeSocket(2) + unsafe helperVersion()
}
`

export const linkedForeignDollarCSource = `
int close$NOCANCEL(int descriptor) { return descriptor; }
int helper$version(void) { return 40; }
`
