/** Stable C ABI symbol for one sealed OS intrinsic. */
export const osRuntimeSymbol = (name: string): string => {
  const words = name
    .replace(/^os/, '')
    .replaceAll(/([a-z])([A-Z])/g, '$1_$2')
    .toLowerCase()
  return `silk_os_${words}_v1`
}
