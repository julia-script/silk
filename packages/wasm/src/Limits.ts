/**
 * Size limits shared by table and memory declarations.
 *
 * @since 0.0.0
 */

/**
 * Minimum and optional maximum size of a table (in elements) or memory (in 64 KiB pages).
 *
 * @category limits
 * @since 0.0.0
 */
export interface Limits {
  readonly min: number | bigint
  readonly max?: number | bigint
}
