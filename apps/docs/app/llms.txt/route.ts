import { source } from '@/lib/source';
import { llms } from 'fumadocs-core/source';

export const revalidate = false;

export function GET(): Response {
  return new Response(llms(source).index());
}
