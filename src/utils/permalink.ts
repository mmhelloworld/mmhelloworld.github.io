import type { CollectionEntry } from 'astro:content';

/**
 * Compute a post's canonical URL path, preserving the original Jekyll/Octopress
 * permalink structure: /blog/YYYY/MM/DD/slug/ (with trailing slash).
 *
 * UTC accessors are used so a bare `YYYY-MM-DD` front-matter date (parsed as UTC
 * midnight) yields the exact same day no matter what timezone the build runs in.
 */
export function postPath(date: Date, slug: string): string {
  const y = date.getUTCFullYear();
  const m = String(date.getUTCMonth() + 1).padStart(2, '0');
  const d = String(date.getUTCDate()).padStart(2, '0');
  return `/blog/${y}/${m}/${d}/${slug}/`;
}

/** The slug for a blog entry: its file id without the extension. */
export function entrySlug(entry: CollectionEntry<'blog'>): string {
  return entry.id.replace(/\.[^.]+$/, '');
}

/** Convenience: full path for a collection entry. */
export function entryPath(entry: CollectionEntry<'blog'>): string {
  return postPath(entry.data.date, entrySlug(entry));
}
