/** Normalize a tag to a URL-safe slug, e.g. "JVM" -> "jvm", "C#" -> "c". */
export function tagSlug(tag: string): string {
  return tag
    .toLowerCase()
    .trim()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '');
}

export function tagPath(tag: string): string {
  return `/tags/${tagSlug(tag)}/`;
}
