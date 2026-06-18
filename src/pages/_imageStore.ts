import path from 'node:path';

const blogRaw = import.meta.glob<{ default: any }>('../content/blog/**/*.{png,jpg,jpeg,gif,webp,svg}', {
  eager: true,
  query: '?url',
});

const weeklyRaw = import.meta.glob<{ default: any }>('../content/weekly/**/*.{png,jpg,jpeg,gif,webp,svg}', {
  eager: true,
  query: '?url',
});

const imageMap: Record<string, string> = {};

function buildMap(glob: typeof blogRaw) {
  for (const [key, mod] of Object.entries(glob)) {
    const val: any = mod;
    const inner = val?.default || val;
    const url = typeof inner === 'string' ? inner : (inner?.src || '');
    if (url) {
      const cleanKey = key.replace(/^\.\.\/content\//, '');
      imageMap[cleanKey] = url;
    }
  }
}

buildMap(blogRaw);
buildMap(weeklyRaw);

export function resolveCover(collection: string, entryId: string, cover?: string): string | undefined {
  if (!cover) return undefined;
  if (!cover.startsWith('./') && !cover.startsWith('../')) return cover;

  const idParts = entryId.split('/');
  idParts.pop();
  const contentDir = idParts.length > 0 ? `${collection}/${idParts.join('/')}/` : `${collection}/`;
  const lookupKey = path.normalize(`${contentDir}${cover}`);

  return imageMap[lookupKey] || cover;
}
