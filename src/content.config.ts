import { defineCollection, z } from 'astro:content';
import { glob } from 'astro/loaders';

const blog = defineCollection({
  loader: glob({ pattern: '**/*.md', base: './src/content/blog' }),
  schema: z.object({
    title: z.string(),
    // Bare YYYY-MM-DD in front matter -> parsed as UTC midnight by z.coerce.date().
    // The post URL day is derived with getUTC* accessors (see src/utils/permalink.ts),
    // so it is stable regardless of the build machine's timezone.
    date: z.coerce.date(),
    tags: z.array(z.string()).default([]),
  }),
});

export const collections = { blog };
