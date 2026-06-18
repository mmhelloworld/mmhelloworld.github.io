import rss from '@astrojs/rss';
import { getCollection } from 'astro:content';
import { SITE_TITLE, SITE_DESCRIPTION } from '../consts';

// Recreates the post URL: /blog/YYYY/MM/DD/slug/ (UTC, matching the post route).
function postPath(date, slug) {
  const y = date.getUTCFullYear();
  const m = String(date.getUTCMonth() + 1).padStart(2, '0');
  const d = String(date.getUTCDate()).padStart(2, '0');
  return `/blog/${y}/${m}/${d}/${slug}/`;
}

export async function GET(context) {
  const posts = (await getCollection('blog')).sort(
    (a, b) => b.data.date.getTime() - a.data.date.getTime()
  );

  return rss({
    title: SITE_TITLE,
    description: SITE_DESCRIPTION,
    site: context.site,
    items: posts.map((post) => {
      const slug = post.id.replace(/\.[^.]+$/, '');
      return {
        title: post.data.title,
        pubDate: post.data.date,
        categories: post.data.tags,
        link: postPath(post.data.date, slug),
      };
    }),
  });
}
