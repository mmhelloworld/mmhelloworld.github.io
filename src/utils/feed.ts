import { Feed } from 'feed';
import { getCollection, render } from 'astro:content';
import { experimental_AstroContainer as AstroContainer } from 'astro/container';
import { SITE_TITLE, SITE_DESCRIPTION, SITE_AUTHOR, SITE_URL } from '../consts';

// Build a `feed` instance from the blog collection, including full rendered
// post HTML. Both the Atom and RSS endpoints share this so the two feeds stay
// in sync. Memoized per site origin so we only render posts once per build.
const cache = new Map<string, Feed>();

export async function buildFeed(site: URL): Promise<Feed> {
  const origin = site.origin;
  const cached = cache.get(origin);
  if (cached) return cached;

  const posts = (await getCollection('blog', ({ data }) => !data.draft)).sort(
    (a, b) => b.data.date.getTime() - a.data.date.getTime()
  );

  const updated = posts.length ? posts[0].data.date : new Date(0);

  const feed = new Feed({
    title: SITE_TITLE,
    description: SITE_DESCRIPTION,
    id: `${origin}/`,
    link: `${origin}/`,
    language: 'en',
    updated,
    generator: 'Astro',
    copyright: `© ${updated.getUTCFullYear()} ${SITE_AUTHOR}`,
    feedLinks: {
      atom: `${origin}/atom.xml`,
      rss: `${origin}/rss.xml`,
    },
    author: { name: SITE_AUTHOR, link: SITE_URL },
  });

  const container = await AstroContainer.create();

  for (const post of posts) {
    const { Content } = await render(post);
    let html = await container.renderToString(Content);
    // Make root-relative URLs (images, links) absolute so they resolve in readers.
    html = html.replace(/(href|src)="\/(?!\/)/g, `$1="${origin}/`);

    const url = `${origin}/post/${post.id}`;
    feed.addItem({
      title: post.data.title,
      id: url,
      link: url,
      description: post.data.description || undefined,
      content: html,
      date: post.data.date,
      category: (post.data.tags || []).map((name) => ({ name })),
      author: [{ name: SITE_AUTHOR, link: SITE_URL }],
    });
  }

  cache.set(origin, feed);
  return feed;
}

/** Insert an XML stylesheet processing instruction after the XML declaration. */
export function withStylesheet(xml: string, href: string): string {
  return xml.replace(
    /^(<\?xml[^>]*\?>)/,
    `$1\n<?xml-stylesheet type="text/xsl" href="${href}"?>`
  );
}
