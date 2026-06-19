import { buildFeed, withStylesheet } from '../utils/feed';

export async function GET(context: { site: URL }) {
  const feed = await buildFeed(context.site);
  const xml = withStylesheet(feed.atom1(), '/atom-styles.xsl');
  return new Response(xml, {
    headers: { 'Content-Type': 'application/atom+xml; charset=utf-8' },
  });
}
