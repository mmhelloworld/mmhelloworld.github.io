import { getCollection } from 'astro:content';

function stripMarkdown(raw: string): string {
  return raw
    .replace(/```[\s\S]*?```/g, ' ')
    .replace(/`[^`]*`/g, ' ')
    .replace(/!\[.*?\]\(.*?\)/g, ' ')
    .replace(/\[([^\]]*)\]\(.*?\)/g, '$1')
    .replace(/<[^>]*>/g, ' ')
    .replace(/^#{1,6}\s+/gm, '')
    .replace(/(\*\*|__)(.*?)\1/g, '$2')
    .replace(/(\*|_)(.*?)\1/g, '$2')
    .replace(/~~(.*?)~~/g, '$1')
    .replace(/^\s*[-*+]\s+/gm, '')
    .replace(/^\s*\d+\.\s+/gm, '')
    .replace(/\n{2,}/g, ' ')
    .replace(/\s+/g, ' ');
}

export async function GET() {
  const blogPosts = await getCollection('blog');

  const entries = [
    ...blogPosts.map((p) => ({
      title: p.data.title,
      description: p.data.description || '',
      category: p.data.category || '',
      tags: p.data.tags || [],
      slug: p.id,
      type: 'blog',
      date: p.data.date.toISOString(),
      body: stripMarkdown(p.body || '').slice(0, 400),
    })),
  ];

  return new Response(JSON.stringify(entries), {
    headers: { 'Content-Type': 'application/json' },
  });
}
