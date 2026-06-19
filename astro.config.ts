import { readFileSync } from 'node:fs';
import { defineConfig } from 'astro/config';
import tailwindcss from '@tailwindcss/vite';
import mdx from '@astrojs/mdx';
import sitemap from '@astrojs/sitemap';
import remarkMath from 'remark-math';
import rehypeKatex from 'rehype-katex';

// Idris 2 TextMate grammar (MIT, from j-nava/idris2-vscode). Shiki has no
// built-in Idris grammar, so register this one and expose it as `idris`/`idris2`.
const idris2Grammar = JSON.parse(
  readFileSync(new URL('./src/grammars/idris2.tmLanguage.json', import.meta.url), 'utf-8')
);

// Preserve the old Jekyll/Octopress permalinks: redirect each historical
// /blog/YYYY/MM/DD/slug/ URL to its new Sify /post/slug location.
const OLD_TO_NEW: Record<string, string> = {
  '/blog/2013/07/10/frege-hello-java/': '/post/frege-hello-java',
  '/blog/2014/03/15/frege-record-accessors-and-mutators/':
    '/post/frege-record-accessors-and-mutators',
  '/blog/2016/02/27/haskell-on-the-jvm-via-ghcjs-and-nashorn/':
    '/post/haskell-on-the-jvm-via-ghcjs-and-nashorn',
  '/blog/2017/01/06/introducing-idris-on-the-jvm-and-an-idris-android-example/':
    '/post/introducing-idris-on-the-jvm-and-an-idris-android-example',
  '/blog/2017/01/10/idris-jvm-guarding-against-java-null-using-maybe-type/':
    '/post/idris-jvm-guarding-against-java-null-using-maybe-type',
  '/blog/2018/02/11/idris-jvm-automated-ffi-with-null-safety/':
    '/post/idris-jvm-automated-ffi-with-null-safety',
  '/blog/2020/12/30/idris-2-bootstrap-compiler-on-the-jvm-with-a-jvm-backend/':
    '/post/idris-2-bootstrap-compiler-on-the-jvm-with-a-jvm-backend',
  '/blog/2021/07/24/idris-2-initial-release-0-dot-2-1-for-the-jvm/':
    '/post/idris-2-initial-release-0-dot-2-1-for-the-jvm',
  '/blog/2024/07/15/idris-jvm-0-7-0-release/': '/post/idris-jvm-0-7-0-release',
  // Old Jekyll feed path -> Sify feed path.
  '/atom.xml': '/rss.xml',
};

export default defineConfig({
  // User GitHub Pages site — served from the domain root (no `base`).
  site: 'https://mmhelloworld.github.io',
  redirects: OLD_TO_NEW,
  integrations: [mdx(), sitemap()],
  vite: {
    plugins: [tailwindcss()],
  },
  markdown: {
    remarkPlugins: [remarkMath],
    rehypePlugins: [rehypeKatex],
    shikiConfig: {
      theme: 'github-dark',
      wrap: true,
      langs: [{ ...idris2Grammar, name: 'idris2', aliases: ['idris'] }],
    },
  },
});
