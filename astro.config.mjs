// @ts-check
import { defineConfig } from 'astro/config';
import sitemap from '@astrojs/sitemap';

// https://astro.build/config
export default defineConfig({
  // User GitHub Pages site — served from the domain root, so NO `base`.
  site: 'https://mmhelloworld.github.io',
  // Match Jekyll's directory-style permalinks: /blog/YYYY/MM/DD/title/
  trailingSlash: 'always',
  integrations: [sitemap()],
  markdown: {
    shikiConfig: {
      themes: { light: 'github-light', dark: 'github-dark' },
      wrap: false,
    },
  },
});
