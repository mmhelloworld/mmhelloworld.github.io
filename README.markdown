# mmhelloworld.github.io

The source for [mmhelloworld.github.io](https://mmhelloworld.github.io) — *The Joy of Programming!*

Built with [Astro](https://astro.build) using the [Sify](https://github.com/santisify/astro-theme-sify)
theme, and deployed to GitHub Pages via GitHub Actions. (Migrated from Octopress/Jekyll.)

## Develop

```bash
npm install
npm run dev        # local dev server at http://localhost:4321
npm run build      # production build into dist/
npm run preview    # preview the production build
```

## Structure

- `src/content/blog/` — posts in Markdown/MDX. Front matter: `title`, `description`, `date`
  (bare `YYYY-MM-DD`), `tags`, optional `category`/`cover`/`draft`.
- `src/pages/post/[...slug].astro` — posts are served at `/post/<slug>` (slug = filename).
- `astro.config.ts` — `redirects` maps every old Jekyll URL (`/blog/YYYY/MM/DD/title/`) to its new
  `/post/<slug>` location, so existing links keep working.
- Feeds: `/atom.xml` (Atom, primary — the original Jekyll feed path) and `/rss.xml` (RSS alias),
  both with full post content, built from one source in `src/utils/feed.ts`
  (`src/pages/atom.xml.ts` / `rss.xml.ts`). Browser-view stylesheets: `public/atom-styles.xsl`,
  `public/rss-styles.xsl`.
- `src/consts.ts` — site metadata, nav, social links, and giscus config.
- `src/components/`, `src/layouts/`, `src/styles/global.css` — theme (Tailwind v4).
- `public/` — static assets served at the site root (`/images`, `/downloads`, favicons).
- `.github/workflows/deploy.yml` — builds and publishes to GitHub Pages on push to `master`.

## Adding a post

Create `src/content/blog/my-post-title.md`:

```markdown
---
title: "My Post Title"
description: "One-line summary for cards, SEO, and the feed."
date: 2026-01-15
tags: [Idris, JVM]
---

Post body in Markdown…
```

The URL becomes `/post/my-post-title`.

## Comments

Comments use [giscus](https://giscus.app) (GitHub Discussions). Set the real `repoId`/`categoryId`
in `src/consts.ts` (comments stay hidden until configured). Old Disqus threads are not carried over.

## Deployment

On push to `master`, GitHub Actions builds the site and deploys it to GitHub Pages.
Repo **Settings → Pages → Source** must be set to **GitHub Actions**.
